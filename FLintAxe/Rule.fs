module Rule
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open System.IO
// Here, you'll need this: https://github.com/fsharp/fsharp/blob/master/src/fsharp/ast.fs

let mutable fullSourceLines = [| |] : string []

let getHintString (range:range) =
    let lines = fullSourceLines.[range.StartLine - 1 .. range.EndLine - 1]
    if lines.Length > 1
    then Array.mapi (fun i (line:string) ->
        if i = 0 then line.Substring (range.StartColumn)
        else if i = lines.Length - 1 then line.Substring (0, range.EndColumn)
        else line) lines |> Array.toList |> String.concat ""
    else lines.[0].Substring (range.StartColumn, range.EndColumn - range.StartColumn)

type LinterResult (range : range, message) =
    member this.Range = range
    member this.Message = message 

    override this.ToString () =
        sprintf "Warning in %s @ (%d,%d)--(%d,%d)\nNear: %s\n%s"
                (Path.GetFileName this.Range.FileName)
                this.Range.StartLine
                this.Range.StartColumn
                this.Range.EndLine
                this.Range.EndColumn
                (getHintString this.Range)
                this.Message


type LinterRule () =
    abstract ExcludeModule : SynModuleOrNamespace -> bool
    abstract ExcludeDeclaration : SynModuleDecl -> bool
    abstract ExcludePattern : SynPat -> bool
    abstract ExcludeExpression : SynExpr -> bool

    default this.ExcludeModule      _ = false
    default this.ExcludeDeclaration _ = false
    default this.ExcludePattern     _ = false
    default this.ExcludeExpression  _ = false

    abstract ProcessModule : SynModuleOrNamespace -> LinterResult list
    abstract ProcessDeclaration : SynModuleDecl -> LinterResult list
    abstract ProcessPattern : SynPat -> LinterResult list
    abstract ProcessExpression : SynExpr -> LinterResult list

    default this.ProcessModule      _ = []
    default this.ProcessDeclaration _ = []
    default this.ProcessPattern     _ = []
    default this.ProcessExpression  _ = []

    member this.Run ast =
        match ast with
            | ParsedInput.ImplFile implFile ->
                let (ParsedImplFileInput (fn, script, name, _, _, modules, _)) = implFile
                List.map this.RunModule modules |> List.concat
            | ParsedInput.SigFile sigFile -> []

    member this.RunModule m =
        this.ProcessModule m @
        if this.ExcludeModule m then []
        else let (SynModuleOrNamespace (_,_,decls,_,_,_,_)) = m
             List.map this.RunDeclaration decls |> List.concat

    member this.RunDeclaration decl =
        this.ProcessDeclaration decl @
        if this.ExcludeDeclaration decl then []
        else match decl with
                | SynModuleDecl.Let (_, bindings, _) ->
                    List.map this.RunBinding bindings |> List.concat
                | _ -> []

    member this.RunBinding = function
        (Binding (_, _, _, _, _, _, _, pattern, _, body, _, _)) ->
            this.RunPattern pattern @ this.RunExpression body

    member this.RunPattern pattern =
        this.ProcessPattern pattern @
        if this.ExcludePattern pattern then []
        else match pattern with
               | SynPat.Named (pat, _, _, _, _)  -> this.RunPattern pat
               | SynPat.Typed (pat, _, _)        -> this.RunPattern pat
               | SynPat.Attrib (pat, _, _)       -> this.RunPattern pat
               | SynPat.Or (pat1, pat2, _)       -> this.RunPattern pat1 @ this.RunPattern pat2
               | SynPat.Ands (pats, _)           -> List.map this.RunPattern pats |> List.concat
               | SynPat.Tuple (pats, _)          -> List.map this.RunPattern pats |> List.concat
               | SynPat.Paren (pat, _)           -> this.RunPattern pat
               | SynPat.ArrayOrList (_, pats, _) -> List.map this.RunPattern pats |> List.concat
               | SynPat.Record (entries, _)      -> List.map (fun (_, pat) -> this.RunPattern pat) entries |> List.concat
               | SynPat.QuoteExpr (expr, _)      -> this.RunExpression expr
               | SynPat.FromParseError (pat, _)  -> this.RunPattern pat
               | _                               -> []

    member this.RunExpression expression =
        this.ProcessExpression expression @
        if this.ExcludeExpression expression then []
        else match expression with
               | SynExpr.Paren (expr, _, _, _)      -> this.RunExpression expr
               | SynExpr.Quote (op, _, expr, _, _)  -> this.RunExpression op @ this.RunExpression expr
               | SynExpr.Typed (expr, _, _)         -> this.RunExpression expr
               | SynExpr.Tuple (exprs, _, _)        -> List.map this.RunExpression exprs |> List.concat
               | SynExpr.ArrayOrList (_, exprs, _)  -> List.map this.RunExpression exprs |> List.concat
               | SynExpr.Record (ctor, copy, fields, _) ->
                   (Option.map (fun (_,expr,_,_,_) -> expr) ctor |> Option.toList) @
                   (Option.map (fun (expr, _) -> expr) copy |> Option.toList) @
                   (List.map (fun (_, mexpr, _) -> Option.toList mexpr) fields |> List.concat)
                      |> List.map this.RunExpression
                      |> List.concat
               | SynExpr.New (_, _, expr, _)        -> this.RunExpression expr
            // | SynExpr.ObjExpr
               | SynExpr.While (_, expr1, expr2, _) -> this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.For (_, _, expr1, _, expr2, expr3, _) ->
                    this.RunExpression expr1 @ this.RunExpression expr2 @ this.RunExpression expr3
               | SynExpr.ForEach (_, _, _, pat, expr1, expr2, _) ->
                    this.RunPattern pat @ this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.ArrayOrListOfSeqExpr (_, expr, _) -> this.RunExpression expr
               | SynExpr.CompExpr (_, _, expr, _)   -> this.RunExpression expr
               | SynExpr.Lambda (_, _, simPats, expr, _) -> this.RunSimplePatterns simPats @ this.RunExpression expr
               | SynExpr.MatchLambda (_, _, matchClauses, _, _) -> List.map this.RunMatchClause matchClauses |> List.concat
               | SynExpr.Match (_, expr, matchClauses, _, _)    ->
                    this.RunExpression expr @ (List.map this.RunMatchClause matchClauses |> List.concat)
               | SynExpr.Do (expr, _)                -> this.RunExpression expr
               | SynExpr.Assert (expr, _)            -> this.RunExpression expr
               | SynExpr.App (_, _, expr1, expr2, _) -> this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.TypeApp (expr,_,_,_,_,_,_)  -> this.RunExpression expr
               | SynExpr.LetOrUse (_, _, bindings, expr, _) ->
                    (List.map this.RunBinding bindings |> List.concat) @
                    this.RunExpression expr
               | SynExpr.TryWith (expr, _, matchClauses, _, _, _, _) ->
                    this.RunExpression expr @ (List.map this.RunMatchClause matchClauses |> List.concat)
               | SynExpr.TryFinally (expr1, expr2, _, _, _) -> this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.Lazy (expr, _)              -> this.RunExpression expr
               | SynExpr.Sequential (_, _, expr1, expr2, _) -> this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.IfThenElse (expr1, expr2, expr3m, _, _, _, _) ->
                    this.RunExpression expr1 @
                    this.RunExpression expr2 @
                    (Option.map this.RunExpression expr3m |> Option.toList |> List.concat)
               | SynExpr.LongIdentSet (_, expr, _) -> this.RunExpression expr
               | SynExpr.DotGet (expr, _, _, _) -> this.RunExpression expr
               | SynExpr.DotSet (expr1, _, expr2, _) -> this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.DotIndexedGet (expr, indices, _, _) ->
                    this.RunExpression expr @ (List.map this.RunIndices indices |> List.concat)
               | SynExpr.DotIndexedSet (expr1, indices, expr2, _, _, _) ->
                    this.RunExpression expr1 @
                    (List.map this.RunIndices indices |> List.concat) @
                    this.RunExpression expr2
               | SynExpr.NamedIndexedPropertySet (_, expr1, expr2, _) -> this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.DotNamedIndexedPropertySet (expr1, _, expr2, expr3, _) ->
                    this.RunExpression expr1 @ this.RunExpression expr2 @ this.RunExpression expr3
               | SynExpr.TypeTest (expr, _, _) -> this.RunExpression expr
               | SynExpr.Upcast (expr, _, _)   -> this.RunExpression expr
               | SynExpr.Downcast (expr, _, _) -> this.RunExpression expr
               | SynExpr.InferredUpcast (expr, _) -> this.RunExpression expr
               | SynExpr.InferredDowncast (expr, _) -> this.RunExpression expr
               | SynExpr.AddressOf (_, expr, _, _) -> this.RunExpression expr
               | SynExpr.TraitCall (_, _, expr, _) -> this.RunExpression expr
               | SynExpr.JoinIn (expr1, _, expr2, _) -> this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.YieldOrReturn (_, expr, _) -> this.RunExpression expr
               | SynExpr.YieldOrReturnFrom (_, expr, _) -> this.RunExpression expr
               | SynExpr.LetOrUseBang (_, _, _, pat, expr1, expr2, _) ->
                    this.RunPattern pat @ this.RunExpression expr1 @ this.RunExpression expr2
               | SynExpr.DoBang (expr, _) -> this.RunExpression expr
               | SynExpr.FromParseError (expr, _) -> this.RunExpression expr
               | SynExpr.DiscardAfterMissingQualificationAfterDot (expr, _) -> this.RunExpression expr
               | _ -> []

    member this.RunIndices = function
        | SynIndexerArg.One expr           -> this.RunExpression expr
        | SynIndexerArg.Two (expr1, expr2) -> this.RunExpression expr1 @ this.RunExpression expr2

    member this.RunMatchClause = function
        | SynMatchClause.Clause (pat, mexpr, expr, _, _) ->
            this.RunPattern pat @
            (Option.map this.RunExpression mexpr |> Option.toList |> List.concat) @
            this.RunExpression expr

    member this.RunSimplePatterns = function
        | SynSimplePats.SimplePats (simplePats, _) -> List.map this.RunSimplePattern simplePats |> List.concat
        | SynSimplePats.Typed (simplePats, _, _)   -> this.RunSimplePatterns simplePats

    member this.RunSimplePattern _ = []