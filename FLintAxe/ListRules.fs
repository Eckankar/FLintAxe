module ListRules
    open Rule
    open Util
    open Microsoft.FSharp.Compiler.Ast

    /// [x :: xs]
    type ConsInSingletonList() =
        inherit LinterRule()

        override this.ProcessPattern pat =
            match pat with
                | SynPat.ArrayOrList (false, [pat'], r) ->
                    match pat' with
                       | SynPat.LongIdent (cons, _, _, Pats [SynPat.Tuple ([x; xs], _)], _, _) ->
                            if longIdEq cons.Lid ["op_ColonColon"]
                            then [LinterResult (r, "When you write [x::xs], you're saying that you want a list containing another list.\n"+
                                                   "In most cases when you do that, you most likely want to write (x::xs) instead. (i.e. with ( ) and not [ ]).")
                                 ]
                            else []
                       | _ -> []
                | _ -> []


    let (rules : LinterRule list) = [
        ConsInSingletonList()
    ]