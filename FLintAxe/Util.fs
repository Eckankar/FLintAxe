module Util
    open Microsoft.FSharp.Compiler.Ast

    // Is LongIdent "lid" equal to reference string list "ref"?
    let longIdEq (lid : LongIdent) (ref : string list) =
        List.map (fun (e:Ident) -> e.idText) lid = ref

