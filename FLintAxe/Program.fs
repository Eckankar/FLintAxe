open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices
open Rule
open Rules

let checker = FSharpChecker.Instance

let parseFile file =
    let f = Path.GetFullPath file
    let contents = File.ReadAllText f
    Rule.fullSourceLines <- File.ReadAllLines f
    let options = checker.GetProjectOptionsFromScript (f, contents)
                    |> Async.RunSynchronously

    checker.ParseFileInProject (f, contents, options)
      |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    if argv.Length < 1 then Environment.Exit 1 else ()

    let file = argv.[0]
    let parse = parseFile file

    if parse.ParseHadErrors then
        printfn "Error parsing '%s':" file
        parse.Errors
            |> Array.map (fun x -> x.ToString())
            |> String.concat "\n"
            |> printfn "%s"
        1
    else
        let ast = parse.ParseTree |> Option.get
        let results = List.map (fun (rule : LinterRule) -> rule.Run ast) rules |> List.concat
        if List.length results > 0
        then printfn "Found %d warnings:" (List.length results)
             List.iter (printfn "%A") results
        else printfn "No warnings found."

        Console.ReadKey () |> ignore
        0 // return an integer exit code