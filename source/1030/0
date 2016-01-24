// F# version of Jessica Kerr's blog post http://blog.jessitron.com/2013/01/from-imperative-to-data-flow-to.html
#load "/jbuedel/Attempt.fsx"

open System
open System.IO
open Attempt

let openFile filename = 
    try 
      System.IO.File.OpenRead filename |> succeed
    with 
    | _ -> fail
    
let readFirstLine (file:TextReader) =
    try 
        file.ReadLine() |> succeed
    with 
    | _ -> fail
    
let getSecret (line:string) =
    if line = null || line.Contains("'") = false then
        fail
    else
        line.Substring(1 + line.IndexOf("'")) |> succeed
    
let readSecret stream =
    attempt {
        let! line = readFirstLine stream
        let! secret = getSecret line
        return secret
    } |> (fun x -> x())

match (readSecret (new StringReader("hello there 'kind sir"))) with
    | None  -> printfn "%s" " Secret not found. "
    | Some(s) -> printfn "Secret is [%s]." s
