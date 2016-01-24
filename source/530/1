type token =
    | WhiteSpace
    | Symbol of char
    | StrToken of string
    | NumToken of float
    | BoolToken of bool

let (|Match|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some m.Value else None

let bool = System.Boolean.Parse
let unquote (s:string) = s.Substring(1,s.Length-2)

let toToken = function
    | Match @"^\s+" s -> s, WhiteSpace
    | Match @"^""[^""\\]*(?:\\.[^""\\]*)*""" s -> s, s |> unquote |> StrToken
    | Match @"^\{|^\}|^\[|^\]|^:|^," s -> s, s.[0] |> Symbol
    | Match @"^\d+(\.\d+)?|\.\d+" s -> s, s |> float |> NumToken
    | Match @"^true|false" s -> s, s |> bool |> BoolToken
    | _ -> invalidOp "Unknown token"

let tokenize s =
    let rec tokenize' index (s:string) =
        if index = s.Length then []
        else
            let next = s.Substring index 
            let text, token = toToken next
            token :: tokenize' (index + text.Length) s
    tokenize' 0 s
    |> List.choose (function WhiteSpace -> None | t -> Some t)

type json =
    | Number of float
    | String of string
    | Boolean of bool
    | Array of json list
    | Object of (string * json) list
    | Null

let rec (|ValueRec|_|) = function
    | NumToken n::t -> Some(Number n, t)
    | BoolToken b::t -> Some(Boolean b, t)
    | StrToken s::t -> Some(String s, t)
    | Symbol '['::ValuesRec(vs, Symbol ']'::t) -> Some(Array vs,t)
    | Symbol '{'::PairsRec(ps, Symbol '}'::t) -> Some(Object ps,t)
    | [] -> Some(Null,[])
    | _ -> None
and (|ValuesRec|_|) = function
    | ValueRec(p,t) ->
        let rec aux p' = function
            | Symbol ','::ValueRec(p,t) -> aux (p::p') t
            | t -> p' |> List.rev,t
        Some(aux [p] t)
    | _ -> None
and (|PairRec|_|) = function
    | StrToken k::Symbol ':'::ValueRec(v,t) -> Some((k,v), t)
    | _ -> None
and (|PairsRec|_|) = function
    | PairRec(p,t) ->
        let rec aux p' = function
            | Symbol ','::PairRec(p,t) -> aux (p::p') t
            | t -> p' |> List.rev,t
        Some(aux [p] t)
    | _ -> None

let parse s = 
    tokenize s |> function 
    | ValueRec(v,[]) -> v
    | _ -> failwith "Failed to parse JSON"

module Test =
    let jsonString = "{
        \"Name\": \"Phil\",
        \"Phone\": 123456789
        }"
    let person = parse jsonString
    let name, phone = person |> function
        | Object(["Name", String name; "Phone",Number phone]) ->
            name, phone
        | _ -> invalidOp "Invalid person"