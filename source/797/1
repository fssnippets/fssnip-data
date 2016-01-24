module String =
    let join (v:seq<string>) s = System.String.Join(s, v)
    let equalsOrdinalIgnoreCase a b = System.String.Equals(a, b, System.StringComparison.OrdinalIgnoreCase)

module Union =
    open Microsoft.FSharp.Reflection
    let getName (x:'a) = match FSharpValue.GetUnionFields(x, typeof<'a>) with case, _ -> case.Name
    let getInfo v t = FSharpType.GetUnionCases t |> Seq.tryFind (fun i -> String.equalsOrdinalIgnoreCase i.Name v)
    let makeUnion<'a> v = FSharpValue.MakeUnion(v, null) :?> 'a
    let parse<'a> v :'a option =
        let i = getInfo v typeof<'a>
        if i = None then None else Some <| makeUnion i.Value
//    let getCaseNames<'ty> () = FSharpType.GetUnionCases(typeof<'ty>) |> Array.map (fun info -> info.Name)

type Descriptor = { Name : string; Description : string }
let descriptor = { Name = null; Description = null }
let environment = { Name = "Environment"; Description= "The simulate world is all of white, white space, white planet. Color, odor and position mean nothing here." }

module AI =
    module Language =
        let localize v culture = if v = "Hello World" && culture = "zh-CN" then "您好" else v

    type Stat = Idle | Walking | Closed
    type Conversation = ``Hello world`` | ``Started walking`` | ``Already walking`` | ``Stoped walking`` | ``Just idle`` | Bye
    type Operator = { Culture : string }
    let defaultOperator = { Culture = "en-US" }

    let part = { descriptor with Description = "It's a part, I don't know how to use it." }
    type Machine = { Model : string; Parts : seq<Descriptor> }
    type R = { Name : string; Stat : Stat; Operator : Operator; LastConversation : string; Body : Machine  }
    let defaultMachine = { Model = "Simulate model 1130"; Parts = [ { part with Name = "Eyes" }; { part with Name = "Nose" }; { part with Name = "Feet"; Description = "Use feet for walking." } ] }
    let defaultAI = { Name = "AI"; Stat = Idle; Operator = defaultOperator; LastConversation = "Initialized."; Body = defaultMachine }

    let say v m = { m with LastConversation = Language.localize (Union.getName v + ".") m.Operator.Culture }
    let create operator = say ``Hello world`` { defaultAI with Operator = operator }
    let close m = say Bye { m with Stat = Closed }
    let walk m = if m.Stat = Idle then say ``Started walking`` { m with Stat = Walking } else say ``Already walking`` m
    let stop m = if m.Stat = Walking then say ``Stoped walking`` { m with Stat = Idle } else say ``Just idle`` m
    
    module Knowledge =
        let d = [ "I", "I am AI."; "Programmer", "Programmer is a_a."; "Operator", "You are operator." ]
        let s m = seq {
            for i in d -> { Name = fst i; Description = snd i }
            yield { Name = "Stat"; Description = Union.getName m.Stat }
            yield { Name = "Body"; Description = m.Body.Model }
            yield { Name = "Parts"; Description = sprintf "%s." <| String.join (m.Body.Parts |> Seq.map (fun i -> i.Name)) ", " }
            for i in m.Body.Parts -> i
            yield { Name = "Actions"; Description = "Walk, Stop, Close." } }
        let get v m =
            let r = s m |> Seq.tryFind (fun p -> String.equalsOrdinalIgnoreCase p.Name v)
            if r = None then "I don't understand." else r.Value.Description
        let describe m = sprintf "%s\n%s\n%s\nI have these knowledge: %s.\nI can do these actions: %s\n" (get "I" m) (get "Programmer" m)  (get "Operator" m) (String.join (s m |> Seq.map (fun i -> i.Name)) ", ") (get "Actions" m)

    let name = "AI"
    let body = defaultMachine
    let knowledge v m =  { m with LastConversation = Knowledge.get v m }
    let describe m = Knowledge.describe m

open AI
let talk f =
    let r = f
    printfn "%s: %s" r.Name r.LastConversation
    r
let create o =
    let r = talk <| create o
    printfn "%s" <| describe r
    r
//let create = talk <| create { defaultOperator with Culture = "zh-CN" }
type Command = Walk | Stop | Exit | Close
let proc m v =
    talk <|
        match Union.parse v with
        | Some c ->
            match c with
            | Walk -> walk m
            | Stop -> stop m
            | Exit | Close -> close m
        | None -> knowledge v m
let sim o =
    async {
        let tracks = Seq.unfold (fun state -> if state.Stat = Closed then None else Some(state, proc state (System.Console.ReadLine()))) (create o)
        ignore <| Seq.length tracks }

Async.StartImmediate <| sim defaultOperator