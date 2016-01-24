//namespace Demo.Lambda

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type var = string
type term = 
  | Lambda of Variable:var * Body:term
  | Var of Variable:var
  | App of Left:term * Right:term

type Fn = delegate of Fn -> Fn

module Lambda =
  let lambdaSymbol = System.Char.ConvertFromUtf32(0x000003BB)
  let rec ToString this = 
    match this with
    | Lambda(var, body) -> 
        sprintf "%s%s.%s" lambdaSymbol var (ToString body)
    | Var(var) -> var
    | App(left,right) -> 
        sprintf "(%s %s)" (ToString left) (ToString right)
  let rec fromExpr (e:Microsoft.FSharp.Quotations.Expr) =
    match e with
    | Patterns.Lambda(v, e) -> Lambda(v.Name, fromExpr e)
    | Patterns.Application(left, right) -> App(fromExpr left, fromExpr right)
    | Patterns.Var(var) -> Var(var.Name)
    | Patterns.Call(Some(left), methodInfo, [right]) when methodInfo.Name = "Invoke" -> 
        App(fromExpr left, fromExpr right)
    | e -> 
        let typeName = e.Type.ToString()
        raise (System.Exception(sprintf "Cannot convert %s to lambda" typeName))
  let rec substitute (varName:var) (withExpr:term) (expr:term) =
    let doSubRec = substitute varName withExpr 
    match expr with
    | Var(name) when varName = name -> withExpr
    | App(left, right) -> App(doSubRec left, doSubRec right)
    | Lambda(var, body) when var <> varName -> Lambda(var, doSubRec body)
    | x -> x
  let (|NonShadowingLamda|_|) varName expr = 
    match expr with
    | Lambda(var, body) when var <> varName -> Some(NonShadowingLamda(var, body))
    | _ -> None
  let rec substitute2 (varName:var) (withExpr:term) (expr:term) =
    let doSubRec = substitute varName withExpr 
    match expr with
    | Var(name) when varName = name -> withExpr
    | App(left, right) -> App(doSubRec left, doSubRec right)
    | NonShadowingLamda varName (var, body) -> Lambda(var, doSubRec body)
    | x -> x

  let alphaNormalize (expr:term) =
    let rec normalizeRec (n:int) (expr:term) =
      let newVarName = sprintf "v%i" n
      let newVar = Var(newVarName)
      let normalize = normalizeRec (n + 1)
      match expr with
      | Lambda(var, body) as l -> 
        let newBody = normalize body
        let subbedVar = substitute var newVar newBody
        Lambda(newVarName, subbedVar)
      | App(left, right) -> App(normalize left, normalize right)
      | x -> x
    normalizeRec 0 expr
  let rec evalStep expr = 
    match expr with
    | App(Lambda(var, expr), right) -> substitute var right expr
    | App(left, right) -> App(evalStep left, evalStep right)
    | Lambda(var, body) -> Lambda(var, evalStep body)
    | x -> x

  let rec eval (maxSteps:int) (term:term) =
    seq {
      yield term
      let last = ref term
      let next = ref (evalStep term)
      while next.Value <> last.Value do
        yield next.Value
        last := next.Value
        next := evalStep next.Value
    } |> Seq.truncate maxSteps |> Seq.toList
  let printEval max term =
    let steps = term |> eval max
    for step in steps do
      printfn "%s" (ToString step)
    let last = steps |> List.rev |> List.head
    last

module BooleanLogic =
  let Id = <@@ fun x -> x @@> |> Lambda.fromExpr 
  let True = <@@ fun x y -> x @@> |> Lambda.fromExpr 
  let False = <@@ fun x y -> y @@> |> Lambda.fromExpr 
  let Not = <@@ fun p a b -> p b a @@> |> Lambda.fromExpr 
  let And = <@@ fun (p:Fn) (q:Fn) -> p.Invoke(q).Invoke(p) @@> |> Lambda.fromExpr 

  let ex1 = App(And, True) |> Lambda.printEval 10

  let Zero = <@@ fun f x -> x @@> |> Lambda.fromExpr 
  let One = <@@ fun f x -> f x @@> |> Lambda.fromExpr
  let Two = <@@ fun f x -> f (f x) @@> |> Lambda.fromExpr
  let Succ = <@@ fun n f x -> f (n f x) @@> |> Lambda.fromExpr
  let Plus = <@@ fun m n f x -> m f (n f x) @@> |> Lambda.fromExpr

  let ZeroSucc = App(Succ, Zero) |> Lambda.printEval 10
  let OnePlus = App(App(Succ, One),One) |> Lambda.printEval 10