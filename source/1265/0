//Example solver for x + y = z

//Input parameters
type Equation =
   { X: int option;
     Y: int option;
     Z: int option; }

//Input values
let equation =
  { X = None;
    Y = None;
    Z = None; }

//Each manipulation of x + y = z with all cases for too much or not enough provided information provided
let solveUnknown eq =
    match eq.X with 
    | Some x ->
      match eq.Y with
      | Some y ->
        match eq.Z with
        | Some z -> printfn "Too much info was provided"
        | None -> printfn "x + y = %d" (x + y)
      | None -> 
        match eq.Z with
        | Some z ->
          printfn "z - x = %d" (z - x)
        | None -> printfn "only x(%d) was provided" x
    | None -> 
      match eq.Y with
      | Some y -> 
        match eq.Z with
        | Some z -> 
          printfn "z - y = %d" (z - y)
        | None -> printfn " only y(%d) was provided" y
      | None ->
        match eq.Z with
        | Some z -> printfn "only z(%d) was provided" z
        | None -> printfn "no info was given";;

//Execute the equation
solveUnknown equation