
let maxDiv = 20
let mutable num = 1

let chkDiv num =
    seq {1..maxDiv}
    |> Seq.forall 
        (fun x -> num % x = 0)

while not <| (chkDiv num) 
    do num <- num + 1
    
printfn "Number Found : %d" num