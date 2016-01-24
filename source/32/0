// create an infinite list of fibonacci numbers
let fibs =
    Seq.unfold
        (fun (n0, n1) ->
            Some(n0, (n1, n0 + n1)))
        (1I,1I)

// take the first twenty items from the list
let first20 = Seq.take 20 fibs

// print the finite list
printfn "%A" first20