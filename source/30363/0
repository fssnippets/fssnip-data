open System

let isUpper (str : string) =
    let rec strIter isUpper arr =
        match arr with
        | [] -> isUpper
        | _ -> 
            match Char.IsLower(arr.Head) with
            | true -> strIter false []
            | false -> strIter true arr.Tail

    strIter true (Array.toList <| str.ToCharArray())