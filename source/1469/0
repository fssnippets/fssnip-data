let checknumber number divisor =
    let divided = (float)number / (float)divisor
    if divided = floor divided then
        divided
        |> int
        |> printfn "%i = %i * %i" number divisor

let rec finddivisor number divisor numbers = 
    match numbers with
    | head :: tail -> 
        checknumber number head
        finddivisor number head tail
    | [] -> printfn "finish"

let divisions number =
    let list = [1..number]
    finddivisor number list.Head list

let ``find my divisors`` = 12345

divisions ``find my divisors``