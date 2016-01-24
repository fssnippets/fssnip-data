open System

let (|UpperCaseCount|) (str: string) =
    str |> Seq.filter (fun c -> c = Char.ToUpper(c)) |> Seq.length

let (|LowerCaseCount|) (str: string) =
    str |> Seq.filter (fun c -> c = Char.ToLower(c)) |> Seq.length

let (|SpecialCharacterCount|) (str: string) =
    let specialCharacters = "!£$%^"
    str |> Seq.filter (fun c -> specialCharacters.Contains(c.ToString())) |> Seq.length
    
    
let isValid (str: string) =
    match str with
    | UpperCaseCount 0 -> (false, "Must have at least 1 upper case character")
    | LowerCaseCount 0 -> (false, "Must have at least 1 lower case character")
    | SpecialCharacterCount 0 -> (false, "Must have at least 1 of !£$%^")
    | _ -> (true, "you are good to go")
    
printf "%A" (isValid "foo")
printf "%A" (isValid "Foo")
printf "%A" (isValid "Foo!")    