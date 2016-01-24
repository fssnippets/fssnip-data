open System
  
// -- | Auxiliary recursive drop function
let rec drop' i l1 p l2 =
    if List.length l2 = List.length l1 - 1 then l2
    else 
        let p = if p = i then p+1  else p 
        drop' i l1 (p+1) (List.append l2 [(List.nth l1 p)])

// -- | Removes the the nth element from list
let drop i l =
    drop' i l 0 []        

// -- | Pop one element from a specific list position
let pop i l =
    match l with
    | [] -> ([],[])
    | h :: t when i >= 0 -> 
        let e = [l.Item i]
        let ll = drop i l
        (e,ll)
    | _ -> ([],[])

// -- | Generate random number within interval a-b with seed s
let myrandom a b s =
    let r = System.Random(s)
    r.Next(a, b)

// -- | Shuffle' auxiliary
let rec shuffle' r l1 l2 =
    let len = List.length l1
    let t = pop r l1
    if len = 0 then l2
    else shuffle' (myrandom 0 (len-1) r) (snd t) (List.append l2 (fst t))  

// -- | Shuffle a list
let shuffle l =
    shuffle' (myrandom 0 ((List.length l)-1) 0) l []
   
// -- | The main entry point.
let main() = 
    let v = [1..100]
    Console.WriteLine("List Shuffle!")
    printfn "%A" (shuffle v)
    
main()
