module NthPrime

let isPrime x =
    let max = (float >> sqrt >> int) x
    [ 2 .. max ]
    |> List.forall (fun y -> x % y <> 0) 

let allPrimes =
    let rec recPrimes n =
        seq {
            if isPrime n then
                yield n
            
            yield! recPrimes (n + 1)
        }
    recPrimes 2

let prime nth : int option =
    match nth with
    | 0 -> None
    | n -> Some (allPrimes |> Seq.item (n - 1))