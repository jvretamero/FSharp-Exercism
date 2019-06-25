module PhoneNumber

open System

let clean (input: string) =
    let filtered =
        input
        |> Seq.filter Char.IsDigit
        |> String.Concat

    let tenDigits =
        if filtered.Length > 10
        then filtered.Substring 1
        else filtered

    let hasAlpha = input |> Seq.exists Char.IsLetter
    let hasPunctuation =
        input
        |> Seq.exists (fun char -> char = '@' || char = ':' || char = '!')

    let success = Ok (UInt64.Parse tenDigits)

    let validateDigits position codeType =
        match tenDigits.[position] with
        | '0' -> Error (codeType + " code cannot start with zero")
        | '1' -> Error (codeType + " code cannot start with one")
        | _ -> success

    let validate () =
        match validateDigits 0 "area" with
        | Ok _ -> validateDigits 3 "exchange"
        | Error message -> Error message

    let validateLength () =
        match filtered.Length with
        | 10 -> validate()
        | 11 ->
            match filtered.[0] with
            | '1' -> validate()
            | _ -> Error "11 digits must start with 1"
        | len when len > 11 -> Error "more than 11 digits"
        | _ -> Error "incorrect number of digits"

    if hasAlpha then Error "alphanumerics not permitted"
    else if hasPunctuation then Error "punctuations not permitted"
    else validateLength ()