module PhoneNumber

open System

let clean (input: string) =
    let filtered =
        input
        |> Seq.filter Char.IsDigit
        |> String.Concat

    let tenDigits =
        if filtered.Length > 10 then
            filtered.Substring 1
        else
            filtered

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

    let validateAreaCode () = validateDigits 0 "area"
    let validateExchange () = validateDigits 3 "exchange"

    let validate () =
        match validateAreaCode () with
        | Ok _ -> validateExchange ()
        | Error message -> Error message

    if hasAlpha then
        Error "alphanumerics not permitted"
    else if hasPunctuation then
        Error "punctuations not permitted"
    else if filtered.Length = 10 then
        validate ()
    else if filtered.Length = 11 then
        if filtered.[0] = '1' then
            validate ()
        else
            Error "11 digits must start with 1"
    else if filtered.Length > 11 then
        Error "more than 11 digits"
    else
        Error "incorrect number of digits"