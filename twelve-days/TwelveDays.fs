module TwelveDays

let recite start stop =
    let buildDay day =
        match day with
        | 1 -> "first"
        | 2 -> "second"
        | 3 -> "third"
        | 4 -> "fourth"
        | 5 -> "fifth"
        | 6 -> "sixth"
        | 7 -> "seventh"
        | 8 -> "eighth"
        | 9 -> "ninth"
        | 10 -> "tenth"
        | 11 -> "eleventh"
        | 12 -> "twelfth"
        | _ -> ""

    let buildGifts day =
        [1..day]
        |> Seq.map (fun day ->
            match day with
            | 1 -> "a Partridge in a Pear Tree"
            | 2 -> "two Turtle Doves"
            | 3 -> "three French Hens"
            | 4 -> "four Calling Birds"
            | 5 -> "five Gold Rings"
            | 6 -> "six Geese-a-Laying"
            | 7 -> "seven Swans-a-Swimming"
            | 8 -> "eight Maids-a-Milking"
            | 9 -> "nine Ladies Dancing"
            | 10 -> "ten Lords-a-Leaping"
            | 11 -> "eleven Pipers Piping"
            | 12 -> "twelve Drummers Drumming"
            | _ -> "")
        |> Seq.rev
        |> Seq.mapi (fun index gift ->
            if index > 0 && gift.[0] = 'a' then
                "and " + gift
            else gift)
        |> String.concat ", "

    let buildPhrase day =
        sprintf "On the %s day of Christmas my true love gave to me: %s." (buildDay day) (buildGifts day)

    [start..stop]
    |> Seq.map buildPhrase
    |> Seq.toList