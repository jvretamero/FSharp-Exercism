module RobotName

let letters = [|'A'..'Z'|]
let numbers = [|'0'..'9'|]

let randomItems (items: 'a array) count =
    let random = System.Random()
    let nextIndex() = random.Next(items.Length)
    List.init count (fun i -> items.[nextIndex()])

let toString (items: char list) = items |> List.toArray |> System.String

let randomString items count = randomItems items count |> toString

type Robot = {Name: string}

let mkRobot() = { Robot.Name = (randomString letters 3) + (randomString numbers 4) }

let name robot = robot.Name

let reset robot = mkRobot()