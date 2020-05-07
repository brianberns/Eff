open Eff.Core

let testState () =

    let comp =
        eff {
            let! x = State.get ()
            do! State.put (x + 1)
            let! y = State.get ()
            do! State.put (y + y)
            return! State.get ()
        } 

    let x = comp |> State.stateHandler 1 |> run // 4
    printfn "%A" x

let testStack () =

    let comp : Eff<Stack<int>, int> =
        eff {
            let! a = Stack.pop ()
            if a = 5 then
                do! Stack.push 7
            else
                do! Stack.push 3
                do! Stack.push 8
            return a
        }

    let stack = [9; 0; 2; 1; 0] |> Eff.Collections.Stack.ofList
    let x = comp |> Stack.stackHandler stack |> run   // (Stack [8; 3; 0; 2; 1; 0], 9)
    printfn "%A" x

[<EntryPoint>]
let main argv =
    testState ()
    testStack ()
    0
