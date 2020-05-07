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

    let x = comp |> State.stateHandler 1 |> Effect.run // (4, 4)
    printfn "%A" x

let testStack () =

    let comp : Inc<Stack<int>, int> =
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
    let x = comp |> Stack.stackHandler stack |> Effect.run   // (Stack [8; 3; 0; 2; 1; 0], 9)
    printfn "%A" x


type CombinedEffect =
    inherit State<int>
    inherit Reader<int>

/// Combine State and Reader effects.
let testCombined () =

    let comp =
        eff {
            do! State.put 1
            let! y = Reader.ask ()
            let! x = State.get ()
            return x + y
        }

    let x =
        comp
            |> State.stateHandler<CombinedEffect, _, _> 0
            |> Reader.readerHandler 1
            |> Effect.run // (1, 2)
    printfn "%A" x

[<EntryPoint>]
let main argv =
    testState ()
    testStack ()
    testCombined ()
    0
