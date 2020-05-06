open Eff.Core

let testState () =

    let comp =
        eff {
            let! x = State.get ()
            do! State.put (x + 1)
            let! y = State.get ()
            do! State.put (y + y)
            let! z = State.get ()
            return z
        } 

    let x = comp |> State.stateHandler 1 |> run // 4
    printfn "%A" x

let testStack () =

    let comp : Eff<StackEffect<int>, int> =
        eff {
            let! a = StackEffect.pop ()
            (*
            if a = 5 then
                do! StackEffect.push 7
            else
                do! StackEffect.push 3
                do! StackEffect.push 8
            *)
            return a
        }

    let stack = [9; 0; 2; 1; 0] |> Stack.ofList
    let x = comp |> StackEffect.stackHandler stack |> run
    printfn "%A" x

[<EntryPoint>]
let main argv =
    testState ()
    testStack ()
    0
