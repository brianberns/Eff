open Eff.Core

// State examples
// val stateTest : unit -> Eff<'a,int> when 'a :> State<int>
let stateTest () = 
    eff {
        let! x = State.get ()
        do! State.put (x + 1)
        let! y = State.get ()
        do! State.put (y + y)
        return! State.get ()
    } 

[<EntryPoint>]
let main argv =

    let x, y = stateTest () |> State.stateHandler 1 |> run // (4, 4)
    printfn "%A, %A" x y

    let x = stateTest () |> State.refHandler 1 |> run // 4
    printfn "%A" x

    0
