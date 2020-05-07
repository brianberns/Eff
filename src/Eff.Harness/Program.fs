open Eff.Core

module StateTest =

    let run () =

        let comp =
            eff {
                let! x = State.get ()
                do! State.put (x + 1)
                let! y = State.get ()
                do! State.put (y + y)
                return! State.get ()
            } 

        let x = comp |> State.stateHandler 1 |> Effect.run
        assert(x = (4, 4))
        printfn "%A" x

module StackTest =

    let run () =

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

        let stack = Eff.Collections.Stack.ofList [9; 0; 2; 1; 0]
        let x = comp |> Stack.stackHandler stack |> Effect.run
        assert(x = (Eff.Collections.Stack.ofList [8; 3; 0; 2; 1; 0], 9))
        printfn "%A" x

/// Combine State and Reader effects.
module StateReaderTest =

    let getComp () : Inc<'a,int> when 'a :> State<int> and 'a :> Reader<int> =
        eff {
            do! State.put 1
            let! y = Reader.ask ()
            let! x = State.get ()
            return x + y
        }

    type CombinedEffect =
        inherit State<int>
        inherit Reader<int>

    let run () =

        let x =
            getComp ()
                |> State.stateHandler<CombinedEffect, _, _> 0
                |> Reader.readerHandler 1
                |> Effect.run
        assert(x = (1, 2))
        printfn "%A" x

module NonDetTest =

    let run () =

        let comp = 
            eff {
                let! x = NonDet.choose (1, 2)
                let! y = NonDet.choose ("1", "2")
                return (x, y)
            }

        let x =
            comp
                |> NonDet.nonDetHandler
                |> Effect.run
        assert(x = [(1, "1"); (1, "2"); (2, "1"); (2, "2")])
        printfn "%A" x

module StateNonDetTest =

    let getComp () =
        eff {
            let! n = State.get ()
            let! x = NonDet.choose (n, n + 1)
            let! y = NonDet.choose (string n, string (n + 1))
            return (x, y)
        }

    type CombinedEffect =
        inherit State<int>
        inherit NonDetEffect

    let run () =
        let x =
            getComp ()
                |> State.stateHandler<CombinedEffect, _, _> 1
                |> NonDet.nonDetHandler
                |> Effect.run
        assert(
            x =
                [
                    1, (1, "1")
                    1, (1, "2")
                    1, (2, "1")
                    1, (2, "2")
                ])
        printfn "%A" x

[<EntryPoint>]
let main argv =
    StateTest.run ()
    StackTest.run ()
    StateReaderTest.run ()
    NonDetTest.run ()
    StateNonDetTest.run ()
    0
