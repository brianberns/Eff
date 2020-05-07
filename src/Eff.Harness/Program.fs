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

        printfn ""
        printfn "State test:"
        let x = comp |> State.stateHandler 1 |> Effect.run
        printfn "%A" x
        assert(x = (4, 4))

module StackTest =

    let run () =

        let comp =
            eff {
                let! a = Stack.pop ()
                if a = 5 then
                    do! Stack.push 7
                else
                    do! Stack.push 3
                    do! Stack.push 8
                return a.ToString()
            }

        printfn ""
        printfn "Stack test:"
        let stack = Eff.Collections.Stack.ofList [9; 0; 2; 1; 0]
        let x = comp |> Stack.stackHandler stack |> Effect.run
        assert(x = (Eff.Collections.Stack.ofList [8; 3; 0; 2; 1; 0], "9"))
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

        printfn ""
        printfn "State/Reader test:"
        let x =
            getComp ()
                |> State.stateHandler<CombinedEffect, _, _> 0
                |> Reader.readerHandler 1
                |> Effect.run
        printfn "%A" x
        assert(x = (1, 2))

module NonDetTest =

    let run () =

        let comp = 
            eff {
                let! x = NonDet.choose (1, 2)
                let! x = NonDet.choose (x, 3)
                if x = 3 then
                    do! NonDet.fail ()
                let! y = NonDet.choose ("1", "2")
                return (x, y)
            }

        printfn ""
        printfn "Non-determinisim test:"
        let x =
            comp
                |> NonDet.nonDetHandler
                |> Effect.run
        printfn "%A" x
        assert(x = [(1, "1"); (1, "2"); (2, "1"); (2, "2")])

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
        printfn ""
        printfn "State/Non-determinism test:"
        let x =
            getComp ()
                |> State.stateHandler<CombinedEffect, _, _> 1
                |> NonDet.nonDetHandler
                |> Effect.run
        printfn "%A" x
        assert(
            x =
                [
                    1, (1, "1")
                    1, (1, "2")
                    1, (2, "1")
                    1, (2, "2")
                ])

module LogTest =

    let getComp n = 
        eff {
            do! Log.logf "Test %d" n
            do! Log.logf "Test %d" (n + 1)
        }

    let run () =

        printfn ""
        printfn "Pure log test:"
        let x =
            getComp 1
                |> Log.pureLogHandler
                |> Effect.run
        printfn "%A" x
        assert(x = ((), ["Test 2"; "Test 1"]))

        printfn ""
        printfn "Console log test:"
        getComp 1
            |> Log.consoleLogHandler
            |> Effect.run // printf side-effect: Log: "Test 1"\n Log: "Test 2"\n

/// http://kcsrk.info/ocaml/multicore/2015/05/20/effects-multicore/
module ConcurrentTest =

    let rec getComp id depth =
        eff {
            do! Log.logf "Starting number %d!" id
            if depth > 0 then 
                do! Log.logf "Forking number %d!" (id * 2 + 1)
                do! Concurrent.fork <| getComp (id * 2 + 1) (depth - 1)
                do! Log.logf "Forking number %d!" (id * 2 + 2)
                do! Concurrent.fork <| getComp (id * 2 + 2) (depth - 1)
            else 
                do! Log.logf "Yielding in number %d!" id
                do! Concurrent.yield' ()
                do! Log.logf "Resumed number %d!" id
            do! Log.logf "Finishing number %d!" id
        }

    type CombinedEffect =
        inherit Log<string>
        inherit Concur

    let run () =

        printfn ""
        printfn "Sequential concurrency test:"
        let comp = getComp 0 2
        let x =
            comp
                |> Concurrent.sequentialHandler<CombinedEffect, _, _>
                |> Effect.run
        printfn "%A" x

        printfn ""
        printfn "Parallel concurrency test:"
        comp
            |> Concurrent.threadPoolHandler<CombinedEffect, _, _>
            |> Effect.run
        System.Threading.Thread.Sleep(500)

/// http://math.andrej.com/2011/12/06/how-to-make-the-impossible-functionals-run-even-faster/
module SearcherTest =

    let findNeighborhood (p : (int -> Inc<'U, bool>) -> Inc<'U, bool>) : Inc<'U, bool> =
        p (fun n -> Searcher.search n)

    let epsilon p n = 
        eff {
            let! (_, s) = findNeighborhood p |> Searcher.findNeighborhoodHandler
            return s
                |> List.tryFind (fun (n', b) -> n = n')
                |> Option.map snd
                |> Option.defaultValue true
        }

    let exists p = p (epsilon p)

    let run () =
        printfn ""
        printfn "Searcher test:"
        let f =
            epsilon (fun f ->
                eff {
                    let! x = f 10
                    let! y = f 11
                    return x <> y
                })
        for i = 0 to 20 do
            let x = Effect.run <| f i
            printfn "%d: %A" i x

[<EntryPoint>]
let main argv =
    StateTest.run ()
    StackTest.run ()
    StateReaderTest.run ()
    NonDetTest.run ()
    StateNonDetTest.run ()
    LogTest.run ()
    ConcurrentTest.run ()
    SearcherTest.run ()
    0
