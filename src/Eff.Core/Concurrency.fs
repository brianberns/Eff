namespace Eff.Core

/// Concurrency effect base.
type Concur = inherit Effect

/// Fork effect.
type Fork(effect : Effect, k : unit -> Effect) =
    interface Concur with
        member self.UnPack(lambda) =
            Fork(effect, lambda.Invoke(k)) :> _
    member self.Effect = effect
    member self.K = k

/// Yield effect.
type Yield(k : unit -> Effect) =
    interface Concur with
        member self.UnPack(lambda) =
            Yield(lambda.Invoke(k)) :> _
    member self.K = k

module Concurrent =

    open System.Collections.Generic

    /// Sequential concurrency effect handler
    let rec sequentialHandler<'U, 'S, 'A when 'U :> Concur and 'U :> Log<'S>> (inc : Inc<'U, 'A>) : Inc<'U, _> =

        let rec loop log (queue : Queue<_>) k (effect : Effect) =
            match effect with
                | :? Fork as fork ->
                    queue.Enqueue(fork.K)
                    loop log queue k fork.Effect
                | :? Yield as yield' ->
                    queue.Enqueue(yield'.K)
                    loop log queue k <| queue.Dequeue() ()
                | :? LogEntry<'S> as logEntry ->
                    let log' = logEntry.Value :: log
                    loop log' queue k <| logEntry.K ()
                | :? Done<'A> as done' ->
                    if queue.Count <> 0 then
                        loop log queue k <| queue.Dequeue() ()
                    else k (done'.Value, List.rev log)
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke(k') =
                                    fun x -> loop log queue k (k' x)
                        }

        let effect = Inc.run inc Effect.done'
        Inc (fun k -> loop [] (Queue<_>()) k effect)

    open System.Threading

    /// Parallel concurrency effect handler.
    let rec threadPoolHandler<'U, 'S, 'A when 'U :> Concur and 'U :> Log<'S>> (inc : Inc<'U, 'A>) : Inc<'U, 'A> =

        let rec loop k (effect : Effect) =
            match effect with
                | :? Fork as fork ->
                    ThreadPool.QueueUserWorkItem(fun _ ->
                        loop k fork.Effect |> ignore) |> ignore
                    loop k <| fork.K ()
                | :? Yield as yield' ->
                    loop k <| yield'.K ()
                | :? LogEntry<'S> as logEntry -> 
                    printfn "Log: %A" logEntry.Value
                    loop k <| logEntry.K ()
                | :? Done<'T> as done' ->
                    k done'.Value
                | _ -> 
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke(k') =
                                    fun x -> loop k (k' x)
                        }

        let effect = Inc.run inc Effect.done'
        Inc (fun k -> loop k effect)

    /// Forks a computation.
    let fork<'U when 'U :> Concur> (inc : Inc<'U, unit>) : Inc<'U, _> =
        Inc (fun k -> Fork(Inc.run inc k, k) :> _)

    /// Yields to another computation.
    let yield'<'U when 'U :> Concur>() : Inc<'U, _> =
        Inc (fun k -> new Yield(k) :> _)
