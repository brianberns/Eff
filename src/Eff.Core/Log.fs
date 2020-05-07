namespace Eff.Core

/// Log effect base.
type Log<'S> = inherit Effect

/// Log entry effect.
type LogEntry<'S>(v : 'S, k : unit -> Effect) =
    interface Log<'S> with
        member self.UnPack(lambda : Lambda) =
            LogEntry(v, lambda.Invoke<unit> k) :> _
    member self.Value = v
    member self.K = k

module Log = 

    /// Pure log effect handler.
    let rec pureLogHandler<'U, 'S, 'A when 'U :> Log<'S>> (inc : Inc<'U, 'A>) : Inc<'U, _> =

        let rec loop log k (effect : Effect) =
            match effect with
                | :? LogEntry<'S> as logEntry ->
                    let log' = logEntry.Value :: log
                    loop log' k (logEntry.K ())
                | :? Done<'A> as done' ->
                    k (done'.Value, log)
                | _ -> 
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke(k') =
                                    fun x -> loop log k (k' x)
                        }

        let effect = Inc.run inc Effect.done'
        Inc (fun k -> loop [] k effect)

    /// Console log effect handler. Actual side-effects!
    let rec consoleLogHandler<'U, 'S, 'A when 'U :> Log<'S>> (inc : Inc<'U, 'A>) : Inc<'U, _> =

        let rec loop k (effect : Effect) =
            match effect with
                | :? LogEntry<'S> as logEntry ->
                    printfn "Log: %A" logEntry.Value
                    loop k (logEntry.K ())
                | :? Done<'A> as done' ->
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

    /// Logs an item.
    let log<'U, 'S when 'U :> Log<'S>> (s : 'S) : Inc<'U, _> =
        Inc (fun k -> LogEntry<'S>(s, k) :> _)

    /// Logs a formatted string.
    let logf fmt =
        Printf.ksprintf log fmt
