namespace Eff.Core

/// State effect base.
type State<'S> = inherit Effect

/// Put effect.
type Put<'S>(v : 'S, k : unit -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =
            Put<'S>(v, lambda.Invoke(k)) :> _
    member self.Value = v
    member self.K = k

/// Get effect.
type Get<'S>(k : 'S -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =             
            Get<'S>(lambda.Invoke(k)) :> _
    member self.K = k

module State = 

    /// State effect handler.
    let rec stateHandler<'S, 'A> (state : 'S) (Eff inc : Eff<State<'S>, 'A>) =

        let rec loop k state (effect : Effect) =
            match effect with
                | :? Get<'S> as get -> loop k state (get.K state)
                | :? Put<'S> as put -> loop k put.Value (put.K ())
                | :? Done<'A> as done' -> k done'.Value
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke<'X> (k' : 'X -> Effect) = 
                                    fun x -> loop k state (k' x)
                        }

        let effect = inc Effect.done'
        Eff (fun k -> loop k state effect)

    /// Sets the current state.
    let put<'S> (s : 'S) : Eff<State<'S>, unit> =
        Effect.shift (fun k -> new Put<'S>(s, k) :> _)

    /// Gets the current state.
    let get<'S>() : Eff<State<'S>, 'S> =
        Effect.shift (fun k -> new Get<'S>(k) :> _)
