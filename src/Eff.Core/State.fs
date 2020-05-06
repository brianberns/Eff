namespace Eff.Core

/// Stateful effect.
type State<'S> = inherit Effect

/// Saves the given value as the current state.
type Put<'S>(v : 'S, k : unit -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =
            Put<'S>(v, lambda.Invoke(k)) :> _
    member self.Value = v
    member self.K = k

/// Gets the current state.
type Get<'S>(k : 'S -> Effect) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =             
            Get<'S>(lambda.Invoke(k)) :> _
    member self.K = k

module State = 

    // state effect handlers
    
    let rec stateHandler<'S, 'A> (state : 'S) (Eff inc : Eff<State<'S>, 'A>) =

        let rec loop k state (effect : Effect) =
            match effect with
                | :? Get<'S> as get -> loop k state (get.K state) 
                | :? Put<'S> as put -> loop k put.Value (put.K ())
                | :? Done<'A> as done' -> k (state, done'.Value)
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke<'X> (k' : 'X -> Effect) = 
                                    fun x -> loop k state (k' x)
                        }

        let effect = inc done'
        Eff (fun k -> loop k state effect)

    let rec refHandler<'S, 'A> (state : 'S) (Eff inc : Eff<State<'S>, 'A>) : Eff<State<'S>, 'A> =

        let valueRef = ref state

        let rec loop  (k : 'A -> Effect) (effect : Effect) : Effect =
            match effect with
            | :? Get<'S> as get -> loop k (get.K !valueRef) 
            | :? Put<'S> as put -> valueRef := put.Value; loop k (put.K ())
            | :? Done<'A> as done' -> k done'.Value
            | _ -> effect.UnPack {
                new Lambda with
                    member self.Invoke<'X> (k' : 'X -> Effect) = 
                        fun x -> loop k (k' x)
            }

        let effect = inc done'
        Eff (fun k -> loop k effect)

    // state helper functions
    let get<'S>() : Eff<State<'S>, 'S> =
        shift (fun k -> new Get<'S>(k) :> _)
    let put<'S> (s : 'S) : Eff<State<'S>, unit> =
        shift (fun k -> new Put<'S>(s, k) :> _)
