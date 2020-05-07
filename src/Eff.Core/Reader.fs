namespace Eff.Core

/// Reader effect base.
type Reader<'E> = inherit Effect

/// Ask effect.
type Ask<'E>(k : 'E -> _) =
    interface Reader<'E> with
        member self.UnPack(lambda) =
            Ask(lambda.Invoke(k)) :> _
    member self.K = k

module Reader = 

    /// Reader effect handler.
    let rec readerHandler<'U, 'E, 'A when 'U :> Reader<'E>> env (inc : Inc<'U, 'A>) : Inc<'U, _> =

        let rec loop k env (effect : Effect) =
            match effect with
                | :? Ask<'E> as ask -> loop k env (ask.K env) 
                | :? Done<'A> as done' -> k done'.Value
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke(k') =
                                    fun x -> loop k env (k' x)
                        }

        let effect = Inc.run inc Effect.done'
        Inc (fun k -> loop k env effect)

    /// Reads the current environment.
    let ask<'U, 'E when 'U :> Reader<'E>> () : Inc<'U, _> =
        Inc (fun k -> Ask<'E>(k) :> _)
