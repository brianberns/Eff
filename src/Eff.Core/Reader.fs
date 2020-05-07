﻿namespace Eff.Core

type Reader<'E> = inherit Effect
type Ask<'E>(k : 'E -> Effect) =
    interface Reader<'E> with
        member self.UnPack(lambda : Lambda) : Effect =
                new Ask<'E>(lambda.Invoke<'E> k) :> _
    member self.K = k

module Reader = 

    let ask<'U, 'E when 'U :> Reader<'E>>() : Inc<'U, 'E> = 
        Inc (fun k -> new Ask<'E>(k) :> _)

    let rec readerHandler<'U, 'E, 'A when 'U :> Reader<'E>> 
        : 'E -> Inc<'U, 'A> -> Inc<'U, 'A> = 
        fun env eff ->
            let rec loop : ('A -> Effect) -> 'E -> Effect -> Effect = fun k env effect ->
                match effect with
                | :? Ask<'E> as ask -> loop k env (ask.K env) 
                | :? Done<'A> as done' -> k done'.Value
                | _ -> effect.UnPack {
                    new Lambda with
                        member self.Invoke<'X> (k' : 'X -> Effect) = 
                            fun x -> loop k env (k' x)
                }
            let (Inc inc) = eff
            let effect = inc Effect.done'
            Inc (fun k -> loop k env effect)
