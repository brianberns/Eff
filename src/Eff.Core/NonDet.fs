namespace Eff.Core

/// Base non-determinisim effect.
type NonDetEffect = 
    inherit Effect
    abstract Invoke<'R> : NonDetUnPack<'R> -> 'R
and NonDetUnPack<'R> =
    abstract Invoke : (unit -> Effect) -> 'R 
    abstract Invoke<'T> : 'T * 'T * ('T -> Effect) -> 'R

/// Choose effect.
and Choose<'T>(first : 'T, second : 'T, k : 'T -> _) =
    interface NonDetEffect with
        override self.Invoke unpack = 
            unpack.Invoke(first, second, k)
        override self.UnPack(lambda : Lambda) =
            Choose(first, second, lambda.Invoke(k)) :> _

/// Fail effect.
and Fail(k : unit -> _) =
    interface NonDetEffect with
        override self.Invoke unpack = 
            unpack.Invoke(k)
        override self.UnPack(lambda : Lambda) =
            Fail(lambda.Invoke(k)) :> _
    
module NonDet =

    /// Non-determinism effect handler.
    let nonDetHandler<'U, 'A when 'U :> NonDetEffect> (inc : Inc<'U, 'A>) : Inc<'U, _> =

        let rec loop k (effect : Effect) =
            match effect with
                | :? NonDetEffect as nonDet ->
                    {
                        new NonDetUnPack<Effect> with
                            member self.Invoke(k') =
                                k []
                            member self.Invoke<'C>(first, second, k' : 'C -> _) =
                                loop (fun seqs ->
                                    loop (fun seqs' ->
                                        k <| List.append seqs seqs')
                                        (k' second))
                                    (k' first)
                    } |> nonDet.Invoke
                | :? Done<'A> as done' ->
                    k [done'.Value]
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke(k') =
                                    fun x -> loop k (k' x)
                        }

        let effect = Inc.run inc Effect.done'
        Inc (fun k -> loop k effect)
    
    /// Chooses non-deterministically between two alternatives.
    let choose<'T, 'U when 'U :> NonDetEffect> (first : 'T, second : 'T) : Inc<'U, _> =
        Inc (fun k -> Choose<'T>(first, second, k) :> _)

    /// Non-deterministic failure.
    let fail<'U when 'U :> NonDetEffect> () : Inc<'U, unit> = 
        Inc (fun k -> Fail(k) :> _)
