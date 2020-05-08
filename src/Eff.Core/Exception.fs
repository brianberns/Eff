namespace Eff.Core

/// Exception effect base.
type Exception = inherit Effect

/// Division-by-zero effect.
type DivisionByZero(numerator : float, k : unit -> Effect) =
    interface Exception with
        member self.UnPack(lambda) =
            DivisionByZero(numerator, lambda.Invoke(k)) :> _
    member self.Numerator = numerator
    member self.K = k

module Exception =

    /// Exception effect handler.
    let rec exceptionHandler<'U, 'E, 'A when 'U :> Exception> (inc : Inc<'U, 'A>) : Inc<'U, Option<'A>> =

        let rec loop k (effect : Effect) =
            match effect with
                | :? DivisionByZero as ex ->
                    printfn "Division by zero"
                    if ex.Numerator < 10.0 then
                        printfn "Resuming"
                        loop k (ex.K ())
                    else
                        printfn "Terminating"
                        Effect.done' (None : Option<'A>)
                | :? Done<'A> as done' ->
                    k (Some done'.Value)
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke(k') =
                                    fun x -> loop k (k' x)
                        }

        let effect = Inc.run inc Effect.done'
        Inc (fun k -> loop k effect)

    /// Throws a division-by-zero exception.
    let divisionByZero<'U when 'U :> Exception> numerator : Inc<'U, unit> =
        Inc (fun k -> DivisionByZero(numerator, k) :> _)
