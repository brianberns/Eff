namespace Eff.Core

/// Search effect.
type Search<'S>(v : 'S, k : bool -> Effect) =
    interface Effect with
        member self.UnPack(lambda) =
            Search<'S>(v, lambda.Invoke<bool> k) :> _
    member self.Value = v
    member self.K = k

module Searcher = 

    /// Search effect handler.
    let rec findNeighborhoodHandler<'U, 'S when 'U :> Search<'S> and 'S : equality> (inc : Inc<'U, bool>) : Inc<'U, _> =

        let rec loop s k (effect : Effect) =
            match effect with
                | :? Search<'S> as search ->
                    let result =
                        s |> List.tryFind (fun (n, b) -> n = search.Value)
                    match result with
                        | Some (n, b) -> loop s k (search.K b)
                        | None ->
                            let sTrue = (search.Value, true) :: s
                            let f = function
                                | (true, s') -> k (true, s')
                                | (false, _) ->
                                    let sFalse = (search.Value, false) :: s
                                    loop sFalse k (search.K false)
                            loop sTrue f (search.K true)
                | :? Done<bool> as done' ->
                    k (done'.Value, s)
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke(k') =
                                    fun x -> loop s k (k' x)
                        }

        let effect = Inc.run inc Effect.done'
        Inc (fun k -> loop [] k effect)

    /// Searches.
    let search<'U, 'S when 'U :> Search<'S>> (v : 'S) : Inc<'U, _> =
        Inc (fun k -> Search<'S>(v, k) :> _)
