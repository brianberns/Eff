﻿namespace Eff.Collections

    /// Immutable stack.
    type Stack<'t> = private Stack of List<'t>

    /// Stack operations.
    module Stack =

        /// Creates a stack.
        let ofList list = Stack list

        /// Pushes an item on the stack.
        let push item (Stack items) =
            Stack (item :: items)

        /// Pops the top of the stack.
        let pop = function
            | Stack (head :: tail) -> head, Stack tail
            | Stack [] -> failwith "Empty stack"

namespace Eff.Core

/// Stack effect base.
type Stack<'S> = inherit Effect

/// Push effect.
type Push<'S>(v : 'S, k : unit -> _) =
    interface Stack<'S> with
        member self.UnPack(lambda : Lambda) : Effect =
            Push(v, lambda.Invoke(k)) :> _
    member self.Value = v
    member self.K = k

/// Pop effect.
type Pop<'S>(k : 'S -> _) =
    interface State<'S> with
        member self.UnPack(lambda : Lambda) : Effect =             
            Pop(lambda.Invoke(k)) :> _
    member self.K = k

module Stack =

    let rec stackHandler<'S, 'A> (stack : Eff.Collections.Stack<'S>) (Eff inc : Eff<Stack<'S>, 'A>) =
        
        let rec loop k stack (effect : Effect) =
            match effect with
                | :? Push<'S> as push ->
                    let stack' = Eff.Collections.Stack.push push.Value stack
                    loop k stack' (push.K ())
                | :? Pop<'S> as pop ->
                    let value, stack' = Eff.Collections.Stack.pop stack
                    loop k stack' (pop.K value)
                | :? Done<'A> as done' ->
                    k (stack, done'.Value)
                | _ ->
                    effect.UnPack
                        {
                            new Lambda with
                                member self.Invoke<'X> (k' : 'X -> Effect) = 
                                    fun x -> loop k stack (k' x)
                        }

        let effect = inc done'
        Eff (fun k -> loop k stack effect)

    /// Pushes a value on the stack.
    let push<'S> (s : 'S) : Eff<Stack<'S>, unit> =
        shift (fun k -> new Push<'S>(s, k) :> _)

    /// Pops a value from the stack.
    let pop<'S>() : Eff<Stack<'S>, 'S> =
        shift (fun k -> new Pop<'S>(k) :> _)
