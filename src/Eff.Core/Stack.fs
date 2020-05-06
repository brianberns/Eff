namespace Eff.Core

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

/// Stack effect base.
type StackEffect<'S> = inherit Effect

/// Push effect.
type Push<'S>(v : 'S, k : unit -> _) =
    interface StackEffect<'S> with
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

module StackEffect = 

    let rec stackHandler<'S, 'A> (stack : Stack<'S>) (Eff inc : Eff<StackEffect<'S>, 'A>) =
        
        let rec loop k stack (effect : Effect) =
            match effect with
                | :? Get<'S> as get ->
                    let value, stack' = Stack.pop stack
                    loop k stack' (get.K value)
                | :? Put<'S> as put ->
                    let stack' = Stack.push put.Value stack
                    loop k stack' (put.K ())
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
    let push<'S> (s : 'S) : Eff<StackEffect<'S>, unit> =
        shift (fun k -> new Push<'S>(s, k) :> _)

    /// Pops a value from the stack.
    let pop<'S>() : Eff<StackEffect<'S>, 'S> =
        shift (fun k -> new Get<'S>(k) :> _)
