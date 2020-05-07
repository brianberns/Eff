namespace Eff.Core

/// Annotation type for effects.
type Effect =

    /// Converts the receiver into another effect using the given lambda.
    abstract UnPack : Lambda -> Effect

/// Converts a continuation to another continuation of the same type.
and Lambda =
    abstract Invoke<'X> : ('X -> Effect) -> ('X -> Effect)

/// An incomplete computation that returns an effect when given a continuation.
type Eff<'U, 'A when 'U :> Effect> =
    Eff of (('A -> Effect) -> Effect)

/// An effect that wraps the given value.
type Done<'A>(v : 'A) =
    member self.Value = v
    interface Effect with
        member self.UnPack(_ : Lambda) : Effect =
            self :> _

module Effect =

    /// A continuation that creates an effect from the given value.
    let done' (v : 'A) : Effect =
        Done(v) :> _

    /// Wraps the given incomplete computation.
    let shift (inc : ('A -> Effect) -> Effect) : Eff<'U, 'A> =
        Eff inc

    /// Extracts a value from the given incomplete computation.
    let rec run<'U, 'A when 'U :> Effect> (Eff inc : Eff<'U, 'A>) : 'A =
        match inc done' with
            | :? Done<'A> as dn -> dn.Value
            | effect -> failwithf "Unhandled effect %A" effect

// Basic builder 
type EffBuilder() =

    /// Wraps the given value in an incomplete computation.
    member self.Return<'U, 'A when 'U :> Effect>(v : 'A) : Eff<'U, 'A> =
        Eff (fun k -> k v)

    /// Directly returns the given computation.
    member self.ReturnFrom(eff : Eff<'U, 'A>) : Eff<'U, 'A> =
        eff

    /// Combines two independent computations.
    member self.Combine(first : Eff<'U, unit>, second : Eff<'U, 'A>) : Eff<'U, 'A> =
        self.Bind(first, fun () -> second)

    /// Unit computation.
    member self.Zero() : Eff<'U, unit> =
        self.Return(())

    /// Composition of incomplete computations.
    member self.Bind<'U, 'A, 'B when 'U :> Effect>(Eff inc : Eff<'U, 'A>, f : 'A -> Eff<'U, 'B>) : Eff<'U, 'B> =
        Eff (fun k ->
            inc (fun v ->
                let (Eff inc') = f v
                inc' k))

    /// Wraps the given lazy function.
    member self.Delay(f : unit -> Eff<'U, 'A>) : Eff<'U, 'A> =
        Eff (fun k ->
            let (Eff inc) = f ()
            inc k)

[<AutoOpen>]
module EffBuilder =

    /// Workflow builder.
    let eff = EffBuilder()
