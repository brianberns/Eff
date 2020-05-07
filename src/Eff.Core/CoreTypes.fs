namespace Eff.Core

/// Annotation type for effects.
type Effect =

    /// Converts the receiver into another effect using the given lambda.
    abstract UnPack : Lambda -> Effect

/// Converts a continuation to another continuation of the same type.
and Lambda =
    abstract Invoke<'X> : ('X -> Effect) -> ('X -> Effect)

/// An incomplete computation that returns an effect when given a continuation.
type Inc<'U, 'A when 'U :> Effect> =
    Inc of (('A -> Effect) -> Effect)

module Inc =

    let run (Inc inc) cont =
        inc cont

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

    /// Extracts a value from the given incomplete computation.
    let rec run<'U, 'A when 'U :> Effect> (inc : Inc<'U, 'A>) : 'A =
        match Inc.run inc done' with
            | :? Done<'A> as dn -> dn.Value
            | effect -> failwithf "Unhandled effect %A" effect

// Basic builder 
type EffBuilder() =

    /// Wraps the given value in an incomplete computation.
    member self.Return<'U, 'A when 'U :> Effect>(v : 'A) : Inc<'U, 'A> =
        Inc (fun k -> k v)

    /// Directly returns the given computation.
    member self.ReturnFrom(eff : Inc<'U, 'A>) : Inc<'U, 'A> =
        eff

    /// Combines two independent computations.
    member self.Combine(first : Inc<'U, unit>, second : Inc<'U, 'A>) : Inc<'U, 'A> =
        self.Bind(first, fun () -> second)

    /// Unit computation.
    member self.Zero() : Inc<'U, unit> =
        self.Return(())

    /// Composition of incomplete computations.
    member self.Bind<'U, 'A, 'B when 'U :> Effect>(inc : Inc<'U, 'A>, f : 'A -> Inc<'U, 'B>) : Inc<'U, 'B> =
        Inc (fun k ->
            Inc.run inc (fun v ->
                Inc.run (f v) k))

    /// Wraps the given lazy function.
    member self.Delay(f : unit -> Inc<'U, 'A>) : Inc<'U, 'A> =
        Inc (Inc.run (f ()))

[<AutoOpen>]
module EffBuilder =

    /// Workflow builder.
    let eff = EffBuilder()
