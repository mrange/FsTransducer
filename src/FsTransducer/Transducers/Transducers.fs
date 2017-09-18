// ----------------------------------------------------------------------------------------------
// Copyright 2017 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

namespace Transducers

open FSharp.Core.OptimizedClosures

#if XXX
type [<Struct>] Finalizer =
  | Action      of a : (unit -> unit)
  | Disposable  of d : System.IDisposable

module Details =
  let inline adapt  f   = FSharpFunc<_, _, _>.Adapt f
  let inline dbreak ()  = System.Diagnostics.Debugger.Break ()

  let dispose d = 
    try
      (d : System.IDisposable).Dispose ()
    with
    | _ -> ()

  let finalize f =
    match f with
    | Action      a -> 
      try
        a ()
      with
      | _ -> ()
    | Disposable  d -> dispose d

open Details

type Context () =
  class
    let mutable isCancelled = false
    let mutable finalizers  = []

    member x.IsCancelled    = isCancelled
    member x.Cancel ()      = isCancelled <- true

    member x.AddFinalizer f = finalizers <- f::finalizers

    interface System.IDisposable with
      member x.Dispose () = 
        let rec loop fs =
          match fs with
          | h::t  -> finalize h; loop t
          | _     -> ()
        loop finalizers
  end

type Transducer<'TIn, 'TOut> =
  interface
    abstract BuildUp: Context -> ('S -> 'TOut -> 'S) -> ('S -> 'TIn -> 'S)
  end

module Transducer =
  let inline compose (l : Transducer<_, _>) (r : Transducer<_, _>) =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = l.BuildUp ctx (r.BuildUp ctx folder)
    }

  [<GeneralizableValue>]
  let id<'T> =
    { new Transducer<'T, 'T> with
        member x.BuildUp ctx folder = folder
    }

  let inline filtering f =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = 
          let folder = adapt folder
          fun s v -> if f v then folder.Invoke (s, v) else s
    }

  let inline filter f t = compose t (filtering f)

  let inline mapping m =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = 
          let folder = adapt folder
          fun s v -> folder.Invoke (s, (m v))
    }

  let inline map m t = compose t (mapping m)

  let inline taking n =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = 
          let folder      = adapt folder
          let mutable rem = n
          fun s v -> 
            if rem > 0 then
              rem <- rem - 1
              folder.Invoke (s, v)
            else
              ctx.Cancel ()
              s
    }

  let inline take n t = compose t (taking n)

  let inline skipping n =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = 
          let folder      = adapt folder
          let mutable rem = n
          fun s v -> 
            if rem > 0 then
              rem <- rem - 1
              s
            else
              folder.Invoke (s, v)
    }

  let inline skip n t = compose t (skipping n)

module Loops =
  module Range =
    let rec loop (ctx : Context) (tf : FSharpFunc<_, _, _>) e acc i =
      if i <= e && not ctx.IsCancelled then
        loop ctx tf e (tf.Invoke(acc, i)) (i + 1)
      else 
        acc

  module Array = 
    let rec loop (ctx : Context) (tf : FSharpFunc<_, _, _>) (s : _ []) acc i  =
      if i < s.Length && not ctx.IsCancelled then
        loop ctx tf s (tf.Invoke (acc, s.[i])) (i + 1)
      else 
        acc

  module Seq = 
    let rec loop (ctx : Context) (tf : FSharpFunc<_, _, _>) (e : System.Collections.Generic.IEnumerator<_>) acc  =
      if e.MoveNext () && not ctx.IsCancelled then
        loop ctx tf e (tf.Invoke (acc, e.Current))
      else 
        acc

  module List =
    let rec loop (ctx : Context) (tf : FSharpFunc<_, _, _>) acc ls =
      match ls with
      | h::t when not ctx.IsCancelled -> 
        loop ctx tf (tf.Invoke (acc, h)) t 
      | _ ->
        acc

module Range =
  let inline transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (b : int) (e : int)  : 'S =
    use ctx             = new Context ()
    let tf              = t.BuildUp ctx f
    let tf              = adapt tf
    Loops.Range.loop ctx tf e z b

module Array =
  let inline transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U []) : 'S =
    use ctx             = new Context ()
    let tf              = t.BuildUp ctx f
    let tf              = adapt tf
    Loops.Array.loop ctx tf s z 0

  let inline sequence (t : Transducer<_, _>) (s : 'T []) : 'U [] =
    let ra      = ResizeArray s.Length
    let f () v  = ra.Add v
    transduce t f () s
    ra.ToArray ()

module Seq =
  let inline transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U seq) : 'S =
    use ctx           = new Context ()
    let tf            = t.BuildUp ctx f
    let tf            = adapt tf
    use e             = s.GetEnumerator ()
    Loops.Seq.loop ctx tf e z

  let inline sequence (t : Transducer<_, _>) (s : seq<'T>) : seq<'U> =
    let gen () : System.Collections.Generic.IEnumerator<'U> = 
      let mutable c = None
      let f _ v     = c <- Some v
      let ctx       = new Context ()
      let tf        = t.BuildUp ctx f
      let tf        = adapt tf
      let e         = s.GetEnumerator ()
      { new System.Collections.Generic.IEnumerator<'U> with
          member x.Current      = c.Value

        interface System.Collections.IEnumerator with
          member x.Current      = box c.Value
          member x.MoveNext ()  =
            c <- None
            let rec loop () =
              if e.MoveNext () then
                tf.Invoke ((), e.Current)
                match c with
                | Some v  -> true
                | None    -> loop ()
              else
                false
            not ctx.IsCancelled && loop ()
          member x.Reset()      =
            c <- None
            e.Reset ()

        interface System.IDisposable with
          member x.Dispose()    = 
            Details.dispose e
            Details.dispose ctx
      }
    { new System.Collections.Generic.IEnumerable<'U> with
        member x.GetEnumerator() = gen ()
      interface System.Collections.IEnumerable with
        member x.GetEnumerator() = gen () :> System.Collections.IEnumerator
    }

module List =
  let inline transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U list) : 'S =
    use ctx             = new Context ()
    let tf              = t.BuildUp ctx f
    let tf              = adapt tf
    Loops.List.loop ctx tf z s

  let inline sequence (t : Transducer<_, _>) (s : 'T list) : 'U list =
    let f ls v  = v::ls
    let rls     = transduce t f [] s
    List.rev rls
#else

type Transducer<'S, 'TIn, 'TOut> = ('S -> 'S) -> ('S -> 'TOut -> 'S) -> ('S -> 'S)*('S -> 'TIn -> 'S)

module Details =
  let inline adapt f    = FSharp.Core.OptimizedClosures.FSharpFunc<_, _, _>.Adapt f

  let inline dbreak ()  = System.Diagnostics.Debugger.Break ()
  let inline result c f = c, f

  let inline transduceRest (ra : ResizeArray<_>) s (folder : OptimizedClosures.FSharpFunc<_, _, _>) completer =
    let mutable acc = s
    for i = 0 to (ra.Count - 1) do
      acc <- folder.Invoke (acc, ra.[i])
    completer acc

open Details

module Transducer =
  open System
 
  let inline compose (l : Transducer<_, _, _>) (r : Transducer<_, _, _>) : Transducer<_, _, _> = 
    fun completer folder -> 
      let c, f = r completer folder
      l c f

  [<GeneralizableValue>]
  let id<'S, 'T> : Transducer<'S, 'T, 'T> = 
    fun completer folder -> result completer folder

  let inline filtering f : Transducer<_, _, _> =
    fun completer folder -> 
      let folder = adapt folder
      result completer <| fun s v -> if f v then folder.Invoke (s, v) else s

  let inline filter f t = compose t (filtering f)

  let inline mapping m : Transducer<_, _, _> =
    fun completer folder -> 
      let folder = adapt folder
      result completer <| fun s v -> folder.Invoke (s, (m v))

  let inline map m t = compose t (mapping m)

  let inline taking n : Transducer<_, _, _> =
    fun completer folder -> 
      let folder      = adapt folder
      let mutable rem = n
      result completer <| fun s v -> 
        if rem > 0 then
          rem <- rem - 1
          folder.Invoke (s, v)
        else
          s

  let inline take n t = compose t (taking n)

  let inline skipping n : Transducer<_, _, _> =
    fun completer folder -> 
      let folder      = adapt folder
      let mutable rem = n
      result completer <| fun s v -> 
        if rem > 0 then
          rem <- rem - 1
          s
        else
          folder.Invoke (s, v)

  let inline skip n t = compose t (skipping n)

  let inline sortingBy (by : _ -> #IComparable) : Transducer<_, _, _> =
    fun completer folder -> 
      let folder  = adapt folder
      let ra      = ResizeArray 16
      let c s     =
        let comp  = System.Comparison<_> (fun l r -> (by l).CompareTo (by r))
        ra.Sort comp
        transduceRest ra s folder completer
      result c <| fun s v -> ra.Add v; s

  let inline sortBy by t = compose t (sortingBy by)

module Range =
  let inline transduce (t : Transducer<_, _, _>) (f : 'S -> 'T -> 'S) (z : 'S) (b : int) (e : int)  : 'S =
    let mutable acc     = z
    let tc, tf          = t id f
    let tf              = adapt tf
    for i = b to e do
      acc <- tf.Invoke (acc, i)
    tc acc

module Array =
  let inline transduce (t : Transducer<_, _, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U []) : 'S =
    let mutable acc     = z
    let tc, tf          = t id f
    let tf              = adapt tf
    for i = 0 to (s.Length - 1) do
      acc <- tf.Invoke (acc, s.[i])
    tc acc

  let inline sequence (t : Transducer<_, _, _>) (s : 'T []) : 'U [] =
    let f (ra : ResizeArray<_>) v   = ra.Add v; ra
    let ra                          = ResizeArray s.Length
    (transduce t f ra s).ToArray ()

#endif

module Expression =
  open System

  open FSharp.Quotations

  type TransducerExpression =
    | Compose     of TransducerExpression*TransducerExpression// ('S -> 'TOut -> 'S) -> ('S -> 'TIn -> 'S)
    | Filter      of Type*Expr // 'TIn -> bool
    | Map         of Type*Type*Expr // 'TIn -> 'TOut

  type Transducer<'TIn, 'TOut> = Transducer of TransducerExpression


  module Transducer =
    let compose (l : Transducer<'T, 'U>) (r : Transducer<'U, 'V>) : Transducer<'T, 'V> =
      let (Transducer le) = l
      let (Transducer re) = r
      Compose (le, re) |> Transducer

    let filtering (f : Expr<'TIn -> bool>) : Transducer<'TIn, 'TIn> =
      Filter (typeof<'TIn>, f) |> Transducer

    let filter f t = compose t (filtering f)

    let mapping (m : Expr<'TIn -> 'TOut>) : Transducer<'TIn, 'TOut> =
      Map (typeof<'TIn>, typeof<'TOut> ,m) |> Transducer

    let map m t = compose t (mapping m)

    let buildUp (t : Transducer<'TIn, 'TOut>) (f : Expr<'S -> 'TOut -> 'S>) : Expr<'S -> 'TIn -> 'S> =
      let (Transducer t) = t
      let rec build (st : Type) (tt : Type) (acc : Expr) (e : TransducerExpression) =
        let sv = Var ("s", st)
        match e with
        | Compose (l, r) ->
          let re = build st tt acc r
          let le = build st tt re l
          le
        | Filter (t, f) ->
          let vv = Var ("v", t)
          Expr.Lambda (
            sv, 
            Expr.Lambda (
              vv,
              Expr.IfThenElse (
                Expr.Application(f, Expr.Var vv),
                Expr.Application(
                  Expr.Application(acc, Expr.Var sv),
                  Expr.Var vv),
                Expr.Var vv
              )))
        | Map (t, mt, m) ->
          let vv = Var ("v", t)
          Expr.Lambda (
            sv, 
            Expr.Lambda (
              vv,
              Expr.Application(
                Expr.Application(acc, Expr.Var sv),
                Expr.Application(m, Expr.Var vv)
              )))


      let collapseLambdaApplications expr =
        let rec loop expr =
          match expr with
          | Patterns.Application (Patterns.Lambda (v, f), e) -> 
            (fun i -> Expr.Let (v, e, i)), f
          | Patterns.Application (Patterns.Application (_, _) as f, e) -> 
            let l0, f0 = loop f
            let l1, f1 = loop (Expr.Application (f0, e))
            (fun i -> l1 (l0 i)), f1
          | _ -> 
            (fun i -> i), expr
        let l, f = loop expr
        let e    = l f
        e
      let rec optimize expr =
        match expr with
        | Patterns.Let (v, e, l) ->
          Expr.Let (v, optimize e, optimize l)
        | Patterns.IfThenElse (t, tb, fb) ->
          let e = optimize tb
          Expr.IfThenElse (optimize t, optimize tb, optimize fb)
        | Patterns.Lambda (v, f)      -> 
          Expr.Lambda (v, optimize f)
        | Patterns.Application (Patterns.Let (v, e, ll), ee) ->
          Expr.Let (v, e, optimize (Expr.Application (ll, ee)))
        | Patterns.Application (_, _) -> 
          let collapsed = collapseLambdaApplications expr
          match collapsed with
          | Patterns.Application (f, e) -> Expr.Application (optimize f, optimize e)  // collapsed failed
          | _ -> optimize collapsed
        | _ -> expr

      let expr = build typeof<'S> typeof<'TOut> f.Raw t
      printfn "Original:\n%A" expr
      let expr = optimize expr
      printfn "Optimized:\n%A" expr

      expr |> Expr.Cast<_>

