﻿// ----------------------------------------------------------------------------------------------
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

#if !XXX
module Details =
  let inline adapt  f   = FSharpFunc<_, _, _>.Adapt f
  let inline dbreak ()  = System.Diagnostics.Debugger.Break ()

open Details

type Context(x : int) =
  class
    [<DefaultValue>]
    val mutable IsRunning : bool

    member x.IsCancelled = not x.IsRunning

    new () as x = Context (0) then x.IsRunning <- true

    member x.Cancel () = x.IsRunning <- false
  end

type Transducer<'TIn, 'TOut> =
  interface
    abstract BuildUp: Context -> ('S -> 'S) -> ('S -> 'TOut -> 'S) -> ('S -> 'S)*('S -> 'TIn -> 'S)
  end

module Transducer =
  let inline compose (l : Transducer<_, _>) (r : Transducer<_, _>) =
    { new Transducer<_, _> with
        member x.BuildUp ctx completer folder = 
          let c, f = r.BuildUp ctx completer folder
          l.BuildUp ctx c f
    }

  [<GeneralizableValue>]
  let id<'T> =
    { new Transducer<'T, 'T> with
        member x.BuildUp ctx completer folder = completer, folder
    }

  let inline filtering f =
    { new Transducer<_, _> with
        member x.BuildUp ctx completer folder = 
          let folder = adapt folder
          completer, fun s v -> if f v then folder.Invoke (s, v) else s
    }

  let inline filter f t = compose t (filtering f)

  let inline mapping m =
    { new Transducer<_, _> with
        member x.BuildUp ctx completer folder = 
          let folder = adapt folder
          completer, fun s v -> folder.Invoke (s, (m v))
    }

  let inline map m t = compose t (mapping m)

  let inline taking n =
    { new Transducer<_, _> with
        member x.BuildUp ctx completer folder = 
          let folder      = adapt folder
          let mutable rem = n
          completer, fun s v -> 
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
        member x.BuildUp ctx completer folder = 
          let folder      = adapt folder
          let mutable rem = n
          completer, fun s v -> 
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
      dbreak ()
      if i <= e && ctx.IsRunning then
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
    let ctx             = Context ()
    let tc, tf          = t.BuildUp ctx id f
    let tf              = adapt tf
    tc (Loops.Range.loop ctx tf e z b)

module Array =
  let inline transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U []) : 'S =
    let ctx             = Context ()
    let tc, tf          = t.BuildUp ctx id f
    let tf              = adapt tf
    tc (Loops.Array.loop ctx tf s z 0)

  let inline sequence (t : Transducer<_, _>) (s : 'T []) : 'U [] =
    let ra      = ResizeArray s.Length
    let f () v  = ra.Add v
    transduce t f () s
    ra.ToArray ()

module Seq =
  let inline transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U seq) : 'S =
    let ctx           = Context ()
    let tc, tf        = t.BuildUp ctx id f
    let tf            = adapt tf
    use e             = s.GetEnumerator ()
    tc (Loops.Seq.loop ctx tf e z)

  let inline sequence (t : Transducer<_, _>) (s : seq<'T>) : seq<'U> =
    let gen () : System.Collections.Generic.IEnumerator<'U> = 
      let mutable c = None
      let f _ v     = c <- Some v
      let ctx       = Context ()
      let tc, tf    = t.BuildUp ctx id f
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
                tc ()
                false
            not ctx.IsCancelled && loop ()
          member x.Reset()      =
            c <- None
            e.Reset ()

        interface System.IDisposable with
          member x.Dispose()    = 
            tc ()
            e.Dispose ()
      }
    { new System.Collections.Generic.IEnumerable<'U> with
        member x.GetEnumerator() = gen ()
      interface System.Collections.IEnumerable with
        member x.GetEnumerator() = gen () :> System.Collections.IEnumerator
    }

module List =
  let inline transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U list) : 'S =
    let ctx             = Context ()
    let tc, tf          = t.BuildUp ctx id f
    let tf              = adapt tf
    tc (Loops.List.loop ctx tf z s)

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
