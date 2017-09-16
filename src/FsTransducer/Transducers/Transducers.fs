namespace  Transducers

type [<Struct>] Finalizer =
  | Action      of a : (unit -> unit)
  | Disposable  of d : System.IDisposable

module Details =
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
        member x.BuildUp ctx folder = fun s v -> if f v then folder s v else s
    }

  let inline filter f t = compose t (filtering f)

  let inline mapping m =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = fun s v -> folder s (m v)
    }

  let inline map m t = compose t (mapping m)

  let inline taking n =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = 
          let mutable rem = n
          fun s v -> 
            if rem > 0 then
              rem <- rem - 1
              folder s v
            else
              ctx.Cancel ()
              s
    }

  let inline take n t = compose t (taking n)

  let inline skipping n =
    { new Transducer<_, _> with
        member x.BuildUp ctx folder = 
          let mutable rem = n
          fun s v -> 
            if rem > 0 then
              rem <- rem - 1
              s
            else
              folder s v
    }

  let inline skip n t = compose t (skipping n)

module Range =
  let transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (b : int) (e : int)  : 'S =
    use ctx             = new Context ()
    let tf              = t.BuildUp ctx f
    let rec loop acc i  =
      if i <= e && not ctx.IsCancelled then
        loop (tf acc i) (i + 1)
      else 
        acc
    loop z b

module Array =
  let transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U []) : 'S =
    use ctx             = new Context ()
    let tf              = t.BuildUp ctx f
    let rec loop acc i  =
      if i < s.Length && not ctx.IsCancelled then
        loop (tf acc s.[i]) (i + 1)
      else 
        acc
    loop z 0

  let sequence (t : Transducer<_, _>) (s : 'T []) : 'U [] =
    let ra      = ResizeArray s.Length
    let f () v  = ra.Add v
    transduce t f () s
    ra.ToArray ()

module Seq =
  let transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U seq) : 'S =
    use ctx           = new Context ()
    let tf            = t.BuildUp ctx f
    use e             = s.GetEnumerator ()
    let rec loop acc  =
      if e.MoveNext () && not ctx.IsCancelled then
        loop (tf acc e.Current)
      else 
        acc
    loop z

  let sequence (t : Transducer<_, _>) (s : seq<'T>) : seq<'U> =
    let gen () : System.Collections.Generic.IEnumerator<'U> = 
      let mutable c = None
      let f _ v     = c <- Some v
      let ctx       = new Context ()
      let tf        = t.BuildUp ctx f
      let e         = s.GetEnumerator ()
      { new System.Collections.Generic.IEnumerator<'U> with
          member x.Current      = c.Value

        interface System.Collections.IEnumerator with
          member x.Current      = box c.Value
          member x.MoveNext ()  =
            c <- None
            let rec loop () =
              if e.MoveNext () then
                tf () e.Current
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
  let transduce (t : Transducer<_, _>) (f : 'S -> 'T -> 'S) (z : 'S) (s : 'U list) : 'S =
    use ctx             = new Context ()
    let tf              = t.BuildUp ctx f
    let rec loop acc ls =
      match ls with
      | h::t when not ctx.IsCancelled -> 
        loop (tf acc h) t
      | _ ->
        acc
    loop z s

  let sequence (t : Transducer<_, _>) (s : 'T list) : 'U list =
    let f ls v  = v::ls
    let rls     = transduce t f [] s
    List.rev rls
