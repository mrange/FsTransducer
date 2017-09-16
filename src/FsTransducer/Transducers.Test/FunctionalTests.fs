module Transducers.FunctionalTests

open Transducers

type Properties() =
  class
    static let apply vs t = Array.sequence t vs
    static member ``id`` (vs : int []) =
      let e = vs
      let a = Transducer.id |> apply vs
      e = a

    static member ``filtering`` (v : int) (vs : int []) =
      let r = abs v % 10 + 1
      let f = fun i -> i % r = 0
      let e = vs |> FsLinq.filter f |> FsLinq.toArray
      let a = Transducer.filtering f |> apply vs
      e = a

    static member ``mapping`` (v : int) (vs : int []) =
      let m = fun i -> i + v |> int64
      let e = vs |> FsLinq.map m |> FsLinq.toArray
      let a = Transducer.mapping m |> apply vs
      e = a

    static member ``taking`` (v : int) (vs : int []) =
      let n = abs v % (2*vs.Length + 1)
      let e = vs |> FsLinq.take n |> FsLinq.toArray
      let a = Transducer.taking n |> apply vs
      e = a

    static member ``skipping`` (v : int) (vs : int []) =
      let n = abs v % (2*vs.Length + 1)
      let e = vs |> FsLinq.skip n |> FsLinq.toArray
      let a = Transducer.skipping n |> apply vs
      e = a

    static member ``composing`` (v : int) (vs : int []) =
      let r = abs v % 10 + 1
      let f = fun i -> i % r = 0
      let m = fun i -> i + v |> int64
      let n = abs v % (2*vs.Length + 1)
      let e = vs |> FsLinq.skip n |> FsLinq.take n |> FsLinq.filter f |> FsLinq.map m |> FsLinq.toArray
      let a = Transducer.skipping n |> Transducer.take n |> Transducer.filter f |> Transducer.map m |> apply vs
      e = a

  end

open FsCheck

let runTests () =
  let config = { Config.Quick with MaxTest = 1000; MaxFail = 1000 }
  Check.All<Properties> config