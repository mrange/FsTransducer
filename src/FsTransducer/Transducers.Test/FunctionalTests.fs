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
      let e = vs |> FsLinq.filter f   |> FsLinq.toArray
      let a = Transducer.filtering f  |> apply vs
      e = a

    static member ``mapping`` (v : int) (vs : int []) =
      let m = fun i -> i + v |> int64
      let e = vs |> FsLinq.map m    |> FsLinq.toArray
      let a = Transducer.mapping m  |> apply vs
      e = a

    static member ``taking`` (v : int) (vs : int []) =
      let n = abs v % (2*vs.Length + 1)
      let e = vs |> FsLinq.take n |> FsLinq.toArray
      let a = Transducer.taking n |> apply vs
      e = a

    static member ``skipping`` (v : int) (vs : int []) =
      let n = abs v % (2*vs.Length + 1)
      let e = vs |> FsLinq.skip n   |> FsLinq.toArray
      let a = Transducer.skipping n |> apply vs
      e = a

    static member ``sorting`` (vs : int []) =
      let by  = string
      let e   = vs |> FsLinq.sortBy by  |> FsLinq.toArray
      let a   = Transducer.sortingBy by |> apply vs
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