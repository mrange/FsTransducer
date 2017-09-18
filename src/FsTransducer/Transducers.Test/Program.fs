open Transducers

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


module TransducerTest =
  open Transducers

  let test () =
    let transducer =
      Transducer.mapping    int64
      |> Transducer.filter  (fun v -> v &&& 1L = 0L)
      |> Transducer.map     ((+) 1L)

    let v = Range.transduce transducer (+) 0L 0 100
    printfn "%A" v

[<EntryPoint>]
let main argv = 
  try
    System.Environment.CurrentDirectory <- System.AppDomain.CurrentDomain.BaseDirectory

    TransducerTest.test ()

#if !DEBUG
    PerformanceTests.test "perf.tsv"
#endif

    FunctionalTests.runTests ()
    0
  with
  | e ->
    printfn "Exception caught: %s" e.Message
    999
