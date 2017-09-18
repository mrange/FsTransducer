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

module TransducerExpressionTest =
  open Transducers.Expression
  open OptimizedClosures
  open FSharp.Linq.RuntimeHelpers.LeafExpressionConverter

  let test () =
    let transducer =
      Transducer.mapping    <@ int64 @>
      |> Transducer.filter  <@ fun v -> v &&& 1L = 0L @>
      |> Transducer.map     <@ ((+) 1L) @>
    let e = Transducer.buildUp transducer <@ (+) @>
    let l = QuotationToExpression e
    let f = EvaluateQuotation e :?> (int64 -> int -> int64)
    printfn "%s" (l.ToString ())
//    System.Diagnostics.Debugger.Break ()
    let f = FSharpFunc<_, _, _>.Adapt f
    let mutable acc = 0L
    for i = 0 to 10 do
      acc <- f.Invoke (acc, i)
      printfn "%A, %A" i acc
    printfn "%A" acc

open FSharp.Quotations
let test () =
  let rec loop expr =
    match expr with
    | Patterns.Application (Patterns.Lambda (v, f), e) -> 
      (fun i -> Expr.Let (v, e, i)), f
    | Patterns.Application (Patterns.Application (_, _) as f, e) -> 
      let l0, f0 = loop f
      let l1, f1 = loop (Expr.Application (f0, e))
      (fun i -> l1 (l0 i) ), f1
    | _ -> 
      (fun i -> i), expr

  let optimize expr =
    let l, f = loop expr
    let e    = l f
    e

  let e = <@ (fun x y -> x + y) 1 2 @>
  printfn "%A" e
  printfn "%A" (optimize e)
  

[<EntryPoint>]
let main argv = 
  try
    System.Environment.CurrentDirectory <- System.AppDomain.CurrentDomain.BaseDirectory

    test ()

    TransducerTest.test ()
    TransducerExpressionTest.test ()

#if !DEBUG
    PerformanceTests.test "perf.tsv"
#endif

    FunctionalTests.runTests ()
    0
  with
  | e ->
    printfn "Exception caught: %s" e.Message
    999
