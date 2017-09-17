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

module Transducers.FsLinq

open System
open System.Linq
open System.Collections.Generic

let inline range   b e = Enumerable.Range    (b, e - b + 1)

let inline filter  f s = Enumerable.Where (s, Func<_, _> f)
let inline map     m s = Enumerable.Select   (s, Func<_, _> m)
let inline take    n s = Enumerable.Take     (s, n)
let inline skip    n s = Enumerable.Skip     (s, n)

let inline sortBy  by s= Enumerable.OrderBy (s, Func<_, _> by)

let inline toArray s   = Enumerable.ToArray  (s)
let inline sum     s   = Enumerable.Sum (s : IEnumerable<int64>)