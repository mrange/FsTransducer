module Transducers.FsLinq

open System
open System.Linq

let filter  f s = Enumerable.Where (s, Func<_, _> f)
let map     m s = Enumerable.Select(s, Func<_, _> m)
let take    n s = Enumerable.Take(s, n)
let toArray s   = Enumerable.ToArray(s)
let skip    n s = Enumerable.Skip(s, n)

