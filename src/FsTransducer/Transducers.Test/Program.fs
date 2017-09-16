open Transducers
open FsCheck

[<EntryPoint>]
let main argv = 
  try
    System.Environment.CurrentDirectory <- System.AppDomain.CurrentDomain.BaseDirectory

#if !DEBUG
    PerformanceTests.test "perf.tsv"
#endif

    FunctionalTests.runTests ()
    0
  with
  | e ->
    printfn "Exception caught: %s" e.Message
    999
