module FS99

open System

(*

  This is a project to play around with F# by doing the 99 Lisp problems in F sharp:

  http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html

*)

[<EntryPoint>]
let main argv =
    ListProblemsTest.run
    1