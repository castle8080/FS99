module ListProblemsTest

open System
open ListProblems

(*
  Runs some calls to List ListProblems
  The code below is not what I would call good practice for testing.
  It was done just to be quick and see some results.
  
  TODO: Add some real tests.
*)

let problem name description work =
    printfn "---------------------------------------"
    printfn "%s: %s" name description
    printfn "---------------------------------------"
    work()
    printfn ""

let run =

    problem "p01" "Find the last box of a list." (fun () ->
        printfn "%d" (last [1;2])
    )

    problem "p02" "Find the last but one box of a list." (fun () ->
        printfn "%s" ((butLast [1;1;2;3;5;8]).ToString())
    )

    problem "p03" "Find the K'th element of a list." (fun () ->
        printfn "%d" (elementAt [1;2;3;4] 3)
    )

    problem "p04" "Find the number of elements of a list." (fun () ->
        printfn "%d" (length [1])
    )

    problem "p05" "Reverse a list." (fun () ->
        printfn "%s" ((reverse [1;2;3]).ToString())
    )

    problem "p06" "Find out whether a list is a palindrome." (fun () ->
        printfn "%b" (isPalindrome [1,2,1])
        printfn "%b" (isPalindrome [1,2,2])
    )

    problem "p07" "Flatten a nested list structure." (fun () ->
        let s = NestedListList [NestedListElement 1; NestedListList [NestedListElement 8; NestedListElement 9]]
        printfn "%s" ((nestedFlatten s).ToString())
    )

    problem "p08" "Eliminate consecutive duplicates of list elements." (fun () ->
        printfn "%s" (String.Join(",", (dedup [1;1;2;3;3;3;4])))
    )

    problem "p09" "Pack consecutive duplicates of list elements into sublists." (fun () ->
        printfn "%s" ((pack [1;1;2;3;3;3]).ToString())
    )

    problem "p10" "Run-length encoding of a list." (fun () ->
        printfn "%s" (String.Join(",", (encode ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e'])))
    )

    problem "p11" "Modified run-length encoding. " (fun () ->
        printfn "%s" (String.Join(",", (encodeModified ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e'])))
    )

    problem "p12" "Decode a run-length encoded list." (fun () ->
        let r = [Single 9; Multiple (8, 4)]
        printf "%s" (String.Join(",", (decode r)))
    )

    problem "p13" "Run-length encoding of a list (direct solution)." (fun () ->
        printfn "%s" (String.Join(",", (encodeDirect ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e'])))
    )

    problem "p14" "Duplicate the elements of a list." (fun () ->
        printfn "%s" (String.Join(",", (dupli [1;1;2])))
    )

    problem "p15" "Replicate the elements of a list a given number of times." (fun () ->
        printfn "%s" (String.Join(",", (repli [1;2] 3)))
    )

    problem "p16" "Drop every N'th element from a list." (fun () ->
        printfn "%s" (String.Join(",", (drop [1;2;3;4;5;6;7] 3)))
    )

    problem "p17" "Split a list into two parts; the length of the first part is given." (fun () ->
        printfn "%s" (String.Join(",", (split [1;2;3;4;5;6;7] 3)))
    )

    problem "p18" "Extract a slice from a list." (fun () ->
        printfn "%s" (String.Join(",", (slice (Seq.toList "abcdefgh") 3 7)))
    )

    problem "p19" "Rotate a list N places to the left." (fun () ->
        printfn "%s" (String.Join(",", (rotate (Seq.toList "abcdefgh") 3)))
        printfn "%s" (String.Join(",", (rotate (Seq.toList "abcdefgh") -2)))
    )

