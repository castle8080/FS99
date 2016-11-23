module ListProblemsTest

open System
open ListProblems
open FsUnit

(*
  Runs some calls to List ListProblems
  The code below is not what I would call good practice for testing.
  It was done just to be quick and see some results.
*)

let problem name description work =
    printfn "---------------------------------------"
    printfn "%s: %s" name description
    printfn "---------------------------------------"
    try
        work()
        printfn "[PASS]"
    with
        | ex ->
            printfn "[FAIL]"
            printfn "%s" ex.Message
            printfn "%s" ex.StackTrace
let run =

    problem "p01" "Find the last box of a list." (fun () ->
        last [1;2] |> should equal 2
        last [7] |> should equal 7
    )

    problem "p02" "Find the last but one box of a list." (fun () ->
        butLast [1;1;2;3;5;8] |> should equal [5;8]
        butLast [1;8] |> should equal [1;8]
    )

    problem "p03" "Find the K'th element of a list." (fun () ->
        elementAt [1;2;3;4] 3 |> should equal 3
    )

    problem "p04" "Find the number of elements of a list." (fun () ->
        length [] |> should equal 0
        length [1] |> should equal 1
        length [1;2;3;4] |> should equal 4
    )

    problem "p05" "Reverse a list." (fun () ->
        reverse [1;2;3] |> should equal [3;2;1]
        reverse [] |> should equal []
    )

    problem "p06" "Find out whether a list is a palindrome." (fun () ->
        isPalindrome [1,2,1] |> should equal true
        isPalindrome [] |> should equal true
        isPalindrome [1;2] |> should equal false
    )

    problem "p07" "Flatten a nested list structure." (fun () ->
        let s = NestedListList [NestedListElement 1; NestedListList [NestedListElement 8; NestedListElement 9]]
        nestedFlatten s |> should equal [1;8;9]
    )

    problem "p08" "Eliminate consecutive duplicates of list elements." (fun () ->
        dedup [1;1;2;3;3;3;4] |> should equal [1;2;3;4]
        dedup [] |> should equal []
        dedup [1] |> should equal [1]
    )

    problem "p09" "Pack consecutive duplicates of list elements into sublists." (fun () ->
        pack [1;1;2;3;3;3] |> should equal [[1;1]; [2]; [3;3;3]]
    )

    problem "p10" "Run-length encoding of a list." (fun () ->
        encode ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
            |> should equal ['a', 4; 'b', 1; 'c', 2; 'a', 2; 'd', 1; 'e', 4]
    )

    problem "p11" "Modified run-length encoding. " (fun () ->
        encodeModified ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
            |> should equal [
                Multiple ('a', 4);
                Single 'b';
                Multiple ('c', 2);
                Multiple ('a', 2);
                Single 'd';
                Multiple ('e', 4)
            ]
    )

    problem "p12" "Decode a run-length encoded list." (fun () ->
        let r = [Single 9; Multiple (8, 4)]
        decode r |> should equal [9;8;8;8;8]
    )

    problem "p13" "Run-length encoding of a list (direct solution)." (fun () ->
        encodeDirect ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']
            |> should equal [
                Multiple ('a', 4);
                Single 'b';
                Multiple ('c', 2);
                Multiple ('a', 2);
                Single 'd';
                Multiple ('e', 4)
            ]
    )

    problem "p14" "Duplicate the elements of a list." (fun () ->
        dupli [1;1;2] |> should equal [1;1;1;1;2;2]
    )

    problem "p15" "Replicate the elements of a list a given number of times." (fun () ->
        repli [1;2] 3 |> should equal [1;1;1;2;2;2]
    )

    problem "p16" "Drop every N'th element from a list." (fun () ->
        drop [1;2;3;4;5;6;7] 3 |> should equal [1;2;4;5;7]
    )

    problem "p17" "Split a list into two parts; the length of the first part is given." (fun () ->
        split [1;2;3;4;5;6;7] 3 |> should equal ([1;2;3], [4;5;6;7])
    )

    problem "p18" "Extract a slice from a list." (fun () ->
        slice (Seq.toList "abcdefgh") 3 7 |> should equal (Seq.toList "cdefg")
    )

    problem "p19" "Rotate a list N places to the left." (fun () ->
        rotate (Seq.toList "abcdefgh") 3 |> should equal (Seq.toList "defghabc")
        rotate (Seq.toList "abcdefgh") -2 |> should equal (Seq.toList "ghabcdef")
        rotate (Seq.toList "abcdefgh") 0 |> should equal (Seq.toList "abcdefgh")
    )

    problem "p20" "Remove the K'th element from a list." (fun () ->
        removeAt (Seq.toList "abcdefgh") 3 |> should equal (Seq.toList "abdefgh")
        removeAt (Seq.toList "abcdefgh") 1 |> should equal (Seq.toList "bcdefgh")
        removeAt (Seq.toList "abcdefgh") 0 |> should equal (Seq.toList "abcdefgh")
    )

    problem "p21" "Insert an element at a given position into a list." (fun () ->
        insertAt 'x' (Seq.toList "abcdefgh") 3 |> should equal (Seq.toList "abcxdefgh")
        insertAt 'x' (Seq.toList "abcdefgh") 1 |> should equal (Seq.toList "xabcdefgh")
        insertAt 'x' (Seq.toList "abcdefgh") 0|> should equal (Seq.toList "xabcdefgh")
        insertAt 'x' (Seq.toList "abcdefgh") 100|> should equal (Seq.toList "abcdefghx")
    )

    problem "p22" "Create a list containing all integers within a given range." (fun () ->
        range 1 5 |> should equal [1;2;3;4;5]
        range 1 1 |> should equal [1]
        range 2 -2 |> should equal [2;1;0;-1;-2]
    )

