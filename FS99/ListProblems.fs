module ListProblems

(*
  For these problems I am not using built-ins
  until a similar function has already been done.
*)

(* P01 *)
let rec last = function
    | [] -> invalidArg "xs" "xs is empty"
    | [x] -> x
    | x :: xs' -> last xs'

(* P02 *)
let rec butLast = function
    | [] -> invalidArg "xs" "xs is empty"
    | [x] -> invalidArg "xs" "only 1 item in xs"
    | [x1; x2] -> [x1; x2]
    | x :: xs' -> butLast xs'

(* P03 *)
let rec elementAt xs n =
    match xs with
        | x :: _ when n = 1 -> x
        | x :: xs' when n > 1 -> elementAt xs' (n - 1)
        | [] -> invalidArg "n" "n is too large"
        | _ -> invalidArg "n" "n is too short"

(* P04 *)
let length xs =
    let rec lengthTr xs n =
        match xs with
            | [] -> n
            | x :: xs' -> lengthTr xs' (n + 1)
    lengthTr xs 0

(* P05 *)
let reverse xs =
    let rec revTr xs acc =
        match xs with
            | [] -> acc
            | x :: xs' -> revTr xs' (x :: acc)
    revTr xs []

(* P06 *)
let isPalindrome xs =
    let rec check = function
        | ([], []) -> true
        | (x :: xs, y :: ys) -> x = y && (check (xs, ys))
        | otherwise -> false
    check (xs, (reverse xs))


(* P07 *)
type NestedList<'a> =
    | NestedListElement of 'a
    | NestedListList of list<NestedList<'a>>

let rec nestedFlatten = function
    | NestedListElement e -> [e]
    | NestedListList items ->
        match items with
            | [] -> []
            | x :: xs ->
                (nestedFlatten x) @ (nestedFlatten (NestedListList xs))

(* P08 *)
let rec dedup = function
    | [] -> []
    | [x] -> [x]
    | x :: x' :: xs ->
        if x = x' then (dedup (x' :: xs))
        else x :: (dedup (x' :: xs))

(* P09 *)
let rec pack = function
    | [] -> []
    | x :: xs ->
        match (pack xs) with
            | [] -> [[x]]
            | (x' :: xs') :: ll ->
                if x = x' then (x :: x' :: xs') :: ll
                else [x] :: (x' :: xs') :: ll
            | _ -> failwith "Invalid result"

(* P10 *)
let encode xs =
    let rec doEncode = function
        | [] -> []
        | x :: xs -> ((List.head x), List.length x) :: (doEncode xs)
    doEncode (pack xs)

(* P11 *)
type CountedElem<'a> =
    | Single of 'a
    | Multiple of ('a * int)

let encodeModified xs =
    let rec doEncode = function
        | [] -> []
        | x :: xs ->
            let len = List.length x
            if len = 1 then (Single (List.head x)) :: (doEncode xs)
            else (Multiple (List.head x, len)) :: (doEncode xs)
    doEncode (pack xs)

(* P12 *)
let decode el =
    let rec expand x n results =
        if n = 0 then results
        else expand x (n - 1) (x :: results)
    let doExpand = function
        | Single x -> expand x 1 []
        | Multiple (x, n) -> expand x n []
    let rec doDecode = function
        | [] -> []
        | ce :: ces -> (doExpand ce) @ (doDecode ces)
    doDecode el

(* P13 *)
let rec encodeDirect = function
    | [] -> []
    | x :: xs ->
        match (encodeDirect xs) with
            | [] -> [Single x]
            | Single x'  :: xcs ->
                if x = x' then Multiple (x', 2) :: xcs
                else Single x :: Single x' :: xcs
            | Multiple (x', x'c) :: xcs ->
                if x = x' then Multiple (x', x'c + 1) :: xcs
                else Single x :: Multiple (x', x'c) :: xcs

(* P14 *)
let rec dupli = function
    | [] -> []
    | x :: xs -> x :: x :: (dupli xs)

(* P15 *)
let rec repli xs n =
    let rec repeat x n result =
        if n = 0 then result
        else repeat x (n - 1) (x :: result)
    let rec repliTr xs result =
        match xs with
            | [] -> result
            | x :: xs' -> repliTr xs' (repeat x n result)
    repliTr xs [] |> List.rev

(* P16 *)
let drop xs n =
    let rec doDrop xs n' =
        match xs with
            | [] -> []
            | x :: xs' ->
                if n' = n then
                    doDrop xs' 1
                else
                    x :: doDrop xs' (n' + 1)
    doDrop xs 1

(* P17 *)
let rec split xs n =
    if n = 0 then
        ([], xs)
    else
        match xs with
            | [] -> ([], [])
            | x :: xs' ->
                let (left, right) = split xs' (n - 1)
                (x :: left, right)

(* P18 *)
let slice xs first last =
    let rec doSlice xs n =
        match xs with
            | _ when n > last -> []
            | [] -> []
            | x :: xs' ->
                if n >= first then
                    x :: doSlice xs' (n + 1)
                else
                    doSlice xs' (n + 1)
    doSlice xs 1

(* P19 *)
let rotate xs n =
    let len = length xs
    let n = (if n < 0 then len + n else n) % len
    let (l1, l2) = split xs n
    l2 @ l1

(* P20 *)
let removeAt xs n =
    let len = length xs
    if n < 1 || n > len then
        xs
    else
        match (split xs (n - 1)) with
            | (l1, e :: l2) -> l1 @ l2
            | _ -> xs

(* P21 *)
let insertAt x xs n =
    let len = length xs
    if n <= 1 then
        x :: xs
    elif n >= len then
        xs @ [x]
    else
        let (l1,l2) = split xs n
        l1 @ (x :: l2)

(* P22 *)
let range start fin =
    let rec createRange start fin skip results =
        if start = fin then
            start :: results
        else
            createRange (start + skip) fin skip (start :: results)
    if start < fin then
        createRange fin start -1 []
    else
        createRange fin start 1 []