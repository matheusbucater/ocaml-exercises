let test left right = 
    let result = left = right in
    match result with
    | true -> print_endline "SUCCESS"
    | false -> print_endline "FAILED";;

let rec last = function 
    | [] -> None
    | [x] -> Some x
    | _ :: tail -> last tail;;

let rec last_two = function
    | [] -> None
    | [_] -> None
    | [x; y] -> Some (x, y)
    | _ :: tail -> last_two tail;;

let rec nth n = function 
    | [] -> None
    | head :: tail -> if n = 0 then Some head else nth (n-1) tail;;

let rec length_aux count = function
    | [] -> count
    | _ :: tail -> length_aux (count + 1) tail;;

let length list = 
    length_aux 0 list;;

let rec rev_aux acc = function
    | [] -> acc
    | head :: tail -> rev_aux (head :: acc) tail;;

let rev list = 
    rev_aux [] list;;

let is_palindrome list = 
    list = rev list;;

type 'a node =
    | One of 'a
    | Many of 'a node  list;;

let rec flatten_aux acc = function
    | [] -> acc
    | One x :: tail -> flatten_aux (x :: acc) tail
    | Many xs :: tail -> flatten_aux (flatten_aux acc xs) tail;;

let flatten list = 
    rev (flatten_aux [] list);;

let () = print_string "LAST - test1: " in
test (last ["a" ; "b" ; "c" ; "d"]) (Some "d");;
let () = print_string "LAST - test2: " in
test (last []) None;;

let () = print_string "LAST_TWO - test1: " in
test (last_two ["a"; "b"; "c"; "d"]) (Some ("c", "d"));;
let () = print_string "LAST_TWO - test2: " in
test (last_two ["a"]) None;;

let () = print_string "NTH - test1: " in
test (nth 2 ["a"; "b"; "c"; "d"; "e"]) (Some "c");;
let () = print_string "NTH - test2: " in
test (nth 2 ["a"]) None;;

let () = print_string "LENGTH - test1: " in
test (length ["a"; "b"; "c"]) 3;;
let () = print_string "LENGTH - test2: " in
test (length []) 0;;

let () = print_string "REV - test1: " in
test (rev ["a"; "b"; "c"]) ["c"; "b"; "a"];;

let () = print_string "IS_PALINDROME - test1: " in
test (is_palindrome ["x"; "a"; "m"; "a"; "x"]) true;;
let () = print_string "IS_PALINDROME - test2: " in
test (is_palindrome ["a"; "b"]) false;;

let () = print_string "FLATTEN - test1: " in
test (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]) ["a"; "b"; "c"; "d"; "e"];;
