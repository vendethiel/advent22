let read_lines filename =
  let f = open_in filename in
  let rec loop () =
    try
      let next = input_line f in
      next :: loop ()
    with End_of_file ->
      close_in f;
      []
  in loop ()

let const (a : 'a) _ : 'a = a
(* Fold a list on its keys *)
let fold_lefti fn (state : 'a) list : 'a = List.fold_left fn state (List.mapi const list)
(* replace element at pos with a in l *)
let replace (l : 'a) pos a : 'a  = List.mapi (fun i x -> if i = pos then a else x) l
(* remove char from string *)
let remove erase s =
  let b = Buffer.create 10 in
  String.iter (fun c -> if c != erase then Buffer.add_char b c) s;
  Buffer.contents b
(* drop elements from a list *)
let rec drop n xs = match (n, xs) with
  | (_, []) -> []
  | (0, xs) -> xs
  | (n, _::xs) -> drop (n - 1) xs

type bag = int list

type op = [ `Add of int | `Mul of int | `Square ]

type mathCheck = [ | `DivisibleBy of int ]
type check = mathCheck * int * int

type monkey =
  { items: bag;
    operation: op;
    test: check;
    inspected: int;
  }
type state = monkey list

exception ParseError of string

let eval_op = function
  | `Add x -> fun y -> x + y
  | `Mul x -> fun y -> x * y
  | `Square -> fun x -> x * x

let eval_check check value = match check with
  | `DivisibleBy div -> value mod div = 0

let run_check (if_, then_, else_) value =
  if eval_check if_ value then then_ else else_

let run monkey =
  List.map (fun item ->
      print_endline ("item = " ^ string_of_int item);
      let new_item = eval_op monkey.operation item in
      print_endline ("new_item = " ^ string_of_int new_item);
      let reduced = Float.to_int (Float.floor ((Float.of_int new_item) /. 3.)) in
      print_endline ("reduced = " ^ string_of_int reduced);
      let move_to = run_check monkey.test reduced in
      print_endline ("thrown = " ^ string_of_int move_to ^ "\n;;");
      (reduced, move_to)
    ) monkey.items

let run_moves moves state = List.fold_left (fun state (new_item, move_to) ->
    let monkey_to_update = List.nth state move_to in
    let updated_items = List.append monkey_to_update.items [new_item] in
    let updated_monkey = { monkey_to_update with
                           items = updated_items } in
    let updated_state = replace state move_to updated_monkey in
    updated_state
  ) state moves

let forward (state : state) : state = fold_lefti (fun state i ->
    let monkey = (List.nth state i) in
    print_endline ("[START MONKEY " ^ string_of_int i);
    let moves = run monkey in
    let state' = run_moves moves state in
    let updated_monkey = { monkey with items = []; inspected = monkey.inspected + (List.length moves) } in
    let state'' = replace state' i updated_monkey in
    print_endline ("END MONKEY " ^ string_of_int i ^ "]");
    state''
  ) state state

let rec process (state : state) = function
  | 0 -> state
  | rem -> process (forward state) (rem - 1)

let parse lines =
  let parse_items items_str =
    (match String.split_on_char ':' (remove ' ' items_str) with
     | [_; items_part2] ->
       List.map int_of_string (String.split_on_char ',' items_part2)
     | _ -> raise (ParseError "cannot parse items")) in
  let parse_op opf =
    (let opf_clean = remove ' ' opf
     in match String.split_on_char '*' opf_clean with
     | [_; "old"] -> `Square
     | [_; mult] -> `Mul (int_of_string mult)
     | _ -> match String.split_on_char '+' opf_clean with
       | [_; addi] -> `Add (int_of_string addi)
       | _ -> raise (ParseError "cannot parse update operation")) in
  let parse_test test =
    (match String.split_on_char ' ' (String.trim test) with
     | ["Test:"; "divisible"; "by"; div] -> `DivisibleBy (int_of_string div)
     | _ -> raise (ParseError "cannot parse test")
    ) in
  let parse_if ifstr =
    (match String.split_on_char ' ' (String.trim ifstr) with
     | ["If"; _; "throw"; "to"; "monkey"; n] -> int_of_string n
     | _ -> raise (ParseError "cannot parse if")) in
  let parse_monkey_lines = function
    | [items_str; opf; test; then_s; else_s] ->
      let test = parse_test test, parse_if then_s, parse_if else_s in
      { items = parse_items items_str; operation = parse_op opf; test = test; inspected = 0 }
    | _ -> raise (ParseError "cannot parse monkey")
  in
  let rec parse_monkeys = function
    | [] | [""] -> []
    | (_::lines) -> (* Discard monkey number *)
      let monkey_lines = List.filteri (fun x _ -> x < 5) lines in
      print_endline "a";
      List.iter (fun x -> print_endline ("[" ^ x ^ "]")) monkey_lines;
        print_endline "b";
      parse_monkey_lines monkey_lines::parse_monkeys (drop 6 lines)
  in
  parse_monkeys lines

let items_string monkey = String.concat "," (List.map string_of_int monkey.items)
let inspect monkeys = List.iteri (fun i m -> print_endline ("monkey " ^ string_of_int i ^ ": " ^ items_string m)) monkeys

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "data.txt" in
  let state = parse lines in
  let processed = process state 20 in
  inspect processed
