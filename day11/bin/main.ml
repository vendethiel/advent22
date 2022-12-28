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

type bag = Z.t list

type op = [ `Add of Z.t | `Mul of Z.t | `Square ]

type mathCheck = [ | `DivisibleBy of Z.t ]
type check = mathCheck * int * int

type monkey =
  { items: bag;
    operation: op;
    test: check;
    inspected: Z.t;
  }
type state = monkey list

exception ParseError of string

let eval_op = function
  | `Add x -> fun y -> Z.(x + y)
  | `Mul x -> fun y -> Z.(x * y)
  | `Square -> fun x -> Z.(x * x)

let eval_check check value = match check with
  | `DivisibleBy divi -> Z.(value mod divi = Z.zero)

let run_check (if_, then_, else_) value =
  if eval_check if_ value then then_ else else_

let run sm monkey =
  List.map (fun item ->
      let new_item = eval_op monkey.operation item in
      let reduced = Z.rem new_item sm in
      let move_to = run_check monkey.test reduced in
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

let forward (state : state) : state =
  let tests = List.map (fun m -> m.test) state in
  let divs = (List.map (fun (`DivisibleBy by, _, _) -> by) tests) in
  let sm = List.fold_left Z.mul Z.one divs in
  fold_lefti (fun state i ->
      let monkey = (List.nth state i) in
      let moves = run sm monkey in
      let state' = run_moves moves state in
      let updated_monkey =
        { monkey with
          items = [];
          inspected = Z.(monkey.inspected + of_int (List.length moves)) } in
      let state'' = replace state' i updated_monkey in
      state''
    ) state state

let rec process (state : state) = function
  | 0 -> state
  | rem -> process (forward state) (rem - 1)

let parse lines =
  let parse_items items_str =
    (match String.split_on_char ':' (remove ' ' items_str) with
     | [_; items_part2] ->
       List.map (fun x -> Z.of_int (int_of_string x)) (String.split_on_char ',' items_part2)
     | _ -> raise (ParseError "cannot parse items")) in
  let parse_op opf =
    (let opf_clean = remove ' ' opf
     in match String.split_on_char '*' opf_clean with
     | [_; "old"] -> `Square
     | [_; mult] -> `Mul (Z.of_int (int_of_string mult))
     | _ -> match String.split_on_char '+' opf_clean with
       | [_; addi] -> `Add (Z.of_int (int_of_string addi))
       | _ -> raise (ParseError "cannot parse update operation")) in
  let parse_test test =
    (match String.split_on_char ' ' (String.trim test) with
     | ["Test:"; "divisible"; "by"; div] -> `DivisibleBy (Z.of_int (int_of_string div))
     | _ -> raise (ParseError "cannot parse test")
    ) in
  let parse_if ifstr =
    (match String.split_on_char ' ' (String.trim ifstr) with
     | ["If"; _; "throw"; "to"; "monkey"; n] -> int_of_string n
     | _ -> raise (ParseError "cannot parse if")) in
  let parse_monkey_lines = function
    | [items_str; opf; test; then_s; else_s] ->
      let test = parse_test test, parse_if then_s, parse_if else_s in
      { items = parse_items items_str; operation = parse_op opf; test = test; inspected = Z.zero }
    | _ -> raise (ParseError "cannot parse monkey")
  in
  let rec parse_monkeys = function
    | [] | [""] -> []
    | (_::lines) -> (* Discard monkey number *)
      let monkey_lines = List.filteri (fun x _ -> x < 5) lines in
      parse_monkey_lines monkey_lines::parse_monkeys (drop 6 lines)
  in
  parse_monkeys lines

let items_string monkey = String.concat "," (List.map Z.to_string monkey.items)
let inspect monkeys = List.iteri (fun i m -> print_endline ("monkey " ^ string_of_int i ^ ": " ^ items_string m)) monkeys

let () =
  Printexc.record_backtrace true;
  let lines = read_lines "data.txt" in
  let state = parse lines in
  let processed = process state 10000 in
  print_endline (String.concat "\n" (List.map Z.to_string (List.map (fun m -> m.inspected) processed)));
    inspect processed
