let filename = Sys.argv.(1)

(*credit: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
(*read in lines from filename into list of ints*)
let lines =
  let ic = open_in filename in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

module CharSet = Set.Make(Char)

(* string to list of char *)
let explode s = List.init (String.length s) (String.get s)

(* make a set from string *)
let char_list_to_set list = List.fold_right CharSet.add list CharSet.empty

let char_val c =
    Char.code c |> fun c_int -> if c_int < 91 then c_int - 38 else c_int - 96
(*Calculates the value of a set acording to specifications of the puzzle*)
let set_value set =
  CharSet.fold (fun x y -> char_val x + y) set 0

(*Puzzle one specific functions*)
(*split the lines :Note broken for unequal length lines but 
  it seems that the puzzle has no such lines so no matter
*)
let divide_line line =
  let half = (String.length line)/2 in
  (explode @@ String.sub line 0 half, explode @@ String.sub line half half)


(*return Charset intersection of pair of char lists*)
let get_pair_intersection (left,right) =
  let left_set = char_list_to_set left in
  let right_set = char_list_to_set right in
  CharSet.inter left_set right_set


    (*now we glue it all together*)

let list_of_set_values =
  List.map divide_line lines
  |> List.map get_pair_intersection
  |> List.map set_value

;;Printf.printf "Puzzle one:%i\n" (List.fold_left (+) 0 list_of_set_values);;



