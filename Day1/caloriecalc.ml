(*get filename from command line*)
let filename = Sys.argv.(1)

(*credit: https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
(*read in lines from filename into list of ints*)
let lines =
  let ic = open_in filename in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let int_of_string_p str = try int_of_string str with e -> 0 in
  let rec loop acc = match try_read () with
    | Some s -> loop (int_of_string_p s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

(*all newlines are  0 so split into list of lists at 0*)
let split_at_delim list d =
  let rec aux acc current = function
    | [] -> [] (*only if list = []*)
    | [x] -> if x = d then [] else (x :: current) :: acc
    | h :: t ->
       if h = d then aux (current :: acc) [] t
       else aux acc (h :: current) t
  in List.rev (aux [] [] list)

(*sum each sublist*)
let pack_to_int list =
  List.map (List.fold_left (+) 0) list

(*helper type*)
type valpair = {index : int; value : int}

(*converts list of ints to valpairs like before*)
let pack_to_pairs list =
  let rec aux ind = function
    | [] -> []
    | [x] -> [{index = ind; value = x}]
    | h::t -> {index = ind; value = h} :: (aux (ind + 1) t)
  in
  aux 0 list

(*combines all the functions to produce the desired output*)
let sorted_list =
  split_at_delim lines 0 |>
    pack_to_int |>
    pack_to_pairs |>
    List.sort (fun x y -> compare x.value y.value) |>
    List.rev;;

Printf.printf "The top three\n";;
for i = 0 to 2 do
  Printf.printf "at %i value is %i\n"
    (List.nth sorted_list i).index
    (List.nth sorted_list i).value
  done;;

(*surprisingly there is no slice in ocaml so this for loop shall do*)
let sum = ref 0;;
for i = 0 to 2 do
  sum := !sum + (List.nth sorted_list i).value
done;;

Printf.printf "Total = %i\n" !sum;;
