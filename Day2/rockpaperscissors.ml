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

(*stores a single game*)
type game = {opp: int; mutable me: int}

(* converts our input to a list of game type *)
let lines_to_games lines =
  let line_to_game line = {
      opp = ((Char.code @@ String.get line 0) - 2) mod 3;
      me = ((Char.code @@ String.get line 2) - 1) mod 3;
    }
  in
  let rec aux games = function
    | [] -> []
    | [x] ->  line_to_game x :: games
    | h :: t -> aux (line_to_game h :: games) t
  in
  aux [] lines

(*These evaluations depend on the possible winning states being a group
 * (0,1,2) where each member is beat by it's neighbor to the left
 *)

(* this was used to calculate who won for the first puzzle *)
let game_val_first game =
  let chicken_dinner game =
    if game.me = (game.opp + 2) mod 3 then 0 (* I lose *)
    else if game.me = (game.opp + 1) mod 3 then 2 (* I win*)
    else 1                                       (* draw *)
  in 
  3 * (chicken_dinner game) + (game.me + 1)

(* this was used to calculate who won for the second puzzle *)
let game_val_second game = match game.me with
  | 0 -> game.me <- (game.opp + 2) mod 3; game_val_first game (* lose *)
  | 1 -> game.me <- (game.opp); game_val_first game           (* draw *)
  | _ -> game.me <- (game.opp + 1) mod 3; game_val_first game (* win *)

(*produces a list of int values for the list of games*)
let games_to_values method_type games =
  let rec aux values = function
    | [] -> []
    | [x] -> (method_type x) :: values
    | h::t -> aux ((method_type h) :: values) t
  in aux [] games;;

(*Now we glue it together*)
Printf.printf "Total val first puzzle:%i\n"
  (lines_to_games lines |> games_to_values game_val_first |> List.fold_left (+) 0);;

Printf.printf "Total val second puzzle:%i\n"
  (lines_to_games lines |> games_to_values game_val_second |> List.fold_left (+) 0) 
