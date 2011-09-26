
(* stdlib by wistery_k. *)

let ( $ ) f x = f x

let ( +> ) x f =
  match x with
    | None -> None
    | Some y -> Some(f y)

let swap f x y = f y x

let atoi = int_of_string
let atof = float_of_string

let maybe f x =
  try 
    Some (f x)
  with _ ->
    None

module String = struct

  include String

  let substr s start =
    String.sub s start (String.length s - start)

end

module List = struct

  include List

  let empty = function
    | [] -> true
    | _  -> false

  let iter lst f =
    List.iter f lst

  let fold_left lst cont f =
    List.fold_left f cont lst

  let fold_left' lst cont f =
    fst $ fold_left lst (cont,0) (fun (cont,i) x -> (f cont i x, i + 1)) 

  let map lst f =
    List.map f lst

  let rev_map lst f =
    List.rev_map f lst

  let filter lst f =
    List.filter f lst

  let separate lst f =
    let yes, no = fold_left lst ([],[]) begin
      fun (yes, no) x -> if f x then (x::yes, no) else (yes, x::no)
    end
    in
      (List.rev yes, List.rev no)

  let separate' lst f =
    let yes, no, _ = fold_left lst ([],[], 0) begin
      fun (yes, no, i) x -> if f x i then (x::yes, no, i+1) else (yes, x::no, i+1)
    end
    in
      (List.rev yes, List.rev no)

  let fold_left2 x y cont f =
    List.fold_left2 f cont x y

  let map2 x y f =
    List.map2 f x y

  let map2' (x,y) f =
    List.map2 f x y

  let rec fold_left3 x y z cont f =
    match x, y, z with
      | [], _, _
      | _, [], _
      | _, _, [] 
	  -> cont
      | x::xs, y::ys, z::zs 
	  -> fold_left3 xs ys zs (f cont x y z) f

  let map3 x y z f =
    List.rev $ fold_left3 x y z [] begin
      fun cont x y z -> (f x y z) :: cont
    end

  let rec fold_left4 x y z u cont f =
    match x, y, z, u with
      | [], _, _, _
      | _, [], _, _
      | _, _, [], _
      | _, _, _, []
	  -> cont
      | x::xs, y::ys, z::zs, u::us 
	  -> fold_left4 xs ys zs us (f cont x y z u) f

  let map4 x y z u f =
    List.rev $ fold_left4 x y z u [] begin
      fun cont x y z u -> (f x y z u) :: cont
    end

  let zip (lst1, lst2) =
    map2 lst1 lst2 begin
      fun x y -> (x,y)
    end

  let unzip lst =
    fold_left (List.rev lst) ([], []) begin
      fun (r1, r2) (x,y) -> (x::r1, y::r2)
    end

end
