
let const ( x : 'a ) = fun _ -> x 

let id ( x : 'a ) = x

let rec take ( lst : 'a list ) ( n : int ) = 
        if (n = 0) then [] else
                List.cons (List.hd lst) (take (List.tl lst) (n - 1))

let rec drop ( lst : 'a list ) ( n : int ) = 
        if (n = 0) then lst else
                  (drop (List.tl lst) (n - 1))

let rec replace ( lst : 'a list ) ( n : int ) ( v : 'a) = 
        if (n > (List.length lst - 1)) then lst else
                List.append (take lst n) (List.cons v (drop lst (n + 1)))

let rec permute = function
        | ([], ys)         -> []
        | ((x :: []) , ys) -> List.map (fun y -> (x , y)) ys
        | ((x :: xs) , ys) -> 
                        let new_lst = permute (xs, ys) in
                        let header  = List.map (fun y -> (x,y)) ys in
                        List.append header new_lst

let rotate_lst = function
        | [] -> []
        | (x :: xs ) -> List.append xs [x]

module type Types = sig
      type value
      type difficulty
      type cord
      type grid
      val get_row            : cord -> value
      val get_column         : cord -> value
      val from_int           : int  -> value
      val val_to_int         : value -> int
      val default_value      : unit -> value
      val default_difficulty : unit -> difficulty
      val  read_difficulty   : int      -> difficulty
      val dif_from_int       : difficulty -> int
      val make_cord          : value -> value -> cord
      val make_grid          : value -> grid
      val  next_cord         : cord -> cord option 
      val  assign_cord       : cord -> cord -> unit
      val  update_grid       : grid -> cord -> value -> unit
      val  get_val           : grid -> cord -> value
end

module ADT_Type : Types = struct

   type value_naked       = One | Two | Three | Four | Five | Six | Seven | Eight | Nine

   let val_to_int         = function
             Some One   -> 1
           | Some Two   -> 2
           | Some Three -> 3
           | Some Four  -> 4
           | Some Five  -> 5
           | Some Six   -> 6
           | Some Seven -> 7
           | Some Eight -> 8
           | Some Nine  -> 9
           | None         -> 0

   type value             = value_naked option
   let from_int = function
             1 -> Some One   
           | 2 -> Some Two   
           | 3 -> Some Three 
           | 4 -> Some Four  
           | 5 -> Some Five  
           | 6 -> Some Six   
           | 7 -> Some Seven 
           | 8 -> Some Eight 
           | 9 -> Some Nine  
           | _ -> None 

   let default_value      = const None
   type difficulty        = Easy | Medium | Hard
   let dif_from_int       = function
           |  Medium -> 1
           |  Hard   -> 2
           |  Easy   -> 0

   let read_difficulty    = function
           | 1 -> Medium 
           | 2 -> Hard  
           | _ -> Easy 

   let default_difficulty = const Easy
   type cord              = { mutable row : value_naked ; mutable column : value_naked }
   let get_row    c       = Some c.row
   let get_column c       = Some c.column

   let make_cord   r c    = 
                            let r_ = match r with 
                                     | Some v -> v
                                     | None   -> One 
                            in
                            let c_ = match c with 
                                     | Some v -> v
                                     | None   -> One 
                            in
                            { row = r_ ; column = c_ }

   type grid              = ((value list) list) ref

   let make_grid    v     = ref (List.init 9 (const (List.init 9 (const v)))) 
   let next_val : ( value_naked -> value_naked option)    = function
             One   -> Some Two 
           | Two   -> Some Three 
           | Three -> Some Four
           | Four  -> Some Five
           | Five  -> Some Six
           | Six   -> Some Seven
           | Seven -> Some Eight
           | Eight -> Some Nine
           | Nine  -> None

   let next_cord : ( cord -> cord option ) = function 
           | {row ; column} -> 
                           match next_val column with
                                  | None   -> ( match next_val row with
                                                 | None   -> None
                                                 | Some x -> Some { row = x ; column = One }  )
                                  | Some x -> Some { row = row ; column = x }
   let get_val g = function
           | { row ; column } -> List.nth (List.nth !g ((val_to_int (Some row) - 1))) ((val_to_int (Some column)) - 1)

   let update_grid  (g : grid) (c : cord)  (v : value ) = 
           let column_num = (val_to_int (get_column c) - 1) in
           let row_num    = (val_to_int (get_row c) - 1) in
           let new_row    = replace (List.nth !g row_num) column_num v in
           let new_grid   = replace !g row_num new_row in
           let ()         = g := new_grid in 
           ()


   let assign_cord arg { row ; column } = let () = 
                                          arg.row  <- row ; arg.column <- column
                                           in ()

end

module LowLevel_Type : Types = struct
 exception OutOfBounds of string
 let inRange n          = (n > 0 && n < 10)
 type value             = char
 let default_value      = const (Char.chr 0)
 let from_int n         = if (inRange n) then (Char.chr n) else (Char.chr 0)
 let val_to_int       v = let new_v = Char.code v in
                          if (inRange new_v) then new_v else 0
                          
 type difficulty        = char 
 let default_difficulty = const (Char.chr 0)
 let dif_from_int     n = if ((Char.code n) > 2) || ((Char.code n) < 0) then 0 else (Char.code n)
 let read_difficulty  n = if (n > 2) || (n < 0) then (Char.chr 0) else (Char.chr n)
 type cord              = bytes
 let blank_cord         = const (Bytes.init 2 (const (Char.chr 1)))
 type grid              = bytes array

 let make_grid    v     = [| 
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) ;
         (Bytes.init 9 (const v)) 
 |]

 let make_cord r c = let cord_ = Bytes.create 2 in
                     let () = Bytes.set cord_ 0 r in
                     let () = Bytes.set cord_ 1 c in
                     cord_

 let get_row    c = Bytes.get c 0
 let get_column c = Bytes.get c 1
 let next_cord (c : cord) : cord option = 
         match Char.code (Bytes.get c 1) with
                    | 0 -> None 
                    | 9 -> ( 
                            match Char.code (Bytes.get c 0) with
                            | 0 -> None
                            | 9 -> None
                            | n ->  let c = Bytes.copy c in
                                    let () = Bytes.set c 0 (Char.chr (n + 1)) in
                                    let () = Bytes.set c 1 (Char.chr 1) in
                                   Some c
                            )
                    | n -> if (n > 9) 
                            then raise (OutOfBounds " greater than 9 ")
                            else let c  = Bytes.copy c in 
                                 let () = Bytes.set c 1 (Char.chr (n + 1)) in
                                 Some c

  let get_val (g : grid) (c : cord) : value = 
    let () = assert (Array.length g = 9) in 
    let row = Array.get g (Char.code 
                              (get_row c) - 1)
    in
    let () = assert (Bytes.length row = 9) in 
    let colNum = (Char.code 
                   (get_column c)) - 1
    in
    let () = assert (colNum < 10) in
    let () = if (colNum > -1) then () 
                    else raise (OutOfBounds (string_of_int colNum))
    in
    Bytes.get row colNum 

  let update_grid g (c : cord) (v : value) : unit = 
    let () = assert (Array.length g = 9) in 
    let row = Array.get g (Char.code 
                              (get_row c) - 1)
    in
    let () = assert (Bytes.length row = 9) in 
    let colNum = (Char.code 
                   (get_column c)) - 1
    in
    let () = assert (colNum < 10) in
    let () = if (colNum > -1) then () 
                    else raise (OutOfBounds (string_of_int colNum))
    in
    Bytes.set row colNum v

  let assign_cord (cord1 : cord) (cord2 : cord) : unit = 
          let row    = Bytes.get cord2 0 in
          let column = Bytes.get cord2 1 in
          let () = Bytes.set cord1 0 row in
          let () = Bytes.set cord1 1 column in
          ()
                                
end

module TestTypes ( T : Types ) = struct 

        exception CordError of string

        let emiter = false

        let show_cord c = 
                       let lft = c |> T.get_row    |> T.val_to_int |> string_of_int in 
                       let rgt = c |> T.get_column |> T.val_to_int |> string_of_int in 
                       String.concat " ; " [ lft ; rgt ; "\n" ] 

        let emit_cord c = let () = c |> show_cord |> print_string in
                          let () = print_string "\n" in
                          ()

        let check_default_cord c =
                  let () = if ( (c |> T.get_row    |> T.val_to_int) = 1 ) then () else
                          raise (CordError "check_default_cord: row")
                  in
                  let () = if ( (c |> T.get_column |> T.val_to_int) = 1 ) then () else
                          raise (CordError "check_default_cord: column")
                  in ()

        let run = 
                  let v1 = T.make_cord (T.from_int 1) (T.from_int 1) in
                  let () = check_default_cord v1 in
                  let rec cord_t_r n = 
                          let () = if emiter then emit_cord v1 else () in
                          if (n > 0) then (
                                  match T.next_cord v1 with 
                                  | Some new_v -> let () = if emiter then ( 
                                                          let () = print_string " next_val: " in
                                                          let () = emit_cord new_v in ()
                                                  ) else () in
                                                    
                                                  let () = T.assign_cord v1 new_v in
                                                  cord_t_r (n - 1)
                                  | None       -> raise ( CordError " In Loop " )
                          )
                          else ()
                  in 
                  let () = cord_t_r 80 in
                  let () = match T.next_cord v1 with
                           | None -> ()
                           | _    -> raise ( CordError "After Loop" )
                  in 
                  let g1 = T.make_grid (T.from_int 0) in 
                  let v3 = T.make_cord (T.from_int 1) (T.from_int 1) in
                  let () = check_default_cord v3 in

                  let mark = T.from_int 3    in
                  let rec grid_t_r n = 
                          if (n > 0) then (
                                  let () = T.update_grid g1 v3 mark in
                                  let x = T.val_to_int (T.get_val g1 v3) in
                                  let () = assert (x = 3) in
                                  ( match (T.next_cord v3) with 
                                      | Some v -> let () = T.assign_cord v3 v in 
                                              grid_t_r (n - 1)
                                      | None   -> 
                                                      let str = String.concat " " [ " Empty val produced after: " ; (show_cord v3) ] in
                                                  raise ( CordError str )
                                  )
                          ) else ()
                  in
                  let () = grid_t_r 80 in

                  let g2 = T.make_grid (T.from_int 0) in 
                  let v4 = T.make_cord (T.from_int 1) (T.from_int 1) in
                  let () = check_default_cord v4 in

                  let rec grid_t_r2 n = 
                          if (n > 0) then (
                                  let x =  T.val_to_int (T.get_val g2 v4) in
                                  let () = assert (x = 0) in
                                  ( match (T.next_cord v4) with 
                                      | Some v -> let () = T.assign_cord v4 v in 
                                              grid_t_r2 (n - 1)
                                      | None   -> 
                                                      let str = String.concat " " [ " Empty val produced after: " ; (show_cord v4) ] in
                                                  raise ( CordError str )
                                  )
                          ) else ()
                  in
                  let () = grid_t_r2 80 in
                  print_string "passed\n" ; ()

end


module type Sudoku_api = sig 
        type value
        type difficulty
        type cord
        type grid
        val  read_grid   : string   -> grid 
        val  read_difficulty   : int      -> difficulty
        val  show_grid   : grid     -> string
        val  create_grid : difficulty  -> grid
        val  isSolved    : grid        -> bool
        val  isSolvable  : grid        -> bool
        val  isInvalid   : grid        -> bool
        val  update_grid : grid  -> cord -> value -> unit
        val  get_empties : grid  -> cord list
        val  get_filled  : grid  -> cord list
        val  copy_grid   : grid  -> grid
        val  solve       : grid  -> grid option
end

module MakeAPI ( T : Types ) : Sudoku_api = struct

        type value         = T.value
        type difficulty    = T.difficulty
        type cord          = T.cord
        type grid          = T.grid
        exception GridError of grid
        let update_grid    = T.update_grid

        let show_cord c = 
                    let lft = c |> T.get_row    |> T.val_to_int |> string_of_int in 
                    let rgt = c |> T.get_column |> T.val_to_int |> string_of_int in 
                    String.concat " ; " [ lft ; rgt ; "\n" ] 

        let read_difficulty = const ( T.default_difficulty () )

        let read_grid str   = 
                let g   = T.make_grid (T.from_int 0) in
                let crd = T.make_cord (T.from_int 1) (T.from_int 1) in
                let ()  = String.iter ( fun c ->
                        let n = match c with
                            | '0' -> 0
                            | '1' -> 1
                            | '2' -> 2
                            | '3' -> 3
                            | '4' -> 4
                            | '5' -> 5
                            | '6' -> 6
                            | '7' -> 7
                            | '8' -> 8
                            | '9' -> 9
                            | _   -> -1 
                        in
                        if ( n = -1 ) then () else
                                match T.next_cord crd with
                                 | Some v -> 
                                       let () = update_grid g crd (T.from_int n) in
                                       let () = T.assign_cord crd v in ()
                                 | None   -> 
                                       let () = update_grid g crd (T.from_int n) in
                                                 ()
                ) str in g

        let show_int n = if (n = 0) then "?" else (string_of_int n) 

        let show_row (row : int) (g : T.grid) = 
                let emit c = String.concat "" [ show_int (T.val_to_int (T.get_val g c)) ; " " ] in 
                let cords  = List.map (fun n -> T.make_cord (T.from_int row) (T.from_int n)) (List.init 9 (fun m -> 1 + m)) in
                let rec emit_three = function
                        |  x :: ( y :: ( z :: xs )) -> String.concat "" [ emit x ; emit y ; emit z ; "| " ; (emit_three xs) ]
                        | rest             -> ""
                in
                emit_three cords

        let show_grid (g : grid) = 
           String.concat ""
                [ 
                show_row 1 g ; "\n" ; show_row 2 g ; "\n" ; show_row 3 g ; "\n" ;
                "-----------------------\n" ;
                show_row 4 g ; "\n" ; show_row 5 g ; "\n" ; show_row 6 g ; "\n" ;
                "-----------------------\n" ;
                show_row 7 g ; "\n" ; show_row 8 g ; "\n" ; show_row 9 g ; "\n" 
                ]

        let gen_row () = 
                let rec f lst = 
                        if (List.length lst) = 9 then lst else 
                        let n = (Random.int 8) + 1 in
                        if List.mem n lst then f lst else
                        f (n :: lst) in
                f []

        let shift_three arg = arg |> rotate_lst |> rotate_lst |> rotate_lst


        let copy_grid g = g |> show_grid |> String.map ( fun c -> if (c = '?') then '0' else c) |> read_grid

        exception ValueReturnError

        let get_grid_row ( c : cord ) ( g : grid )    = 
                let r = T.get_row c in
                let rec get_grid_row_helper ( n : int ) (acum : value list) =
                        if (n = 0) then acum else
                           let v = T.get_val g (T.make_cord r (T.from_int n)) in
                           get_grid_row_helper (n - 1) (List.cons v acum)  
                        in 
                        get_grid_row_helper 9 []

        let get_grid_column ( c : cord ) ( g : grid )    = 
                let col = T.get_column c in
                let rec get_grid_column_helper ( n : int ) (acum : value list) =
                        if (n = 0) then acum else
                           let v = T.get_val g (T.make_cord (T.from_int n) col) in
                           get_grid_column_helper (n - 1) (List.cons v acum)  
                        in 
                        get_grid_column_helper 9 []

        type box_val   = 
                          First
                        | Second
                        | Third

        let box_val_offset = function
                | First  -> 0
                | Second -> 3
                | Third  -> 6

        let toBoxVal n = if n <= 3 then First else
                         if n <= 6 then Second else
                                       Third

        type box_cord  = BCord of box_val * box_val

        exception GetBoxError
        let get_box (g : grid) = function
                | BCord (row,column) -> 
                                let base = [1 ; 2 ; 3] in
                                let row_base    = List.map (fun x -> x + (box_val_offset row))    base in 
                                let column_base = List.map (fun x -> x + (box_val_offset column)) base in 
                                let cords = (
                                        match (row_base, column_base) with
                                        | ( ( x :: y :: z :: [] ) , ( x' :: y' :: z' :: [] )) -> 
                                                        [ 
                                                                (x, x') ; (x, y') ; (x, z') ;
                                                                (y, x') ; (y, y') ; (y, z') ;
                                                                (z, x') ; (z, y') ; (z, z') 
                                                        ]
                                        | _ -> raise GetBoxError
                                ) in
                                List.map (fun (x,y) -> T.get_val g (T.make_cord (T.from_int x) (T.from_int y))) cords


        let get_grid_box ( c : cord ) ( g : grid ) =
                let box_row    = c |> T.get_row    |> T.val_to_int |> toBoxVal in 
                let box_column = c |> T.get_column |> T.val_to_int |> toBoxVal in 
                get_box g (BCord (box_row, box_column))

        let val_repeats (lst : value list) (v : value) = 
                List.length (
                        List.filter (fun x -> x = v) 
                          ( List.filter
                           (fun x -> (T.val_to_int x) <> 0)
                           lst )
        ) > 1 

        let isInvalid_item (c : cord) (g : grid) = 
                let v   = T.get_val g c in
                let row = get_grid_row c g in
                if val_repeats row v then  true else
                        let column = get_grid_column c g in
                        if val_repeats column v then true else 
                                let box = get_grid_box c g in
                                val_repeats box v 


        let ints  = [ 1 ; 2; 3; 4; 5; 6; 7; 8; 9 ] 
        let cords = List.map (fun (x,y) -> T.make_cord (T.from_int x) (T.from_int y)) (permute (ints, ints)) 

        let get_empties g = 
                List.filter (fun c -> T.get_val g c |> T.val_to_int |> (fun x -> x = 0)) cords 

        let get_filled g = 
                List.filter (fun c -> T.get_val g c |> T.val_to_int |> (fun x -> x <> 0)) cords 

        let get_filled_by n g = 
                List.filter (fun c -> T.get_val g c |> T.val_to_int |> (fun x -> x = n)) cords 

        let possibles n g = 
                let empties = get_empties g in
                let filled  = get_filled_by n g in
                let f  c = 
                        let row    = T.get_row c      in
                        let column = T.get_column c   in
                        if List.mem row (List.map T.get_row filled) then false else
                          if List.mem column (List.map T.get_column filled) then false else
                                  true
                in
                List.filter f empties

        let split_into_boxes (lst : cord list) : (cord list) list =
                let ( first_rows , rest )        = List.partition (fun c -> (c |> T.get_row |> T.val_to_int) < 4) lst  in
                let ( second_rows , third_rows ) = List.partition (fun c -> (c |> T.get_row |> T.val_to_int) < 7) rest in
                let f arg = 
                        let (first_col, rest)       = List.partition (fun c -> (c |> T.get_column |> T.val_to_int) < 4) arg in
                        let (second_col, third_col) = List.partition (fun c -> (c |> T.get_column |> T.val_to_int) < 7) rest in
                        [ first_col ; second_col ; third_col ] 
                in
                List.append (List.append (f first_rows) (f second_rows)) (f third_rows)
        
        let search_num (n : int) (g : grid) : cord list = 

                let potentials = possibles n g in

                let v = T.from_int n in 
                let boxes = split_into_boxes potentials in
                let f = function
                        | [] -> false
                        | (x :: xs) -> ( 
                                        let box = get_grid_box x g in

                                        not (List.mem v box)
                        )
                in  

                List.flatten (List.filter (fun lst -> List.length lst = 1) (List.filter f boxes))

        let search_solve (g : grid) : grid = 
                let new_grid = copy_grid g in
                let () = List.iter ( fun n ->

                let search_space = search_num n new_grid in

                           List.iter ( fun c -> 
                                   let v = T.from_int n in 
                                   update_grid new_grid c v
                                     ) search_space 
                               ) ints
        in new_grid

        let isInvalid   g = 
                List.fold_left (||) false (
                        List.map (fun c -> isInvalid_item c g) (get_filled g)
                )

        let rec brute_solve g = 
                let empties = get_empties g in
                match empties with 
                | []             -> Some g
                | (focus :: _ )  -> 
                                let new_g = copy_grid g in
                                let rec helper n = 
                                        if n > 9 then None else
                                                let () = update_grid new_g focus (T.from_int n) in
                                                if not (isInvalid_item focus new_g) then 
                                                        ( match brute_solve new_g with
                                                            | None -> helper (n + 1)
                                                            | ans  -> ans
                                                        ) else helper (n + 1)
                                in helper 1
         let solve g = g |> search_solve |> brute_solve

         let isSolvable g = 
                 match brute_solve g with
                 | None   -> false
                 | Some _ -> true


        let isSolved    g = (not (isInvalid g))  && ((get_empties g ) = [])

        exception CreateGridError
        let create_grid level = 

                let init_row = gen_row () in

                let filler   = [ 0 ; 0 ; 0 ;
                                 0 ; 0 ; 0 ;
                                 0 ; 0 ; 0 
                ] in

                let init_grid = 
                        init_row                             @ filler @ filler @
                        (shift_three init_row)               @ filler @ filler @
                        (shift_three (shift_three init_row)) @ filler @ filler
                in        
                let filled = (
                       match  solve init_grid with
                       | Some g -> g
                       | None   -> raie CreateGridError
                ) in

                
                T.make_grid (T.from_int 0) 

end

module TestAPI(S : Sudoku_api) = struct
        let str = String.concat "" [
                " 5 3 0 0 7 0 0 0 0 " ;
                " 6 0 0 1 9 5 0 0 0 " ;
                " 0 9 8 0 0 0 0 6 0 " ;
                " 8 0 0 0 6 0 0 0 3 " ;
                " 4 0 0 8 0 3 0 0 1 " ;
                " 7 0 0 0 2 0 0 0 6 " ;
                " 0 6 0 0 0 0 2 8 0 " ;
                " 0 0 0 4 1 9 0 0 5 " ;
                " 0 0 0 0 8 0 0 7 9 " 
        ]

        let g = S.read_grid str

        let () = ( 
        match S.solve g with
        | None   -> print_string "solving failed \n"
        | Some v -> v |> S.show_grid |> print_string
        ) 

end

module type Program  = sig
        type grid
        type cord       
        val current_grid      : grid
        val current_cord      : cord
        val createGridforUser : unit -> unit
        val displayGrid       : unit -> unit
        val update_cord       : unit -> unit
        val update_at_cord    : unit -> unit
end

open TestAPI(MakeAPI(LowLevel_Type)) 
