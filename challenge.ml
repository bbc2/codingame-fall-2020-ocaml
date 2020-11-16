module Cast = struct
  type t =
    { id : int
    ; delta_0 : int
    ; delta_1 : int
    ; delta_2 : int
    ; delta_3 : int
    ; castable : bool }
end

module Brew = struct
  type t =
    { id : int
    ; delta_0 : int
    ; delta_1 : int
    ; delta_2 : int
    ; delta_3 : int
    ; price : int }
end

module Player_info = struct
  type t =
    { inv_0 : int
    ; inv_1 : int
    ; inv_2 : int
    ; inv_3 : int
    ; score : int }
end

module Action = struct
  type t =
    | Brew of {id : int}
    | Cast of {id : int}
    | Rest
    | Wait
end

module Input = struct
  type t =
    { brews : Brew.t list
    ; casts_self : Cast.t list
    ; casts_other : Cast.t list
    ; info_self : Player_info.t
    ; info_other : Player_info.t }
end

module Output = struct
  type t = {action : Action.t}
end

module Strategy = struct
  module State = struct
    type t = unit
  end

  let init = ()

  let can_make ~(player : Player_info.t) (brew : Brew.t) =
    player.inv_0 + brew.delta_0 >= 0
    && player.inv_1 + brew.delta_1 >= 0
    && player.inv_2 + brew.delta_2 >= 0
    && player.inv_3 + brew.delta_3 >= 0

  let rank_recipes (recipe_0 : Brew.t) (recipe_1 : Brew.t) =
    compare recipe_1.price recipe_0.price

  let play ~(state : State.t) ~(input : Input.t) : State.t * Output.t =
    let best_recipe =
      input.brews
      |> List.filter (can_make ~player:input.info_self)
      |> List.sort rank_recipes
      |> fun l -> List.nth_opt l 0
    in
    let action : Action.t =
      match best_recipe with
      | None -> Wait
      | Some recipe -> Brew {id = recipe.id}
    in
    (state, {action})
end

module Raw = struct
  module Action_info = struct
    type t =
      { id : int
      ; type_ : string
      ; delta_0 : int
      ; delta_1 : int
      ; delta_2 : int
      ; delta_3 : int
      ; price : int
      ; tome_index : int
      ; tax_count : int
      ; castable : int
      ; repeatable : int }
  end

  module Player_info = struct
    type t =
      { inv_0 : int
      ; inv_1 : int
      ; inv_2 : int
      ; inv_3 : int
      ; score : int }
  end

  module Action = struct
    type t =
      | Brew of {id : int}
      | Cast of {id : int}
      | Rest
      | Wait
  end

  module Input_game = struct
    type t = unit
  end

  module Input_round = struct
    type t =
      { actions : Action_info.t list
      ; info_self : Player_info.t
      ; info_other : Player_info.t }
  end

  module Output = struct
    type t = {action : Action.t}
  end
end

module Abstract = struct
  let make_brew
      { Raw.Action_info.id
      ; type_ = _
      ; delta_0
      ; delta_1
      ; delta_2
      ; delta_3
      ; price
      ; tome_index = _
      ; tax_count = _
      ; castable = _
      ; repeatable = _ } : Brew.t =
    {id; delta_0; delta_1; delta_2; delta_3; price}

  let make_cast
      { Raw.Action_info.id
      ; type_ = _
      ; delta_0
      ; delta_1
      ; delta_2
      ; delta_3
      ; price = _
      ; tome_index = _
      ; tax_count = _
      ; castable
      ; repeatable = _ } : Cast.t =
    { id
    ; delta_0
    ; delta_1
    ; delta_2
    ; delta_3
    ; castable =
        ( match castable with
        | 0 -> false
        | 1 -> true
        | _ ->
          failwith (Printf.sprintf "Unexpected castable value: %d" castable) )
    }

  let make_player_info {Raw.Player_info.inv_0; inv_1; inv_2; inv_3; score} :
      Player_info.t =
    {Player_info.inv_0; inv_1; inv_2; inv_3; score}

  let make_input ~game:(_ : Raw.Input_game.t) ~(round : Raw.Input_round.t) =
    { Input.brews =
        round.actions
        |> List.filter (fun action -> action.Raw.Action_info.type_ = "BREW")
        |> List.map make_brew
    ; casts_self =
        round.actions
        |> List.filter (fun action -> action.Raw.Action_info.type_ = "CAST")
        |> List.map make_cast
    ; casts_other =
        round.actions
        |> List.filter (fun action ->
               action.Raw.Action_info.type_ = "OPPONENT_CAST")
        |> List.map make_cast
    ; info_self = make_player_info round.info_self
    ; info_other = make_player_info round.info_other }

  let unmake_action (action : Action.t) : Raw.Action.t =
    match action with
    | Brew {id} -> Brew {id}
    | Cast {id} -> Cast {id}
    | Rest -> Rest
    | Wait -> Wait

  let unmake_output ~(output : Output.t) : Raw.Output.t =
    {action = unmake_action output.action}
end

module Io = struct
  let make_action_info
      id
      type_
      delta_0
      delta_1
      delta_2
      delta_3
      price
      tome_index
      tax_count
      castable
      repeatable =
    { Raw.Action_info.id
    ; type_
    ; delta_0
    ; delta_1
    ; delta_2
    ; delta_3
    ; price
    ; tome_index
    ; tax_count
    ; castable
    ; repeatable }

  let make_player_info inv_0 inv_1 inv_2 inv_3 score =
    {Raw.Player_info.inv_0; inv_1; inv_2; inv_3; score}

  let action_to_str (action : Raw.Action.t) : string =
    match action with
    | Brew {id} -> Printf.sprintf "BREW %d" id
    | Cast {id} -> Printf.sprintf "CAST %d" id
    | Rest -> "REST"
    | Wait -> "WAIT"

  let output_to_str (output : Raw.Output.t) : string =
    action_to_str output.action

  let dump_output ~output = output |> output_to_str |> print_endline

  let load_input_game () = ()

  let load_input_round () =
    let action_count = int_of_string (input_line stdin) in
    let actions =
      List.init action_count (fun _ ->
          Scanf.sscanf (input_line stdin)
            " %d  %s  %d  %d  %d  %d  %d  %d  %d  %d  %d" make_action_info)
    in
    let info_self =
      Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d" make_player_info
    in
    let info_other =
      Scanf.sscanf (input_line stdin) " %d  %d  %d  %d  %d" make_player_info
    in
    {Raw.Input_round.actions; info_self; info_other}
end

let rec loop ~game ~state =
  let round = Io.load_input_round () in
  let input = Abstract.make_input ~game ~round in
  let (state, output) = Strategy.play ~state ~input in
  Io.dump_output ~output:(Abstract.unmake_output ~output);
  loop ~game ~state

let main () =
  let game = Io.load_input_game () in
  let state = Strategy.init in
  loop ~game ~state
