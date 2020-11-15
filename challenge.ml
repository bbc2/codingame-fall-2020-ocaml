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
    | Wait
end

module Input = struct
  type t =
    { actions : Action_info.t list
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

  let play ~(state : State.t) ~input:(_ : Input.t) : State.t * Output.t =
    (state, {Output.action = Wait})
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
  let make_action_info
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
      ; repeatable } : Action_info.t =
    { id
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

  let make_action_infos actions = actions |> List.map make_action_info

  let make_player_info {Raw.Player_info.inv_0; inv_1; inv_2; inv_3; score} :
      Player_info.t =
    {inv_0; inv_1; inv_2; inv_3; score}

  let make_input ~game:(_ : Raw.Input_game.t) ~(round : Raw.Input_round.t) =
    { Input.actions = make_action_infos round.actions
    ; info_self = make_player_info round.info_self
    ; info_other = make_player_info round.info_other }

  let unmake_action (action : Action.t) : Raw.Action.t =
    match action with
    | Brew {id} -> Brew {id}
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
