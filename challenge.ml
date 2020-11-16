module Seq = struct
  include Seq

  let rec append (a : 'a Seq.t) (b : 'a Seq.t) () =
    match a () with
    | Nil -> b ()
    | Cons (x, tl) -> Cons (x, append tl b)
end

module Quat = struct
  type t =
    { x_0 : int
    ; x_1 : int
    ; x_2 : int
    ; x_3 : int }

  let add quat_0 quat_1 =
    { x_0 = quat_0.x_0 + quat_1.x_0
    ; x_1 = quat_0.x_1 + quat_1.x_1
    ; x_2 = quat_0.x_2 + quat_1.x_2
    ; x_3 = quat_0.x_3 + quat_1.x_3 }

  let positive quat =
    quat.x_0 >= 0 && quat.x_1 >= 0 && quat.x_2 >= 0 && quat.x_3 >= 0

  let size quat = quat.x_0 + quat.x_1 + quat.x_2 + quat.x_3
end

module Cast = struct
  type t =
    { id : int
    ; delta : Quat.t
    ; castable : bool }
end

module Brew = struct
  type t =
    { id : int
    ; delta : Quat.t
    ; price : int }
end

module Player_info = struct
  type t =
    { inventory : Quat.t
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

module Simulator = struct
  module State = struct
    type t =
      { profit : int
      ; first_action : Action.t option
      ; inventory : Quat.t
      ; brews : Brew.t list
      ; casts : Cast.t list }

    let next_states (state : t) : t Seq.t =
      let casts =
        state.casts
        |> List.to_seq
        |> Seq.filter_map (fun (cast : Cast.t) ->
               let target_inventory = Quat.add state.inventory cast.delta in
               if
                 cast.castable
                 && Quat.positive target_inventory
                 && Quat.size target_inventory <= 10
               then
                 Some
                   { state with
                     first_action =
                       ( match state.first_action with
                       | None -> Some (Cast {id = cast.id})
                       | Some _ -> state.first_action )
                   ; inventory = target_inventory
                   ; casts =
                       state.casts
                       |> List.filter (fun (c : Cast.t) -> c.id != cast.id)
                       |> List.cons {cast with castable = false} }
               else
                 None)
      in
      let brews =
        state.brews
        |> List.to_seq
        |> Seq.filter_map (fun (brew : Brew.t) ->
               let target_inventory = Quat.add state.inventory brew.delta in
               if Quat.positive target_inventory then
                 Some
                   { state with
                     first_action =
                       ( match state.first_action with
                       | None -> Some (Brew {id = brew.id})
                       | Some _ -> state.first_action )
                   ; profit = state.profit + brew.price
                   ; inventory = target_inventory
                   ; brews =
                       state.brews
                       |> List.filter (fun (b : Brew.t) -> b.id != brew.id) }
               else
                 None)
      in
      let rest =
        let all_castable = ref true in
        let target_casts =
          state.casts
          |> List.map (fun (cast : Cast.t) ->
                 if not cast.castable then all_castable := false;
                 {cast with castable = true})
        in
        if not !all_castable then
          Some
            { state with
              first_action =
                ( match state.first_action with
                | None -> Some Rest
                | Some _ -> state.first_action )
            ; casts = target_casts }
        else
          None
      in
      Seq.empty
      |> Seq.append brews
      |> Seq.append casts
      |> Seq.append (Option.to_seq rest)
  end

  let simulate ~(init : State.t) : State.t Seq.t =
    let stage_1 = State.next_states init in
    let stage_2 = Seq.append stage_1 (Seq.flat_map State.next_states stage_1) in
    let stage_3 = Seq.append stage_2 (Seq.flat_map State.next_states stage_2) in
    let stage_4 = Seq.append stage_3 (Seq.flat_map State.next_states stage_3) in
    let stage_5 = Seq.append stage_4 (Seq.flat_map State.next_states stage_4) in
    let stage_6 = Seq.append stage_5 (Seq.flat_map State.next_states stage_5) in
    let stage_7 = Seq.append stage_6 (Seq.flat_map State.next_states stage_6) in
    let stage_8 = Seq.append stage_7 (Seq.flat_map State.next_states stage_7) in
    stage_8
end

module Strategy = struct
  module State = struct
    type t = unit
  end

  let init = ()

  let can_make ~(player : Player_info.t) (brew : Brew.t) =
    Quat.add player.inventory brew.delta |> Quat.positive

  let rank_recipes (recipe_0 : Brew.t) (recipe_1 : Brew.t) =
    compare recipe_1.price recipe_0.price

  let max_profit ~init (states : Simulator.State.t Seq.t) =
    Seq.fold_left
      (fun (current_max : Simulator.State.t) (state : Simulator.State.t) ->
        if state.profit >= current_max.profit then
          state
        else
          current_max)
      init states

  let play ~(state : State.t) ~(input : Input.t) : State.t * Output.t =
    let init =
      { Simulator.State.profit = 0
      ; first_action = None
      ; brews = input.brews
      ; casts = input.casts_self
      ; inventory = input.info_self.inventory }
    in
    let best_state : Simulator.State.t =
      Simulator.simulate ~init |> max_profit ~init
    in
    let action : Action.t =
      match best_state.first_action with
      | None -> Wait
      | Some action -> action
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
    { id
    ; delta = {x_0 = delta_0; x_1 = delta_1; x_2 = delta_2; x_3 = delta_3}
    ; price }

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
    ; delta = {x_0 = delta_0; x_1 = delta_1; x_2 = delta_2; x_3 = delta_3}
    ; castable =
        ( match castable with
        | 0 -> false
        | 1 -> true
        | _ ->
          failwith (Printf.sprintf "Unexpected castable value: %d" castable) )
    }

  let make_player_info {Raw.Player_info.inv_0; inv_1; inv_2; inv_3; score} :
      Player_info.t =
    { Player_info.inventory =
        {x_0 = inv_0; x_1 = inv_1; x_2 = inv_2; x_3 = inv_3}
    ; score }

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
