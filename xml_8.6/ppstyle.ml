(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2016     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(*module String = CString*) (*modified by Jian Liu*)

type t = string
(** We use the concatenated string, with dots separating each string. We
    forbid the use of dots in the strings. *)

module Self = (*added by Jian Liu*)
struct
  type t = string
  let compare = compare
end

module String_map = Map.Make(Self) (*added by Jian Liu*)

let tags : Terminal.style option String_map.t ref = ref String_map.empty

let make ?style tag =
  let check s = if String.contains s '.' then invalid_arg "Ppstyle.make" in
  let () = List.iter check tag in
  let name = String.concat "." tag in
  let () = assert (not (String_map.mem name !tags)) in
  let () = tags := String_map.add name style !tags in
  name

let repr t = String.split_on_char '.' t

let get_style tag =
  try String_map.find tag !tags with Not_found -> assert false

let set_style tag st =
  try tags := String_map.add tag st !tags with Not_found -> assert false

let clear_styles () =
  tags := String_map.map (fun _ -> None) !tags

let dump () = String_map.bindings !tags

let parse_config s =
  let styles = Terminal.parse s in
  let set accu (name, st) =
    try String_map.add name (Some st) accu with Not_found -> accu
  in
  tags := List.fold_left set !tags styles

let tag = Pp.Tag.create "ppstyle"

(** Default tag is to reset everything *)
let default = Terminal.({
  fg_color = Some `DEFAULT;
  bg_color = Some `DEFAULT;
  bold = Some false;
  italic = Some false;
  underline = Some false;
  negative = Some false;
})

let empty = Terminal.make ()

let error_tag =
  let style = Terminal.make ~bold:true ~fg_color:`WHITE ~bg_color:`RED () in
  make ~style ["message"; "error"]

let warning_tag =
  let style = Terminal.make ~bold:true ~fg_color:`WHITE ~bg_color:`YELLOW () in
  make ~style ["message"; "warning"]

let debug_tag =
  let style = Terminal.make ~bold:true ~fg_color:`WHITE ~bg_color:`MAGENTA () in
  make ~style ["message"; "debug"]

let pp_tag t = match Pp.Tag.prj t tag with
| None -> ""
| Some key -> key
