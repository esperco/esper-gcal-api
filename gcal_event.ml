(* Google calendar events manipulation *)
open Gcal_api_t

(* Compare events by their start time. Events with start time
   corresponding to a None option are considered as all day events
   and are less than events with a start time. *)
let compare_by_start_time get_event e1 e2 =
  match (get_event e1).ev_start, (get_event e2).ev_start with
    Some {dateTime=Some time1}, Some {dateTime=Some time2} ->
      Util_time.compare time1 time2
  | Some {dateTime=None}, Some {dateTime=None} ->
      0
  | Some {dateTime=None}, Some {dateTime=Some time2} ->
      -1
  | Some {dateTime=Some time1}, Some {dateTime=None} ->
      1
  | _ -> assert false

(* Filters out cancelled events and sorts the result. get_event is
   a function that fetches an event from an element of l. All day events
   are considered less than events with a start time. *)
let sort_by_start_time l get_event =
  let scheduled_events =
    List.filter (fun x -> (get_event x).ev_start <> None) l
  in
  List.stable_sort (compare_by_start_time get_event) scheduled_events
