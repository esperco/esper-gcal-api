(* Google calendar events manipulation *)
open Gcal_api_t

(* Compare events by their start time. Events with start time
   corresponding to a None option are considered as all day events
   and are less than events with a start time. *)
let compare_events_by_start_time get_event e1 e2 =
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
let sort_events_by_start_time l get_event =
  let scheduled_events =
    List.filter (fun x -> (get_event x).ev_start <> None) l
  in
  List.stable_sort (compare_events_by_start_time get_event) scheduled_events

(*
   Simplify a calendar obtained from CalendarsList into
   a calendar obtained from Calendars.

   Not sure why they are different things:
   https://developers.google.com/google-apps/calendar/concepts#calendar_vs_calendarList
*)
let calendar_metadata_of_calendar_list_item
    (x : calendar_list_item) : calendar_metadata =
  {
    cal_kind = x.ci_kind;
    cal_etag = x.ci_etag;
    cal_id = x.ci_id;
    cal_summary = x.ci_summary;
    cal_description = x.ci_description;
    cal_location = x.ci_location;
    cal_timeZone = x.ci_timeZone;
  }

let get_timezone x =
  match x.cal_timeZone with
  | None ->
      (* don't know if this ever happens *)
      "UTC"
  | Some tz ->
      tz
