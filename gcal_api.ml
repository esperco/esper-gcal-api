open Printf
open Lwt
open Log
open Util_url.Op
open Gcal_api_t

module Http = Util_http_client.Wrap (Util_http_client.Original) (struct
  type orig_result = Util_http_client.response
  type result = Util_http_client.response
  let wrap f = Cloudwatch.time "google.api.calendar.any" f
end)

type http_response = Util_http_client.response
type with_token = (string -> http_response Lwt.t) -> http_response Lwt.t

let http_fail call_name (status, headers, body) =
  Google_http.fail call_name status body

let error_message_starts_with ~prefix json_body =
  try
    let error_resp = Gcal_api_j.error_response_of_string json_body in
    BatString.starts_with error_resp.error.message prefix
  with _ ->
    false

let url_encode s = Util_url.encode s

let unquote s =
  match Yojson.Basic.from_string s with
  | `String s -> s
  | _ -> assert false

let int_of_access_role (x: access_role) =
  match x with
  | `None -> 0
  | `FreeBusyReader -> 1
  | `Reader -> 2
  | `Writer -> 3
  | `Owner -> 4

let compare_access_role a b =
  compare (int_of_access_role a) (int_of_access_role b)

let call_calendar_list
    ?minAccessRole ?maxResults ?pageToken
    ?showHidden access_token =
  let string_of_access_role x =
    unquote (Gcal_api_j.string_of_access_role x) in
  let string x = x in
  let query = ("maxResults",    string_of_int,         maxResults)
          @^@ ("minAccessRole", string_of_access_role, minAccessRole)
          @^@ ("pageToken",     string,                pageToken)
          @^@ ("showHidden",    string_of_bool,        showHidden)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~query
      "/calendar/v3/users/me/calendarList" in
  Http.get ~headers:[Google_auth.auth_header access_token] uri

let calendar_list
    ?minAccessRole ?maxResults ?pageToken ?showHidden
    with_token =
  Cloudwatch.time "google.api.calendar.calendar_list" (fun () ->
    with_token (fun token ->
      call_calendar_list ?minAccessRole ?maxResults ?pageToken ?showHidden token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.calendar_list_response_of_string body in
        return (Some result)
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "calendar_list" x
  )

(*
   Fetch all the pages returned by calendar_list.
   The parameters are the same as those supported by events_list,
   except for:
   - maxResults, which refers to the total number of results
     to return, not a page size;
   - pageToken, which is not available here.
*)
let calendar_list_unpaged
  ?minAccessRole
  ?maxResults (* maximum number of results to return, in total *)
  ?showHidden
  with_token =

  let rec loop acc max_remaining page_token =
    let maxResults =
      match max_remaining with
      | None -> 250 (* maximum page size supported by Google *)
      | Some n -> min n 250
    in
    calendar_list
      ?minAccessRole
      ~maxResults
      ?pageToken:page_token
      ?showHidden
      with_token
    >>= function
    | None ->
        return None
    | Some x ->
        let acc = List.rev_append x.cl_items acc in
        let max_remaining =
          match max_remaining with
          | None -> None
          | Some m -> Some (m - List.length acc)
        in
        (match x.cl_nextPageToken, max_remaining with
         | Some page_token, Some max_remaining when max_remaining > 0 ->
             loop acc (Some max_remaining) (Some page_token)
         | Some page_token, None ->
             loop acc None (Some page_token)
         | _ ->
             return (Some (List.rev acc))
        )
  in
  loop [] maxResults None

let call_calendar_list_get calendar_id access_token =
  let uri =
    Google_api_util.make_uri
      ("/calendar/v3/users/me/calendarList/"
       ^ url_encode (Gcalid.to_string calendar_id))
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri

let calendar_list_get calendar_id with_token =
  Cloudwatch.time "google.api.calendar.calendar_list_get" (fun () ->
    with_token (fun token ->
      call_calendar_list_get calendar_id token
    )  >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.calendar_list_item_of_string body in
        return (Some result)
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "calendar_list_get" x
  )

let call_update_calendar_list_item ?colorRgbFormat calid edit access_token =
  let query = ("colorRgbFormat", string_of_bool, colorRgbFormat) @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/users/me/calendarList/" ^ Gcalid.to_string calid)
  in
  let content_type =
    (* Without this header, Google will return 200 but not actually update! *)
    ("Content-Type", "application/json")
  in
  let headers = [Google_auth.auth_header access_token; content_type] in
  let body = Gcal_api_j.string_of_calendar_list_item_edit edit in
  Http.patch ~headers ~body uri

let update_calendar_list_item ?colorRgbFormat calid edit with_token =
  Cloudwatch.time "google.api.calendar.update_calendar_list_item" (fun () ->
    with_token (fun token ->
      call_update_calendar_list_item ?colorRgbFormat calid edit token
    ) >>= function
  | `OK, _headers, body ->
      let result = Gcal_api_j.calendar_list_item_of_string body in
      return (Some result)
  | `Not_found, _, _ ->
      return None
  | x ->
      http_fail "update_calendar_list_item" x
  )

let call_freebusy
    ?timeZone
    ?groupExpansionMax ?calendarExpansionMax
    ~timeMin ~timeMax calendar_ids access_token =
  let bzq_items = List.map (fun id -> {Gcal_api_t.cid_id = id})
                    calendar_ids in
  let q = {Gcal_api_t.bzq_timeMin = timeMin; bzq_timeMax = timeMax;
           bzq_timeZone = timeZone;
           bzq_groupExpansionMax = groupExpansionMax;
           bzq_calendarExpansionMax = calendarExpansionMax;
           bzq_items} in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let url = Google_api_util.make_uri "/calendar/v3/freeBusy" in
  Http.post ~headers
    ~body:(Gcal_api_j.string_of_freebusy_request q)
    url

let freebusy
    ?timeZone
    ?groupExpansionMax ?calendarExpansionMax
    ~timeMin ~timeMax calendar_ids
    with_token =
  Cloudwatch.time "google.api.calendar.freebusy" (fun () ->
    with_token (fun token ->
      call_freebusy ?timeZone
        ?groupExpansionMax ?calendarExpansionMax
        ~timeMin ~timeMax calendar_ids
        token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.freebusy_response_of_string body in
        return (Some result)
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "freebusy" x
  )

(*
   Subset of Cohttp.Code.status_code;
   other statuses are converted into exceptions.
*)
type events_list_result = [
  | `OK of events_list_response
  | `Not_found
  | `Gone
]

let string_of_property (k, v) =
  sprintf "%s=%s" k v

let call_events_list
    ?alwaysIncludeEmail
    ?iCalUID
    ?maxAttendees
    ?maxResults
    ?orderBy
    ?pageToken
    ?privateExtendedProperties
    ?q
    ?sanitizeHtml
    ?sharedExtendedProperties
    ?showDeleted
    ?showHiddenInvitations
    ?singleEvents
    ?syncToken
    ?timeMax
    ?timeMin
    ?timeZone
    ?updatedMin
    calendar_id access_token =

  let string_of_order_by x =
    unquote (Gcal_api_j.string_of_event_order_by x) in
  let string x = x in
  let query = ("alwaysIncludeEmail",    string_of_bool, alwaysIncludeEmail)
          @^@ ("iCalUID",               string,         iCalUID)
          @^@ ("maxAttendees",          string_of_int,  maxAttendees)
          @^@ ("maxResults",            string_of_int,  maxResults)
          @^@ ("orderBy",               string_of_order_by, orderBy)
          @^@ ("pageToken",             string,         pageToken)
          @^@ ("privateExtendedProperty", string_of_property,
                                          privateExtendedProperties)
          @^^@ ("q",                    string,         q)
          @^@ ("sanitizeHtml",          string_of_bool, sanitizeHtml)
          @^@ ("sharedExtendedProperty", string_of_property,
                                         sharedExtendedProperties)
          @^^@ ("showDeleted",          string_of_bool, showDeleted)
          @^@ ("showHiddenInvitations", string_of_bool, showHiddenInvitations)
          @^@ ("singleEvents",          string_of_bool, singleEvents)
          @^@ ("syncToken",             string, syncToken)
          @^@ ("timeMax",               Util_time.to_string, timeMax)
          @^@ ("timeMin",               Util_time.to_string, timeMin)
          @^@ ("timeZone",              string, timeZone)
          @^@ ("updatedMin",            Util_time.to_string, updatedMin)
          @^@ [] in
  let calid = Gcalid.to_string calendar_id in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/calendars/" ^ url_encode calid ^ "/events")
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri

let events_list
    ?alwaysIncludeEmail
    ?iCalUID
    ?maxAttendees
    ?maxResults
    ?orderBy
    ?pageToken
    ?privateExtendedProperties
    ?q
    ?sanitizeHtml
    ?sharedExtendedProperties
    ?showDeleted
    ?showHiddenInvitations
    ?singleEvents
    ?syncToken
    ?timeMax
    ?timeMin
    ?timeZone
    ?updatedMin
    calendar_id with_token
  : events_list_result Lwt.t =

  Cloudwatch.time "google.api.calendar.events_list" (fun () ->
    with_token (fun token ->
      call_events_list
        ?alwaysIncludeEmail
        ?iCalUID
        ?maxAttendees
        ?maxResults
        ?orderBy
        ?pageToken
        ?privateExtendedProperties
        ?q
        ?sanitizeHtml
        ?sharedExtendedProperties
        ?showDeleted
        ?showHiddenInvitations
        ?singleEvents
        ?syncToken
        ?timeMax
        ?timeMin
        ?timeZone
        ?updatedMin
        calendar_id token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.events_list_response_of_string body in
        return (`OK result)
    | `Not_found, _, _ ->
        return `Not_found
    | `Gone, _, _ ->
        (*
           We're getting this error:
           - when requesting events with updatedMin
             "too far in the past", presumably also not long ago
             if the user didn't have access to the calendar
             at the requested time.
           - when syncToken has expired.
        *)
        return `Gone
    | x ->
        http_fail "events_list" x
  )

(* exceptions raised when fetching data from within events_stream *)
exception Events_gone
exception Events_not_found

let events_stream
  ?alwaysIncludeEmail
  ?iCalUID
  ?maxAttendees
  ?maxResults (* maximum number of results to return, in total *)
  ?orderBy
  ?privateExtendedProperties
  ?q
  ?sanitizeHtml
  ?sharedExtendedProperties
  ?showDeleted
  ?showHiddenInvitations
  ?singleEvents
  ?syncToken
  ?timeMax
  ?timeMin
  ?timeZone
  ?updatedMin
  calid uid
  =
  let limited =
    maxResults <> None || (timeMin <> None && timeMax <> None)
  in
  if not limited && singleEvents = Some true then
    invalid_arg "Gcal.events_stream: \
                 recurring events will expand to infinity";

  (match maxResults with
   | Some n when n <= 0 ->
       invalid_arg "Gcal.events_stream: maxResults must be positive"
   | _ -> ()
  );

  let last_response = ref None in
  let get_last_response () =
    match !last_response with
    | None -> invalid_arg "no last response before reaching the end \
                           of the event stream"
    | Some resp -> resp in

  let stream = Util_lwt_stream.create_paged_stream (maxResults, Some None)
    (fun (max_remaining, page_token) ->
      let maxResults =
        match max_remaining with
        | None -> 2500 (* maximum page size supported by Google *)
        | Some n -> min n 2500
      in
      assert (maxResults > 0);
      match page_token with
      | Some pageToken ->
          events_list
            ?alwaysIncludeEmail
            ?iCalUID
            ?maxAttendees
            ~maxResults
            ?orderBy
            ?pageToken
            ?privateExtendedProperties
            ?q
            ?sanitizeHtml
            ?sharedExtendedProperties
            ?showDeleted
            ?showHiddenInvitations
            ?singleEvents
            ?syncToken
            ?timeMax
            ?timeMin
            ?timeZone
            ?updatedMin
            calid uid
          >>= (function
            | `Gone ->      fail Events_gone
            | `Not_found -> fail Events_not_found
            | `OK x ->
                let max_remaining, no_remaining =
                  match max_remaining with
                  | None -> None, false
                  | Some m ->
                      let rem = m - List.length x.evs_items in
                      let max_remaining = Some rem in
                      let finished = rem <= 0 in
                      max_remaining, finished
                in
                (* Beware, Google returns a nextPageToken even if maxResults
                   has been reached. *)
                let finished =
                  no_remaining
                  || x.evs_nextPageToken = None
                  || x.evs_items = []
                in
                let page_token =
                  if finished then (
                    last_response := Some x;
                    None
                  )
                  else
                    Some x.evs_nextPageToken
                in
                let continue = not finished in
                return ((max_remaining, page_token), x.evs_items, continue))

      | None -> return ((max_remaining, page_token), [], false)
    )
  in
  stream, get_last_response

type events_list_unpaged_result = [
  | `OK of (string (* timezone *)
            * Gcalid.t
            * Gcal_api_t.event list
            * string option (* next sync token *))
  | `Not_found
  | `Gone
]

(*
   Fetch all the pages returned by events_list.
   The parameters are the same as those supported by events_list,
   except for:
   - maxResults, which refers to the total number of results
     to return, not a page size;
   - pageToken, which is not available here.

   Note that specifying maxResults can prevent the next sync token from being
   obtained.

   Stats on Joe's very busy calendar:
   - 1284 events over the past 10 weeks
   - 434 events in the future
   - 45 events 6 months or more in the future
 *)
let events_list_unpaged
  ?alwaysIncludeEmail
  ?iCalUID
  ?maxAttendees
  ?maxResults (* maximum number of results to return, in total *)
  ?orderBy
  ?privateExtendedProperties
  ?q
  ?sanitizeHtml
  ?sharedExtendedProperties
  ?showDeleted
  ?showHiddenInvitations
  ?singleEvents
  ?syncToken
  ?timeMax
  ?timeMin
  ?timeZone
  ?updatedMin
  calid uid
  : events_list_unpaged_result Lwt.t =

  let stream, get_last_response =
    events_stream
      ?alwaysIncludeEmail
      ?iCalUID
      ?maxAttendees
      ?maxResults
      ?orderBy
      ?privateExtendedProperties
      ?q
      ?sanitizeHtml
      ?sharedExtendedProperties
      ?showDeleted
      ?showHiddenInvitations
      ?singleEvents
      ?syncToken
      ?timeMax
      ?timeMin
      ?timeZone
      ?updatedMin
      calid uid in
  catch
    (fun () ->
      Lwt_stream.to_list stream >>= fun events ->
      let x = get_last_response () in
      return (`OK (x.evs_timeZone, calid, events, x.evs_nextSyncToken))
    )
    (fun e ->
      match Trax.unwrap e with
      | Events_not_found -> return `Not_found
      | Events_gone      -> return `Gone
      | _                -> Trax.raise __LOC__ e
    )


(* So we can get the time zone without doing an event list *)
let call_get_calendar_metadata calendar_id access_token =
  let uri =
    Google_api_util.make_uri
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id))
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri

let get_calendar_metadata calendar_id with_token =
  Cloudwatch.time "google.api.calendar.get_calendar_metadata" (fun () ->
    with_token (fun token ->
      call_get_calendar_metadata calendar_id token
    ) >>= function
    | `OK, _headers, body ->
        let x = Gcal_api_j.calendar_metadata_of_string body in
        return (Some x)
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "get_calendar_metadata" x
  )

let call_get_event
    ?alwaysIncludeEmail
    ?maxAttendees ?sanitizeHtml ?timeZone
    calendar_id event_id access_token =
  let string x = x in
  let query = ("alwaysIncludeEmail", string_of_bool, alwaysIncludeEmail)
          @^@ ("maxAttendees",       string_of_int,  maxAttendees)
          @^@ ("sanitizeHtml",       string_of_bool, sanitizeHtml)
          @^@ ("timeZone",           string,         timeZone)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/events/" ^ Geventid.to_string event_id)
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri

let get_event
    ?alwaysIncludeEmail ?maxAttendees ?sanitizeHtml ?timeZone
    calendar_id event_id
    with_token =
  Cloudwatch.time "google.api.calendar.get_event" (fun () ->
    with_token (fun token ->
      call_get_event
        ?alwaysIncludeEmail ?maxAttendees ?sanitizeHtml ?timeZone
        calendar_id event_id
        token
    ) >>= function
    | `OK, _headers, body ->
        return (Some (Gcal_api_j.event_of_string body))
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "get_event" x
  )

let call_delete_event ?sendNotifications calendar_id event_id token =
  let query = ("sendNotifications", string_of_bool, sendNotifications)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events/" ^ Geventid.to_string event_id)
  in
  Http.delete ~headers:[Google_auth.auth_header token] uri

let delete_event ?sendNotifications calendar_id event_id with_token =
  Cloudwatch.time "google.api.calendar.delete_event" (fun () ->
    with_token (fun token ->
      call_delete_event ?sendNotifications calendar_id event_id token
    )  >>= function
    | (`OK|`No_content|`Gone|`Not_found), _headers, _body ->
        return ()
    | x ->
        http_fail "delete_event" x
  )

let call_insert_empty_event
    ?sanitizeHtml ?sendNotifications
    calendar_id text access_token =
  let query = ("sanitizeHtml",      string_of_bool, sanitizeHtml)
          @^@ ("sendNotifications", string_of_bool, sendNotifications)
          @^@ ["text", [text]] in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/events/quickAdd")
  in
  Http.post ~headers:[Google_auth.auth_header access_token] uri

let insert_empty_event
    ?sanitizeHtml ?sendNotifications
    calendar_id text with_token =
  Cloudwatch.time "google.api.calendar.insert_empty_event" (fun () ->
    with_token (fun token ->
      call_insert_empty_event
        ?sanitizeHtml ?sendNotifications
        calendar_id text token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.event_of_string body in
        return (Some result)
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "quickAdd" x
  )

let call_move_event
    ?sendNotifications
    ~source
    ~destination
    event_id
    access_token =
  let query = ("sendNotifications", string_of_bool, sendNotifications)
          @^@ ["destination", [Gcalid.to_string destination]] in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string source)
       ^ "/events/"
       ^ url_encode (Geventid.to_string event_id)
       ^ "/move")
  in
  Http.post ~headers:[Google_auth.auth_header access_token] uri

let move_event
    ?sendNotifications
    ~source
    ~destination
    event_id
    with_token =
  Cloudwatch.time "google.api.calendar.move_event" (fun () ->
    with_token (fun token ->
      call_move_event
        ?sendNotifications
        ~source
        ~destination
        event_id
        token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.event_of_string body in
        return (Some result)
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "move_event" x
  )

let call_insert_event
    ?maxAttendees ?sanitizeHtml
    ?sendNotifications calendar_id event access_token =

  let query = ("maxAttendees",            string_of_int,  maxAttendees)
          @^@ ("sendNotifications",       string_of_bool, sendNotifications)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/events")
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let body = Gcal_api_j.string_of_event_edit event in
  Http.post ~headers ~body uri

let insert_event
  ?maxAttendees
  ?sanitizeHtml
  ?sendNotifications
  calendar_id event with_token =
  Cloudwatch.time "google.api.calendar.insert_event" (fun () ->
    with_token (fun token ->
      call_insert_event
        ?maxAttendees ?sanitizeHtml ?sendNotifications
        calendar_id event token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.event_of_string body in
        return (Some result)
    | `Not_found, _, body ->
        logf `Warning "Inserting event failed with 404: %s" body;
        return None
    | x ->
        http_fail "insert_event" x
  )

let event_edit_of_event event =
  let s = Gcal_api_j.string_of_event event in
  try Some (Gcal_api_j.event_edit_of_string s)
  with _ ->
    logf `Info "Google calendar event %s was canceled"
      (Geventid.to_string event.ev_id);
    None

let call_update_event
    ?alwaysIncludeEmail
    ?maxAttendees
    ?sanitizeHtml
    ?sendNotifications
    calendar_id event_id
    event access_token =
  let query = ("alwaysIncludeEmail", string_of_bool, alwaysIncludeEmail)
          @^@ ("maxAttendees",       string_of_int,  maxAttendees)
          @^@ ("sanitizeHtml",       string_of_bool, sanitizeHtml)
          @^@ ("sendNotifications",  string_of_bool, sendNotifications)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~query
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/events/" ^ Geventid.to_string event_id)
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let body = Gcal_api_j.string_of_event_edit event in
  Http.put ~headers ~body uri

type update_event_result =
  [ `OK of event | `Not_found | `Invalid_sequence_value ]

let update_event
    ?alwaysIncludeEmail
    ?maxAttendees
    ?sanitizeHtml
    ?sendNotifications
    calendar_id event_id event with_token : update_event_result Lwt.t =
  Cloudwatch.time "google.api.calendar.update_event" (fun () ->
    with_token (fun token ->
      call_update_event
        ?alwaysIncludeEmail
        ?maxAttendees
        ?sanitizeHtml
        ?sendNotifications
        calendar_id event_id event token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.event_of_string body in
        return (`OK result)
    | `Forbidden, _, _ ->
        (* The event may be on another person's calendar,
           even though Google claims it's on ours.
           This happens when it was added through an invite. *)
        return `Not_found
    | `Not_found, _, _ ->
        return `Not_found
    | `Bad_request, _, body
      when error_message_starts_with ~prefix:"Invalid sequence value" body ->
        return `Invalid_sequence_value
    | x ->
        http_fail "update_event" x
  )

(** Creates a new calendar with the given summary. Returns a
 *  description of the new calendar which includes the new id.
 *)
let call_insert_calendar ~summary ?timeZone access_token =
  let uri =
    Google_api_util.make_uri "/calendar/v3/calendars/"
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let create = {
    Gcal_api_t.cc_summary = summary;
    cc_timeZone = timeZone;
  } in
  let body = Gcal_api_j.string_of_calendar_list_item_create create in
  Http.post ~headers ~body uri

let insert_calendar ~summary ?timeZone with_token =
  Cloudwatch.time "google.api.calendar.insert_calendar" (fun () ->
    with_token (fun token ->
      call_insert_calendar ~summary ?timeZone token
    ) >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.calendar_list_item_of_string body in
        return result
    | x ->
        http_fail "insert_calendar" x
  )

(* List access control rules for an owned calendar *)
let call_list_acl_rules calendar_id access_token =
  let uri =
    Google_api_util.make_uri
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/acl")
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  Http.get ~headers uri

let list_acl_rules calendar_id with_token =
  Cloudwatch.time "google.api.calendar.list_acl_rules" (fun () ->
    with_token (fun token ->
      call_list_acl_rules calendar_id token
    ) >>= function
    | `OK, _headers, body ->
        return (Some (Gcal_api_j.acl_list_response_of_string body))
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "list_acl_rules" x
  )

(* Insert a new access control list entry for a calendar *)
let call_insert_acl_rule calendar_id level share_with_email access_token =
  let uri =
    Google_api_util.make_uri
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/acl")
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let create = {
    Gcal_api_t.new_acl_role = level;
    new_acl_scope = {
      Gcal_api_t.sco_type_ = `User;
      sco_value = Some (Email.to_string share_with_email)
    }
  } in
  let body = Gcal_api_j.string_of_new_acl_rule create in
  Http.post ~headers ~body uri

let insert_acl_rule calendar_id level share_with_email with_token =
  Cloudwatch.time "google.api.calendar.insert_acl_rule" (fun () ->
    with_token (fun token ->
      call_insert_acl_rule calendar_id level share_with_email token
    ) >>= function
    | `OK, _headers, body ->
        return (Gcal_api_j.acl_rule_of_string body)
    | x ->
        http_fail "insert_acl_rule" x
  )

(* Update an existing access control list entry for a calendar *)
let call_update_acl_rule calendar_id rule_id level access_token =
  let uri =
    Google_api_util.make_uri
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/acl/" ^ rule_id)
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let edit = { Gcal_api_t.edit_acl_role = level } in
  let body = Gcal_api_j.string_of_edit_acl_rule edit in
  Http.put ~headers ~body uri

let update_acl_rule calendar_id rule_id level with_token =
  Cloudwatch.time "google.api.calendar.update_acl_rule" (fun () ->
    with_token (fun token ->
      call_update_acl_rule calendar_id rule_id level token
    ) >>= function
    | `OK, _headers, body ->
        return (Gcal_api_j.acl_rule_of_string body)
    | x ->
        http_fail "update_acl_rule" x
  )

(* Delete an access control list entry by its ID *)
let call_delete_acl_rule calendar_id rule_id access_token =
  let uri =
    Google_api_util.make_uri
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/acl/" ^ rule_id)
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  Http.delete ~headers uri

let delete_acl_rule calendar_id rule_id with_token =
  Cloudwatch.time "google.api.calendar.delete_acl_rule" (fun () ->
    with_token (fun token ->
      call_delete_acl_rule calendar_id rule_id token
    ) >>= function
    | `No_content, _headers, body ->
        return () (* Success *)
    | `Bad_request, _headers, body ->
        return () (* No such rule, delete ignored *)
    | x ->
        http_fail "delete_acl_rule" x
  )

(*** Listeners ***)

let call_watch_events
    ?channel_token
    ?ttl_seconds
    calendar_id
    ~channel_id
    ~receiving_url
    access_token =
  let url =
    Google_api_util.make_uri
      ("/calendar/v3/calendars/"
       ^ url_encode (Gcalid.to_string calendar_id)
       ^ "/events/watch")
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let request: watch_request = {
    id = channel_id;
    token = channel_token;
    type_ = `Web_hook;
    address = receiving_url;
    expiration = None;
    params = Some { ttl = ttl_seconds };
  } in
  Http.post ~headers
    ~body:(Gcal_api_j.string_of_watch_request request)
    url

let watch_events
    ?channel_token
    ?ttl_seconds
    calendar_id
    ~channel_id
    ~receiving_url
    with_token =
  Cloudwatch.time "google.api.calendar.watch_events" (fun () ->
    with_token (fun access_token ->
      call_watch_events
        ?channel_token
        ?ttl_seconds
        calendar_id
        ~channel_id
        ~receiving_url
        access_token
    ) >>= function
    | `OK, _headers, body ->
        return (Some (Gcal_api_j.watch_response_of_string body))
    | `Not_found, _, _ ->
        return None
    | x ->
        http_fail "watch_events" x
  )

let call_watch_calendar_list
    ?channel_token
    ?ttl_seconds
    ~channel_id
    ~receiving_url
    access_token =
  let url =
    Google_api_util.make_uri "/calendar/v3/users/me/calendarList/watch"
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let request: watch_request = {
    id = channel_id;
    token = channel_token;
    type_ = `Web_hook;
    address = receiving_url;
    expiration = None;
    params = Some { ttl = ttl_seconds };
  } in
  Http.post ~headers
    ~body:(Gcal_api_j.string_of_watch_request request)
    url

let watch_calendar_list
    ?channel_token
    ?ttl_seconds
    ~channel_id
    ~receiving_url
    with_token =
  Cloudwatch.time "google.api.calendar.watch_calendar_list" (fun () ->
    with_token (fun access_token ->
      call_watch_calendar_list
        ?channel_token
        ?ttl_seconds
        ~channel_id
        ~receiving_url
        access_token
    ) >>= function
    | `OK, _headers, body ->
        return (Gcal_api_j.watch_response_of_string body)
    | x ->
        http_fail "watch_calendar_list" x
  )

let call_unwatch ?channel_token ~channel_id ~resource_id access_token =
  let url =
    Google_api_util.make_uri "/calendar/v3/channels/stop"
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let request = {
    id = channel_id;
    resourceId = resource_id;
    token = channel_token;
  } in
  Http.post ~headers
    ~body:(Gcal_api_j.string_of_unwatch_request request)
    url

let unwatch ?channel_token ~channel_id ~resource_id with_token =
  Cloudwatch.time "google.api.calendar.unwatch" (fun () ->
    with_token (fun token ->
      call_unwatch ?channel_token ~channel_id ~resource_id token
    ) >>= function
    | (`No_content | `OK), _headers, body ->
        return ()
    | x ->
        http_fail "unwatch" x
  )

let resource_state_of_string = function
  | "sync" -> `Sync
  | "exists" -> `Exists
  | "not_exists" -> `Not_exists
  | s -> Http_exn.bad_request ("Unknown resource state " ^ s)

let parse_notification
    (get_http_header: string -> string list) : notification =

  let get k =
    match get_http_header k with
    | [] -> Http_exn.bad_request (sprintf "Missing %s header" k)
    | s :: _ -> s
  in
  let opt k =
    match get_http_header k with
    | [] -> None
    | s :: _ -> Some s
  in
  {
    channel_id = get "x-goog-channel-id";
    message_number = int_of_string (get "x-goog-message-number");
    resource_id = get "x-goog-resource-id";
    resource_state = resource_state_of_string (get "x-goog-resource-state");
    resource_uri = opt "x-goog-resource-uri";
    channel_expiration = opt "x-goog-channel-expiration";
    channel_token = opt "x-goog-channel-token";
  }

(* Google provides:
   { key1: { fg1, bg1 }, key2: { fg2, bg2 }, ... }
   We want:
   [ { key1, fg1, bg1 }, { key2, fg2, bg2 }, ... ]
*)
let inject_keys colors =
  let open Yojson.Basic.Util in
  List.map (fun (key, color) ->
    Gcal_api_v.create_color
      ~color_key:key
      ~background:(color |> member "background" |> to_string)
      ~foreground:(color |> member "foreground" |> to_string)
      ()
  ) colors

(* Using Yojson Util instead of Atdgen because the response from Google is
   not quite what we want.
*)
let inject_keys_into_color json =
  let open Yojson.Basic.Util in
  let col_kind = json |> member "kind" |> to_string in
  let updated = json |> member "updated" |> to_string_option in
  let calendar = json |> member "calendar" |> to_assoc in
  let event = json |> member "event" |> to_assoc in
  Gcal_api_v.create_colors_response
    ~col_kind
    ?col_updated:(BatOption.map Util_time.of_string updated)
    ~col_calendar:(inject_keys calendar)
    ~col_event:(inject_keys event)
    ()

let call_get_colors access_token =
  let uri =
    Google_api_util.make_uri "/calendar/v3/colors"
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri

let get_colors with_token =
  Cloudwatch.time "google.api.calendar.colors.get" (fun () ->
    with_token (fun token ->
      call_get_colors token
    ) >>= function
    | `OK, _headers, body ->
        return (inject_keys_into_color (Yojson.Basic.from_string body))
    | x ->
        http_fail "get_colors" x
  )
