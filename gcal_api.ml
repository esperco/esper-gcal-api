open Printf
open Lwt
open Log
open Util_url.Op
open Gcal_api_t

module Http = Util_http_client.Wrap (Util_http_client.Original) (struct
  type orig_result = Util_http_client.response
  type result = Util_http_client.response
  let wrap f =
    Cloudwatch.time "google.api.calendar.any" (fun () ->
      Perf.time "google_calendar" f
    )
end)

(* Plus or minus *)
let (+/-) base interval =
  (if Random.bool () then (+) else (-)) base (Random.int (interval + 1))

let url_encode s = Util_url.encode s

let handle_error body loc =
  let open Gcal_api_t in
  let {error = {code; message}} =
    Gcal_api_j.error_response_of_string body in
  Http_exn.internal_error (Printf.sprintf "%d %s; %s"
                             code message loc)

let handle_error_status request_name (status, headers, body) =
  match status with
  | `Unauthorized -> return `Retry_unauthorized

  | `Forbidden (* Google sometimes uses 403 Rate Limit Exceeded *)
  | `Internal_server_error (* 500 *)
  | `Bad_gateway (* 502 *)
  | `Service_unavailable (* 503 *)
  | `Gateway_timeout (* 504 *) -> return `Retry_5xx

  | _ ->
      handle_error body request_name

let unquote s =
  match Yojson.Basic.from_string s with
  | `String s -> s
  | _ -> assert false

let try_calendar_list
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
      ~path:"/calendar/v3/users/me/calendarList"
      ~query
      () in
  Http.get ~headers:[Google_auth.auth_header access_token] uri
  >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.calendar_list_response_of_string body in
        return (`Result (Some result))
    | `Not_found, _, _ ->
        return (`Result None)
    | x ->
        handle_error_status "calendar_list" x

let calendar_list
    ?minAccessRole ?maxResults ?pageToken ?showHidden
    google_request =
  Cloudwatch.time "google.api.calendar.calendar_list" (fun () ->
    google_request (fun token ->
      try_calendar_list ?minAccessRole ?maxResults ?pageToken ?showHidden token
    )
  )

(*
   Fetch all the pages returned by calendar_list.
   The parameters are the same as those supported by events_list,
   except for:
   - maxResults, which refers to the total number of results
     to return, not a page size;
   - pageToken, which is not available here.
*)
let calendar_list_unpaged'
  ?minAccessRole
  ?maxResults (* maximum number of results to return, in total *)
  ?showHidden
  google_request =

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
      google_request
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

let calendar_list_unpaged
  ?minAccessRole
  ?maxResults (* maximum number of results to return, in total *)
  ?showHidden
  uid
=
  calendar_list_unpaged'
    ?minAccessRole
    ?showHidden
    (User_account.google_request uid)

let calendar_list_unpaged_for_team
  ?minAccessRole
  ?maxResults (* maximum number of results to return, in total *)
  ?showHidden
  teamid email
=
  calendar_list_unpaged'
    ?minAccessRole
    ?showHidden
    (User_team.Team_other_google_token.request (teamid, email))

let try_calendar_list_get calendar_id access_token =
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/users/me/calendarList/"
             ^ url_encode (Gcalid.to_string calendar_id))
      () in
  Http.get ~headers:[Google_auth.auth_header access_token] uri
  >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.calendar_list_item_of_string body in
        return (`Result (Some result))
    | `Not_found, _, _ ->
        return (`Result None)
    | x ->
        handle_error_status "calendar_list_get" x

let calendar_list_get calendar_id uid =
  let http_call () =
    Cloudwatch.time "google.api.calendar.calendar_list_get" (fun () ->
      User_account.google_request uid (fun token ->
        try_calendar_list_get calendar_id token
      )
    )
  in
  let cache_key =
    Cache.make_key
      "calendar_list_item"
      (Uid.to_string uid ^ "," ^ Gcalid.to_string calendar_id)
  in
  Cache.fetch
    ~exptime:(600 +/- 60)
    cache_key
    (fun x -> Some (Gcal_api_j.calendar_list_item_of_string x))
    (BatOption.map Gcal_api_j.string_of_calendar_list_item)
    http_call

let try_update_calendar_list_item ?colorRgbFormat access_token uid calid edit =
  let query = ("colorRgbFormat", string_of_bool, colorRgbFormat) @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/users/me/calendarList/" ^ Gcalid.to_string calid)
      ~query
      () in
  let content_type =
    (* Without this header, Google will return 200 but not actually update! *)
    ("Content-Type", "application/json")
  in
  let headers = [Google_auth.auth_header access_token; content_type] in
  let body = Gcal_api_j.string_of_calendar_list_item_edit edit in
  Http.patch ~headers ~body uri >>= function
  | `OK, _headers, body ->
      let result = Gcal_api_j.calendar_list_item_of_string body in
      let cache_key =
        Cache.make_key
          "calendar_list_item"
          (Uid.to_string uid ^ "," ^ Gcalid.to_string calid)
      in
      Cache.store cache_key ~exptime:(600 +/- 60) body >>= fun () ->
      let list_cache_key =
        Cache.make_key "calendar_list" (Uid.to_string uid)
      in
      Cache.delete list_cache_key >>= fun _ ->
      return (`Result (Some result))
  | `Not_found, _, _ ->
      return (`Result None)
  | x ->
      handle_error_status "update_calendar_list_item" x

let update_calendar_list_item ?colorRgbFormat calid edit uid =
  Cloudwatch.time "google.api.calendar.update_calendar_list_item" (fun () ->
    User_account.google_request uid (fun token ->
      try_update_calendar_list_item ?colorRgbFormat token uid calid edit
    )
  )

let try_freebusy
    timeMin timeMax ?timeZone
    ?groupExpansionMax ?calendarExpansionMax
    calendar_ids access_token =
  let bzq_items = List.map (fun id -> {Gcal_api_t.cid_id = id})
                    calendar_ids in
  let q = {Gcal_api_t.bzq_timeMin = timeMin; bzq_timeMax = timeMax;
           bzq_timeZone = timeZone;
           bzq_groupExpansionMax = groupExpansionMax;
           bzq_calendarExpansionMax = calendarExpansionMax;
           bzq_items} in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let url = Google_api_util.make_uri ~path: "/calendar/v3/freeBusy" () in
  Http.post ~headers
    ~body:(Gcal_api_j.string_of_freebusy_request q)
    url
  >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.freebusy_response_of_string body in
        return (`Result (Some result))
    | `Not_found, _, _ ->
        return (`Result None)
    | x ->
        handle_error_status "freebusy" x

let freebusy
    timeMin timeMax ?timeZone
    ?groupExpansionMax ?calendarExpansionMax
    calendar_ids
    uid =
  Cloudwatch.time "google.api.calendar.freebusy" (fun () ->
    User_account.google_request uid (fun token ->
      try_freebusy timeMin timeMax ?timeZone
        ?groupExpansionMax ?calendarExpansionMax
        calendar_ids
        token
    )
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

let make_event_key calid evid =
  let key =
    String.concat "," [
      Gcalid.to_string calid;
      Geventid.to_string evid;
    ]
  in
  Cache.make_key "event" key

let try_events_list
    ?alwaysIncludeEmail
    ?iCalUID
    ?maxAttendees
    ?maxResults
    ?orderBy
    ?pageToken
    ?q
    ?sanitizeHtml
    ?showDeleted
    ?showHiddenInvitations
    ?singleEvents
    ?syncToken
    ?timeMax
    ?timeMin
    ?timeZone
    ?updatedMin
    uid calendar_id access_token =

  let string_of_order_by x =
    unquote (Gcal_api_j.string_of_event_order_by x) in
  let string x = x in
  let query = ("alwaysIncludeEmail",    string_of_bool, alwaysIncludeEmail)
          @^@ ("iCalUID",               string,         iCalUID)
          @^@ ("maxAttendees",          string_of_int,  maxAttendees)
          @^@ ("maxResults",            string_of_int,  maxResults)
          @^@ ("orderBy",               string_of_order_by, orderBy)
          @^@ ("pageToken",             string,         pageToken)
          @^@ ("q",                     string,         q)
          @^@ ("sanitizeHtml",          string_of_bool, sanitizeHtml)
          @^@ ("showDeleted",           string_of_bool, showDeleted)
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
      ~path:("/calendar/v3/calendars/" ^ url_encode calid ^ "/events")
      ~query
      ()
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri
  >>= function
    | `OK, _headers, body ->
        let result = Gcal_api_j.events_list_response_of_string body in
        Util_conc.iter result.evs_items (fun event ->
          let cache_key = make_event_key calendar_id event.ev_id in
          Cache.store ~exptime:(60 +/- 30) cache_key
            (Gcal_api_j.string_of_event event)
        ) >>= fun () ->
        return (`Result (`OK result))
    | `Not_found, _, _ ->
        return (`Result `Not_found)
    | `Gone, _, _ ->
        (*
           We're getting this error:
           - when requesting events with updatedMin
             "too far in the past", presumably also not long ago
             if the user didn't have access to the calendar
             at the requested time.
           - when syncToken has expired.
        *)
        return (`Result `Gone)
    | x ->
        handle_error_status "events_list" x

let events_list
    ?alwaysIncludeEmail
    ?iCalUID
    ?maxAttendees
    ?maxResults
    ?orderBy
    ?pageToken
    ?q
    ?sanitizeHtml
    ?showDeleted
    ?showHiddenInvitations
    ?singleEvents
    ?syncToken
    ?timeMax
    ?timeMin
    ?timeZone
    ?updatedMin
    calendar_id uid
  : events_list_result Lwt.t =

  Cloudwatch.time "google.api.calendar.events_list" (fun () ->
    User_account.google_request uid (fun token ->
      try_events_list
        ?alwaysIncludeEmail
        ?iCalUID
        ?maxAttendees
        ?maxResults
        ?orderBy
        ?pageToken
        ?q
        ?sanitizeHtml
        ?showDeleted
        ?showHiddenInvitations
        ?singleEvents
        ?syncToken
        ?timeMax
        ?timeMin
        ?timeZone
        ?updatedMin
        uid calendar_id token
    )
  )

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
  ?q
  ?sanitizeHtml
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

  let limited =
    maxResults <> None || (timeMin <> None && timeMax <> None)
  in
  if not limited && singleEvents = Some true then
    invalid_arg "Gcal.events_list_unpaged: \
                 recurring events will expand to infinity";

  let rec loop acc max_remaining page_token =
    let maxResults =
      match max_remaining with
      | None -> 2500 (* maximum page size supported by Google *)
      | Some n -> min n 2500
    in
    events_list
      ?alwaysIncludeEmail
      ?iCalUID
      ?maxAttendees
      ~maxResults
      ?orderBy
      ?pageToken: page_token
      ?q
      ?sanitizeHtml
      ?showDeleted
      ?showHiddenInvitations
      ?singleEvents
      ?syncToken
      ?timeMax
      ?timeMin
      ?timeZone
      ?updatedMin
      calid uid
    >>= function
    | `Not_found ->
        return `Not_found
    | `Gone ->
        return `Gone
    | `OK x ->
        let acc = List.rev_append x.evs_items acc in
        let max_remaining =
          match max_remaining with
          | None -> None
          | Some m -> Some (m - List.length acc)
        in
        match x.evs_nextPageToken, max_remaining with
        | Some page_token, Some max_remaining when max_remaining > 0 ->
            loop acc (Some max_remaining) (Some page_token)
        | Some page_token, None ->
            loop acc None (Some page_token)
        | _ ->
            return (`OK (x.evs_timeZone, calid,
                         List.rev acc, x.evs_nextSyncToken))
  in
  loop [] maxResults None


(* So we can get the time zone without doing an event list *)
let try_get_calendar_metadata ~calendar_id access_token =
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id))
      ()
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri
  >>= function
  | `OK, _headers, body ->
      let x = Gcal_api_j.calendar_metadata_of_string body in
      return (`Result (Some x))
  | `Not_found, _, _ ->
      return (`Result None)
  | x ->
      handle_error_status "get_calendar_metadata" x

let get_calendar_metadata ~calendar_id uid =
  let http_call () =
    Cloudwatch.time "google.api.calendar.get_calendar_metadata" (fun () ->
      User_account.google_request uid (fun token ->
        try_get_calendar_metadata ~calendar_id token
      )
    )
  in
  let cache_key =
    Cache.make_key
      "calendar_metadata"
      (Uid.to_string uid ^ "," ^ Gcalid.to_string calendar_id)
  in
  Cache.fetch
    ~exptime:(600 +/- 60)
    cache_key
    (fun x -> Some (Gcal_api_j.calendar_metadata_of_string x))
    (BatOption.map Gcal_api_j.string_of_calendar_metadata)
    http_call

let try_get_event
    ~calendar_id ~event_id ?alwaysIncludeEmail
    ?maxAttendees ?sanitizeHtml ?timeZone
    access_token =
  let string x = x in
  let query = ("alwaysIncludeEmail", string_of_bool, alwaysIncludeEmail)
          @^@ ("maxAttendees",       string_of_int,  maxAttendees)
          @^@ ("sanitizeHtml",       string_of_bool, sanitizeHtml)
          @^@ ("timeZone",           string,         timeZone)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events/" ^ Geventid.to_string event_id)
      ~query
      ()
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri
  >>= function
  | `OK, _headers, body ->
      return (`Result (Some (Gcal_api_j.event_of_string body)))
  | `Not_found, _, _ ->
      return (`Result None)
  | x ->
      handle_error_status "get_event" x

let get_event
    ~calendar_id ~event_id
    ?alwaysIncludeEmail ?maxAttendees ?sanitizeHtml ?timeZone
    uid =
  Cloudwatch.time "google.api.calendar.get_event" (fun () ->
    User_account.google_request uid (fun token ->
      try_get_event
        ~calendar_id ~event_id
        ?alwaysIncludeEmail ?maxAttendees ?sanitizeHtml ?timeZone
        token
    )
  )

let try_delete_event ~calendar_id ~event_id ?sendNotifications uid token =
  let query = ("sendNotifications", string_of_bool, sendNotifications)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events/" ^ Geventid.to_string event_id)
      ~query
      ()
  in
  Http.delete ~headers:[Google_auth.auth_header token] uri
  >>= function
  | (`OK|`No_content|`Gone|`Not_found), _headers, _body ->
      let cache_key = make_event_key calendar_id event_id in
      Cache.delete cache_key >>= fun _ ->
      return (`Result ())
  | x ->
      handle_error_status "delete_event" x

let delete_event ~calendar_id ~event_id ?sendNotifications uid =
  Cloudwatch.time "google.api.calendar.delete_event" (fun () ->
    User_account.google_request uid (fun token ->
      try_delete_event ~calendar_id ~event_id ?sendNotifications uid token
    )
  )

let try_insert_empty_event
    ?sanitizeHtml ?sendNotifications
    ~calendar_id uid text access_token =
  let query = ("sanitizeHtml",      string_of_bool, sanitizeHtml)
          @^@ ("sendNotifications", string_of_bool, sendNotifications)
          @^@ ["text", [text]] in
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events/quickAdd")
      ~query
      ()
  in
  Http.post ~headers:[Google_auth.auth_header access_token] uri
  >>= function
  | `OK, _headers, body ->
      let result = Gcal_api_j.event_of_string body in
      let cache_key = make_event_key calendar_id result.ev_id in
      Cache.store cache_key ~exptime:(30 +/- 10) body >>= fun () ->
      return (`Result (Some result))
  | `Not_found, _, _ ->
      return (`Result None)
  | x ->
      handle_error_status "quickAdd" x

let insert_empty_event
    ?sanitizeHtml ?sendNotifications
    ~calendar_id text uid =
  Cloudwatch.time "google.api.calendar.insert_empty_event" (fun () ->
    User_account.google_request uid (fun token ->
      try_insert_empty_event
        ?sanitizeHtml ?sendNotifications
        ~calendar_id uid text token
    )
  )

let try_move_event
    ~calendar_id ~destination ~event_id
    ?sendNotifications uid access_token =
  let query = ("sendNotifications", string_of_bool, sendNotifications)
          @^@ ["destination", [destination]] in
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events/" ^ event_id ^ "/move")
      ~query
      ()
  in
  Http.post ~headers:[Google_auth.auth_header access_token] uri
  >>= function
  | `OK, _headers, body ->
      let result = Gcal_api_j.event_of_string body in
      let cache_key = make_event_key calendar_id result.ev_id in
      Cache.store cache_key ~exptime:(30 +/- 10) body >>= fun () ->
      return (`Result (Some result))
  | `Not_found, _, _ ->
      return (`Result None)
  | x ->
      handle_error_status "move_event" x

let move_event
    ~calendar_id ~destination ~event_id
    ?sendNotifications uid =
  Cloudwatch.time "google.api.calendar.move_event" (fun () ->
    User_account.google_request uid (fun token ->
      try_move_event
        ~calendar_id ~destination ~event_id
        ?sendNotifications uid token
    )
  )

let try_insert_event
    ~calendar_id ?maxAttendees ?sanitizeHtml
    ?sendNotifications uid event access_token =

  let query = ("maxAttendees",            string_of_int,  maxAttendees)
          @^@ ("sendNotifications",       string_of_bool, sendNotifications)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events")
      ~query
      ()
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let body = Gcal_api_j.string_of_event_edit event in
  Http.post ~headers ~body uri >>= function
  | `OK, _headers, body ->
      let result = Gcal_api_j.event_of_string body in
      let cache_key = make_event_key calendar_id result.ev_id in
      Cache.store cache_key ~exptime:(30 +/- 10) body >>= fun () ->
      return (`Result (Some result))
  | `Not_found, _, body ->
      logf `Warning "Inserting event failed with 404: %s" body;
      return (`Result None)
  | x ->
      handle_error_status "insert_event" x

let insert_event
  ~calendar_id ?maxAttendees ?sanitizeHtml
  ?sendNotifications event uid =
  Cloudwatch.time "google.api.calendar.insert_event" (fun () ->
    User_account.google_request uid (fun token ->
      try_insert_event
        ~calendar_id ?maxAttendees ?sanitizeHtml
        ?sendNotifications uid event token
    )
  )

let event_edit_of_event event =
  let s = Gcal_api_j.string_of_event event in
  try Some (Gcal_api_j.event_edit_of_string s)
  with _ ->
    logf `Info "Google calendar event %s was canceled"
      (Geventid.to_string event.ev_id);
    None

let try_update_event
    ~calendar_id ~event_id ?alwaysIncludeEmail
    ?maxAttendees ?sanitizeHtml ?sendNotifications uid event access_token =
  let query = ("alwaysIncludeEmail", string_of_bool, alwaysIncludeEmail)
          @^@ ("maxAttendees",       string_of_int,  maxAttendees)
          @^@ ("sendNotifications",  string_of_bool, sendNotifications)
          @^@ [] in
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events/" ^ Geventid.to_string event_id)
      ~query
      ()
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let body = Gcal_api_j.string_of_event_edit event in
  Http.put ~headers ~body uri >>= function
  | `OK, _headers, body ->
      let result = Gcal_api_j.event_of_string body in
      let cache_key = make_event_key calendar_id result.ev_id in
      Cache.store cache_key ~exptime:(30 +/- 10) body >>= fun () ->
      return (`Result (Some result))
  | `Forbidden, _, _ ->
      (* The event may be on another person's calendar,
         even though Google claims it's on ours.
         This happens when it was added through an invite. *)
      return (`Result None)
  | `Not_found, _, _ ->
      return (`Result None)
  | x ->
      handle_error_status "update_event" x

let update_event
    ~calendar_id ~event_id ?alwaysIncludeEmail
    ?maxAttendees ?sanitizeHtml ?sendNotifications event uid =
  Cloudwatch.time "google.api.calendar.update_event" (fun () ->
    User_account.google_request uid (fun token ->
      try_update_event
        ~calendar_id ~event_id ?alwaysIncludeEmail
        ?maxAttendees ?sanitizeHtml ?sendNotifications uid event token
    )
  )

(** Creates a new calendar with the given summary. Returns a
 *  description of the new calendar which includes the new id.
 *)
let try_insert_calendar ~summary ?timeZone uid access_token =
  let uri =
    Google_api_util.make_uri ~path:("/calendar/v3/calendars/") ()
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let create = {
    Gcal_api_t.cc_summary = summary;
    cc_timeZone = timeZone;
  } in
  let body = Gcal_api_j.string_of_calendar_list_item_create create in
  Http.post ~headers ~body uri >>= function
  | `OK, _headers, body ->
      let result = Gcal_api_j.calendar_list_item_of_string body in
      let cache_key =
        Cache.make_key
          "calendar_list_item"
          (Uid.to_string uid ^ "," ^ Gcalid.to_string result.ci_id)
      in
      Cache.store cache_key ~exptime:(600 +/- 60) body >>= fun () ->
      let list_cache_key =
        Cache.make_key "calendar_list" (Uid.to_string uid)
      in
      Cache.delete list_cache_key >>= fun _ ->
      return (`Result result)
  | x ->
      handle_error_status "insert_calendar" x

let insert_calendar ~summary ?timeZone uid =
  Cloudwatch.time "google.api.calendar.insert_calendar" (fun () ->
    User_account.google_request uid
      (fun token -> try_insert_calendar ~summary ?timeZone uid token)
  )

(* List access control rules for an owned calendar *)
let try_list_acl_rules ~calendar_id access_token =
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/acl")
      ()
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  Http.get ~headers uri >>= function
  | `OK, _headers, body ->
      return (`Result (Some (Gcal_api_j.acl_list_response_of_string body)))
  | `Not_found, _, _ ->
      return (`Result None)
  | x ->
      handle_error_status "list_acl_rules" x

let list_acl_rules ~calendar_id (request_id, google_request) =
  let http_call () =
    Cloudwatch.time "google.api.calendar.list_acl_rules" (fun () ->
      google_request (fun token ->
        try_list_acl_rules ~calendar_id token
      )
    )
  in
  let cache_key =
    Cache.make_key
      "acl"
      (request_id ^ "," ^ Gcalid.to_string calendar_id)
  in
  Cache.fetch
    ~exptime:(600 +/- 60)
    cache_key
    (fun x -> Some (Gcal_api_j.acl_list_response_of_string x))
    (BatOption.map Gcal_api_j.string_of_acl_list_response)
    http_call

let list_acl_rules_for_user ~calendar_id uid =
  list_acl_rules ~calendar_id
    (Uid.to_string uid, User_account.google_request uid)

(* Insert a new access control list entry for a calendar *)
let try_insert_acl_rule ~calendar_id ~request_id level share_with_email
                        access_token =
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/acl")
      ()
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
  Http.post ~headers ~body uri >>= function
  | `OK, _headers, body ->
      let cache_key =
        Cache.make_key
          "acl"
          (request_id ^ "," ^ Gcalid.to_string calendar_id)
      in
      Cache.delete cache_key >>= fun _ ->
      return (`Result (Gcal_api_j.acl_rule_of_string body))
  | x ->
      handle_error_status "insert_acl_rule" x

let insert_acl_rule ~calendar_id (request_id, google_request)
                    level share_with_email =
  Cloudwatch.time "google.api.calendar.insert_acl_rule" (fun () ->
    google_request (fun token ->
      try_insert_acl_rule ~calendar_id ~request_id level share_with_email token
    )
  )

(* Update an existing access control list entry for a calendar *)
let try_update_acl_rule ~calendar_id ~request_id rule_id level access_token =
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/acl/" ^ rule_id) ()
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let edit = { Gcal_api_t.edit_acl_role = level } in
  let body = Gcal_api_j.string_of_edit_acl_rule edit in
  Http.put ~headers ~body uri >>= function
  | `OK, _headers, body ->
      let cache_key =
        Cache.make_key
          "acl"
          (request_id ^ "," ^ Gcalid.to_string calendar_id)
      in
      Cache.delete cache_key >>= fun _ ->
      return (`Result (Gcal_api_j.acl_rule_of_string body))
  | x ->
      handle_error_status "update_acl_rule" x

let update_acl_rule ~calendar_id (request_id, google_request) rule_id level =
  Cloudwatch.time "google.api.calendar.update_acl_rule" (fun () ->
    google_request (fun token ->
      try_update_acl_rule ~calendar_id ~request_id rule_id level token
    )
  )

(* Delete an access control list entry by its ID *)
let try_delete_acl_rule ~calendar_id ~request_id rule_id access_token =
  let uri =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/acl/" ^ rule_id) ()
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  Http.delete ~headers uri >>= function
  | `No_content, _headers, body ->
      let cache_key =
        Cache.make_key
          "acl"
          (request_id ^ "," ^ Gcalid.to_string calendar_id)
      in
      Cache.delete cache_key >>= fun _ ->
      return (`Result ()) (* Success *)
  | `Bad_request, _headers, body ->
      return (`Result ()) (* No such rule, delete ignored *)
  | x ->
      handle_error_status "delete_acl_rule" x

let delete_acl_rule ~calendar_id (request_id, google_request) rule_id =
  Cloudwatch.time "google.api.calendar.delete_acl_rule" (fun () ->
    google_request (fun token ->
      try_delete_acl_rule ~calendar_id ~request_id rule_id token
    )
  )

let google_request_for_authorized = function
  | `User uid ->
      Uid.to_string uid,
      User_account.google_request uid
  | `Other {Api_t.for_teamid; for_account} ->
      Email.to_string for_account,
      User_team.Team_other_google_token.request (for_teamid, for_account)

let share_calendar ~calendar_id authorized emails =
  let request = google_request_for_authorized authorized in
  list_acl_rules ~calendar_id request >>= function
  | None -> Http_exn.not_found "Cannot access calendar"
  | Some response ->
      Util_conc.iter emails (fun email ->
        let request = google_request_for_authorized authorized in
        let for_email = function
          | { acl_scope = { sco_value = Some e } }
            when e = Email.to_string email -> true
          | _ -> false
        in
        match BatList.Exceptionless.find for_email response.acls_items with
        | None ->
            insert_acl_rule ~calendar_id request `Writer email
            >>= fun _inserted -> return ()
        | Some acl ->
            match acl.acl_role with
            | `Writer | `Owner -> return ()
            | _ ->
                update_acl_rule ~calendar_id request acl.acl_id `Writer
                >>= fun _updated -> return ()
      )

(* search list of calendars to find an event *)
let find_event uid eventid calendars =
  Util_lwt.find_map_left calendars (fun (cal, opt_metadata) ->
    get_event uid ~calendar_id:cal.Api_t.cal_google_cal_id ~event_id:eventid
    >>= function
    | None -> return None
    | Some e ->
        match e.Gcal_api_t.ev_status with
        | `Cancelled -> return None
        | _ -> return (Some (e, cal.Api_t.cal_google_cal_id))
  )

(* get start time for an event returns None if event was cancelled *)
let get_event_start_time uid calendars eventid =
  find_event uid eventid calendars >>= fun event ->
  match event with
  | None -> return None
  | Some (e, _) ->
      match e.Gcal_api_j.ev_start with
      | None -> return None
      | Some event_time ->
          match event_time.Gcal_api_j.dateTime with
          | None -> return None
          | Some t -> return (Some (eventid, t))

(*** Listeners ***)

let try_watch_events
    ~calendar_id
    ~channel_id
    ~receiving_url
    ?channel_token
    ?ttl_seconds
    access_token =
  let url =
    Google_api_util.make_uri
      ~path:("/calendar/v3/calendars/"
             ^ url_encode (Gcalid.to_string calendar_id)
             ^ "/events/watch")
      ()
  in
  let headers = [Google_auth.auth_header access_token;
                 "Content-Type", "application/json"] in
  let request: watch_events_request = {
    id = channel_id;
    token = channel_token;
    type_ = `Web_hook;
    address = receiving_url;
    expiration = None;
    params = Some { ttl = ttl_seconds };
  } in
  Http.post ~headers
    ~body:(Gcal_api_j.string_of_watch_events_request request)
    url >>= function
  | `OK, _headers, body ->
      return (`Result (Gcal_api_j.watch_events_response_of_string body))
  | x ->
      handle_error_status "watch_events" x

let watch_events
    ~calendar_id
    ~channel_id
    ~receiving_url
    ?channel_token
    ?ttl_seconds
    uid =
  Cloudwatch.time "google.api.calendar.watch_events" (fun () ->
    User_account.google_request uid (fun access_token ->
      try_watch_events
        ~calendar_id
        ~channel_id
        ~receiving_url
        ?channel_token
        ?ttl_seconds
        access_token
    )
  )

let try_unwatch ~channel_id ~resource_id ?channel_token access_token =
  let url =
    Google_api_util.make_uri
      ~path: "/calendar/v3/channels/stop"
      ()
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
    url >>= function
  | (`No_content | `OK), _headers, body ->
      return (`Result ())
  | x ->
      handle_error_status "unwatch" x

let unwatch ~channel_id ~resource_id ?channel_token uid =
  Cloudwatch.time "google.api.calendar.unwatch" (fun () ->
    User_account.google_request uid (fun token ->
      try_unwatch ~channel_id ~resource_id ?channel_token token
    )
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

let try_get_colors access_token =
  let uri =
    Google_api_util.make_uri ~path:("/calendar/v3/colors") ()
  in
  Http.get ~headers:[Google_auth.auth_header access_token] uri
  >>= function
  | `OK, _headers, body ->
      return (`Result (inject_keys_into_color (Yojson.Basic.from_string body)))
  | x ->
      handle_error_status "get_colors" x

let get_colors uid =
  let http_call () =
    Cloudwatch.time "google.api.calendar.colors.get" (fun () ->
      User_account.google_request uid try_get_colors
    )
  in
  let cache_key = Cache.make_key "colors" (Uid.to_string uid) in
  Cache.fetch
    ~exptime:(600 +/- 60)
    cache_key
    Gcal_api_j.colors_response_of_string
    (fun x -> Some (Gcal_api_j.string_of_colors_response x))
    http_call

(* tests from toplevel

let (>>=) = Lwt.(>>=)

let test email password f =
  Account.login (Email.of_string email) password >>= fun (uid, _secret) ->
  Account.get_google_oauth_token uid >>= function
    | None -> failwith "unauthorized"
    | Some token -> f token

let test_calendar_list email password = test email password (fun token ->
  Gcal.calendar_list token ())

let t1 = Util_time.of_string "2013-10-01T00:00:00"
let t2 = Util_time.of_string "2013-12-01T00:00:00"
let test_freebusy email password = test email password (fun token ->
  let open Gcal_api_t in
  Gcal.calendar_list token () >>= fun {cl_items} ->
  let ids = List.map (fun {ci_id} -> ci_id) cl_items in
  Gcal.freebusy token t1 t2 ids)

let test_events_list email password = test email password (fun token ->
  Gcal.events_list token email)

let test_quick_add email password text = test email password (fun token ->
  Gcal.insert_empty_event token ~sendNotifications:true
    ~calendar_id:email text)

let test_delete_event email password event_id = test email password
  (fun token -> Gcal.delete_event token ~sendNotifications:true
                  ~calendar_id:email ~event_id ())

let test_get_event email password event_id = test email password (fun token ->
  Gcal.get_event token ~calendar_id:email ~event_id ())

let test_move_event email password event_id destination = test email password
  (fun token -> Gcal.move_event token ~sendNotifications:true
                  ~calendar_id:email ~event_id ~destination ())

let event_time time = {
  Gcal_api_t.dateTime = Some time;
  date = None;
  timeZone = None;
}

let test_event = {
  Gcal_api_t.ew_status = `Tentative;
  ew_summary = "quite a test";
  ew_description = None;
  ew_location = None;
  ew_colorId = None;
  ew_start = event_time (Util_time.of_string "2013-11-12T12:34:56");
  ew_end'  = event_time (Util_time.of_string "2013-11-12T13:45:56");
  ew_recurrence = [];
  ew_originalStartTime = None;
  ew_transparency = `Opaque;
  ew_visibility = `Default;
  ew_sequence = 0;
  ew_attendees = [];
  ew_attendeesOmitted = false;
  ew_extendedProperties = None;
  ew_gadget = None;
  ew_anyoneCanAddSelf = true;
  ew_guestsCanInviteOthers = true;
  ew_guestsCanSeeOtherGuests = true;
  ew_reminders = {Gcal_api_t.reminder_useDefault = true;
                                    reminder_overrides = []};
  ew_source = None;
}

let test_event2 =
  {test_event with Gcal_api_t.ew_summary = "lalala"}

let test_insert_event email password = test email password (fun token ->
  Gcal.insert_event token ~calendar_id:email test_event)

let test_update_event email password event_id = test email password
  (fun token -> Gcal.update_event token ~sendNotifications:true
                  ~calendar_id:email ~event_id test_event2)
*)
