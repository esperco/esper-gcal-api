(*
   Google Calendar API.

   Implementation notes: this library should eventually become standalone.
*)


type http_response = Util_http_client.response

type with_token = ((string -> http_response Lwt.t) -> http_response Lwt.t)
  (* Type of the user-provided function that takes care of obtaining
     and storing an access token and then trying the given HTTP calls
     until successful.

     A good function that performs retries with exponential backoff
     as advised by Google is Google_http.request.
  *)

val compare_access_role :
  Gcal_api_t.access_role -> Gcal_api_t.access_role -> int

val calendar_list :
  ?minAccessRole:Gcal_api_j.access_role ->
  ?maxResults:int ->
  ?pageToken:string ->
  ?showHidden:bool ->
  with_token ->
  Gcal_api_j.calendar_list_response option Lwt.t

val calendar_list_unpaged :
  ?minAccessRole:Gcal_api_j.access_role ->
  ?maxResults:int ->
  ?showHidden:bool ->
  with_token ->
  Gcal_api_t.calendar_list_item list option Lwt.t

val calendar_list_get :
  Gcalid.t ->
  with_token ->
  Gcal_api_j.calendar_list_item option Lwt.t

val update_calendar_list_item :
  ?colorRgbFormat:bool ->
  Gcalid.t ->
  Gcal_api_j.calendar_list_item_edit ->
  with_token ->
  Gcal_api_j.calendar_list_item option Lwt.t

val freebusy :
  Gcal_api_t.timestamp ->
  Gcal_api_t.timestamp ->
  ?timeZone:string ->
  ?groupExpansionMax:int ->
  ?calendarExpansionMax:int ->
  Gcal_api_t.gcalid list ->
  with_token ->
  Gcal_api_j.freebusy_response option Lwt.t

type events_list_result =
    [ `Gone | `Not_found | `OK of Gcal_api_t.events_list_response ]

val events_list :
  ?alwaysIncludeEmail:bool ->
  ?iCalUID:string ->
  ?maxAttendees:int ->
  ?maxResults:int ->
  ?orderBy:Gcal_api_j.event_order_by ->
  ?pageToken:string ->
  ?q:string ->
  ?sanitizeHtml:bool ->
  ?showDeleted:bool ->
  ?showHiddenInvitations:bool ->
  ?singleEvents:bool ->
  ?syncToken:string ->
  ?timeMax:Util_time.t ->
  ?timeMin:Util_time.t ->
  ?timeZone:string ->
  ?updatedMin:Util_time.t ->
  Gcalid.t ->
  with_token ->
  events_list_result Lwt.t

type events_list_unpaged_result =
    [ `Gone
    | `Not_found
    | `OK of string * Gcalid.t * Gcal_api_t.event list * string option ]

val events_list_unpaged :
  ?alwaysIncludeEmail:bool ->
  ?iCalUID:string ->
  ?maxAttendees:int ->
  ?maxResults:int ->
  ?orderBy:Gcal_api_j.event_order_by ->
  ?q:string ->
  ?sanitizeHtml:bool ->
  ?showDeleted:bool ->
  ?showHiddenInvitations:bool ->
  ?singleEvents:bool ->
  ?syncToken:string ->
  ?timeMax:Util_time.t ->
  ?timeMin:Util_time.t ->
  ?timeZone:string ->
  ?updatedMin:Util_time.t ->
  Gcalid.t ->
  with_token ->
  events_list_unpaged_result Lwt.t

val get_calendar_metadata :
  calendar_id:Gcalid.t ->
  with_token ->
  Gcal_api_j.calendar_metadata option Lwt.t

val get_event :
  calendar_id:Gcalid.t ->
  event_id:Geventid.t ->
  ?alwaysIncludeEmail:bool ->
  ?maxAttendees:int ->
  ?sanitizeHtml:bool ->
  ?timeZone:string ->
  with_token ->
  Gcal_api_j.event option Lwt.t

val delete_event :
  calendar_id:Gcalid.t ->
  event_id:Geventid.t ->
  ?sendNotifications:bool ->
  with_token ->
  unit Lwt.t

val insert_empty_event :
  ?sanitizeHtml:bool ->
  ?sendNotifications:bool ->
  calendar_id:Gcalid.t ->
  string ->
  with_token ->
  Gcal_api_j.event option Lwt.t

val move_event :
  calendar_id:Gcalid.t ->
  destination:string ->
  event_id:string ->
  ?sendNotifications:bool ->
  with_token ->
  Gcal_api_j.event option Lwt.t

val insert_event :
  calendar_id:Gcalid.t ->
  ?maxAttendees:int ->
  ?sanitizeHtml:'a ->
  ?sendNotifications:bool ->
  Gcal_api_j.event_edit ->
  ((string -> http_response Lwt.t) ->
   (Cohttp.Code.status_code * 'b * string) Lwt.t) ->
  Gcal_api_j.event option Lwt.t

val event_edit_of_event : Gcal_api_j.event -> Gcal_api_j.event_edit option

val update_event :
  calendar_id:Gcalid.t ->
  event_id:Geventid.t ->
  ?alwaysIncludeEmail:bool ->
  ?maxAttendees:int ->
  ?sanitizeHtml:bool ->
  ?sendNotifications:bool ->
  Gcal_api_j.event_edit ->
  ((string -> http_response Lwt.t) ->
   (Cohttp.Code.status_code * 'b * string) Lwt.t) ->
  Gcal_api_j.event option Lwt.t

val insert_calendar :
  summary:string ->
  ?timeZone:string ->
  with_token ->
  Gcal_api_j.calendar_list_item Lwt.t

val list_acl_rules :
  calendar_id:Gcalid.t ->
  with_token ->
  Gcal_api_j.acl_list_response option Lwt.t

val insert_acl_rule :
  calendar_id:Gcalid.t ->
  Gcal_api_t.access_role ->
  Email.t ->
  with_token ->
  Gcal_api_j.acl_rule Lwt.t

val update_acl_rule :
  calendar_id:Gcalid.t ->
  string ->
  Gcal_api_t.access_role ->
  with_token ->
  Gcal_api_j.acl_rule Lwt.t

val delete_acl_rule :
  calendar_id:Gcalid.t ->
  string ->
  ((string -> request_id:'a -> http_response Lwt.t) ->
   (Cohttp.Code.status_code * 'b * string) Lwt.t) ->
  unit Lwt.t

val share_calendar :
  calendar_id:Gcalid.t ->
  Email.t list ->
  with_token ->
  unit Lwt.t

val watch_events :
  calendar_id:Gcalid.t ->
  channel_id:string ->
  receiving_url:string ->
  ?channel_token:string ->
  ?ttl_seconds:float ->
  with_token ->
  Gcal_api_j.watch_response Lwt.t

val watch_calendar_list :
  channel_id:string ->
  receiving_url:string ->
  ?channel_token:string ->
  ?ttl_seconds:float ->
  with_token ->
  Gcal_api_j.watch_response Lwt.t

val unwatch :
  channel_id:string ->
  resource_id:string ->
  ?channel_token:string ->
  with_token ->
  unit Lwt.t

val parse_notification : (string -> string list) -> Gcal_api_t.notification

val get_colors :
  with_token ->
  Gcal_api_v.colors_response Lwt.t
