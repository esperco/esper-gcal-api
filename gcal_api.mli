(*
   Google Calendar API.

   Implementation notes: this library should eventually become standalone.
*)


type http_response = Util_http_client.response

type with_token =
  ((string -> http_response Lwt.t) -> http_response Lwt.t)
  (*
     Type of the user-provided function that takes care of obtaining
     and storing an access token and then trying the given HTTP calls
     until successful.

     A good function that performs retries with exponential backoff
     as advised by Google is Google_http.request.
  *)

val compare_access_role :
  Gcal_api_t.access_role -> Gcal_api_t.access_role -> int

val calendar_list :
  ?minAccessRole:Gcal_api_t.access_role ->
  ?maxResults:int ->
  ?pageToken:string ->
  ?showHidden:bool ->
  with_token ->
  Gcal_api_t.calendar_list_response option Lwt.t

val calendar_list_unpaged :
  ?minAccessRole:Gcal_api_t.access_role ->
  ?maxResults:int ->
  ?showHidden:bool ->
  with_token ->
  Gcal_api_t.calendar_list_item list option Lwt.t

val calendar_list_get :
  Gcalid.t ->
  with_token ->
  Gcal_api_t.calendar_list_item option Lwt.t

val update_calendar_list_item :
  ?colorRgbFormat:bool ->
  Gcalid.t ->
  Gcal_api_t.calendar_list_item_edit ->
  with_token ->
  Gcal_api_t.calendar_list_item option Lwt.t

val freebusy :
  ?timeZone:string ->
  ?groupExpansionMax:int ->
  ?calendarExpansionMax:int ->
  timeMin:Gcal_api_t.timestamp ->
  timeMax:Gcal_api_t.timestamp ->
  Gcal_api_t.gcalid list ->
  with_token ->
  Gcal_api_t.freebusy_response option Lwt.t

type events_list_result =
    [ `Gone | `Not_found | `OK of Gcal_api_t.events_list_response ]

val events_list :
  ?alwaysIncludeEmail:bool ->
  ?iCalUID:string ->
  ?maxAttendees:int ->
  ?maxResults:int ->
  ?orderBy:Gcal_api_t.event_order_by ->
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
  ?orderBy:Gcal_api_t.event_order_by ->
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

(* exceptions raised when fetching data from within events_stream *)
exception Events_gone
exception Events_not_found

val events_stream :
  ?alwaysIncludeEmail:bool ->
  ?iCalUID:string ->
  ?maxAttendees:int ->
  ?maxResults:int ->
  ?orderBy:Gcal_api_t.event_order_by ->
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
  Gcal_api_t.event Lwt_stream.t * (unit -> Gcal_api_t.events_list_response)

val get_calendar_metadata :
  Gcalid.t ->
  with_token ->
  Gcal_api_t.calendar_metadata option Lwt.t

val get_event :
  ?alwaysIncludeEmail:bool ->
  ?maxAttendees:int ->
  ?sanitizeHtml:bool ->
  ?timeZone:string ->
  Gcalid.t ->
  Geventid.t ->
  with_token ->
  Gcal_api_t.event option Lwt.t

val delete_event :
  ?sendNotifications:bool ->
  Gcalid.t ->
  Geventid.t ->
  with_token ->
  unit Lwt.t

val insert_empty_event :
  ?sanitizeHtml:bool ->
  ?sendNotifications:bool ->
  Gcalid.t ->
  string ->
  with_token ->
  Gcal_api_t.event option Lwt.t

val move_event :
  ?sendNotifications:bool ->
  source:Gcalid.t ->
  destination:Gcalid.t ->
  Geventid.t ->
  with_token ->
  Gcal_api_t.event option Lwt.t

val insert_event :
  ?maxAttendees:int ->
  ?sanitizeHtml:'a ->
  ?sendNotifications:bool ->
  Gcalid.t ->
  Gcal_api_t.event_edit ->
  ((string -> http_response Lwt.t) ->
   (Cohttp.Code.status_code * 'b * string) Lwt.t) ->
  Gcal_api_t.event option Lwt.t

val event_edit_of_event : Gcal_api_t.event -> Gcal_api_t.event_edit option

type update_event_result =
  [ `OK of Gcal_api_t.event | `Not_found | `Invalid_sequence_value ]

val update_event :
  ?alwaysIncludeEmail:bool ->
  ?maxAttendees:int ->
  ?sanitizeHtml:bool ->
  ?sendNotifications:bool ->
  Gcalid.t ->
  Geventid.t ->
  Gcal_api_t.event_edit ->
  with_token ->
  update_event_result Lwt.t

val insert_calendar :
  summary:string ->
  ?timeZone:string ->
  with_token ->
  Gcal_api_t.calendar_list_item Lwt.t

val list_acl_rules :
  Gcalid.t ->
  with_token ->
  Gcal_api_t.acl_list_response option Lwt.t

val insert_acl_rule :
  Gcalid.t ->
  Gcal_api_t.access_role ->
  Email.t ->
  with_token ->
  Gcal_api_t.acl_rule Lwt.t

val update_acl_rule :
  Gcalid.t ->
  string ->
  Gcal_api_t.access_role ->
  with_token ->
  Gcal_api_t.acl_rule Lwt.t

val delete_acl_rule :
  Gcalid.t ->
  string ->
  ((string -> http_response Lwt.t) ->
   (Cohttp.Code.status_code * 'b * string) Lwt.t) ->
  unit Lwt.t

val share_calendar :
  Gcalid.t ->
  Email.t list ->
  with_token ->
  unit Lwt.t

val watch_events :
  ?channel_token:string ->
  ?ttl_seconds:float ->
  Gcalid.t ->
  channel_id:string ->
  receiving_url:string ->
  with_token ->
  Gcal_api_t.watch_response option Lwt.t

val watch_calendar_list :
  ?channel_token:string ->
  ?ttl_seconds:float ->
  channel_id:string ->
  receiving_url:string ->
  with_token ->
  Gcal_api_t.watch_response Lwt.t

val unwatch :
  ?channel_token:string ->
  channel_id:string ->
  resource_id:string ->
  with_token ->
  unit Lwt.t

val parse_notification : (string -> string list) -> Gcal_api_t.notification

val get_colors :
  with_token ->
  Gcal_api_v.colors_response Lwt.t
