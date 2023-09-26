open Devkit
open Result

let default_api_path = [ "api"; "8" ]

type url = {
  base : string;
  path : string list;
}

type nginx = {
  base_url : string;
  additional_headers : string list;
  api_path : string list;
}

let string_of_url url = Printf.sprintf "%s/%s" url.base (String.concat "/" url.path)

let endpoints_cache = Hashtbl.create 100

let pp_endpoints_cache () = Hashtbl.iter (fun k _v -> Printf.printf "%s\n" k) endpoints_cache

let add_endpoint url = Hashtbl.replace endpoints_cache url None

let update_endpoint url terminal_val = Hashtbl.replace endpoints_cache url (Some terminal_val)

let create ?(additional_headers = []) ?(api_path = default_api_path) base_url =
  { base_url; additional_headers; api_path }

type resp_error =
  | MethodDisabled
  | UnknownVersion
  | UpstreamNotFound
  | UpstreamStatic
  | Other of int

type nginx_response =
  | Upstream of Nginx_t.upstream
  | Endpoints of Nginx_t.endpoints
  | ErrorObj of Nginx_t.error_obj
  | Nginx of Nginx_t.nginx
  | Peer_state of Nginx_t.peer_state
  | Ssl of Nginx_t.ssl
  | Success
  | Upstreams of Nginx_t.upstream list
  | UpstreamServer of Nginx_t.upstream_server

type 't r = ('t, string) result Lwt.t

module ReqErr : Map.OrderedType = struct
  type t = string * int
  let compare (fn1, code1) (fn2, code2) =
    match String.compare fn1 fn2 with
    | 0 -> Int.compare code1 code2
    | c -> c
end

module ErrsMap = Map.Make (ReqErr)

let _errs_map = ErrsMap.empty

let return_generic_response url ?body code parse str =
  let body_str =
    match body with
    | None -> ""
    | Some (`Raw (_ct, str)) -> str
    | _ -> ""
  in
  if code >= 200 && code < 300 then Lwt.return (Ok (parse str))
  else Lwt.return (Error (Printf.sprintf "unsuccessful request %s: [%s] (HTTP code: %d): %s" url body_str code str))

let do_api8 verb ng path ?body parse_resp =
  let nm = Printf.sprintf "%s %s" (Web.string_of_http_action verb) (String.concat ":" path) in
  let url = string_of_url { base = ng.base_url; path = ng.api_path @ path } in
  let body = Option.map (fun v -> `Raw ("application/json", v)) body in
  match%lwt Web.http_request_lwt' ~headers:ng.additional_headers ?body verb url with
  | `Ok (code, str) ->
    (try return_generic_response url ?body code parse_resp str
     with exn -> Lwt.return (Error (Printf.sprintf "error\n  calling %s - %s" nm (Printexc.to_string exn))))
  | `Error _ as e -> Lwt.fail_with (Printf.sprintf "error calling %s - %s" nm (Web.show_result e))

let do_api8_del_req ng path parse_resp = do_api8 `DELETE ng path parse_resp

let do_api8_get_req ng path parse_resp = do_api8 `GET ng path parse_resp

let do_api8_patch_req ng path body parse_resp = do_api8 `PATCH ng path ~body parse_resp

let do_api8_post_req ng path body parse_resp = do_api8 `POST ng path ~body parse_resp

let endpoints ng path = do_api8_get_req ng path (fun str -> Nginx_j.endpoints_of_string str)

let endpoints_cache_size () = Hashtbl.length endpoints_cache

let upd_all_endpoints ng =
  let rec upd_aux path =
    match%lwt endpoints ng path with
    | Ok l ->
      update_endpoint (string_of_url { base = ng.base_url; path = [ "api"; "8" ] @ path }) false;
      Lwt_list.iter_s upd_aux
        (List.map
           (fun i ->
             let np = path @ [ i ] in
             add_endpoint (string_of_url { base = ng.base_url; path = [ "api"; "8" ] @ np });
             np)
           l)
    | _ -> Lwt.return_unit
  in
  let%lwt res = upd_aux [] in
  Lwt.return (Ok res)

let nginx ng = do_api8_get_req ng [ "nginx" ] (fun str -> Nginx_j.nginx_of_string str)

module Upstream = struct
  type t = Nginx_t.upstream
  type upstream_id = string

  let upstream_id (str : string) = str

  (* val list : nginx -> t list r *)

  (** Fetch a list of all upstreams configured *)
  let list ng =
    do_api8_get_req ng [ "http"; "upstreams" ] (fun resp -> List.map snd (Nginx_j.upstream_collection_of_string resp))

  (* val get : nginx -> upstream_id -> t r *)

  (** Fetch state and configuration of the given upstream *)
  let get ng upstr_id =
    do_api8_get_req ng [ "http"; "upstreams"; upstr_id ] (fun (str : string) -> Nginx_j.upstream_of_string str)

  (* val reset : nginx -> upstream_id -> unit r *)

  (** Reset statistics of the given upstream *)
  let reset ng upstr_id = do_api8_del_req ng [ "http"; "upstreams"; upstr_id ] (Fun.const ())

  module Server = struct
    type t = Nginx_t.upstream_server
    type server_id = int

    let server_id id : int = id

    (* val list : nginx -> upstream_id -> t list r *)

    (** Fetch a list of all upstreams configured *)
    let list ng upstr_id =
      do_api8_get_req ng [ "http"; "upstreams"; upstr_id; "servers" ] Nginx_j.upstream_server_list_of_string

    (* val add : nginx -> upstream_id -> t -> unit r *)

    (** Add a server to upstream server_list *)
    let add ng upstr_id srv =
      do_api8_post_req ng
        [ "http"; "upstreams"; upstr_id; "servers" ]
        (Nginx_j.string_of_upstream_server_new srv)
        Nginx_j.upstream_server_of_string

    (* val get : nginx -> upstream_id -> server_id -> t r *)

    (** Fetch status and configuration of the given server *)
    let get ng upstr_id srv =
      do_api8_get_req ng
        [ "http"; "upstreams"; upstr_id; "servers"; string_of_int srv ]
        (fun str -> Nginx_j.upstream_server_of_string str)

    (* val remove : nginx -> upstream_id -> server_id -> unit r *)

    (** Remove the server from the upstream group *)
    let remove ng upstr_id srv_id =
      do_api8_del_req ng [ "http"; "upstreams"; upstr_id; "servers"; string_of_int srv_id ] (Fun.const ())

    let update ng upstr_id srv_id conf =
      do_api8_patch_req ng
        [ "https"; "upstreams"; upstr_id; "servers"; string_of_int srv_id ]
        (Yojson.Basic.to_string conf) (Fun.const ())
  end
end
