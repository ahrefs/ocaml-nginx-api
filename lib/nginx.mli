(** An nginx instance to be configured *)

type nginx = {
  base_url : string;
  additional_headers : string list;
  api_path : string list;
}

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

val create : ?additional_headers:string list -> ?api_path:string list -> string -> nginx

type 't r = ('t, string) result Lwt.t

val nginx : nginx -> Nginx_t.nginx r

val endpoints : nginx -> string list -> Nginx_j.endpoints r

val pp_endpoints_cache : unit -> unit

val upd_all_endpoints : nginx -> unit r

val endpoints_cache_size : unit -> int

module Upstream : sig
  type t = Nginx_t.upstream
  type upstream_id = string

  val upstream_id : string -> upstream_id

  (** Fetch a list of all upstreams configured *)
  val list : nginx -> t list r

  (** Fetch state and configuration of the given upstream *)
  val get : nginx -> upstream_id -> t r

  (** Reset statistics of the given upstream *)
  val reset : nginx -> upstream_id -> unit r

  module Server : sig
    type t = Nginx_t.upstream_server
    type server_id = int

    val server_id : int -> server_id

    (** Fetch a list of all upstreams configured *)
    val list : nginx -> upstream_id -> t list r

    (** Add a server to upstream server_list *)
    val add : nginx -> upstream_id -> Nginx_t.upstream_server_new -> t r

    (** Fetch status and configuration of the given server *)
    val get : nginx -> upstream_id -> server_id -> t r

    (** Remove the server from the upstream group *)
    val remove : nginx -> upstream_id -> server_id -> unit r

    (** Updates configuration of the server in the upstream group *)
    val update : nginx -> upstream_id -> server_id -> Yojson.Basic.t -> unit r
  end
end
