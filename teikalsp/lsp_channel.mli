open Jsonrpc

type channel
type t = channel

val notify : channel -> Lsp.Server_notification.t -> unit

type on_request = {
  f :
    'response.
    channel ->
    'response Lsp.Client_request.t ->
    ('response, Response.Error.t) result;
}

(* TODO: request*)
val listen :
  input:_ Eio.Flow.source ->
  output:_ Eio.Flow.sink ->
  on_request:on_request ->
  on_notification:(channel -> Lsp.Client_notification.t -> unit) ->
  unit

(* val input_loop : input:Chan.input ->
   output:Chan.output -> (Jsonrpc.Packet.t -> Jsonrpc.Packet.t list) -> unit) *)
