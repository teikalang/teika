module Io : sig
  type 'a t

  val return : 'a -> 'a t
  val raise : exn -> 'a t
  val await : 'a t -> 'a

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  module Chan : sig
    type input
    type output

    (* eio *)
    val of_source : #Eio.Flow.source -> input
    val with_sink : #Eio.Flow.sink -> (output -> 'a) -> 'a

    (* lsp *)
    val read_line : input -> string option t
    val read_exactly : input -> int -> string option t
    val write : output -> string -> unit t
  end
end = struct
  type 'a t = sw:Eio.Switch.t -> ('a, exn) result Eio.Promise.t

  let await : 'a t -> 'a =
   fun t -> Eio.Switch.run @@ fun sw -> Eio.Promise.await_exn (t ~sw)

  let return : 'a -> 'a t =
   fun value ~sw:_ -> Eio.Promise.create_resolved (Ok value)

  let error : exn -> 'a t =
   fun desc ~sw:_ -> Eio.Promise.create_resolved (Error desc)

  let async : (sw:Eio.Switch.t -> ('a, exn) result) -> 'a t =
   fun f ~sw ->
    let promise, resolver = Eio.Promise.create () in
    ( Eio.Fiber.fork ~sw @@ fun () ->
      try
        let result = f ~sw in
        Eio.Promise.resolve resolver result
      with exn -> Eio.Promise.resolve resolver @@ Error exn );
    promise

  let bind : 'a t -> ('a -> 'b t) -> 'b t =
   fun t f ->
    async @@ fun ~sw ->
    match Eio.Promise.await (t ~sw) with
    | Ok value -> Eio.Promise.await @@ f value ~sw
    | Error desc -> Error desc

  let raise = error

  module O = struct
    let ( let+ ) x f = bind x @@ fun value -> return @@ f value
    let ( let* ) = bind
  end

  module Chan = struct
    type input = Input of { mutex : Eio.Mutex.t; buf : Eio.Buf_read.t }
    type output = Output of { mutex : Eio.Mutex.t; buf : Eio.Buf_write.t }

    (* TODO: magic numbers *)
    let initial_size = 1024
    let max_size = 1024 * 1024

    let of_source source =
      let mutex = Eio.Mutex.create () in
      let buf = Eio.Buf_read.of_flow ~initial_size ~max_size source in
      Input { mutex; buf }

    let with_sink sink f =
      let mutex = Eio.Mutex.create () in
      Eio.Buf_write.with_flow ~initial_size sink @@ fun buf ->
      f @@ Output { mutex; buf }

    let read_line input =
      let (Input { mutex; buf }) = input in
      async @@ fun ~sw:_ ->
      (* TODO: what this protect does? *)
      Eio.Mutex.use_rw ~protect:true mutex @@ fun () ->
      match Eio.Buf_read.eof_seen buf with
      | true -> Ok None
      | false -> Ok (Some (Eio.Buf_read.line buf))

    let read_exactly input size =
      let (Input { mutex; buf }) = input in
      async @@ fun ~sw:_ ->
      Eio.Mutex.use_rw ~protect:true mutex @@ fun () ->
      match Eio.Buf_read.eof_seen buf with
      | true -> Ok None
      | false -> Ok (Some (Eio.Buf_read.take size buf))

    let write output str =
      let (Output { mutex; buf }) = output in
      async @@ fun ~sw:_ ->
      Eio.Mutex.use_rw ~protect:true mutex @@ fun () ->
      Ok (Eio.Buf_write.string buf str)
  end
end

module Lsp_io = Lsp.Io.Make (Io) (Io.Chan)

let rec input_loop state ~input ~output =
  (* TODO: buffering and async handling *)
  match Io.await @@ Lsp_io.read input with
  | Some _packet ->
      Format.eprintf "packet\n%!";
      let packets = [] in
      List.iter (fun packet -> Io.await @@ Lsp_io.write output packet) packets;
      input_loop state ~input ~output
  | exception exn -> (* TODO: handle this exception *) raise exn
  | None ->
      (* TODO: this means EOF right? *)
      ()

let main () =
  Eio_main.run @@ fun env ->
  let state = () in
  let input = Io.Chan.of_source env#stdin in
  Io.Chan.with_sink env#stdout @@ fun output -> input_loop state ~input ~output

let () = main ()
