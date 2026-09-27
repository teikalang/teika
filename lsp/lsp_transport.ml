module Sync_io = struct
  type 'a t = 'a

  let return value = value
  let raise = Stdlib.raise

  module O = struct
    let ( let+ ) value f = f value
    let ( let* ) value f = f value
  end
end

module Channel = struct
  type input = in_channel
  type output = out_channel

  let read_line input =
    match input_line input with
    | line -> Some line
    | exception End_of_file -> None

  let read_exactly input length =
    if length = 0 then Some ""
    else
      match really_input_string input length with
      | contents -> Some contents
      | exception End_of_file ->
          raise (Lsp.Io.Error "unexpected EOF while reading a packet")

  let write output chunks =
    List.iter (output_string output) chunks;
    flush output
end

module Transport = Lsp.Io.Make (Sync_io) (Channel)
