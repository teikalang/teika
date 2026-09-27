open Lsp.Types

type t = {
  uri : DocumentUri.t;
  language_id : LanguageKind.t;
  version : int;
  text : string;
}

type edit_error =
  | Invalid_position of Position.t
  | Invalid_range of Range.t
  | Invalid_offset of int

module Change = struct
  type t = {
    range : Range.t option;
    range_length : int option;
    start_offset : int;
    end_offset : int;
    inserted_text : string;
    replaced_text : string;
  }

  let range change = change.range
  let range_length change = change.range_length
  let start_offset change = change.start_offset
  let end_offset change = change.end_offset
  let inserted_text change = change.inserted_text
  let replaced_text change = change.replaced_text
end

let create ~uri ~language_id ~version ~text =
  { uri; language_id; version; text }

let uri document = document.uri
let language_id document = document.language_id
let version document = document.version
let text document = document.text
let invalid_position position = Error (Invalid_position position)

module Utf = struct
  let decode_utf8 text offset limit =
    let byte index = Char.code text.[index] in
    let first = byte offset in
    if first < 0x80 then (offset + 1, first)
    else
      let width, initial, minimum =
        if first land 0xe0 = 0xc0 then (2, first land 0x1f, 0x80)
        else if first land 0xf0 = 0xe0 then (3, first land 0x0f, 0x800)
        else if first land 0xf8 = 0xf0 then (4, first land 0x07, 0x10000)
        else (1, 0xfffd, 0)
      in
      if width = 1 || offset + width > limit then (offset + 1, 0xfffd)
      else
        let rec consume index codepoint =
          if index = offset + width then Some codepoint
          else
            let continuation = byte index in
            if continuation land 0xc0 <> 0x80 then None
            else
              consume (index + 1)
                ((codepoint lsl 6) lor (continuation land 0x3f))
        in
        match consume (offset + 1) initial with
        | Some codepoint
          when codepoint >= minimum && codepoint <= 0x10ffff
               && not (codepoint >= 0xd800 && codepoint <= 0xdfff) ->
            (offset + width, codepoint)
        | Some _ | None -> (offset + 1, 0xfffd)

  let utf16_width codepoint = if codepoint > 0xffff then 2 else 1
end

let line_bounds document line =
  if line < 0 then None
  else
    let length = String.length document.text in
    let rec find_start offset current_line =
      if current_line = line then Some offset
      else
        match String.index_from_opt document.text offset '\n' with
        | None -> None
        | Some newline -> find_start (newline + 1) (current_line + 1)
    in
    match find_start 0 0 with
    | None -> None
    | Some start ->
        let physical_end =
          Option.value
            (String.index_from_opt document.text start '\n')
            ~default:length
        in
        let logical_end =
          if physical_end > start && document.text.[physical_end - 1] = '\r'
          then physical_end - 1
          else physical_end
        in
        Some (start, logical_end)

let offset_of_position document (position : Position.t) =
  match line_bounds document position.line with
  | None -> invalid_position position
  | Some (line_start, line_end) ->
      if position.character < 0 then invalid_position position
      else
        let rec advance offset character =
          if character = position.character then Ok offset
          else if offset >= line_end then Ok line_end
          else
            let next_offset, codepoint =
              Utf.decode_utf8 document.text offset line_end
            in
            let next_character = character + Utf.utf16_width codepoint in
            if next_character > position.character then
              invalid_position position
            else advance next_offset next_character
        in
        advance line_start 0

let position_of_offset document target =
  let length = String.length document.text in
  if target < 0 || target > length then Error (Invalid_offset target)
  else
    let rec advance offset line character =
      if offset = target then Ok (Position.create ~line ~character)
      else if offset > target then Error (Invalid_offset target)
      else if
        document.text.[offset] = '\r'
        && offset + 1 < length
        && document.text.[offset + 1] = '\n'
      then
        if target = offset + 1 then Ok (Position.create ~line ~character)
        else advance (offset + 2) (line + 1) 0
      else if document.text.[offset] = '\n' then
        advance (offset + 1) (line + 1) 0
      else
        let next_offset, codepoint =
          Utf.decode_utf8 document.text offset length
        in
        if next_offset > target then Error (Invalid_offset target)
        else advance next_offset line (character + Utf.utf16_width codepoint)
    in
    advance 0 0 0

let full_range document =
  let start = Position.create ~line:0 ~character:0 in
  let end_ =
    match position_of_offset document (String.length document.text) with
    | Ok position -> position
    | Error _ -> assert false
  in
  Range.create ~start ~end_

let replace text ~start_offset ~end_offset ~with_ =
  let prefix = String.sub text 0 start_offset in
  let suffix = String.sub text end_offset (String.length text - end_offset) in
  prefix ^ with_ ^ suffix

let apply_change document = function
  | `TextDocumentContentChangeWholeDocument whole ->
      let inserted_text = whole.TextDocumentContentChangeWholeDocument.text in
      let end_offset = String.length document.text in
      let change : Change.t =
        {
          range = None;
          range_length = None;
          start_offset = 0;
          end_offset;
          inserted_text;
          replaced_text = document.text;
        }
      in
      Ok ({ document with text = inserted_text }, change)
  | `TextDocumentContentChangePartial partial -> (
      let range = partial.TextDocumentContentChangePartial.range in
      let range_length = partial.rangeLength in
      let inserted_text = partial.text in
      match
        ( offset_of_position document range.start,
          offset_of_position document range.end_ )
      with
      | Ok start_offset, Ok end_offset when start_offset <= end_offset ->
          let replaced_text =
            String.sub document.text start_offset (end_offset - start_offset)
          in
          let text =
            replace document.text ~start_offset ~end_offset ~with_:inserted_text
          in
          let change : Change.t =
            {
              range = Some range;
              range_length;
              start_offset;
              end_offset;
              inserted_text;
              replaced_text;
            }
          in
          Ok ({ document with text }, change)
      | Ok _, Ok _ -> Error (Invalid_range range)
      | Error error, _ | _, Error error -> Error error)

let apply_changes document ~version changes =
  let rec loop current applied = function
    | [] -> Ok ({ current with version }, List.rev applied)
    | change :: changes -> (
        match apply_change current change with
        | Error _ as error -> error
        | Ok (current, applied_change) ->
            loop current (applied_change :: applied) changes)
  in
  loop document [] changes

let string_of_edit_error = function
  | Invalid_position position ->
      Format.asprintf "invalid position %d:%d" position.line position.character
  | Invalid_range range ->
      Format.asprintf "invalid range %d:%d-%d:%d" range.start.line
        range.start.character range.end_.line range.end_.character
  | Invalid_offset offset -> Format.asprintf "invalid byte offset %d" offset
