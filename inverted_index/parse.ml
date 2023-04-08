(* Returns the entire contents of the provided filename *)
let read_whole_file filename =
  let file = open_in_bin filename in
  let size = in_channel_length file in
  let contents = really_input_string file size in
  close_in_noerr file;
  contents

let isalpha c =
  (Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z')
  || (Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z')

let isnum c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9'
let isalnum c = isalpha c || isnum c

(* Splits a string into words, each with a position-in-characters in the file *)
let split_words s : (string * int) list =
  let rec f i l =
    if i < 0 then l
    else
      let c = String.get s i in
      if isalnum c then g (i - 1) i l else f (i - 1) l
  and g i j l =
    if i < 0 then (String.sub s (i + 1) (j - i), i + 1) :: l
    else
      let c = String.get s i in
      if isalnum c then g (i - 1) j l
      else f (i - 1) ((String.sub s (i + 1) (j - i), i + 1) :: l)
  in
  f (String.length s - 1) []

(* take up to the first n elements of a list *)
let rec take (l : 'a list) (n : int) : 'a list =
  if n = 0 then [] else match l with [] -> [] | x :: t -> x :: take t (n - 1)

type document = { id : int; title : string; contents : string }

(* Computes word vectors for a set of documents. The given file should
 * contain a list of documents: one per line. Each document is of the
 * format: "id @ title @ body" such that '@' does not appear in the title
 * or contents. You could potentially use map-reduce to compute these
 * word vectors *)
let load_documents (filename : string) : document array =
  let f = open_in filename in
  let rec next i accum =
    match try Some (input_line f) with End_of_file -> None with
    | None -> accum
    | Some line -> (
        match String.split_on_char '@' line with
        | [ id; title; contents ] ->
            let rec loop i accum =
              if i < int_of_string id then
                loop (i + 1) ({ id = i; title = ""; contents = "" } :: accum)
              else if i > int_of_string id then
                failwith
                  ("Line number " ^ string_of_int i
                 ^ " identifies as document number " ^ id)
              else next (i + 1) ({ id = i; title; contents } :: accum)
            in
            loop i accum
        | _ -> failwith "malformed input")
  in
  let docs =
    try List.rev (next 0 [])
    with e ->
      close_in f;
      raise e
  in
  close_in f;
  Array.of_list docs
