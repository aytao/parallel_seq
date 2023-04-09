open Seq
open Sequence

(* doc_id is a string, representing the document id, 
 * which will be used in the values in the index. *)
module DocID = struct
  type t = int

  let compare = compare
end

type doc_id = DocID.t

module DMap = Map.Make (String)

(* doc_loc_index  a mapping with:
 * strings as keys
 * Sets of locations -- each location has
     doc_id:  which document this word is in
     int:  this word appears as the nth word in the file
     int:  this word starts at the nth character of the file
 *)
type location = doc_id * int * int
type doc_loc_index = location array DMap.t
type index_builder = location list DMap.t

let combine_indexes (x : doc_loc_index) (y : doc_loc_index) : doc_loc_index =
  let combine k a b = Some (Array.append a b) in
  DMap.union combine x y

let isalpha c =
  (Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z')
  || (Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z')

let isnum c = Char.code '0' <= Char.code c && Char.code c <= Char.code '9'
let isalnum c = isalpha c || isnum c

(* Splits a string into words, each with a position-in-characters in the file *)
let add_page (id : int) (contents : string) (index : index_builder) :
    index_builder =
  let add_word word w_loc c_loc index =
    let word = String.lowercase_ascii word in
    let l = try DMap.find word index with Not_found -> [] in
    DMap.add word ((id, w_loc, c_loc) :: l) index
  in
  let content_length = String.length contents in
  let rec f i w_loc index =
    if i >= content_length then index
    else
      let c = String.get contents i in
      if isalnum c then g (i + 1) i w_loc index else f (i + 1) w_loc index
  and g i word_start w_loc index =
    if i >= content_length then
      let word = String.sub contents word_start (i - word_start) in
      add_word word w_loc word_start index
    else
      let c = String.get contents i in
      if isalnum c then g (i + 1) word_start w_loc index
      else
        let word = String.sub contents word_start (i - word_start) in
        f (i + 1) (w_loc + 1) (add_word word w_loc word_start index)
  in
  f 0 0 index

let process (docs_filename : string) (chunk_start : int) (chunk_end : int) =
  let docs_file = open_in_bin docs_filename in
  (* seek to next start of line *)
  let () =
    if chunk_start != 0 then
      let () = seek_in docs_file (chunk_start - 1) in
      let c = input_char docs_file in
      if c != '\n' then
        let _ = input_line docs_file in
        ()
  in
  let rec next accum =
    if pos_in docs_file >= chunk_end then accum
    else
      match try Some (input_line docs_file) with End_of_file -> None with
      | None -> accum
      | Some line -> (
          match String.split_on_char '@' line with
          | [ id; contents ] ->
              let id = int_of_string id in
              next (add_page id contents accum)
          | _ -> failwith line)
  in
  let index =
    try next DMap.empty
    with e ->
      let () = close_in docs_file in
      raise e
  in
  let () = close_in docs_file in
  DMap.map (fun l -> List.rev l |> Array.of_list) index

let make_index (docs_filename : string) : doc_loc_index =
  let ceil_div num den = (num + den - 1) / den in
  let docs_file = open_in_bin docs_filename in
  let file_len = in_channel_length docs_file in
  let chunk_size = ceil_div file_len Defines.num_domains in
  let () = close_in_noerr docs_file in
  S.tabulate
    (fun i ->
      let chunk_start = i * chunk_size in
      let chunk_end = min ((i + 1) * chunk_size) file_len in
      process docs_filename chunk_start chunk_end)
    Defines.num_domains
  |> S.reduce combine_indexes DMap.empty

(* let index = make_index "index_data/test_index_1000.txt" *)

(*
DMap.iter
  (fun word seq ->
    print_string "Key: {";
    print_string word;
    print_string "} Values: {";
    Array.iter
      (fun (i, w, c) ->
        print_int i;
        print_string ":";
        print_int w;
        print_string ":";
        print_int c;
        print_string " ")
      seq;
    print_string "}\n")
  index *)