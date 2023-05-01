(**************************************************************************)
(* Copyright (C) 2023  Andrew Tao                                         *)
(*                                                                        *)
(* This library is free software; you can redistribute it and/or          *)
(* modify it under the terms of the GNU Lesser General Public             *)
(* License as published by the Free Software Foundation; either           *)
(* version 2.1 of the License, or (at your option) any later version.     *)
(*                                                                        *)
(* This library is distributed in the hope that it will be useful,        *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of         *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      *)
(* Lesser General Public License for more details.                        *)
(*                                                                        *)
(* You should have received a copy of the GNU Lesser General Public       *)
(* License along with this library; if not, write to the Free Software    *)
(* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,             *)
(* MA  02110-1301  USA                                                    *)
(**************************************************************************)

module type S = sig
  type 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  val iteri : (int -> 'a -> unit) -> 'a t -> unit
  val length : 'a t -> int
  val empty : unit -> 'a t
  val singleton : 'a -> 'a t
  val nth : 'a t -> int -> 'a
  val cons : 'a -> 'a t -> 'a t
  val tabulate : (int -> 'a) -> int -> 'a t
  val repeat : 'a -> int -> 'a t
  val append : 'a t -> 'a t -> 'a t
  val seq_of_array : 'a array -> 'a t
  val array_of_seq : 'a t -> 'a array
  val zip : 'a t * 'b t -> ('a * 'b) t
  val split : 'a t -> int -> 'a t * 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val reduce : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a
  val map_reduce : ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> 'a t -> 'b
  val scan : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
  val flatten : 'a t t -> 'a t
  val filter : ('a -> bool) -> 'a t -> 'a t
end

type seq_type = Sequential | Parallel of (Domainslib.Task.pool * int)

val get_module : seq_type -> (module S)
