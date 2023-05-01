/**************************************************************************/
/* Copyright (C) 2023  Andrew Tao                                         */
/*                                                                        */
/* This library is free software; you can redistribute it and/or          */
/* modify it under the terms of the GNU Lesser General Public             */
/* License as published by the Free Software Foundation; either           */
/* version 2.1 of the License, or (at your option) any later version.     */
/*                                                                        */
/* This library is distributed in the hope that it will be useful,        */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of         */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      */
/* Lesser General Public License for more details.                        */
/*                                                                        */
/* You should have received a copy of the GNU Lesser General Public       */
/* License along with this library; if not, write to the Free Software    */
/* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,             */
/* MA  02110-1301  USA                                                    */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define SAFE_VAL -1

/* Adapted from the OCaml runtime Array.make on 4/24/23 */
/* [len] is a [value] representing number of words */
CAMLprim value caml_make_uninitialized_vect(value len)
{
  CAMLparam1(len);
  CAMLlocal1(res);
  mlsize_t size, i, num_bytes;

  size = Long_val(len);
  num_bytes = size * sizeof(value);
  if (size == 0)
  {
    res = Atom(0);
  }
  else
  {
    if (size <= Max_young_wosize)
    {
      res = caml_alloc_small(size, 0);
    }
    else if (size > Max_wosize)
      caml_invalid_argument("Array_handler.get_uninitialized");
    else
    {
      res = caml_alloc_shr(size, 0);
    }

    for (i = 0; i < size; i++)
      Field(res, i) = SAFE_VAL;
  }
  /* Give the GC a chance to run, and run memprof callbacks */
  caml_process_pending_actions();
  CAMLreturn(res);
}