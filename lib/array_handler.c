#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define SAFE_BYTE -1

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
      caml_invalid_argument("Array.make");
    else
    {
      res = caml_alloc_shr(size, 0);
    }

    // for (i = 0; i < size; i++)
    //   Field(res, i) = init;
    memset((void *)res, SAFE_BYTE, num_bytes);
  }
  /* Give the GC a chance to run, and run memprof callbacks */
  caml_process_pending_actions();
  CAMLreturn(res);
}