#line 2 "src/ezlibcob/libcob_stubs_header.c"
/**************************************************************************/
/*                                                                        */
/*                        SuperBOL OSS Studio                             */
/*                                                                        */
/*  Copyright (c) 2022-2026 OCamlPro SAS                                  */
/*                                                                        */
/* All rights reserved.                                                   */
/* This source code is licensed under the GNU Affero General Public       */
/* License version 3 found in the LICENSE.md file in the root directory   */
/* of this source tree.                                                   */
/*                                                                        */
/**************************************************************************/

#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Note: we need to include gmp.h because cob_decimal is only defined
   if gmp.h is before libcob.h. But this is inconsistent as we don't
   really know what cobc has decided... This should be seen as a bug
   in GnuCOBOL, libcob.h should use an exported version of config.h
   for that. */
#include <gmp.h>
#include "libcob/common.h"

#include "stubs.h"

CAMLprim value ml_cob_resolve_cobol_and_call (value function_v)
{
	CAMLparam1( function_v );
	cob_call_union  unifunc;

	unifunc.funcvoid =
		cob_resolve_cobol (String_val(function_v), 0, 1);
        CAMLreturn( Val_int( unifunc.funcint()) );
}

/* The following line number should be offset +2 to its line number */
#line 51 "_build/default/src/ezlibcob/libcob_stubs.c"
