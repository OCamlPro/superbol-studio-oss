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

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/alloc.h"
#include "caml/fail.h"

#include "cobaux_caml.h"

void camlidl_check_cob_status (int cob_status) {
  if (cob_status != 0) {
    caml_failwith ("libcob function returned non-zero status");
  }
}

static value
camlidl_cob_pic_symbol_c2ml (struct __cob_pic_symbol_ *s) {
  value v;
  v = caml_alloc_tuple (2);
  Store_field (v, 0, Int_val (s->symbol));
  Store_field (v, 1, Int_val (s->times_repeated));
  return v;
}

static void
camlidl_cob_pic_symbol_ml2c (value val, struct __cob_pic_symbol_ *s) {
  s->symbol = (char) Int_val (Field (val, 0));
  s->times_repeated = (int) Int_val (Field (val, 1));
}

value
camlidl_ml_cob_pic_symbols_c2ml (ml_pic_symbols *pic) {
  value val;
  val = caml_alloc_array ((value (*)(const char *))camlidl_cob_pic_symbol_c2ml,
			  (const char**) *pic);
  free (*pic);			/* CHECKME */
  return val;
}

void
camlidl_ml_cob_pic_symbols_ml2c (value val, ml_pic_symbols *pic) {
  mlsize_t len = caml_array_length (val);
  *pic = (ml_pic_symbols) malloc (sizeof (cob_pic_symbol) * len);
  for (int i = 0; i < len; i++) {
    camlidl_cob_pic_symbol_ml2c (Field (val, i), &(*pic)[i]);
  }
}

static ml_cob_field_type ml_cob_field_type_ml2c[24] = {
  TYPE_UNKNOWN			,
  TYPE_GROUP			,
  TYPE_BOOLEAN			,
 
  TYPE_NUMERIC			,
  TYPE_NUMERIC_DISPLAY		,
  TYPE_NUMERIC_BINARY		,
  TYPE_NUMERIC_PACKED		,
  TYPE_NUMERIC_FLOAT		,
  TYPE_NUMERIC_DOUBLE		,
  TYPE_NUMERIC_L_DOUBLE		,
  TYPE_NUMERIC_FP_DEC64		,
  TYPE_NUMERIC_FP_DEC128	,
  TYPE_NUMERIC_FP_BIN32		,
  TYPE_NUMERIC_FP_BIN64		,
  TYPE_NUMERIC_FP_BIN128	,
  TYPE_NUMERIC_COMP5		,

  TYPE_NUMERIC_EDITED		,

  TYPE_ALNUM			,
  TYPE_ALPHANUMERIC		,
  TYPE_ALPHANUMERIC_ALL		,
  TYPE_ALPHANUMERIC_EDITED	,

  TYPE_NATIONAL			,
  TYPE_NATIONAL_EDITED		,
};

void
camlidl_ml_cob_field_type_ml2c (value val, ml_cob_field_type *s) {
  int idx = Int_val (val);
  if (idx >= 0 && idx < 24) {
    *s = ml_cob_field_type_ml2c[idx];
  } else {
    caml_failwith ("camlidl_cob_field_type_ml2c: invalid value for item of type `ml_cob_field_type`");
  }
}

value
camlidl_ml_cob_field_type_c2ml (ml_cob_field_type *s) {
  int idx = 0;
  switch (*s) {
  case TYPE_UNKNOWN:		 default: idx = 0; break;
  case TYPE_GROUP			: idx = 1; break;
  case TYPE_BOOLEAN			: idx = 2; break;

  /* case TYPE_NUMERIC			: idx = 3; break; */
  case TYPE_NUMERIC_DISPLAY		: idx = 4; break;
  case TYPE_NUMERIC_BINARY		: idx = 5; break;
  case TYPE_NUMERIC_PACKED		: idx = 6; break;
  case TYPE_NUMERIC_FLOAT		: idx = 7; break;
  case TYPE_NUMERIC_DOUBLE		: idx = 8; break;
  case TYPE_NUMERIC_L_DOUBLE		: idx = 9; break;
  case TYPE_NUMERIC_FP_DEC64		: idx = 10; break;
  case TYPE_NUMERIC_FP_DEC128		: idx = 11; break;
  case TYPE_NUMERIC_FP_BIN32		: idx = 12; break;
  case TYPE_NUMERIC_FP_BIN64		: idx = 13; break;
  case TYPE_NUMERIC_FP_BIN128		: idx = 14; break;
  case TYPE_NUMERIC_COMP5		: idx = 15; break;

  case TYPE_NUMERIC_EDITED		: idx = 16; break;

  case TYPE_ALNUM			: idx = 17; break;
  case TYPE_ALPHANUMERIC		: idx = 18; break;
  case TYPE_ALPHANUMERIC_ALL		: idx = 19; break;
  case TYPE_ALPHANUMERIC_EDITED		: idx = 20; break;

  case TYPE_NATIONAL			: idx = 21; break;
  case TYPE_NATIONAL_EDITED		: idx = 22; break;
  }
  return Val_int (idx);
}
