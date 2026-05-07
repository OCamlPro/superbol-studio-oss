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

#ifndef __COBAUX_TYPING_HELPER_H__
#define __COBAUX_TYPING_HELPER_H__
#include "libcob.h"

/* Helper type to translate a set of constants into a proper enum. */
enum ml_cob_field_type {
  UNKNOWN		= COB_TYPE_UNKNOWN,
  GROUP			= COB_TYPE_GROUP,
  BOOLEAN		= COB_TYPE_BOOLEAN,

  NUMERIC		= COB_TYPE_NUMERIC,
  NUMERIC_DISPLAY	= COB_TYPE_NUMERIC_DISPLAY,
  NUMERIC_BINARY	= COB_TYPE_NUMERIC_BINARY,
  NUMERIC_PACKED	= COB_TYPE_NUMERIC_PACKED,
  NUMERIC_FLOAT		= COB_TYPE_NUMERIC_FLOAT,
  NUMERIC_DOUBLE	= COB_TYPE_NUMERIC_DOUBLE,
  NUMERIC_L_DOUBLE	= COB_TYPE_NUMERIC_L_DOUBLE,
  NUMERIC_FP_DEC64	= COB_TYPE_NUMERIC_FP_DEC64,
  NUMERIC_FP_DEC128	= COB_TYPE_NUMERIC_FP_DEC128,
  NUMERIC_FP_BIN32	= COB_TYPE_NUMERIC_FP_BIN32,
  NUMERIC_FP_BIN64	= COB_TYPE_NUMERIC_FP_BIN64,
  NUMERIC_FP_BIN128	= COB_TYPE_NUMERIC_FP_BIN128,
  NUMERIC_COMP5		= COB_TYPE_NUMERIC_COMP5,

  NUMERIC_EDITED	= COB_TYPE_NUMERIC_EDITED,

  ALNUM			= COB_TYPE_ALNUM,
  ALPHANUMERIC		= COB_TYPE_ALPHANUMERIC,
  ALPHANUMERIC_ALL	= COB_TYPE_ALPHANUMERIC_ALL,
  ALPHANUMERIC_EDITED	= COB_TYPE_ALPHANUMERIC_EDITED,

  NATIONAL		= COB_TYPE_NATIONAL,
  NATIONAL_EDITED	= COB_TYPE_NATIONAL_EDITED,
};

#endif	/* __COBAUX_TYPING_HELPER_H__ */
