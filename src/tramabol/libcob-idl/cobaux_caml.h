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

#ifndef __COBAUX_CAML_H__
#define __COBAUX_CAML_H__

#include "../cobaux/cobaux.h"

#include "module.h"
#include "field.h"
#include "termio.h"

value camlidl_ml_cob_pic_symbols_c2ml(ml_pic_symbols*);
void camlidl_ml_cob_pic_symbols_ml2c(value, ml_pic_symbols*);

value camlidl_ml_cob_field_type_c2ml (ml_cob_field_type *s);
void camlidl_ml_cob_field_type_ml2c (value val, ml_cob_field_type *s);

void camlidl_check_cob_status (int cob_status);

#endif	/* __COBAUX_CAML_H__ */
