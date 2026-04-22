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

#include "cobaux.h"

void
cobaux_display (const int to_device, const int newline, const int varcnt,
		cob_field ** fields) {
  int i;
  for (i = 0; i < varcnt; i++) {
    cob_display (to_device, i == varcnt - 1 && newline, 1, fields[i]);
  }
}

void
cobaux_display1 (const int to_device, const int newline, cob_field * field) {
  cob_display (to_device, newline, 1, field);
}
