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

#ifndef __COBAUX_H__
#define __COBAUX_H__

#include "libcob.h"
#include "cobaux_typing_helper.h"

/* cobaux_module.c */

extern int
cobaux_module_init (cob_module **module_ptr,
		    cob_global **globals_ptr,
		    const char * const module_name,
		    const char * const module_source);

extern void
cobaux_module_enter (cob_module * const module);

#endif	/* __COBAUX_H__ */
