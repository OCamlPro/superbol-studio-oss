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

typedef struct __memory {
  size_t size;
  unsigned char *array;
} memory_t;

/* cobaux_module.c */

extern int
cobaux_module_init (cob_module **module_ptr,
		    cob_global **globals_ptr,
		    const char * const module_name,
		    const char * const module_source);

extern void
cobaux_module_enter (cob_module * const module);

/* cobaux_field.c */

/* struct __cob_field * */
/* cobaux_field_create (struct __memory * const memory, */
/* 		     const size_t offset, */
/* 		     const size_t size, */
/* 		     const struct __cob_field_attr * const attrs); */

/* cobaux_termio.c */

/* Note: unused for now */
extern void
cobaux_display (const int to_device, const int newline, const int varcnt,
		cob_field ** fields);

/* Note: unused for now */
extern void
cobaux_display1 (const int to_device, const int newline, cob_field * field);

#endif	/* __COBAUX_H__ */
