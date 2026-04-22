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
#include <string.h>

#include "cobaux.h"

/* --- */

/* Note: allocates the header and payload array as a single chunk. */
memory_t *
cobaux_alloc_memory (const size_t size) {
  memory_t *m = (memory_t*) calloc (sizeof (memory_t) + size, 1);
  m->array = (unsigned char*) (m + 1);
  m->size = size;
  return m;
}

cob_field *
cobaux_field_create (memory_t * const memory,
		     const size_t offset,
		     const size_t size,
		     const cob_field_attr * const attrs) {
  cob_field * f = (cob_field*) malloc (sizeof (cob_field));
  cob_field_attr * attr = (cob_field_attr*) malloc (sizeof (cob_field_attr));
  memcpy (attr, attrs, sizeof (cob_field_attr));
  f->size = size;
  f->attr = attr;
  f->data = &memory->array[offset];
  return f;
}
