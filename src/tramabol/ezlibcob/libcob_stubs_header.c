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


#include <dlfcn.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* Note: we need to include gmp.h because cob_decimal is only defined
   if gmp.h is before libcob.h. But this is inconsistent as we don't
   really know what cobc has decided... This should be seen as a bug
   in GnuCOBOL, libcob.h should use an exported version of config.h
   for that. */
#include <gmp.h>
#include "libcob/common.h"

#include "stubs.h"

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define CAMLparam6(p1,p2,p3,p4,p5,p6) \
  CAMLparam0 (); \
  CAMLxparam3 (p1, p1, p3) ; \
  CAMLxparam3 (p4, p5, p6)

#define CAMLparam7(p1,p2,p3,p4,p5,p6,p7) \
  CAMLparam0 (); \
  CAMLxparam4 (p1, p1, p3, p4) ; \
  CAMLxparam3 (p5, p6, p7)

#define CAMLparam8(p1,p2,p3,p4,p5,p6,p7,p8) \
  CAMLparam0 (); \
  CAMLxparam4 (p1, p1, p3, p4) ; \
  CAMLxparam4 (p5, p6, p7, p8)

#define CAMLparam9(p1,p2,p3,p4,p5,p6,p7,p8,p9) \
  CAMLparam0 (); \
  CAMLxparam5 (p1, p1, p3, p4, p5) ; \
  CAMLxparam4 (p6, p7, p8, p9)

#define CAMLparam10(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10) \
  CAMLparam0 (); \
  CAMLxparam5 (p1, p1, p3, p4, p5) ; \
  CAMLxparam5 (p6, p7, p8, p9, p10)

#define Comp_val(x)		((void *)Field((x), 0))
#define Val_comp(x,v)		((x) = caml_alloc_small(1, Abstract_tag), \
				 Field((x), 0) = (value)(v))

#define Ptr_val(x)		((void *)Field((x), 3))
#define Val_ptr(x,id,c,is,v)	((x) = caml_alloc_small(4, Abstract_tag), \
				 Field((x), 0) = (value)(id), \
				 Field((x), 1) = (value)(c), \
				 Field((x), 2) = (value)(is), \
				 Field((x), 3) = (value)(v))

#define Array_val(x)		 ((void *)Field((x), 3))
#define Val_array(x,id,c,is,v,s) ((x) = caml_alloc_small(5, Abstract_tag), \
				  Field((x), 0) = (value)(id), \
				  Field((x), 1) = (value)(c), \
				  Field((x), 2) = (value)(is), \
				  Field((x), 3) = (value)(v), \
				  Field((x), 4) = (value)(s))

static inline void *copy(const void *src, size_t size)
{
	void *dst = malloc(size);
	if (dst != NULL) {
		memcpy(dst, src, size);
	}
	return dst;
}

#define copy(x) copy(&(x), sizeof(x))

#define min(a,b) ((a) <= (b) ? (a) : (b))

enum kind_id {
	VOID,
	CHAR,
	SINT8,
	UINT8,
	SINT16,
	UINT16,
	SINT32,
	UINT32,
	SINT64,
	UINT64,
	CFLOAT,
	CDOUBLE,
	CENUM,
	CCOMP,
	/* Blocks */
	CPTR,
	CARRAY
};

typedef struct kind_t {
	struct kind_t *contents;
	int size;
	enum kind_id id;
} kind_t;

static kind_t * mk_kind(enum kind_id id, struct kind_t *contents, int size)
{
	kind_t *kind = malloc(sizeof(kind_t));
	kind->id = id;
	kind->contents = contents;
	kind->size = size;
	return kind;
}

static kind_t translate_kind(value kind_v)
{
	kind_t kind;
	if (Is_long(kind_v)) {
		kind.id = VOID + Long_val(kind_v);
		kind.contents = NULL;
		kind.size = 0;
	} else {
		kind.id = CPTR + Tag_val(kind_v);
		kind.contents = malloc(sizeof(kind_t));
		*(kind.contents) = translate_kind(Field(kind_v, 0));
       		if (kind.id == CARRAY) {
			kind.size = Long_val(Field(kind_v, 1));
		} else {
			kind.size = 0;
		}
	}
	return kind;
}

static kind_t * copy_kind(kind_t *skind)
{
	kind_t *dkind = NULL;
	if (skind != NULL) {
		dkind = malloc(sizeof(kind_t));
		dkind->id = skind->id;
		dkind->size = skind->size;
		dkind->contents = copy_kind(skind->contents);
	}
	return dkind;
}

static long kind_size(enum kind_id id)
{
	switch (id) {
	case VOID: return 1; /* Actually 0 */
	case CHAR: return 1;
	case SINT8: return 1;
	case UINT8: return 1;
	case SINT16: return 2;
	case UINT16: return 2;
	case SINT32: return 4;
	case UINT32: return 4;
	case SINT64: return 8;
	case UINT64: return 8;
	case CFLOAT: return 4;
	case CDOUBLE: return 8;
	case CENUM: return 4;
	case CCOMP: return 8;
	case CPTR: return 8;
	case CARRAY: return 8;
	}
	return 8;
}

static value ml_alloc_ptr(const kind_t *kind, void *data)
{
	CAMLparam0();
	CAMLlocal1(res_v);
	Val_ptr(res_v, kind->id, kind->contents, kind->size, data);
	CAMLreturn(res_v);
}

static value ml_alloc_array(const kind_t *kind, void *data, long size)
{
	CAMLparam0();
	CAMLlocal1(res_v);
	Val_array(res_v, kind->id, kind->contents, kind->size, data, size);
	CAMLreturn(res_v);
}

CAMLprim value ml_ptr_null(value kind_v)
{
	CAMLparam1(kind_v);
	CAMLlocal1(res_v);
	kind_t kind = translate_kind(kind_v);
	res_v = ml_alloc_ptr(&kind, NULL);
	CAMLreturn(res_v);
}

CAMLprim value ml_ptr_create(value default_v, value kind_v)
{
	CAMLparam2(default_v, kind_v);
	CAMLlocal2(value_v, res_v);
	kind_t kind = translate_kind(kind_v);
	void *data = (void *)calloc(1, kind_size(kind.id));
	if (Is_some(default_v)) {
		value_v = Some_val(default_v);
		switch (kind.id) {
		case CHAR: *((char *)data) = (char)Long_val(value_v); break;
		case SINT8: *((signed char *)data) = (signed char)Long_val(value_v); break;
		case UINT8: *((unsigned char *)data) = (unsigned char)Long_val(value_v); break;
		case SINT16: *((signed short *)data) = (signed short)Long_val(value_v); break;
		case UINT16: *((unsigned short *)data) = (unsigned short)Long_val(value_v); break;
		case SINT32: *((signed int *)data) = (signed int)Long_val(value_v); break;
		case UINT32: *((unsigned int *)data) = (unsigned int)Long_val(value_v); break;
		case SINT64: *((signed long *)data) = (signed long)Int64_val(value_v); break;
		case UINT64: *((unsigned long *)data); (unsigned long)Int64_val(value_v); break;
		case CFLOAT: *((float *)data) = (float)Double_val(value_v); break;
		case CDOUBLE: *((double *)data) = (double)Double_val(value_v); break;
		case CENUM: *((signed int *)data) = (signed int)Long_val(value_v); break;
		case CCOMP: *((value *)data) = Field(value_v, 0); break;
		case CPTR: *((value *)data) = Field(value_v, 3); break;
		case CARRAY: *((value *)data) = Field(value_v, 3); break;
		}
	}
	res_v = ml_alloc_ptr(&kind, data);
	CAMLreturn(res_v);
}

CAMLprim value ml_ptr_free(value ptr_v)
{
	void *data = (void *)Field(ptr_v, 3);
	free(data);
	Field(ptr_v, 3) = (value)NULL;
	return Val_unit;
}

CAMLprim value ml_ptr_get(value ptr_v)
{
	CAMLparam1(ptr_v);
	CAMLlocal1(res_v);
	enum kind_id id = (enum kind_id)Field(ptr_v, 0);
	void *data = (void *)Field(ptr_v, 3);
	switch (id) {
	case CHAR: res_v = Val_long(*((char *)data)); break;
	case SINT8: res_v = Val_long(*((signed char *)data)); break;
	case UINT8: res_v = Val_long(*((unsigned char *)data)); break;
	case SINT16: res_v = Val_long(*((signed short *)data)); break;
	case UINT16: res_v = Val_long(*((unsigned short *)data)); break;
	case SINT32: res_v = Val_long(*((signed int *)data)); break;
	case UINT32: res_v = Val_long(*((unsigned int *)data)); break;
	case SINT64: res_v = caml_copy_int64(*((signed long *)data)); break;
	case UINT64: res_v = caml_copy_int64(*((unsigned long *)data)); break;
	case CFLOAT: res_v = caml_copy_double(*((float *)data)); break;
	case CDOUBLE: res_v = caml_copy_double(*((double *)data)); break;
	case CENUM: res_v = Val_long(*((signed int *)data)); break;
	case CCOMP:
		res_v = caml_alloc_small(1, Abstract_tag);
		Field(res_v, 0) = *((value *)data);
		break;
	case CPTR: {
		kind_t *ikind = (kind_t *)Field(ptr_v, 1);
		res_v = ml_alloc_ptr(ikind, *((void **)data));
		break;
	}
	case CARRAY: {
		kind_t *ikind = (kind_t *)Field(ptr_v, 1);
		long size = (long)Field(ptr_v, 2);
		res_v = ml_alloc_array(ikind, *((void **)data), size);
		break; // TODO: maybe just (value)data
	}
	}
	CAMLreturn(res_v);
}

CAMLprim value ml_ptr_set(value ptr_v, value value_v)
{
	enum kind_id id = (enum kind_id)Field(ptr_v, 0);
	void *data = (void *)Field(ptr_v, 3);
	switch (id) {
	case CHAR: *((char *)data) = (char)Long_val(value_v); break;
	case SINT8: *((signed char *)data) = (signed char)Long_val(value_v); break;
	case UINT8: *((unsigned char *)data) = (unsigned char)Long_val(value_v); break;
	case SINT16: *((signed short *)data) = (signed short)Long_val(value_v); break;
	case UINT16: *((unsigned short *)data) = (unsigned short)Long_val(value_v); break;
	case SINT32: *((signed int *)data) = (signed int)Long_val(value_v); break;
	case UINT32: *((unsigned int *)data) = (unsigned int)Long_val(value_v); break;
	case SINT64: *((signed long *)data) = (signed long)Int64_val(value_v); break;
	case UINT64: *((unsigned long *)data); (unsigned long)Int64_val(value_v); break;
	case CFLOAT: *((float *)data) = (float)Double_val(value_v); break;
	case CDOUBLE: *((double *)data) = (double)Double_val(value_v); break;
	case CENUM: *((signed int *)data) = (signed int)Long_val(value_v); break;
	case CCOMP: *((value *)data) = Field(value_v, 0); break;
	case CPTR: *((value *)data) = Field(value_v, 3); break;
	case CARRAY: *((value *)data) = Field(value_v, 3); break;
	}
	return Val_unit;
}

CAMLprim value ml_ptr_cast(value kind_v, value ptr_v)
{
	CAMLparam2(kind_v, ptr_v);
	CAMLlocal1(res_v);
	kind_t kind = translate_kind(kind_v);
	void *data = (char *)Field(ptr_v, 3);
	res_v = ml_alloc_ptr(&kind, data);
	CAMLreturn(res_v);
}

CAMLprim value ml_ptr_add(value ptr_v, value int_v)
{
	CAMLparam1(ptr_v);
	CAMLlocal1(res_v);
	enum kind_id id = (enum kind_id)Field(ptr_v, 0);
	void *data = (void *)Field(ptr_v, 3);
	data += Long_val(int_v) * kind_size(id);
	Val_ptr(res_v, Field(ptr_v, 0),
		copy_kind((kind_t *)Field(ptr_v, 1)),
		Field(ptr_v, 2), data);
	CAMLreturn(res_v);
}

CAMLprim value ml_ptr_sub(value ptr_v, value int_v)
{
	CAMLparam1(ptr_v);
	CAMLlocal1(res_v);
	enum kind_id id = (enum kind_id)Field(ptr_v, 0);
	void *data = (void *)Field(ptr_v, 3);
	data -= Long_val(int_v) * kind_size(id);
	Val_ptr(res_v, Field(ptr_v, 0),
		copy_kind((kind_t *)Field(ptr_v, 1)),
		Field(ptr_v, 2), data);
	CAMLreturn(res_v);
}

CAMLprim value ml_ptr_diff(value ptr1_v, value ptr2_v)
{
	enum kind_id id = (enum kind_id)Field(ptr1_v, 0);
	void *data1 = (void *)Field(ptr1_v, 3);
	void *data2 = (void *)Field(ptr2_v, 3);
	return (data1 - data2) / kind_size(id);
}

CAMLprim value ml_array_create(value default_v, value kind_v, value size_v)
{
	CAMLparam2(default_v, kind_v);
	CAMLlocal2(value_v, res_v);
	kind_t kind = translate_kind(kind_v);
	long size = Long_val(size_v);
	void *data = (void *)malloc(size * kind_size(kind.id));
	if (Is_some(default_v)) {
		value_v = Some_val(default_v);
		for (int i = 0; i < size; ++i) {
			switch (kind.id) {
			case CHAR: ((char *)data)[i] = (char)Long_val(value_v); break;
			case SINT8: ((signed char *)data)[i] = (signed char)Long_val(value_v); break;
			case UINT8: ((unsigned char *)data)[i] = (unsigned char)Long_val(value_v); break;
			case SINT16: ((signed short *)data)[i] = (signed short)Long_val(value_v); break;
			case UINT16: ((unsigned short *)data)[i] = (unsigned short)Long_val(value_v); break;
			case SINT32: ((signed int *)data)[i] = (signed int)Long_val(value_v); break;
			case UINT32: ((unsigned int *)data)[i] = (unsigned int)Long_val(value_v); break;
			case SINT64: ((signed long *)data)[i] = (signed long)Int64_val(value_v); break;
			case UINT64: ((unsigned long *)data)[i]; (unsigned long)Int64_val(value_v); break;
			case CFLOAT: ((float *)data)[i] = (float)Double_val(value_v); break;
			case CDOUBLE: ((double *)data)[i] = (double)Double_val(value_v); break;
			case CENUM: ((signed int *)data)[i] = (signed int)Long_val(value_v); break;
			case CCOMP: ((value *)data)[i] = Field(value_v, 0); break;
			case CPTR: ((value *)data)[i] = Field(value_v, 3); break;
			case CARRAY: ((value *)data)[i] = Field(value_v, 3); break;
			}
		}
	}
	res_v = ml_alloc_array(&kind, data, size);
	CAMLreturn(res_v);
}

CAMLprim value ml_array_free(value array_v)
{
	void *data = (void *)Field(array_v, 3);
	free(data);
	Field(array_v, 3) = (value)NULL;
	return Val_unit;
}

CAMLprim value ml_array_get(value array_v, value index_v)
{
	CAMLparam1(array_v);
	CAMLlocal1(res_v);
	enum kind_id id = (enum kind_id)Field(array_v, 0);
	void *data = (void *)Field(array_v, 3);
	long index = Long_val(index_v);
	switch (id) {
	case CHAR: res_v = Val_long(((char *)data)[index]); break;
	case SINT8: res_v = Val_long(((signed char *)data)[index]); break;
	case UINT8: res_v = Val_long(((unsigned char *)data)[index]); break;
	case SINT16: res_v = Val_long(((signed short *)data)[index]); break;
	case UINT16: res_v = Val_long(((unsigned short *)data)[index]); break;
	case SINT32: res_v = Val_long(((signed int *)data)[index]); break;
	case UINT32: res_v = Val_long(((unsigned int *)data)[index]); break;
	case SINT64: res_v = caml_copy_int64(((signed long *)data)[index]); break;
	case UINT64: res_v = caml_copy_int64(((unsigned long *)data)[index]); break;
	case CFLOAT: res_v = caml_copy_double(((float *)data)[index]); break;
	case CDOUBLE: res_v = caml_copy_double(((double *)data)[index]); break;
	case CENUM: res_v = Val_long(((signed int *)data)[index]); break;
	case CCOMP:
		res_v = caml_alloc_small(1, Abstract_tag);
		Field(res_v, 0) = (((value *)data)[index]);
		break;
	case CPTR: {
		kind_t *ikind = (kind_t *)Field(array_v, 1);
		res_v = ml_alloc_ptr(ikind, ((void **)data)[index]);
		break;
	}
	case CARRAY: {
		kind_t *ikind = (kind_t *)Field(array_v, 1);
		long size = (long)Field(array_v, 2);
		res_v = ml_alloc_array(ikind, ((void **)data)[index], size);
		break; // TODO: maybe just (value)data
	}
	}
	CAMLreturn(res_v);
}

CAMLprim value ml_array_set(value array_v, value index_v, value value_v)
{
	enum kind_id id = (enum kind_id)Field(array_v, 0);
	void *data = (void *)Field(array_v, 3);
	long size = (long)Field(array_v, 4);
	long index = Long_val(index_v);
	switch (id) {
	case CHAR: ((char *)data)[index] = (char)Long_val(value_v); break;
	case SINT8: ((signed char *)data)[index] = (signed char)Long_val(value_v); break;
	case UINT8: ((unsigned char *)data)[index] = (unsigned char)Long_val(value_v); break;
	case SINT16: ((signed short *)data)[index] = (signed short)Long_val(value_v); break;
	case UINT16: ((unsigned short *)data)[index] = (unsigned short)Long_val(value_v); break;
	case SINT32: ((signed int *)data)[index] = (signed int)Long_val(value_v); break;
	case UINT32: ((unsigned int *)data)[index] = (unsigned int)Long_val(value_v); break;
	case SINT64: ((signed long *)data)[index] = (signed long)Int64_val(value_v); break;
	case UINT64: ((unsigned long *)data)[index]; (unsigned long)Int64_val(value_v); break;
	case CFLOAT: ((float *)data)[index] = (float)Double_val(value_v); break;
	case CDOUBLE: ((double *)data)[index] = (double)Double_val(value_v); break;
	case CENUM: ((signed int *)data)[index] = (signed int)Long_val(value_v); break;
	case CCOMP: ((value *)data)[index] = Field(value_v, 0); break;
	case CPTR: ((value *)data)[index] = Field(value_v, 3); break;
	case CARRAY: ((value *)data)[index] = Field(value_v, 3); break;
	}
	return Val_unit;
}

CAMLprim value ml_array_to_ptr(value array_v)
{
	CAMLparam1(array_v);
	CAMLlocal1(res_v);
	Val_ptr(res_v, Field(array_v, 0),
		copy_kind((kind_t *)Field(array_v, 1)),
		Field(array_v, 2), Field(array_v, 3));
	CAMLreturn(res_v);
}

CAMLprim value ml_array_of_ptr(value size_v, value ptr_v)
{
	CAMLparam2(ptr_v, size_v);
	CAMLlocal1(res_v);
	Val_array(res_v, Field(ptr_v, 0),
                  copy_kind((kind_t *)Field(ptr_v, 1)),
                  Field(ptr_v, 2), Field(ptr_v, 3), Long_val(size_v));
	CAMLreturn(res_v);
}

CAMLprim value ml_array_blit(value sarray_v, value spos_v, value darray_v, value dpos_v, value len_v)
{
	enum kind_id id = (enum kind_id)Field(sarray_v, 0);
	void *sdata = (void *)Field(sarray_v, 3);
	long spos = Long_val(spos_v);
	void *ddata = (void *)Field(darray_v, 3);
	long dpos = Long_val(dpos_v);
	long len = Long_val(len_v);
	long elem_size = kind_size(id);
	memcpy(ddata + dpos * elem_size, sdata + spos * elem_size, len * elem_size);
	return Val_unit;
}

CAMLprim value ml_array_of_string(value string_v)
{
	CAMLparam1(string_v);
	CAMLlocal1(res_v);
	const char *string = (const char *)String_val(string_v);
	long size = caml_string_length(string_v);
	char *data = (char *)malloc(size);
        memcpy(data, string, size);
	Val_array(res_v, CHAR, NULL, 0, data, size);
	CAMLreturn(res_v);
}

CAMLprim value ml_array_to_string(value array_v)
{
	CAMLparam1(array_v);
	CAMLlocal1(res_v);
	long size = (long)Field(array_v, 4);
	const char *data = (const char *)Field(array_v, 3);
        res_v = caml_alloc_initialized_string(size, data);
	CAMLreturn(res_v);
}

CAMLprim value ml_array_get_string(value array_v, value pos_v, value len_v)
{
	CAMLparam1(array_v);
	CAMLlocal1(res_v);
	const char *data = (const char *)Field(array_v, 3);
	long pos = Long_val(pos_v);
	long len = Long_val(len_v);
        res_v = caml_alloc_initialized_string(len, data + pos);
	CAMLreturn(res_v);
}

CAMLprim value ml_array_set_string(value array_v, value pos_v, value string_v)
{
	CAMLparam2(array_v, string_v);
	char *data = (char *)Field(array_v, 3);
	long pos = Long_val(pos_v);
	const char *string = (const char *)String_val(string_v);
	long len = caml_string_length(string_v);
        memcpy(data + pos, string, len);
	CAMLreturn(Val_unit);
}

CAMLprim value ml_cob_resolve_cobol_and_call (value function_v)
{
	CAMLparam1( function_v );
	cob_call_union  unifunc;

	unifunc.funcvoid =
		cob_resolve_cobol (String_val(function_v), 0, 1);
        CAMLreturn( Val_int( unifunc.funcint()) );
}

/* The following line number should be offset +2 to its line number */
#line 552 "_build/default/src/ezlibcob/libcob_stubs.c"
