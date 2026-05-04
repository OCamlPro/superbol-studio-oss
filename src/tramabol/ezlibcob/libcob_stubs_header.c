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

/* each argument is a [cob_field*], prefixed by the number of args */
#define COB_VA_FIELDN                const int, ...
/* each argument is a [cob_decimal**], prefixed by the number of args */
#define COB_VA_DECIMALSN             const cob_u32_t, ...
/* each argument is a [cob_decimal*], prefixed by the number of args */
#define COB_VA_DECIMALN              const cob_u32_t, ...
/* each pair of arguments is [u32, uli] */
#define COB_VA_U32_ULI               ...
/* each argument is a [cob_file*] */
#define COB_VA_FILES                 ...
/* each argument is a [cob_file*], and later a [EXTFH_FUNC] */
#define COB_VA_FILE_EXTFH            ...
/* each argument is a [cob_field*] */
#define COB_VA_FIELDS                ...
/* arguments are specified by a format string */
#define COB_VA_FORMAT                const char *, ...
/* only one optional argument, a [void*] if present */
#define COB_VA_OPTIONAL_VOIDS       ...
/* not used actually */
#define COB_VA_DISCARDED            ...
/* these ones are too complex to handle */
#define COB_VA_FORMATTED_ARGS       ...
#define NOT_IMPLEMENTED
#include "stubs.h"

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define FIELD_MODULE_ML_MODULE           0
#define FIELD_MODULE_NEED_INIT           1
#define FIELD_MODULE_SIZE_OF             2

#define FIELD_CONTEXT_ML_CONTEXT         0
#define FIELD_CONTEXT_NEED_MODULE_INIT   1
#define FIELD_CONTEXT_SIZE_OF            2


struct ml_module {
	cob_module* module;
	const char* module_name;
	const char *cob_module_path;
};

/* This would be the equivalent of the memory allocated at every
   call to an entry point */
struct ml_context {
	struct ml_module *ml_module ;
	cob_global *cob_glob_ptr ;
	cob_field **cob_procedure_params;
	cob_decimal *cob_decimal_base ;
};

#define ML_MODULE(f_v)         ( (struct ml_module *)Field( f_v, 0) )
#define ML_CONTEXT(f_v)        ( (struct ml_context *)Field( f_v, 0) )
#define ML_COB_FIELD_ATTR(f_v) ( (cob_field_attr*)Field( f_v, 0) )
#define ML_COB_FIELD(f_v)      ( (cob_field*)Field( f_v, 0) )
#define ML_COB_FILE(f_v)       ( (cob_file*)Field( f_v, 0) )
#define ML_COB_VOIDS(f_v)       ( (void*)Field( f_v, 0) )
#define ML_BUFFER(f_v)         ( (char*)Field( f_v, 0) )
#define ML_COB_DECIMAL(v)      ( cs->cob_decimal_base + Int_val(v) )

CAMLprim value ml_cob_init (value args_v)
{
	int argc = caml_array_length (args_v);
	int i;
	char** argv = (char**)malloc( (argc+1) * sizeof(char*) );
	for (i=0; i<argc; i++){
		value s_v = Field (args_v, i);
		int len = caml_string_length (s_v);
		char *s = (char*) malloc ( len+1 );
		strncpy(s, String_val(s_v), len+1);
		argv[i]=s;
	}
	argv[argc] = NULL;
	cob_init (argc, argv);
}

/*
CAMLprim value ml_cob_stop_run (value status_v)
{
	cob_stop_run (Int_val(status_v));
	return Val_unit;
}
*/

CAMLprim value ml_cob_module_create (value module_name_v)
{
	CAMLparam0();
	CAMLlocal1(module_v);
	struct ml_module* ms =
		(struct ml_module*) calloc( sizeof(struct ml_module), 0 );

	ms->module_name = String_val (module_name_v);

	module_v = caml_alloc ( FIELD_MODULE_SIZE_OF, Abstract_tag );
	Field (module_v, FIELD_MODULE_ML_MODULE) = (value)ms;
	Field (module_v, FIELD_MODULE_NEED_INIT) = Val_true;
	CAMLreturn(module_v);
}

static void cob_program_ (int entry)
{
	exit(0);
}

static void cob_program ()
{
	cob_program_ (0);
}

CAMLprim value ml_cob_module_enter (value module_v)
{
	CAMLparam1 (module_v);
	CAMLlocal1 (context_v);
	int retcode = -1;

	struct ml_module* ms = ML_MODULE( module_v );
	struct ml_context* cs =
		(struct ml_context*) calloc( sizeof(struct ml_context), 0 );

	if (cob_module_global_enter (&ms->module,
				     &cs->cob_glob_ptr, 0, /*entry*/0, 0)){
		/* TODO check what we should free.. */
		caml_failwith ("cob_module_global_enter");
	} else {
		context_v = caml_alloc (FIELD_CONTEXT_SIZE_OF,
					Abstract_tag );
		value need_init = Field (module_v, FIELD_MODULE_NEED_INIT);
		Field (context_v, FIELD_CONTEXT_ML_CONTEXT) = (value) cs;
		Field (context_v, FIELD_CONTEXT_NEED_MODULE_INIT) = need_init;
		cs->ml_module = ms;
		cob_decimal_alloc (1, &cs->cob_decimal_base);
		cob_global *cob_glob_ptr = cs->cob_glob_ptr;
		cob_module *module = ms->module;

		if ( need_init == Val_true ){
			cob_field **cob_procedure_params = NULL; /* TODO */

			module->cob_procedure_params = cob_procedure_params;


			ms->cob_module_path = cob_glob_ptr->cob_main_argv0;

			module->module_name = ms->module_name;
			module->module_formatted_date = "";
			module->module_source = "";
			module->gc_version = "";
			module->module_entry.funcptr = (void *(*)())cob_program;
			module->module_cancel.funcptr = (void *(*)())cob_program_;
			module->module_ref_count = NULL;
			module->module_path = &ms->cob_module_path;
			module->module_active = 0;
			module->module_date = 20260419;
			module->module_time = 172921;
			module->module_type = 0;
			module->module_param_cnt = 0;
			module->ebcdic_sign = 0;
			module->decimal_point = '.';
			module->currency_symbol = '$';
			module->numeric_separator = ',';
			module->flag_filename_mapping = 1;
			module->flag_binary_truncate = 1;
			module->flag_pretty_display = 1;
			module->flag_host_sign = 0;
			module->flag_no_phys_canc = 1;
			module->flag_main = 1;
			module->flag_fold_call = 0;
			module->flag_exit_program = 0;
			module->flag_debug_trace = 0;
			module->flag_dump_ready = 0;
			module->xml_mode = 1;
			module->module_stmt = 0;
			module->module_sources = NULL;

			module->collating_sequence = NULL;
			module->crt_status = NULL;
			module->cursor_pos = NULL;
			module->xml_code = NULL;
			module->xml_event = NULL;
			module->xml_information = NULL;
			module->xml_namespace = NULL;
			module->xml_namespace_prefix = NULL;
			module->xml_nnamespace = NULL;
			module->xml_nnamespace_prefix = NULL;
			module->xml_ntext = NULL;
			module->xml_text = NULL;
			module->json_code = NULL;
			module->json_status = NULL;

			cob_set_cancel (module);
		}

		module->module_active++;
	}
	CAMLreturn( context_v );
}

CAMLprim value ml_cob_module_leave (value context_v)
{
	struct ml_context *cs = ML_CONTEXT( context_v );
	struct ml_module *ms = cs->ml_module;
	cob_module *module = ms->module;

	if (module->module_active) {
		module->module_active--;
	}
	cob_module_leave (module);

	free( cs );
	Field( context_v,0) = (value)NULL;
	return Val_unit;
}

CAMLprim value ml_cob_field_attr_create (
	value type_v,
	value digits_v,
	value scale_v,
	value flags_v,
	value pic_v
	)
{
	CAMLparam1(pic_v);
	CAMLlocal1(res_v);
	cob_field_attr *a = (cob_field_attr*)malloc (sizeof(cob_field_attr));
	a->type = Int_val( type_v );
	a->digits = Int_val( digits_v );
	a->scale = Int_val( scale_v );
	a->flags = Int_val ( flags_v );

	if (pic_v != Val_int(0) ){
		const char *pic = String_val(Field(pic_v,0));
		int len, pos;
		const char *c;
		char prev_char = pic[0];
		len = 0;
		for (c = pic; *c; c++){
			if (*c != prev_char){
				len++;
				prev_char = *c;
			}
		}
		cob_pic_symbol* picture =
			(cob_pic_symbol*)malloc ( (len+1) * sizeof(cob_pic_symbol) );
		len = 0;
		pos = 0;
		prev_char = pic[0];
		for (c = pic; *c; c++){
			if (*c != prev_char){
				picture[pos].symbol = prev_char;
				picture[pos].times_repeated = len;
				pos++;
				len=0;
				prev_char = *c;
			} else {
				len++;
			}
		}
		picture[pos].symbol = 0;
		a->pic = picture;
	} else {
		a->pic = NULL;
	}
	res_v = caml_alloc (1, Abstract_tag);
	Field (res_v, 0) = (value)a;
	CAMLreturn (res_v);
}

CAMLprim value ml_cob_buffer_create (
	value size_v
	)
{
	CAMLparam0();
	CAMLlocal1(res_v);
	int size = Int_val(size_v);
	char *b = (char*)malloc (size);
	res_v = caml_alloc (2, Abstract_tag);
	Field (res_v, 0) = (value)b;
	Field (res_v, 1) = Val_int (size);
	CAMLreturn (res_v);
}

CAMLprim value ml_cob_field_create (
	value size_v,
	value buffer_v,
	value buf_pos_v,
	value attr_v
	)
{
	CAMLparam2(buffer_v, attr_v);
	CAMLlocal1(res_v);
	int buf_len = Int_val( Field( buffer_v, 1));
	int buf_pos = Int_val( buf_pos_v );
	int size = Int_val( size_v );
	if (buf_pos+size > buf_len){
		caml_failwith ("ml_cob_field");
	}
	cob_field *f = (cob_field*)malloc (sizeof(cob_field));
	f->size = size;
	f->data = ML_BUFFER( buffer_v ) + buf_pos;
	f->attr = ML_COB_FIELD_ATTR( attr_v );
	res_v = caml_alloc (1, Abstract_tag);
	Field (res_v, 0) = (value)f;
	CAMLreturn (res_v);
}

CAMLprim value ml_cob_field_init (value f_v, value str_v)
{
	CAMLparam2( f_v, str_v );
	cob_field *f = ML_COB_FIELD( f_v );
	if (caml_string_length(str_v) < f->size)
		caml_failwith ("ml_cob_field_init");
	memcpy (f->data, String_val (str_v), f->size);
	CAMLreturn( Val_unit );
}

/*

CAMLprim value ml_cob_decimal_set_field
   (value context_v, value reg_v, value f_v)
{
	struct ml_context *cs = ML_CONTEXT( context_v );
	cob_field *f = ML_COB_FIELD( f_v );
	cob_decimal_set_field (cs->cob_decimal_base + Int_val(reg_v), f);
}

CAMLprim value ml_cob_decimal_get_field
  (value context_v, value reg_v, value f_v, value flags_v)
{
	struct ml_context *cs = ML_CONTEXT( context_v );
	return Val_int(
		cob_decimal_get_field (cs->cob_decimal_base + Int_val(reg_v),
				       ML_COB_FIELD( f_v),
				       Int_val( flags_v )) );
}

CAMLprim value ml_cob_decimal_add
   (value context_v, value reg1_v, value reg2_v)
{
	struct ml_context *cs = ML_CONTEXT( context_v );
	cob_decimal_add (cs->cob_decimal_base + Int_val(reg1_v),
			 cs->cob_decimal_base + Int_val(reg2_v));
}
*/

CAMLprim value ml_cob_memcpy
   (value src_v, value dst_v, value size_v)
{
	cob_field* src = ML_COB_FIELD(src_v);
	cob_field* dst = ML_COB_FIELD(dst_v);
	int size = Int_val( size_v );
	memcpy ( dst->data, src->data, size );
	return Val_unit;
}

CAMLprim value ml_cob_display (value to_device_v, value newline_v, value fields_v) {
  CAMLparam1 (fields_v);
  int to_device = Int_val (to_device_v);
  int newline = Bool_val (newline_v);
  int nfields = Wosize_val (fields_v);
  int i;
  for (i = 0; i < nfields; i++) {
    cob_display (to_device, i == nfields - 1 && newline, 1,
		 ML_COB_FIELD (Field (fields_v,i)));
  }
}	

CAMLprim value ml_cob_module_free (value module_v)
{
	free( ML_MODULE(module_v) );
	Field( module_v,0) = (value)NULL;
	return Val_unit;
}

CAMLprim value ml_cob_buffer_free (value buffer_v)
{
	free( ML_BUFFER(buffer_v) );
	Field( buffer_v,0) = (value)NULL;
	return Val_unit;
}

CAMLprim value ml_cob_field_attr_free (value field_v)
{
	free( ML_COB_FIELD_ATTR(field_v) );
	Field( field_v,0) = (value)NULL;
	return Val_unit;
}

CAMLprim value ml_cob_field_free (value field_v)
{
	free( ML_COB_FIELD(field_v) );
	Field( field_v,0) = (value)NULL;
	return Val_unit;
}

CAMLprim value ml_cob_resolve_cobol_and_call (value function_v)
{
	CAMLparam1( function_v );
	cob_call_union  unifunc;

	unifunc.funcvoid =
		cob_resolve_cobol (String_val(function_v), 0, 1);
        CAMLreturn( Val_int( unifunc.funcint()) );
}

#include <caml/callback.h>

void ml_cob_introspect_fields(void)
{
	int i;
	cob_global *cobglobptr = cob_get_global_ptr ();
	cob_module* module = cobglobptr->cob_current_module;
	static const value * closure_f = NULL;
	if (closure_f == NULL)
		closure_f = caml_named_value("ml_introspect_field");
	for (i=0; i<cobglobptr->cob_call_params; i++) {
		value v = caml_alloc (1, Abstract_tag);
		Field (v,0) = (value) module->cob_procedure_params[i];
		value res = caml_callback(*closure_f, v);
	}
}

CAMLprim value ml_cob_field_get_size (value field_v)
{
	cob_field* f = ML_COB_FIELD(field_v);
	return Val_int( f -> size);
}

CAMLprim value ml_cob_field_set_buffer (value field_v, value buffer_v, value pos_v)
{
	cob_field* f = ML_COB_FIELD( field_v );
	char *buf = ML_BUFFER( buffer_v );
	int pos = Int_val( pos_v ) ;
	f->data = buf + pos;
	return Val_unit;
}

CAMLprim value ml_cob_subfield_set_pos (value subfield_v, value field_v, value pos_v)
{
	cob_field* subf = ML_COB_FIELD(subfield_v);
	cob_field* f = ML_COB_FIELD(field_v);
	int pos = Int_val( pos_v ) ;
	subf->data = (char*)(f->data) + pos;
	return Val_unit;
}

CAMLprim value ml_cob_field_get_buffer (value field_v)
{
	CAMLparam1( field_v );
	CAMLlocal1( res_v );
	cob_field* f = ML_COB_FIELD(field_v);
	res_v = caml_alloc(2, Abstract_tag);
	Field( res_v, 0) = (value)f->data;
	Field( res_v, 1) = Val_int(f->size);
	CAMLreturn( res_v );
}

CAMLprim value ml_cob_field_get_attr (value field_v)
{
	CAMLparam1( field_v );
	CAMLlocal1( res_v );
	cob_field* f = ML_COB_FIELD(field_v);
	res_v = caml_alloc(1, Abstract_tag);
	Field( res_v, 0) = (value)f->attr;
	CAMLreturn( res_v );
}

CAMLprim value ml_cob_buffer_get_addr (value buffer_v)
{
	return Val_long(Field(buffer_v,0));
}

CAMLprim value ml_cob_buffer_get_string (value buffer_v)
{
	CAMLparam1( buffer_v );
	CAMLlocal1( res_v );
	int size = Int_val(Field(buffer_v,1));
	res_v = caml_alloc_string(size);
	memcpy( (void*)res_v, (void*)Field(buffer_v,0), size);
	CAMLreturn( res_v );
}

CAMLprim value ml_cob_buffer_get_substring (value buffer_v,
					    value pos_v, value len_v)
{
	CAMLparam1( buffer_v );
	CAMLlocal1( res_v );
	int pos = Int_val( pos_v );
	int len = Int_val( len_v );
	int size = Int_val(Field(buffer_v,1));
	if (pos+len > size)
		caml_failwith("ml_cob_buffer_get_substring");
	res_v = caml_alloc_string(len);
	memcpy( (void*)res_v,
		(void*)(Field(buffer_v,0)+pos), len);
	CAMLreturn( res_v );
}

CAMLprim value ml_cob_field_attr_get_ty (value field_attr_v)
{
	cob_field_attr* f = ML_COB_FIELD_ATTR(field_attr_v);
	return Val_int( f -> type);
}

CAMLprim value ml_cob_field_attr_get_digits (value field_attr_v)
{
	cob_field_attr* f = ML_COB_FIELD_ATTR(field_attr_v);
	return Val_int( f -> digits);
}

CAMLprim value ml_cob_field_attr_get_scale (value field_attr_v)
{
	cob_field_attr* f = ML_COB_FIELD_ATTR(field_attr_v);
	return Val_int( f -> scale);
}

CAMLprim value ml_cob_field_attr_get_flags (value field_attr_v)
{
	cob_field_attr* f = ML_COB_FIELD_ATTR(field_attr_v);
	return Val_int( f -> flags);
}

CAMLprim value ml_cob_field_attr_get_pic (value field_attr_v)
{
	CAMLparam1( field_attr_v );
	CAMLlocal2( res_v, pic_v );
	cob_field_attr* f = ML_COB_FIELD_ATTR(field_attr_v);
	if ( f->pic != NULL ){
		int len = 0;
		int i;
		for(i = 0; f->pic[i].symbol; i++)
			len += f->pic[i].times_repeated;
		pic_v = caml_alloc_string(len);
		char *cursor = (char*)pic_v;
		for(i = 0; f->pic[i].symbol; i++){
			char c = f->pic[i].symbol;
			for(int j=f->pic[i].times_repeated; j>0; j++){
				*cursor++ = c;
			}
		}
		res_v = caml_alloc (1,0);
		Field (res_v, 0) = pic_v ;
	} else {
		res_v = Val_none ;
	}
	CAMLreturn( res_v );
}

/*
CAMLprim value ml_cob_field_get_buffer (value field_v)
{
	CAMLparam1( field_v );
	CAMLlocal1( res_v );
	cob_field* f = ML_COB_FIELD(field_v);
	res_v = caml_alloc(2, Abstract_tag);
	Field( res_v, 0) = (value)f->data;
	Field( res_v, 1) = Val_int(f->size);
	CAMLreturn( res_v );
}
*/

value ml_cob_call_COB_VA_FIELDN (cob_field* (*f)(COB_VA_FIELDN), value fields_v)
{
	CAMLparam1( fields_v );
	CAMLlocal1( res_v );
	cob_field* res;
	int nfields = Wosize_val( fields_v );

	switch (nfields){
	case 0:
		res = f(nfields); break;
	case 1:
		res = f(nfields
			,ML_COB_FIELD(Field(fields_v,0))
		); break;
	case 2: res = f (nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
		); break;
	case 3: res = f (nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
		); break;
	case 4: res = f (nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
		); break;
	case 5: res = f (nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
		); break;
	case 6: res = f (nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
			    ,ML_COB_FIELD(Field(fields_v,5))
		); break;
	case 7: res = f (nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
			    ,ML_COB_FIELD(Field(fields_v,5))
			    ,ML_COB_FIELD(Field(fields_v,6))
		); break;
	case 8: res = f (nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
			    ,ML_COB_FIELD(Field(fields_v,5))
			    ,ML_COB_FIELD(Field(fields_v,6))
			    ,ML_COB_FIELD(Field(fields_v,7))
		); break;
	default: caml_failwith ("COB_VA_FIELDN with nargs > 8");
	}

	res_v = caml_alloc (1, Abstract_tag);
	Field (res_v, 0) = (value) res ;
	CAMLreturn (res_v);
}

value ml_cob_call_int2_COB_VA_FIELDN (cob_field* (*f)(const int, const int, COB_VA_FIELDN), value arg0_v, value arg1_v, value fields_v)
{
	CAMLparam1( fields_v );
	CAMLlocal1( res_v );
	const int arg0 = Long_val( arg0_v );
	const int arg1 = Long_val( arg1_v );
	cob_field* res;
	int nfields = Wosize_val( fields_v );

	switch (nfields){
	case 0:
		res = f (arg0, arg1, nfields); break;
	case 1:
		res = f (arg0, arg1, nfields
			,ML_COB_FIELD(Field(fields_v,0))
		); break;
	case 2: res = f (arg0, arg1, nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
		); break;
	case 3: res = f (arg0, arg1, nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
		); break;
	case 4: res = f (arg0, arg1, nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
		); break;
	case 5: res = f (arg0, arg1, nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
		); break;
	case 6: res = f (arg0, arg1, nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
			    ,ML_COB_FIELD(Field(fields_v,5))
		); break;
	case 7: res = f (arg0, arg1, nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
			    ,ML_COB_FIELD(Field(fields_v,5))
			    ,ML_COB_FIELD(Field(fields_v,6))
		); break;
	case 8: res = f (arg0, arg1, nfields
			    ,ML_COB_FIELD(Field(fields_v,0))
			    ,ML_COB_FIELD(Field(fields_v,1))
			    ,ML_COB_FIELD(Field(fields_v,2))
			    ,ML_COB_FIELD(Field(fields_v,3))
			    ,ML_COB_FIELD(Field(fields_v,4))
			    ,ML_COB_FIELD(Field(fields_v,5))
			    ,ML_COB_FIELD(Field(fields_v,6))
			    ,ML_COB_FIELD(Field(fields_v,7))
		); break;
	default: caml_failwith ("COB_VA_FIELDN with nargs > 8");
	}

	res_v = caml_alloc (1, Abstract_tag);
	Field (res_v, 0) = (value) res ;
	CAMLreturn (res_v);
}

/* The following line number should be offset +2 to its line number */
#line 742 "_build/default/src/ezlibcob/libcob_stubs.c"
