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

#define  COB_PACKAGE_VERSION		"4.0"
#define  COB_MODULE_FORMATTED_DATE	"Apr 20 2026 16:57:20"
#define  COB_MODULE_DATE		20260420
#define  COB_MODULE_TIME		165720

/* Module path */
static const char		*cob_module_path = NULL;

/* Call parameters */
cob_field		*cob_procedure_params[1];

int cobaux_module_init (cob_module **module_ptr,
			cob_global **globals_ptr,
			const char * const module_name,
			const char * const module_source) {
  cob_module *module;

  /* Check initialized, check module allocated, */
  /* set global pointer, */
  /* push module stack, save call parameter count */
  if (cob_module_global_enter (module_ptr, globals_ptr, 0, 0, NULL))
    return -1;

  module = *module_ptr;

  /* Set address of module parameter list */
  module->cob_procedure_params = cob_procedure_params;

  module->num_symbols = 0;
  module->module_symbols = NULL;
  
  module->module_name = module_name;
  module->module_formatted_date = COB_MODULE_FORMATTED_DATE;
  module->module_source = module_source;
  module->gc_version = COB_PACKAGE_VERSION;
  /* module->module_entry.funcptr = (void *(*)())display; */
  /* module->module_cancel.funcptr = (void *(*)())display_; */
  module->module_ref_count = NULL;
  module->module_path = &cob_module_path;
  module->module_active = 0;
  module->module_date = COB_MODULE_DATE;
  module->module_time = COB_MODULE_TIME;
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
  module->flag_no_phys_canc = 0;
  module->flag_main = 0;
  module->flag_fold_call = 0;
  module->flag_dialect = COB_DIALECT_DEFAULT;
  module->flag_file_format = COB_FILE_IS_DFLT;
  module->flag_exit_program = 0;
  module->flag_debug_trace |= 0;
  module->flag_dump_sect = 0x00;
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

  return 0;
}

void cobaux_module_enter (cob_module * const module) {
  /* Increment module active */
  module->module_active++;
}
