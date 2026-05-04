// (* Functions in common.c *)

COB_EXPIMP const char*	cob_get_sig_name (int);
COB_EXPIMP const char*	cob_get_sig_description (int);
COB_EXPIMP void		print_info	(void);
COB_EXPIMP void		print_info_detailed	(const int);
COB_EXPIMP int		cob_load_config	(void);
COB_EXPIMP void		print_runtime_conf	(void);

COB_EXPIMP void		cob_set_exception	(const int);
COB_EXPIMP int		cob_last_exception_is	(const int);

COB_EXPIMP int		cob_last_exit_code	(void);
COB_EXPIMP const char*	cob_last_runtime_error	(void);

NOT_IMPLEMENTED
COB_EXPIMP void		cob_runtime_hint	(COB_VA_FORMAT) COB_A_FORMAT12;
NOT_IMPLEMENTED
COB_EXPIMP void		cob_runtime_error	(COB_VA_FORMAT) COB_A_FORMAT12;
NOT_IMPLEMENTED
COB_EXPIMP void		cob_runtime_warning	(COB_VA_FORMAT) COB_A_FORMAT12;

COB_EXPIMP void		cob_cleanup_thread (void);

// (* General functions *)

COB_EXPIMP int		cob_is_initialized	(void);
NOT_IMPLEMENTED
COB_EXPIMP cob_global		*cob_get_global_ptr	(void);

NOT_IMPLEMENTED
COB_EXPIMP void	cob_init			(const int, char ** );
NOT_IMPLEMENTED
COB_EXPIMP void	cob_init_nomain		(const int, char ** );
COB_EXPIMP void	cob_common_init		(void * );

NOT_IMPLEMENTED
COB_EXPIMP int	cob_module_global_enter	(cob_module **, cob_global **,
						 const int, const int, const unsigned int * );
NOT_IMPLEMENTED
COB_EXPIMP void	cob_module_enter		(cob_module **, cob_global **,
						 const int);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_module_leave		(cob_module * );

NOT_IMPLEMENTED
COB_EXPIMP void	cob_module_free	(cob_module ** );

DECLNORET COB_EXPIMP void	cob_stop_run	(const int) COB_A_NORETURN;
DECLNORET COB_EXPIMP void	cob_stop_error	(void) COB_A_NORETURN;
NOT_IMPLEMENTED
DECLNORET COB_EXPIMP void	cob_fatal_error	(const enum cob_fatal_error) COB_A_NORETURN;
DECLNORET COB_EXPIMP void	cob_hard_failure_internal (const char * ) COB_A_NORETURN;

COB_EXPIMP void	*cob_malloc			(const size_t) COB_A_MALLOC;
COB_EXPIMP void	*cob_realloc			(void *, const size_t, const size_t) COB_A_MALLOC;
COB_EXPIMP char	*cob_strdup				(const char * );
COB_EXPIMP void	cob_free			(void * );
COB_EXPIMP void	*cob_fast_malloc		(const size_t) COB_A_MALLOC;
COB_EXPIMP void	*cob_cache_malloc		(const size_t) COB_A_MALLOC;
COB_EXPIMP void	*cob_cache_realloc		(void *, const size_t);
COB_EXPIMP void	cob_cache_free			(void * );

COB_EXPIMP void	cob_set_locale			(cob_field *, const int);

COB_EXPIMP int 	cob_setenv		(const char *, const char *, int);
COB_EXPIMP int 	cob_unsetenv		(const char * );
COB_EXPIMP char	*cob_getenv_direct		(const char * );
COB_EXPIMP char *cob_expand_env_string	(const char * );
COB_EXPIMP char	*cob_getenv			(const char * );
COB_EXPIMP int	cob_putenv			(char * );

COB_EXPIMP void	cob_check_version		(const char *, const char *,
						 const int);

NOT_IMPLEMENTED
COB_EXPIMP struct cob_func_loc *cob_save_func	(cob_field **,
									const int, COB_VA_FIELDN);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_restore_func		(struct cob_func_loc * );

COB_EXPIMP void	cob_accept_arg_number		(cob_field * );
COB_EXPIMP void	cob_accept_arg_value		(cob_field * );
COB_EXPIMP void	cob_accept_command_line		(cob_field * );
COB_EXPIMP void	cob_accept_date			(cob_field * );
COB_EXPIMP void	cob_accept_date_yyyymmdd	(cob_field * );
COB_EXPIMP void	cob_accept_day			(cob_field * );
COB_EXPIMP void	cob_accept_day_yyyyddd		(cob_field * );
COB_EXPIMP void	cob_accept_day_of_week		(cob_field * );
COB_EXPIMP void	cob_accept_environment		(cob_field * );
COB_EXPIMP void	cob_accept_exception_status	(cob_field * );
COB_EXPIMP void	cob_accept_time			(cob_field * );
COB_EXPIMP void	cob_accept_microsecond_time (cob_field * );
COB_EXPIMP void	cob_accept_user_name		(cob_field * );
COB_EXPIMP void	cob_display_command_line	(cob_field * );
COB_EXPIMP void	cob_display_environment		(const cob_field * );
COB_EXPIMP void	cob_display_env_value		(const cob_field * );
COB_EXPIMP void	cob_display_arg_number		(cob_field * );
COB_EXPIMP void	cob_get_environment		(const cob_field *, cob_field * );
COB_EXPIMP void	cob_set_environment		(const cob_field *,
						 const cob_field * );
COB_EXPIMP void	cob_chain_setup			(void *, const size_t,
						 const size_t);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_allocate			(unsigned char **, cob_field *,
						 cob_field *, cob_field * );
NOT_IMPLEMENTED
COB_EXPIMP void	cob_free_alloc			(unsigned char **, unsigned char * );
COB_EXPIMP void	cob_continue_after		(cob_field * );
COB_EXPIMP int	cob_extern_init			(void);
COB_EXPIMP int	cob_tidy			(void);
NOT_IMPLEMENTED
COB_EXPIMP char	*cob_command_line		(int, int *, char ***,
						 char ***, char ** );

COB_EXPIMP void	cob_incr_temp_iteration 	(void);
COB_EXPIMP void	cob_temp_name			(char *, const char * );

// (* System routines *)
COB_EXPIMP int	cob_sys_exit_proc	(const void *, const void * );
COB_EXPIMP int	cob_sys_error_proc	(const void *, const void * );
COB_EXPIMP int	cob_sys_runtime_error_proc (const void *, const void * );
COB_EXPIMP int	cob_sys_system		(const void * );
COB_EXPIMP int	cob_sys_hosted		(void *, const void * );
COB_EXPIMP int	cob_sys_and		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_or		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_nor		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_xor		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_imp		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_nimp		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_eq		(const void *, void *, const int);
COB_EXPIMP int	cob_sys_not		(void *, const int);
COB_EXPIMP int	cob_sys_xf4		(void *, const void * );
COB_EXPIMP int	cob_sys_xf5		(const void *, void * );
COB_EXPIMP int	cob_sys_x91		(void *, const void *, void * );
COB_EXPIMP int	cob_sys_toupper		(void *, const int);
COB_EXPIMP int	cob_sys_tolower		(void *, const int);
COB_EXPIMP int	cob_sys_oc_nanosleep	(const void * );
COB_EXPIMP int	cob_sys_getpid		(void);
COB_EXPIMP int	cob_sys_return_args	(void * );
COB_EXPIMP int	cob_sys_parameter_size	(void * );
COB_EXPIMP int	cob_sys_fork	(void);
COB_EXPIMP int	cob_sys_waitpid	(const void * );

// (* cob_sys_getopt_long_long *)
COB_EXPIMP int	cob_sys_getopt_long_long	(void*, void*, void*, const int, void*, void* );


COB_EXPIMP int	cob_sys_sleep		(const void * );
COB_EXPIMP int	cob_sys_calledby	(void * );
NOT_IMPLEMENTED
COB_EXPIMP int	cob_sys_justify		(void *, COB_VA_OPTIONAL_VOIDS);
NOT_IMPLEMENTED
COB_EXPIMP int	cob_sys_printable	(void *, COB_VA_OPTIONAL_VOIDS);

COB_EXPIMP int	cob_sys_extfh		(const void *, void * );

// (* Utilities *)

COB_EXPIMP void	cob_trace_sect		(const char *name);
COB_EXPIMP void	cob_trace_para		(const char *name);
COB_EXPIMP void	cob_trace_entry		(const char *name);
COB_EXPIMP void	cob_trace_exit		(const char *name);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_trace_statement		(const enum cob_statement);	// (* 3.2 + *)

// (* compatibility functions up to GnuCOBOL before 3.2 *)

COB_EXPIMP void			*cob_external_addr	(const char *, const int);
NOT_IMPLEMENTED
COB_EXPIMP unsigned char	*cob_get_pointer	(const void * );
COB_EXPIMP void			cob_ready_trace		(void);
COB_EXPIMP void			cob_reset_trace		(void);
COB_EXPIMP void			cob_nop (void);


NOT_IMPLEMENTED
COB_EXPIMP void			cob_set_runtime_option		(enum cob_runtime_option_switch opt, void *p);
NOT_IMPLEMENTED
COB_EXPIMP void			*cob_get_runtime_option		(enum cob_runtime_option_switch opt);

COB_EXPIMP void			cob_stack_trace (void *target);		// (* 'target' is FILE *  *)
COB_EXPIMP void			cob_backtrace	(void *target, int count);		// (* 'target' is FILE *  *)

// (* note: these are internal (cobc/libcob -> libcob) only functions *)
NOT_IMPLEMENTED
COB_EXPIMP struct cob_time cob_get_current_date_and_time	(void);	// (* returning datetime without nanos *)
NOT_IMPLEMENTED
COB_EXPIMP int cob_set_date_from_epoch		(struct cob_time *, unsigned const char * );

// (* Registration of external handlers *)
// (* COB_EXPIMP void	cob_reg_sighnd	(void ( *sighnd) (int)); *)

// (* Raise signal (run both internal and external handlers) *)
COB_EXPIMP void	cob_raise		(int);

// (* Switch *)

COB_EXPIMP int	cob_get_switch		(const int);
COB_EXPIMP void	cob_set_switch		(const int, const int);

// (* Comparison *)

COB_EXPIMP int	cob_cmp			(cob_field *, cob_field * );

// (* Class check *)

COB_EXPIMP int	cob_is_omitted		(const cob_field * );
COB_EXPIMP int	cob_is_numeric		(const cob_field * );
COB_EXPIMP int	cob_is_alpha		(const cob_field * );
COB_EXPIMP int	cob_is_upper		(const cob_field * );
COB_EXPIMP int	cob_is_lower		(const cob_field * );

// (* Table sort *)

COB_EXPIMP void	cob_table_sort_init	(const size_t, const unsigned char * );
COB_EXPIMP void	cob_table_sort_init_key	(cob_field *, const int,
					 const unsigned int);
COB_EXPIMP void	cob_table_sort		(cob_field *, const int);

// (* Run-time error checking *)

COB_EXPIMP void	cob_check_numeric	(const cob_field *, const char * );
COB_EXPIMP void	cob_correct_numeric	(cob_field * );
COB_EXPIMP void	cob_check_based		(const unsigned char *, const char * );
COB_EXPIMP void	cob_check_linkage	(const unsigned char *, const char *);
COB_EXPIMP void	cob_check_odo		(const int, const int, const int,
					 const char *, const char * );
COB_EXPIMP void	cob_check_subscript	(const int, const int,
					 const char *, const int);
COB_EXPIMP void	cob_check_ref_mod_detailed	(const char *, const int, const int,
					 const int, const int, const int);
COB_EXPIMP void	cob_check_ref_mod_minimal	(const char *,
					 const int, const int);
COB_EXPIMP void	cob_check_beyond_exit (const char *);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_check_fence 	(const char *, const char *,
					 const enum cob_statement, const char * );

// (* Comparison functions *)
COB_EXPIMP int	cob_numeric_cmp		(cob_field *, cob_field *);
COB_EXPIMP int	cob_bcd_cmp		(cob_field *, cob_field *);
COB_EXPIMP int	cob_numeric_display_cmp		(cob_field *, cob_field *);
COB_EXPIMP int	cob_numeric_display_cmp_zero	(cob_field *);
COB_EXPIMP int	cob_bcd_cmp_zero	(cob_field *);


// (* Functions in strings.c *)

COB_EXPIMP void cob_inspect_init	(cob_field *, const cob_u32_t);
COB_EXPIMP void cob_inspect_init_converting	(cob_field *);
COB_EXPIMP void cob_inspect_start	(void);
COB_EXPIMP void cob_inspect_before	(const cob_field *);
COB_EXPIMP void cob_inspect_after	(const cob_field *);
COB_EXPIMP void cob_inspect_characters	(cob_field *);
COB_EXPIMP void cob_inspect_all		(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_leading	(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_first	(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_trailing	(cob_field *, cob_field *);
COB_EXPIMP void cob_inspect_converting	(const cob_field *, const cob_field *);
COB_EXPIMP void cob_inspect_translating (const unsigned char *);
COB_EXPIMP void cob_inspect_finish	(void);

COB_EXPIMP void cob_string_init		(cob_field *, cob_field *);
COB_EXPIMP void cob_string_delimited	(cob_field *);
COB_EXPIMP void cob_string_append	(cob_field *);
COB_EXPIMP void cob_string_finish	(void);


// (*   Functions in move.c       *)


COB_EXPIMP void		cob_move	(cob_field *, cob_field *);
COB_EXPIMP void		cob_move_ibm	(void *, void *, const int);
COB_EXPIMP void		cob_init_table	(void *, const size_t, const size_t);
COB_EXPIMP void		cob_set_int	(cob_field *, const int);
COB_EXPIMP int		cob_get_int	(cob_field *);
COB_EXPIMP void		cob_set_llint   (cob_field *, cob_s64_t, const cob_s64_t);
COB_EXPIMP void		cob_set_llcon   (cob_field *, const cob_s64_t);
COB_EXPIMP void		cob_set_compx   (cob_field *, const cob_s64_t);
COB_EXPIMP cob_s64_t	cob_get_llint	(cob_field *);
COB_EXPIMP void		cob_alloc_move(cob_field *, cob_field *, const int);

// (* Functions in move.c for C access to COBOL data - GnuCOBOL COBOL-C-API *)

COB_EXPIMP char *	cob_get_picx( void *cbldata, size_t len, void *charfld, size_t charlen);
COB_EXPIMP cob_s64_t	cob_get_s64_comp3(void *cbldata, int len);
COB_EXPIMP cob_s64_t	cob_get_s64_comp5(void *cbldata, int len);
COB_EXPIMP cob_s64_t	cob_get_s64_compx(void *cbldata, int len);
COB_EXPIMP cob_s64_t	cob_get_s64_pic9 (void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_comp3(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_comp5(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_comp6(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_compx(void *cbldata, int len);
COB_EXPIMP cob_u64_t	cob_get_u64_pic9 (void *cbldata, int len);
NOT_IMPLEMENTED
COB_EXPIMP float 	cob_get_comp1(void *cbldata);
NOT_IMPLEMENTED
COB_EXPIMP double	cob_get_comp2(void *cbldata);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_put_comp1(float val, void *cbldata);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_put_comp2(double val, void *cbldata);
COB_EXPIMP void 	cob_put_picx( void *cbldata, size_t len, void *string);
COB_EXPIMP void		cob_put_s64_comp3(cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_s64_comp5(cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_s64_compx(cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_s64_pic9 (cob_s64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_comp3(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_comp5(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_comp6(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_compx(cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_u64_pic9 (cob_u64_t val, void *cbldata, int len);
COB_EXPIMP void		cob_put_pointer(void *val, void *cbldata);


// (* Functions in numeric.c *)


COB_EXPIMP void	cob_decimal_init	(cob_decimal * );
COB_EXPIMP void	cob_decimal_clear	(cob_decimal * );
COB_EXPIMP void cob_decimal_set_llint	(cob_decimal *, const cob_s64_t);
COB_EXPIMP void cob_decimal_set_ullint	(cob_decimal *, const cob_u64_t);
COB_EXPIMP void	cob_decimal_set_field	(cob_decimal *, cob_field * );
COB_EXPIMP int	cob_decimal_get_field	(cob_decimal *, cob_field *, const int);
COB_EXPIMP void	cob_decimal_set		(cob_decimal *, cob_decimal * );	// (* to be removed in 4.x *)
COB_EXPIMP void	cob_decimal_add		(cob_decimal *, cob_decimal * );
COB_EXPIMP void	cob_decimal_sub		(cob_decimal *, cob_decimal * );
COB_EXPIMP void	cob_decimal_mul		(cob_decimal *, cob_decimal * );
COB_EXPIMP void	cob_decimal_div		(cob_decimal *, cob_decimal * );
COB_EXPIMP void	cob_decimal_pow		(cob_decimal *, cob_decimal * );
COB_EXPIMP int	cob_decimal_cmp		(cob_decimal *, cob_decimal * );
COB_EXPIMP void	cob_decimal_align(cob_decimal *, const int);
COB_EXPIMP void cob_logical_not (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_and (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_xor (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_or  (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_left  (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_right (cob_decimal *d0, cob_decimal *d1);
COB_EXPIMP void cob_logical_left_c  (cob_decimal *d0, cob_decimal *d1, int sz);
COB_EXPIMP void cob_logical_right_c (cob_decimal *d0, cob_decimal *d1, int sz);

COB_EXPIMP void	cob_add			(cob_field *, cob_field *, const int);
COB_EXPIMP void	cob_sub			(cob_field *, cob_field *, const int);
COB_EXPIMP void	cob_mul			(cob_field *, cob_field *, const int);
COB_EXPIMP void	cob_div			(cob_field *, cob_field *, const int);
COB_EXPIMP int	cob_add_int		(cob_field *, const int, const int);
COB_EXPIMP int	cob_sub_int		(cob_field *, const int, const int);
COB_EXPIMP void	cob_div_quotient	(cob_field *, cob_field *,
					 cob_field *, const int);
COB_EXPIMP void	cob_div_remainder	(cob_field *, const int);

COB_EXPIMP int	cob_cmp_int		(cob_field *, const int);
COB_EXPIMP int	cob_cmp_uint		(cob_field *, const unsigned int);
COB_EXPIMP int	cob_cmp_llint		(cob_field *, const cob_s64_t);
COB_EXPIMP int	cob_cmp_packed		(cob_field *, const cob_s64_t);
COB_EXPIMP int	cob_cmp_numdisp		(const unsigned char *,
					 const size_t, const cob_s64_t,
					 const cob_u32_t);
COB_EXPIMP int	cob_cmp_float		(cob_field *, cob_field * );
COB_EXPIMP void	cob_set_packed_zero	(cob_field * );
COB_EXPIMP void	cob_set_packed_int	(cob_field *, const int);

COB_EXPIMP void	cob_move_bcd		(cob_field *, cob_field * );

NOT_IMPLEMENTED
COB_EXPIMP void	cob_decimal_alloc	(COB_VA_DECIMALSN);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_decimal_push	(COB_VA_DECIMALSN);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_decimal_pop		(COB_VA_DECIMALN);

COB_EXPIMP void	cob_gmp_free		(void * );

COB_EXPIMP cob_s32_t	cob_s32_pow	(cob_s32_t, cob_s32_t);
COB_EXPIMP cob_s64_t	cob_s64_pow (cob_s64_t, cob_s64_t);


// (* Functions in call.c *)

DECLNORET COB_EXPIMP void	cob_call_error		(void) COB_A_NORETURN;
NOT_IMPLEMENTED
COB_EXPIMP void		cob_field_constant (cob_field *f, cob_field *t, cob_field_attr *a, void *d);

NOT_IMPLEMENTED
COB_EXPIMP void		cob_set_cancel		(cob_module * );
NOT_IMPLEMENTED
COB_EXPIMP int		cob_encode_program_id (const unsigned char * const, unsigned char * const,
						 const int, const int);
COB_EXPIMP void		*cob_resolve		(const char * );
COB_EXPIMP void		*cob_resolve_cobol	(const char *, const int,
						 const int);
COB_EXPIMP void		*cob_resolve_func	(const char * );
COB_EXPIMP const char	*cob_resolve_error	(void);
NOT_IMPLEMENTED
COB_EXPIMP void		*cob_call_field		(const cob_field *,
						 const struct cob_call_struct *,
						 const unsigned int,
						 const int);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_cancel_field	(const cob_field *,
						 const struct cob_call_struct * );
COB_EXPIMP void		cob_cancel		(const char * );
NOT_IMPLEMENTED
COB_EXPIMP int		cob_call_with_exception_check (const char*, const int, void ** );
NOT_IMPLEMENTED
COB_EXPIMP int		cob_call		(const char *, const int, void ** );
NOT_IMPLEMENTED
COB_EXPIMP int		cob_func		(const char *, const int, void ** );


// (* Functions in move.c for C access to COBOL data - GnuCOBOL COBOL-C-API *)

COB_EXPIMP int		cob_get_num_params ( void );
COB_EXPIMP int		cob_get_param_constant ( int num_param );
COB_EXPIMP int		cob_get_param_digits( int num_param );
COB_EXPIMP int		cob_get_param_scale( int num_param );
COB_EXPIMP int		cob_get_param_sign ( int num_param );
COB_EXPIMP int		cob_get_param_size ( int num_param );
COB_EXPIMP int		cob_get_param_type ( int num_param );
COB_EXPIMP void *	cob_get_param_data ( int num_param );
COB_EXPIMP cob_s64_t	cob_get_s64_param  ( int num_param );
COB_EXPIMP cob_u64_t	cob_get_u64_param  ( int num_param );
NOT_IMPLEMENTED
COB_EXPIMP double	cob_get_dbl_param  ( int num_param );
NOT_IMPLEMENTED
COB_EXPIMP void		cob_put_dbl_param  ( int num_param, double value );
COB_EXPIMP void		cob_put_s64_param  ( int num_param, cob_s64_t value );
COB_EXPIMP void		cob_put_u64_param  ( int num_param, cob_u64_t value );
COB_EXPIMP char *	cob_get_picx_param ( int num_param, void *charfld, size_t charlen );
COB_EXPIMP void *	cob_get_grp_param  ( int num_param, void *charfld, size_t charlen );
COB_EXPIMP void 	cob_put_picx_param ( int num_param, void *charfld );
COB_EXPIMP void  	cob_put_grp_param  ( int num_param, void *charfld, size_t charlen );

COB_EXPIMP const char	*cob_get_param_str ( int num_param, char *buff, size_t size);
COB_EXPIMP const char	*cob_get_param_str_buffered ( int num_param );
COB_EXPIMP int		cob_put_param_str ( int num_param, const char *src );

NOT_IMPLEMENTED
COB_EXPIMP void		cob_runtime_warning_external	(const char *, const int,
						COB_VA_FORMAT) COB_A_FORMAT34;

// (* get access to one of the fields (to only operate with libcob functions on it!) *)
COB_EXPIMP cob_field	*cob_get_param_field (int n, const char *caller_name);
COB_EXPIMP int		cob_get_field_size (const cob_field * );
COB_EXPIMP int		cob_get_field_type (const cob_field * );
COB_EXPIMP int		cob_get_field_digits	(const cob_field * );
COB_EXPIMP int		cob_get_field_scale	(const cob_field * );
COB_EXPIMP int		cob_get_field_sign	(const cob_field * );
COB_EXPIMP int		cob_get_field_constant (const cob_field * );
COB_EXPIMP const char	*explain_field_type (const cob_field * );

// (* get the field's pretty-display value *)
COB_EXPIMP const char	*cob_get_field_str (const cob_field *, char *buff, size_t size);
// (* get the field's pretty-display value with an internal buffer for one-time access *)
COB_EXPIMP const char	*cob_get_field_str_buffered (const cob_field * );
// (* set the field's data using the appropriate internal type, returns EINVAL if data is invalid *)
COB_EXPIMP int		cob_put_field_str (const cob_field *, const char * );


// (* Functions in screenio.c *)

COB_EXPIMP void		cob_screen_line_col	(cob_field *, const int);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_screen_display	(cob_screen *, cob_field *,
					 cob_field *, const int);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_screen_accept	(cob_screen *, cob_field *,
					 cob_field *, cob_field *,
					 const int);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_accept_field	(cob_field *, const cob_flags_t, const char *, COB_VA_FIELDS);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_display_field	(cob_field *, const cob_flags_t, const char *, COB_VA_FIELDS);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_field_display	(cob_field *, cob_field *, cob_field *,
					 cob_field *, cob_field *, cob_field *,
					 cob_field *, const cob_flags_t);
NOT_IMPLEMENTED
COB_EXPIMP void		cob_field_accept	(cob_field *, cob_field *, cob_field *,
					 cob_field *, cob_field *, cob_field *,
					 cob_field *, cob_field *, cob_field *,
					 const cob_flags_t);
COB_EXPIMP int		cob_display_text (const char * );

NOT_IMPLEMENTED
COB_EXPIMP int		cob_display_formatted_text (COB_VA_FORMAT);
COB_EXPIMP int		cob_get_char	(void);
COB_EXPIMP void		cob_set_cursor_pos	(int, int);
COB_EXPIMP void		cob_accept_escape_key	(cob_field * );
COB_EXPIMP int		cob_sys_clear_screen	(void);
COB_EXPIMP int		cob_sys_sound_bell	(void);
COB_EXPIMP int		cob_sys_get_scr_size	(unsigned char *, unsigned char * );
COB_EXPIMP int		cob_sys_set_scr_size	(unsigned char *, unsigned char * );
COB_EXPIMP int		cob_sys_scr_dump	(unsigned char * );
COB_EXPIMP int		cob_sys_scr_restore	(unsigned char * );
COB_EXPIMP int		cob_sys_window		(unsigned char*, unsigned char * );
COB_EXPIMP int		cob_sys_get_char	(unsigned char * );
COB_EXPIMP int		cob_get_text		(char *, int);
COB_EXPIMP int		cob_get_scr_cols	(void);
COB_EXPIMP int		cob_get_scr_lines	(void);
COB_EXPIMP int		cob_sys_get_csr_pos	(unsigned char * );
COB_EXPIMP int		cob_sys_set_csr_pos	(unsigned char * );
COB_EXPIMP int		cob_sys_open_vfile	(unsigned char *, unsigned char * );
COB_EXPIMP int		cob_sys_read_vfile	(cob_u16_t, cob_u32_t, cob_u32_t, unsigned char* );
COB_EXPIMP int		cob_sys_write_vfile	(cob_u16_t, cob_u32_t, cob_u32_t, unsigned char* );
COB_EXPIMP int		cob_sys_close_vfile	(cob_u16_t);
COB_EXPIMP int		cob_sys_open_vfile2	(unsigned char *, unsigned char * );
COB_EXPIMP int		cob_sys_read_vfile2	(cob_u16_t, cob_u64_t, cob_u32_t, unsigned char* );
COB_EXPIMP int		cob_sys_write_vfile2	(cob_u16_t, cob_u64_t, cob_u32_t, unsigned char* );



// (* Functions in termio.c *)

NOT_IMPLEMENTED
COB_EXPIMP void cob_display	(const int, const int, COB_VA_FIELDN);
COB_EXPIMP void cob_dump_output (const char * );
COB_EXPIMP void cob_dump_file (const char *, cob_file * );

// (* it looks like this function is useless *)
NOT_IMPLEMENTED
COB_EXPIMP void cob_dump_field	(const int, const char *, cob_field *, const cob_uli_t, const cob_u32_t, ...);
COB_EXPIMP void cob_accept	(cob_field * );

// (* Functions in fileio.c *)

NOT_IMPLEMENTED
COB_EXPIMP void	cob_file_external_addr (const char *,
				 cob_file **, cob_file_key **,
				 const int nkeys, const int linage);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_file_malloc (cob_file **, cob_file_key **,
				 const int nkeys, const int linage);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_file_free   (cob_file **, cob_file_key ** );
COB_EXPIMP void cob_commit		(void);
COB_EXPIMP void cob_rollback	(void);
COB_EXPIMP void cob_pre_open	(cob_file *f);
COB_EXPIMP void cob_pre_open_def (cob_file *f, char *setdef, char *isdef, int checkit);

NOT_IMPLEMENTED
COB_EXPIMP int	cob_findkey (cob_file *, cob_field *, int *, int * );

NOT_IMPLEMENTED
COB_EXPIMP void cob_open	(cob_file *, const enum cob_open_mode, const int, cob_field *);
COB_EXPIMP void cob_close	(cob_file *, cob_field *, const int, const int);
COB_EXPIMP void cob_read	(cob_file *, cob_field *, cob_field *, const int);
COB_EXPIMP void cob_read_next	(cob_file *, cob_field *, const int);
COB_EXPIMP void cob_rewrite	(cob_file *, cob_field *, const int, cob_field * );
COB_EXPIMP void cob_delete	(cob_file *, cob_field * );
COB_EXPIMP void cob_start	(cob_file *, const int, cob_field *,
				 cob_field *, cob_field * );
COB_EXPIMP void cob_write	(cob_file *, cob_field *, const int,
				 cob_field *, const unsigned int);

COB_EXPIMP void cob_delete_file	(cob_file *, cob_field *, const int);
COB_EXPIMP void cob_unlock_file	(cob_file *, cob_field *);


// (* functions in fileio.c for the MF style EXTFH interface *)
NOT_IMPLEMENTED
COB_EXPIMP int	EXTFH		(unsigned char *opcode, FCD3 *fcd);

NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_open		(EXTFH_FUNC callfh, cob_file *,
					const enum cob_open_mode, const int, cob_field *);
NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_close		(EXTFH_FUNC callfh, cob_file *,
					cob_field *, const int, const int);
NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_read		(EXTFH_FUNC callfh, cob_file *,
					cob_field *, cob_field *, const int);
NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_read_next	(EXTFH_FUNC callfh, cob_file *,
					cob_field *, const int);
NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_rewrite	(EXTFH_FUNC callfh, cob_file *,
					cob_field *, const int, cob_field * );
NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_delete	(EXTFH_FUNC callfh, cob_file *,
					cob_field * );
NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_start		(EXTFH_FUNC callfh, cob_file *,
					const int, cob_field *,
					cob_field *, cob_field * );
NOT_IMPLEMENTED
COB_EXPIMP void cob_extfh_write		(EXTFH_FUNC callfh, cob_file *,
					cob_field *, const int,
				 	cob_field *, const unsigned int);
COB_EXPIMP void cob_file_fcd_adrs		(cob_file *, void * );
COB_EXPIMP void cob_file_fcdkey_adrs	(cob_file *, void * );

// (* File system routines *)
COB_EXPIMP int cob_sys_open_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char * );
COB_EXPIMP int cob_sys_create_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char * );
COB_EXPIMP int cob_sys_read_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char * );
COB_EXPIMP int cob_sys_write_file	(unsigned char *, unsigned char *,
					 unsigned char *, unsigned char *,
					 unsigned char * );
COB_EXPIMP int cob_sys_close_file	(unsigned char * );
COB_EXPIMP int cob_sys_flush_file	(unsigned char * );
COB_EXPIMP int cob_sys_delete_file	(unsigned char * );
COB_EXPIMP int cob_sys_copy_file	(unsigned char *, unsigned char * );
COB_EXPIMP int cob_sys_check_file_exist	(unsigned char *, unsigned char * );
COB_EXPIMP int cob_sys_rename_file	(unsigned char *, unsigned char * );
COB_EXPIMP int cob_sys_get_current_dir	(const int, const int, unsigned char * );
COB_EXPIMP int cob_sys_change_dir	(unsigned char * );
COB_EXPIMP int cob_sys_create_dir	(unsigned char * );
COB_EXPIMP int cob_sys_delete_dir	(unsigned char * );
COB_EXPIMP int cob_sys_chdir		(unsigned char *, unsigned char * );
COB_EXPIMP int cob_sys_mkdir		(unsigned char * );
COB_EXPIMP int cob_sys_copyfile		(unsigned char *, unsigned char *,
					 unsigned char * );
COB_EXPIMP int cob_sys_file_info	(unsigned char *, unsigned char * );
COB_EXPIMP int cob_sys_file_delete	(unsigned char *, unsigned char * );

// (* SORT routines *)
COB_EXPIMP void	cob_file_sort_init	(cob_file *, const unsigned int,
					 const unsigned char *,
					 void *, cob_field * );
NOT_IMPLEMENTED
COB_EXPIMP void	cob_file_sort_options (cob_file *, const char *, COB_VA_DISCARDED);
COB_EXPIMP void	cob_file_sort_init_key	(cob_file *, cob_field *,
					 const int, const unsigned int);
COB_EXPIMP void	cob_file_sort_close	(cob_file * );
COB_EXPIMP void	cob_file_sort_using	(cob_file *, cob_file * );

// (* COB_EXPIMP void	cob_file_sort_using_extfh           *)
// (* (cob_file *, cob_file *,                              *)
// (*    int ( *callfh)(unsigned char *opcode, FCD3 *fcd)); *)

NOT_IMPLEMENTED
COB_EXPIMP void	cob_file_sort_giving	(cob_file *, const size_t, COB_VA_FILES);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_file_sort_giving_extfh	(cob_file *, const size_t, COB_VA_FILE_EXTFH);
COB_EXPIMP void	cob_file_release	(cob_file * );
COB_EXPIMP void	cob_file_return		(cob_file * );

COB_EXPIMP char * cob_path_to_absolute (const char *path);

// (* Functions in reportio.c *)

NOT_IMPLEMENTED
COB_EXPIMP int  cob_report_initiate	(cob_report *);
NOT_IMPLEMENTED
COB_EXPIMP int  cob_report_terminate(cob_report *);
NOT_IMPLEMENTED
COB_EXPIMP int  cob_report_generate	(cob_report *, cob_report_line *);
NOT_IMPLEMENTED
COB_EXPIMP void cob_report_suppress	(cob_report *, cob_report_line *);

// (* Functions in mlio.c *)

COB_EXPIMP int	cob_is_valid_uri	(const char * );
COB_EXPIMP int	cob_is_xml_namestartchar	(const int);
COB_EXPIMP int	cob_is_xml_namechar	(const int);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_xml_generate	(cob_field *, cob_ml_tree *,
					 cob_field *, const int, cob_field *,
					 cob_field *, const char);
NOT_IMPLEMENTED
COB_EXPIMP void cob_json_generate	(cob_field *, cob_ml_tree *,
					 cob_field *, const char);
NOT_IMPLEMENTED
COB_EXPIMP void	cob_xml_generate_new	(cob_field *, cob_ml_tree *,
					 cob_field *, const int, cob_field *,
					 cob_field *, const char);
NOT_IMPLEMENTED
COB_EXPIMP int	cob_xml_parse	(cob_field *, cob_field *,
					 cob_field *, const int, void **);


// (* Functions in intrinsic.c *)

COB_EXPIMP void		cob_put_indirect_field		(cob_field * );
COB_EXPIMP void		cob_get_indirect_field		(cob_field * );
COB_EXPIMP cob_field *cob_switch_value			(const int);
COB_EXPIMP cob_field *cob_intr_binop			(cob_field *, const int,
							 cob_field * );

COB_EXPIMP int cob_check_numval				(const cob_field *,
							 const cob_field *,
							 const int, const int);

COB_EXPIMP int cob_valid_date_format			(const char * );
COB_EXPIMP int cob_valid_datetime_format		(const char *, const char);
COB_EXPIMP int cob_valid_time_format			(const char *, const char);

COB_EXPIMP cob_field *cob_intr_current_date		(const int, const int);
COB_EXPIMP cob_field *cob_intr_when_compiled		(const int, const int,
							 cob_field * );
COB_EXPIMP cob_field *cob_intr_module_date		(void);
COB_EXPIMP cob_field *cob_intr_module_time		(void);
COB_EXPIMP cob_field *cob_intr_module_id		(void);
COB_EXPIMP cob_field *cob_intr_module_caller_id		(void);
COB_EXPIMP cob_field *cob_intr_module_source		(void);
COB_EXPIMP cob_field *cob_intr_module_formatted_date	(void);
COB_EXPIMP cob_field *cob_intr_module_path		(void);
COB_EXPIMP cob_field *cob_intr_exception_file		(void);
COB_EXPIMP cob_field *cob_intr_exception_location	(void);
COB_EXPIMP cob_field *cob_intr_exception_status		(void);
COB_EXPIMP cob_field *cob_intr_exception_statement	(void);
COB_EXPIMP cob_field *cob_intr_mon_decimal_point	(void);
COB_EXPIMP cob_field *cob_intr_num_decimal_point	(void);
COB_EXPIMP cob_field *cob_intr_mon_thousands_sep	(void);
COB_EXPIMP cob_field *cob_intr_num_thousands_sep	(void);
COB_EXPIMP cob_field *cob_intr_currency_symbol		(void);
COB_EXPIMP cob_field *cob_intr_char			(cob_field * );
COB_EXPIMP cob_field *cob_intr_ord			(cob_field * );
COB_EXPIMP cob_field *cob_intr_stored_char_length	(cob_field * );
COB_EXPIMP cob_field *cob_intr_combined_datetime	(cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_date_of_integer		(cob_field * );
COB_EXPIMP cob_field *cob_intr_day_of_integer		(cob_field * );
COB_EXPIMP cob_field *cob_intr_integer_of_date		(cob_field * );
COB_EXPIMP cob_field *cob_intr_integer_of_day		(cob_field * );
COB_EXPIMP cob_field *cob_intr_test_date_yyyymmdd	(cob_field * );
COB_EXPIMP cob_field *cob_intr_test_day_yyyyddd		(cob_field * );
COB_EXPIMP cob_field *cob_intr_test_numval		(cob_field * );
COB_EXPIMP cob_field *cob_intr_test_numval_c		(cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_test_numval_f		(cob_field * );
COB_EXPIMP cob_field *cob_intr_factorial		(cob_field * );

COB_EXPIMP cob_field *cob_intr_pi			(void);
COB_EXPIMP cob_field *cob_intr_e			(void);
COB_EXPIMP cob_field *cob_intr_exp			(cob_field * );
COB_EXPIMP cob_field *cob_intr_exp10			(cob_field * );
COB_EXPIMP cob_field *cob_intr_abs			(cob_field * );
COB_EXPIMP cob_field *cob_intr_acos			(cob_field * );
COB_EXPIMP cob_field *cob_intr_asin			(cob_field * );
COB_EXPIMP cob_field *cob_intr_atan			(cob_field * );
COB_EXPIMP cob_field *cob_intr_cos			(cob_field * );
COB_EXPIMP cob_field *cob_intr_log			(cob_field * );
COB_EXPIMP cob_field *cob_intr_log10			(cob_field * );
COB_EXPIMP cob_field *cob_intr_sin			(cob_field * );
COB_EXPIMP cob_field *cob_intr_sqrt			(cob_field * );
COB_EXPIMP cob_field *cob_intr_tan			(cob_field * );

COB_EXPIMP cob_field *cob_intr_upper_case		(const int, const int,
							 cob_field * );
COB_EXPIMP cob_field *cob_intr_lower_case		(const int, const int,
							 cob_field * );
COB_EXPIMP cob_field *cob_intr_reverse			(const int, const int,
							 cob_field * );
COB_EXPIMP cob_field *cob_intr_concatenate		(const int, const int,
							 COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_substitute		(const int, const int,
							 COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_substitute_case		(const int, const int,
							 COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_trim			(const int, const int,
							 cob_field *, const int);
COB_EXPIMP cob_field *cob_intr_length			(cob_field * );
COB_EXPIMP cob_field *cob_intr_byte_length		(cob_field * );
COB_EXPIMP cob_field *cob_intr_integer			(cob_field * );
COB_EXPIMP cob_field *cob_intr_integer_part		(cob_field * );
COB_EXPIMP cob_field *cob_intr_fraction_part		(cob_field * );
COB_EXPIMP cob_field *cob_intr_sign			(cob_field * );
COB_EXPIMP cob_field *cob_intr_lowest_algebraic		(cob_field * );
COB_EXPIMP cob_field *cob_intr_highest_algebraic	(cob_field * );
COB_EXPIMP cob_field *cob_intr_numval			(cob_field * );
COB_EXPIMP cob_field *cob_intr_numval_c			(cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_numval_f			(cob_field * );
COB_EXPIMP cob_field *cob_intr_annuity			(cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_mod			(cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_rem			(cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_sum			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_ord_min			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_ord_max			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_min			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_max			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_midrange			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_median			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_mean			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_range			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_random			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_variance			(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_standard_deviation	(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_present_value		(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_year_to_yyyy		(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_date_to_yyyymmdd		(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_day_to_yyyyddd		(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_locale_compare		(COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_locale_date		(const int, const int,
							 cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_locale_time		(const int, const int,
							 cob_field *, cob_field * );

COB_EXPIMP cob_field *cob_intr_seconds_past_midnight	(void);
COB_EXPIMP cob_field *cob_intr_lcl_time_from_secs	(const int, const int,
							 cob_field *, cob_field * );

COB_EXPIMP cob_field *cob_intr_seconds_from_formatted_time	(cob_field *,
								 cob_field * );

COB_EXPIMP cob_field *cob_intr_boolean_of_integer	(cob_field *, cob_field * );
COB_EXPIMP cob_field *cob_intr_char_national		(cob_field * );
NOT_IMPLEMENTED
COB_EXPIMP cob_field *cob_intr_display_of		(const int, const int,
							 const int, COB_VA_DISCARDED);
COB_EXPIMP cob_field *cob_intr_exception_file_n		(void);
COB_EXPIMP cob_field *cob_intr_exception_location_n	(void);
COB_EXPIMP cob_field *cob_intr_formatted_current_date	(const int, const int,
							 cob_field * );
COB_EXPIMP cob_field *cob_intr_formatted_date		(const int, const int,
							 cob_field *, cob_field * );

/* (* COMMENTED OUT as only readable from OCAML. OCaml will only discard
   the "/*" and corresponding terminator, and the OCaml comments inside... *)

COB_EXPIMP cob_field *cob_intr_formatted_datetime
   "cob_intr_formatted_datetime1"
   (const int, const int,
    const int,
    cob_field* fmt, cob_field* days, int use_system_offset);

COB_EXPIMP cob_field *cob_intr_formatted_datetime
   "cob_intr_formatted_datetime2"
   (const int, const int,
    const int,
    cob_field* fmt, cob_field* days,
    cob_field* offset_time, int use_system_offset);

COB_EXPIMP cob_field *cob_intr_formatted_time
   "cob_intr_formatted_time1"
   (const int, const int,
    const int,
    cob_field* format, cob_field* time,
    int use_system_offset);

COB_EXPIMP cob_field *cob_intr_formatted_time
   "cob_intr_formatted_time2"
   (const int, const int,
    const int,
    cob_field* format, cob_field* time,
    cob_field* offset_time, int use_system_offset);

*/

COB_EXPIMP cob_field *cob_intr_integer_of_boolean	(cob_field * );
NOT_IMPLEMENTED
COB_EXPIMP cob_field *cob_intr_national_of		(const int, const int,
							 const int, COB_VA_DISCARDED);
NOT_IMPLEMENTED
COB_EXPIMP cob_field *cob_intr_standard_compare		(const int, COB_VA_DISCARDED);
COB_EXPIMP cob_field *cob_intr_test_formatted_datetime	(cob_field *, cob_field * );

COB_EXPIMP cob_field *cob_intr_integer_of_formatted_date	(cob_field *,
								 cob_field * );
COB_EXPIMP cob_field *cob_intr_content_length		(cob_field * );
COB_EXPIMP cob_field *cob_intr_content_of		(const int, const int,
							 COB_VA_FIELDN);
COB_EXPIMP cob_field *cob_intr_bit_of		(cob_field * );
COB_EXPIMP cob_field *cob_intr_bit_to_char		(cob_field * );
COB_EXPIMP cob_field *cob_intr_hex_of (cob_field* );
COB_EXPIMP cob_field *cob_intr_hex_to_char (cob_field* );

// (* Functions in cconv.c *)

NOT_IMPLEMENTED
COB_EXPIMP int cob_load_collation (const char *, cob_u8_t *, cob_u8_t * );

