#ifdef __CYGWIN__
#define _WIN32
#endif


#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

#ifdef _WIN32

#include <windows.h>
#include <sys/types.h>


#endif

/*********************************************************************/

#ifdef _WIN32

/* from ocamltopwin, startocaml.c */
static int ReadRegistryValue(HKEY h,
                             char ** keys, int pos, int len,
                             const char *entry,
                             DWORD *dwType,
                             unsigned char *dest,
                             unsigned long size)
{
    LONG ret;

    if( pos < len ){
      HKEY hret;
      /*      fprintf(stderr, "RegOpenKeyExA(%s)\n", keys[pos]); */
      if (RegOpenKeyExA(h, keys[pos], 0, KEY_QUERY_VALUE, &hret) != ERROR_SUCCESS)
        return -1;
      ret = ReadRegistryValue(hret, keys, pos+1, len, entry, dwType, dest, size);
      RegCloseKey(hret);
      return ret;
    } else {
       /*       fprintf(stderr, "RegQueryValueExA(%s)\n", entry); */
       ret = RegQueryValueExA(h, entry, 0, dwType, dest, &size);
       if (ret == ERROR_SUCCESS)
         return size;
       else
	 return -1;
    }
}

/* from ocamltopwin, startocaml.c */
static int DeleteRegistryValue(HKEY h,
                        char ** keys, int pos, int len,
			const char *entry
                        )
{
    int ret;

    if( pos < len ){
      HKEY hret; LONG lResult;
      /*      fprintf(stderr, "RegOpenKeyExA(%s)\n", keys[pos]); */
      lResult = RegOpenKeyExA(h, keys[pos], 0, KEY_ALL_ACCESS, &hret);
      /* fprintf(stderr, "lResult=%d RegOpenKeyExA %s\n", lResult, keys[pos]); */
      if ( lResult != ERROR_SUCCESS){
	if ( lResult == ERROR_FILE_NOT_FOUND ) return 0;
        return -1;
      }
      ret = DeleteRegistryValue(hret, keys, pos+1, len, entry);
      RegCloseKey(hret);
      return ret;
    } else {
      /* fprintf(stderr, "RegDeleteKey(%s)\n", entry); */
       LONG lResult = RegDeleteValue(h, entry);
       /* fprintf(stderr, "lResult=%d RegDeleteKey %s\n", lResult, entry); */
       if (lResult == ERROR_SUCCESS)
         return 1;
       else {
         if (lResult == ERROR_FILE_NOT_FOUND ) return 0;
	 return -1;
       }
    }
}

static int WriteRegistryValue(HKEY h,
                         char ** keys, int pos , int len,
			 const char * entry,
           	         DWORD dwType,
                         const BYTE * data)
{
    LONG ret;

    if( pos < len ){
      HKEY hret;
      /* fprintf(stderr, "RegOpenKeyExA(%s)\n", keys[pos]); */
      if (RegOpenKeyExA(h, keys[pos], 0, KEY_ALL_ACCESS, &hret)
	  != ERROR_SUCCESS){
	DWORD disp;
	if (RegCreateKeyExA(h, keys[pos], 0, NULL, 0, KEY_ALL_ACCESS, NULL, &hret, &disp)
	    != ERROR_SUCCESS) {
	  return -1;
        }
      }
      ret = WriteRegistryValue(hret, keys, pos+1, len, entry, dwType, data);
      RegCloseKey(hret);
      return ret;
    } else {
      /*  fprintf(stderr, "RegSetValueEx(%s)\n", entry); */
      int size = strlen((const char*) data);
       ret = RegSetValueEx(h, entry, 0, dwType, data, size + 1);
       if (ret == ERROR_SUCCESS)
         return 1;
       else
	 return -1;
    }
}

static HKEY HKEY_of_value(value hroot_v)
{
  switch(Int_val(hroot_v)){
  case 0 :
    /*    fprintf(stderr, "From HKEY_CLASSES_ROOT\n"); */
    return HKEY_CLASSES_ROOT;
  case 1 :
    /* fprintf(stderr, "From HKEY_CURRENT_CONFIG\n"); */
    return HKEY_CURRENT_CONFIG;
  case 2 :
    /* fprintf(stderr, "From HKEY_CURRENT_USER\n"); */
    return HKEY_CURRENT_USER;
  case 3 :
    /* fprintf(stderr, "From HKEY_LOCAL_MACHINE\n"); */
    return HKEY_LOCAL_MACHINE;
  case 4 :
    /* fprintf(stderr, "From HKEY_USERS\n"); */
    return HKEY_USERS;
  default: break;
  }
  caml_failwith("HKEY_of_value");
}

static DWORD dwType_of_value(value dwType_v)
{
  switch(Int_val(dwType_v)){
  case 0 : return REG_SZ;
  case 1: return REG_EXPAND_SZ;
  case 2: return REG_DWORD;
  case 3: return REG_QWORD;
  case 4: return REG_BINARY;
  default: break;
  }
  caml_failwith("dwType_of_value");
}

static value value_of_dwType(DWORD dwType)
{
  switch(dwType){
  case REG_SZ: return Val_int(0);
  case REG_EXPAND_SZ: return Val_int(1);
  case REG_DWORD: return Val_int(2);
  case REG_QWORD: return Val_int(3);
  case REG_BINARY: return Val_int(4);
  default: return Val_int(5);
  }
}

#endif

/*
hroot_v: 0 = HKEY_CURRENT_USER
         1 = HKEY_LOCAL_MACHINE
return: size of result
    or  -1 : error
*/
value win32_read_registry_value_ml(
                             value hroot_v,
                             value keys_v,
                             value entry_v,
                             value dwType_v,
                             value result_v
                             )
{
#ifdef _WIN32
  DWORD dwType = REG_SZ;
  int ret =
     ReadRegistryValue(
		  HKEY_of_value(hroot_v),
		  (char**) keys_v,
		  0, Wosize_val(keys_v),
		  String_val(entry_v),
                  &dwType,
		  (unsigned char*)String_val(result_v),
		  caml_string_length(result_v));
  if (ret > 0){
    Field(dwType_v, 0) = value_of_dwType(dwType);
    return Val_int(ret-1); /* Fabrice: the ending 0 is counted ! */
  } else
    return Val_int(-1);
#else
  caml_failwith("NO_IMPLEMENTATION(win32_read_registry_value_ml)");
#endif
}

value  win32_write_registry_value_ml(
                             value hroot_v,
                             value keys_v,
                             value entry_v,
			     value dwType_v,
                             value result_v
                             )
{
#ifdef _WIN32

  if (
      WriteRegistryValue(HKEY_of_value(hroot_v),
                         (char**)keys_v,
                         0, Wosize_val(keys_v),
                         String_val(entry_v),
			 dwType_of_value(dwType_v),
                         (unsigned char*)String_val(result_v)
                    ) > 0
       )
    return Val_unit;
  else {
    win32_maperr(GetLastError());
    caml_failwith("win32_write_registry_value_ml");
  }
#else
   caml_failwith("NO_IMPLEMENTATION(win32_write_registry_value_ml)");
#endif
}



value  win32_delete_registry_value_ml(
                             value hroot_v,
                             value keys_v,
                             value entry_v
                             )
{
#ifdef _WIN32
  int ret = DeleteRegistryValue(HKEY_of_value(hroot_v),
                    (char**)keys_v,
		    0, Wosize_val(keys_v),
                    String_val(entry_v)
	      );
  if (ret == 1) return Val_true;
  if (ret == 0) return Val_false;
  win32_maperr(GetLastError());
  caml_failwith("win32_delete_registry_value_ml");
#else
  caml_failwith("NO_IMPLEMENTATION(win32_delete_registry_value_ml)");
#endif
}



/* This function is usually called after changing some variable in the
   environment, to broadcast to other applications that they should
   take the corresponding changes into account. The argument in that
   case is often "Environment". */
value win32_broadcast_setting_change_ml(value varname_v)
{
#ifdef _WIN32
  PDWORD dwReturnValue;
  SendMessageTimeout(HWND_BROADCAST,
                     WM_SETTINGCHANGE,
                     0,
		     (LPARAM)String_val(varname_v),
                     SMTO_ABORTIFHUNG,
                     500,
		     (PDWORD_PTR)&dwReturnValue);
  return Val_unit;
#else
   caml_failwith("NO_IMPLEMENTATION(win32_broadcast_setting_change_ml)");
#endif
}
