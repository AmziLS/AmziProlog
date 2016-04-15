/****************************************************************************

defs.h -- General purpose Amzi! definitions


Copyright (c) 1992-2002 by Amzi! inc.  All Rights Reserved.

****************************************************************************/

#if defined (__linux__) || defined(__sun)
#include <wchar.h>
#include <wctype.h>
#include <ctype.h>
#endif

// Unicode definitions

#ifdef _UNICODE

#define aS(x) L ## x
//#define aCHAR wchar_t

// String manipulation

#define Lstrlen    wcslen
#define Lstrcmp    wcscmp
#define Lstrncmp   wcsncmp
#define Lstrcpy    wcscpy
#define Lstrncpy   wcsncpy
#define Lstrstr    wcsstr
#define Lstrcat    wcscat
#define Lstrncat   wcsncat
#define Lstrcspn   wcscspn
#define Lstrchr    wcschr
#define Lstrrchr   wcsrchr
#define Lstrtok    wcstok
#define Lstrpbrk   wcspbrk

#define Lisspace   iswspace
#define Lisdigit   iswdigit
#define Lisalpha   iswalpha
#define Lisupper   iswupper
#define Lislower   iswlower
#define Ltolower   towlower

#define Latol      _wtol

#define Lfgets     fgetws
#define Lvsnprintf _vsnwprintf
#define Lvsprintf  vswprintf
#define Lsprintf   swprintf
#define Lprintf    wprintf

#define Lstrftime  wcsftime

#define Lmemset    memset
#define Lmemcpy    memcpy

// Other string dependent macros
#define Lsystem     _wsystem
#define Lfopen      _wfopen
#define Lgetenv     _wgetenv
#define Lfputs      fputws
#define Lfgets      fgetws
#define Lfgetc      fgetwc
#define Lungetc     ungetwc
#define Lvfprintf   vfwprintf
#define CharInt     wint_t     // getc character integers

#define L_splitpath _wsplitpath

#else // not _UNICODE

#define aS(x) x
//#define aCHAR char

// String functions
#define Lstrlen    strlen
#define Lstrcmp    strcmp
#define Lstrncmp   strncmp
#define Lstrcpy    strcpy
#define Lstrncpy   strncpy
#define Lstrstr    strstr
#define Lstrcat    strcat
#define Lstrncat   strncat
#define Lstrcspn   strcspn
#define Lstrchr    strchr
#define Lstrrchr   strrchr
#define Lstrtok    strtok
#define Lstrpbrk   strpbkr

#define Lisspace   isspace
#define Lisdigit   isdigit
#define Lisalpha   isalpha
#define Lisupper   isupper
#define Lislower   islower
#define Ltolower   towlower

#define Latol      atol
#define Latof      atof

#define Lfgets     fgets
#define Lvsnprintf _vsnprintf
#define Lvsprintf  vsprintf
#define Lsprintf   sprintf
#define Lprintf    printf

#define Lstrftime  strftime

#define Lmemset    memset
#define Lmemcpy    memcpy

// Other string-dependent functions
#define Lsystem   system
#define Lfopen    fopen
#define Lgetenv   getenv
#define Lfputs    fputs
#define Lfgets    fgets
#define Lfgetc    fgetc
#define Lungetc   ungetc
#define Lvfprintf vfprintf
#define CharInt   int     // getc character integers

#define L_splitpath _splitpath
#endif // _UNICODE

#define NL  aS("\n")
#define SP  aS(" ")
#define EOS aS('\0')

enum LFLAG {LOFF, LON};
enum LBOOL {LFALSE, LTRUE};

#ifndef _MAX_PATH
#define _MAX_PATH 512
#endif
