/************************************************************\
*
* lenv.h -- environment specific functions
*
* Copyright (c) 1992-2012 by Amzi! inc.  All Rights Reserved.
*
\************************************************************/

#ifndef LENV_H
#define LENV_H

#define PROLOG_NAME aS("Amzi! Prolog + Logic Server")

#if defined(_DEBUG) || defined(DEBUG)

#define AMZI_VERSION aS("10.0.04 Debugging")

#else
#if defined P64
#define AMZI_VERSION aS("10.0.04 64-bit")
#else
#define AMZI_VERSION aS("10.0.04 32-bit")
#endif

#endif
#define VERSION_NUMBER 100   // 10.0
#define BUILD_NUMBER 4      // .03
// This is the version and build of the last compatible
// compiler.  That is, these numbers should change
// whenever the format of the .plm or .xpl file changes,
// BUT NOT otherwise.
#define COMPATIBLE_VERSION 70
#define COMPATIBLE_BUILD 8

//------------------------------------------------------
// The Rats Nest
//
// Environment specific functions, which could vary
// based on OS, machine, Unicode/ANSI, C compiler etc.,
// some requiring our own implementation, others not.
//
//

#ifndef _MAX_PATH
#define _MAX_PATH 1024
#endif

#ifndef _MAX_FNAME
#define _MAX_FNAME 512
#endif

#ifndef _MAX_DIR
#define _MAX_DIR 512
#endif

#ifndef _MAX_EXT
#define _MAX_EXT 128
#endif

#ifndef _MAX_DRIVE
#define _MAX_DRIVE 128
#endif

// it seems some C++s don't support wostream, in which
// case these operator overloads let the wide and ascii
// strings to go back and forth OK.
std::ostream& operator<<( std::ostream &os, wchar_t *s );
#if !defined(UNIX)
std::wostream& operator<<( std::wostream &os, char *s ); 
#endif

#if defined(BIG__ENDIAN) 
#define RevEndian32(v) v = ( ((v & 0xff) << 24) | \
                             ((v & 0xff00) <<  8) | \
                             ((aUINT32)(v & 0xff0000) >> 8) | \
                             ((aUINT32)(v & 0xff000000) >> 24) )
#define RevEndian16(v) v = ( ((v & 0xff) << 8) | \
                             ((aUINT16)(v & 0xff00) >> 8) )
#else
#define RevEndian32(v)
#define RevEndian16(v)
#endif

// for Unicode I/O, which might require a flip if its
// reading a file created in other endian from this machine
#define FlipEndian16(v) v = ( ((v & 0xff) << 8) | \
                             ((aUINT16)(v & 0xff00) >> 8) )


//#ifdef SOLARIS
//inline int max(int x, int y) { return (x > y ? x : y); }
//inline int min(int x, int y) { return (x < y ? x : y); }
//#endif

#ifdef UNIX
#undef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#undef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif

#if defined(WINDOWS)
#define LSXptr HINSTANCE
#define longlong  __int64
#elif defined(UNIX)
#define longlong long long
#define LSXptr void *
#endif

#if defined(WINDOWS)
extern HINSTANCE  g_hInstance;
#endif

// Windows operating system version, use to customize certain
// functions, such as fopen, for Unicode/non-Unicode use.
enum OSVer
{
   OtherOS,
   W95,
   WNT
};

class LEnvironment;
extern LEnvironment g_lenv;
//extern LEnvironment *lenv;  // ptr to Logic Server environment

class LEnvironment
{
private:
   OSVer m_osver;
   bool b_tty_initialized;

public:
   LEnvironment();
   OSVer getOSVer()
      { return m_osver; }
   FILE *asys_fopen(aCHAR *fname, aCHAR *mode, aCHAR *subdir);
   aCHAR *aget_path(aCHAR *fname, aCHAR *ext, aCHAR *subdir);
   int e_keyZb();
   aCHAR *amzi_directory();  // return the Amzi! directory
   bool is_ide();   // running from the ide?
   bool is_command();   // running from the ide?
   aCHAR *application_path();
   aCHAR *get_user_name();  // get the computer user name
//   char *hardware_signature();
   void computer_name(char*, int);

//----------------------------------------------------------
// Functions which are different for their Unicode flavors
// C++ standard versions are default, and expanded inline.
//
#ifdef _UNICODE

private:
   wchar_t* wcstok_it;   // iterator for standard wcstok.

public:
   long e_wtol(const wchar_t* s);
   double e_wtof(const wchar_t* s);
   void e_inittty();
   void e_resettty();

   size_t e_mbs_out(LEngine* m_peng, char* sout, wchar_t* sin, size_t len);
   size_t e_mbs_in(LEngine* m_peng, wchar_t* sout, char* sin, size_t len);

#if defined(MSC) || defined(HPUX) || defined(WALRUS)
   wchar_t *e_wcstok(wchar_t *s1, const wchar_t *s2);
#else
   wchar_t *e_wcstok(wchar_t *s1, const wchar_t *s2)
   { return wcstok(s1, s2, &wcstok_it); }
#endif

#if defined(HPUX) || defined(SOLARIS)
   wchar_t *e_wcsstr(const wchar_t *s1, const wchar_t *s2);
#else
   wchar_t *e_wcsstr(const wchar_t *s1, const wchar_t *s2)
   { return const_cast<wchar_t*>(wcsstr(s1, s2)); }
#endif

   int e_wsystem(const wchar_t *s);
   FILE *e_wfopen(const wchar_t *fname, const wchar_t *mode);
   wchar_t *e_wgetenv_dup(const wchar_t *evar);

#if defined(LINUX) || defined(HPUX)
   size_t e_wcsftime(wchar_t *buf, size_t maxsize, const wchar_t *fmt, const struct tm *timptr);
#else
   size_t e_wcsftime(wchar_t *buf, size_t maxsize, const wchar_t *fmt, const struct tm *timptr)
   { return wcsftime(buf, maxsize, fmt, timptr); }
#endif

#if defined(LINUX)
   wint_t e_fgetwc(FILE *f);
   wint_t e_ungetwc(wint_t c, FILE *f);
   wint_t e_fputwc(wint_t c, FILE *f);
   wchar_t* e_fgetws(wchar_t *s, int n, FILE *f);
   int e_fputws(const wchar_t *buf, FILE *f);
#else
   wint_t e_fgetwc(FILE *f) { return fgetwc(f); }
   wint_t e_ungetwc(wint_t c, FILE *f) { return ungetwc(c, f); }
   wint_t e_fputwc(wint_t c, FILE *f) { return fputwc(c, f); }
   wchar_t* e_fgetws(wchar_t *s, int n, FILE *f) { return fgetws(s, n, f); }
   int e_fputws(const wchar_t *buf, FILE *f) { return fputws(buf, f); }
#endif

#if defined(LINUX) || defined(HPUX) || defined(SOLARIS) || defined(MACOSX)
   int e_vswprintf(wchar_t *buf, const wchar_t *fmt, va_list args);
#else
   int e_vswprintf(wchar_t *buf, const wchar_t *fmt, va_list args)
     { return vswprintf(buf, fmt, args); }
#endif

#if defined(LINUX) || defined(HPUX) || defined(SOLARIS) || defined(MACOSX)
   int e_vfwprintf(FILE *fp, const wchar_t *fmt, va_list args);
#else
   int e_vfwprintf(FILE *fp, const wchar_t *fmt, va_list args)
     { return vfwprintf(fp, fmt, args); }
#endif

   // dropping support for this safer, but non-stardard varient
   //int e_vsnwprintf(wchar_t *buf, int buflen, const wchar_t *fmt, va_list args);

   // Because of the ..., the defaults for these are defined as
   // macros in defs.h
   int e_wprintf(const wchar_t *fmt, ...);
   int e_swprintf(wchar_t *buf, size_t buflen, const wchar_t *fmt, ...);
   //int e_fwprintf(FILE *fp, const wchar_t *fmt, ...);

private:
#if defined(UNIX)
   int vsnwpf(wchar_t *buf, int buflen, const wchar_t *fmt, va_list args);
#endif

public:
   char *new_mbs_dup(const wchar_t *s);
   wchar_t *new_wcs_dup(const char *s);

#else  // ANSI char versions 

   char *e_getenv_dup(const char *evar);
   int e_sprintf(char *buf, size_t num, const char *fmt, ...);

#endif  // _UNICODE or not

};

#endif //LENV_H
