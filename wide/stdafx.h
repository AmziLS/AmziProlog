// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

// Unicode ifdefs, including some not in tchar.h
#ifdef _UNICODE
#ifndef UNICODE
#define UNICODE
#endif
#define Lsprintf swprintf
#else
#define Lsprintf sprintf
#endif

#include <tchar.h>
#define EOS _T('\0')
#define NL  _T('\n')

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions (including VB)
#include <afxtempl.h>		// MFC collection class templates.