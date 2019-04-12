//-----------------------------------------------------------------------------
// Utility C functions
//

// Windows operating system version, use to customize certain
// functions, such as fopen, for Unicode/non-Unicode use.
enum OSVer
{
   OtherOS,
   W95,
   WNT
};
extern OSVer g_osver;

extern BYTE g_osrel;

BOOL chk_ext(_TCHAR* fbuf, _TCHAR* ext);
void force_ext(_TCHAR* fbuf, _TCHAR* ext);
BOOL os_file_exists(_TCHAR* fbuf);
BOOL file_needs_update(_TCHAR* f1, _TCHAR* f2);
FILE* os_fopen(_TCHAR* fname, _TCHAR* mode);
void os_remove(_TCHAR* fname);
void get_osver();
//BOOL ValidProject(CString);
_TCHAR * aWfgets(_TCHAR * buf, int len, FILE* fp);
