/**********************************************************\
*
* lenv.cpp -- system dependent functions
*
* Copyright (c) 1992-2009 by Amzi!.  All Rights Reserved.
*
*
\**********************************************************/

#include "inc.h"
#include "pch.h"

#if defined(WINDOWS)
#include <intrin.h>
#endif

#if defined(UNIX)
#include <termios.h>
#include <unistd.h>
#if defined(OSX)
#include <libproc.h>
#endif

#endif

#include "utf_checked.h"
#include <string>
using namespace std;
//#include "utf_unchecked.h"

LEnvironment g_lenv;
//LEnvironment *lenv = &g_lenv;
//LEnvironment *lenv = new LEnvironment;  // only one, stays for session

std::ostream& operator<<( std::ostream &os, wchar_t *s )
// Used for LANDFILL now, when platform doesn't support
// wostream.
{
   int len = (int)wcslen(s)+1;
   char* as;
   LNEWZ(as, char[len*2]);
   wcstombs(as, s, len*2);
   os << as;
   delete as;
   return os;
}

//#if !defined(LINUX)
#if !defined(GNU)
std::wostream& operator<<( std::wostream &os, char *s )
// Used for LANDFILL now, when platform doesn't support
// wostream.
{
   int len = (int)strlen(s) + 1;
   wchar_t* ws;
   LNEWZ(ws, wchar_t[len]);
   mbstowcs(ws, s, len);
   os << ws;
   delete ws;
   return os;
}
#endif

LEnvironment::LEnvironment()
{
#if defined(WINDOWS) && defined(P32)
   DWORD v = ::GetVersion();
   if (v < 0x80000000)
      m_osver = WNT;
   else
      m_osver = W95;
#else
   m_osver = OtherOS;
#endif
   b_tty_initialized = false;
}

char *LEnvironment::new_mbs_dup(const wchar_t *s)
// copy a wc string to a mb string.
{
   int len = (int)wcslen(s)+1;
   char *buf;
   LNEWZ(buf, char[len*2]);
   wcstombs(buf, s, len*2);
   return buf;
}

wchar_t *LEnvironment::new_wcs_dup(const char *s)
// copy a mb string to a wc string.
{
   int len = (int)strlen(s)+1;
   wchar_t *buf;
   LNEWZ(buf, wchar_t[len]);
   mbstowcs(buf, s, len);
   return buf;
}

size_t LEnvironment::e_mbs_out(LEngine* m_peng, char* sout, wchar_t* sin, size_t len)
{
   if (pSTATE->m_utf8io == LON) {
      //cout << "\ne_mbs_out: " << sin << "\n";
      std::vector<unsigned short> utf16line;
      wchar_t *p = sin;
      while (*p) {
         utf16line.push_back(*p);
         p++;
      }
      utf16line.push_back(0);
      string utf8line;  // the output
      utf8::utf16to8(utf16line.begin(), utf16line.end(), back_inserter(utf8line));
      strncpy(sout, utf8line.c_str(), len);
   }
   else {
      wcstombs(sout, sin, len);
   }
   //copy utf8line to sout
   return strlen(sout);
}

size_t LEnvironment::e_mbs_in(LEngine* m_peng, wchar_t* sout, char* sin, size_t len)
{
   if (pSTATE->m_utf8io == LON) {
      //cout << "\ne_mbs_in: " << sin << "\n";
      string line(sin);
      string::iterator end_it = utf8::find_invalid(line.begin(), line.end());
      vector<unsigned short> utf16line;
      utf8::utf8to16(line.begin(), end_it, back_inserter(utf16line));
      wchar_t *p = sout;
      vector<unsigned short>::iterator v_it = utf16line.begin();
      while (v_it != utf16line.end()) {
         *p = *v_it;
         p++;
         v_it++;
      }
      *p = 0;
   }
  else {
      mbstowcs(sout, sin, len);
   }
   //copy utf8line to sout
   return Lstrlen(sout);
}


#if defined(WINDOWS)

HINSTANCE  g_hInstance = NULL;

// asys_fopen gets the module path, but we need a way to get it
// either Unicode NT or non-Unicode Win9x
DWORD GetModuleFileNameAW(HMODULE hMod, STRptr fname, DWORD size)
{
#ifdef _UNICODE
   if (g_lenv.getOSVer() == W95)
   {
      char *fa;
      LNEWZ(fa, char[size]);
      DWORD dw = GetModuleFileNameA(hMod, fa, size);
      mbstowcs(fname, fa, size+1);
      delete fa;
      return dw;
   }
#endif
   return GetModuleFileName(hMod, fname, size);
}

FILE *LEnvironment::asys_fopen(aCHAR *fname, aCHAR *mode, aCHAR *subdir)
{           // a smart open that looks in the rightAmzi! system dirs for files.
   FILE    *fp;
   aCHAR    path[_MAX_PATH];
   aCHAR    *amzi_path;

   aCHAR    wdrive[_MAX_DRIVE];
   aCHAR    wdir[_MAX_DIR];
   aCHAR    wfname[_MAX_FNAME];
   aCHAR    wext[_MAX_EXT];

   // first try where ever we are now
   fp = Lfopen(fname, mode);
   if (fp) 
     return(fp);

   // next try the environment variable
   //amzi_path = Lgetenv_dup(aS("AMZI_DIR"));    // try from the env variable
   amzi_path = amzi_directory();    // try from the env variable

   if (amzi_path)
   {
      Lstrcpy(path, amzi_path);
      delete amzi_path;
      Lstrcat(path, aS("\\"));
      Lstrcat(path, subdir);
      Lstrcat(path, aS("\\"));
      Lstrcat(path, fname);

      fp = Lfopen(path, mode);
      if (fp)
        return(fp);
   }

   // see if we can get it from the .exe directory

   GetModuleFileNameAW(NULL, path, _MAX_PATH);
   L_splitpath(path, wdrive, wdir, wfname, wext);
   Lstrcpy(path, wdrive);
   Lstrcat(path, wdir);

   //Lstrcat(path, aS("\\..\\"));
   Lstrcat(path, aS("..\\"));
   Lstrcat(path, subdir);
   Lstrcat(path, aS("\\"));
   Lstrcat(path, fname);
   fp = Lfopen(path, mode);
   if (fp)  
     return(fp);

#if defined(LIB_DLL)
   // try the DLL directory, if dynamically loaded, might
   // not be instantiated.
   if (g_hInstance)
   {
      GetModuleFileNameAW(g_hInstance, path, _MAX_PATH);
      L_splitpath(path, wdrive, wdir, wfname, wext);
      Lstrcpy(path, wdrive);
      Lstrcat(path, wdir);

      //Lstrcat(path, aS("\\..\\"));
      Lstrcat(path, aS("..\\"));
      Lstrcat(path, subdir);
      Lstrcat(path, aS("\\"));
      Lstrcat(path, fname);
      fp = Lfopen(path, mode);
      if (fp)  
        return(fp);
   }
#endif  // LIB_DLL

   return NULL;
}

aCHAR *LEnvironment::amzi_directory()
{
   // NOTE - Caller must delete the path!
   aCHAR    path[_MAX_PATH];

   aCHAR    wdrive[_MAX_DRIVE];
   aCHAR    wdir[_MAX_DIR];
   aCHAR    wfname[_MAX_FNAME];
   aCHAR    wext[_MAX_EXT];

   aCHAR *pathstr;
   int len;
   LPathString ps;
   // finds amzi.dll
   GetModuleFileNameAW(g_hInstance, path, _MAX_PATH);
   L_splitpath(path, wdrive, wdir, wfname, wext);
   ps = (LPathString)wdrive;
   ps += wdir;

   ps += aS("..\\");
   len = ps.Length();
   LNEWZ(pathstr, aCHAR[len+1]);
   Lstrcpy(pathstr, ps);
   return(pathstr);
}

bool LEnvironment::is_ide()
{
   // NOTE - Caller must delete the path!
   aCHAR    path[_MAX_PATH];
   aCHAR    wdrive[_MAX_DRIVE];
   aCHAR    wdir[_MAX_DIR];
   aCHAR    wfname[_MAX_FNAME];
   aCHAR    wext[_MAX_EXT];

   GetModuleFileNameAW(NULL, path, _MAX_PATH);
   L_splitpath(path, wdrive, wdir, wfname, wext);

   if (Lstrcmp(wfname, aS("eclipse")) == 0 ||
	   Lstrcmp(wfname, aS("wideA")) == 0 ||
	   Lstrcmp(wfname, aS("wideW")) == 0)
      return true;
   else
      return false;
}

aCHAR *LEnvironment::get_user_name()
{
   HKEY hKey;
   DWORD dwBufLen=255;
   LONG lRet;
   aCHAR *aOwner;
   
   LNEWZ(aOwner, aCHAR[dwBufLen+1]);

   // Surprise! The name is in different places depending on the OS
   if (g_lenv.getOSVer() == W95)
      lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
         aS("SOFTWARE\\Microsoft\\Windows\\CurrentVersion"),
         0, KEY_QUERY_VALUE, &hKey );
   else
      lRet = RegOpenKeyEx( HKEY_LOCAL_MACHINE,
         aS("SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"),
         0, KEY_QUERY_VALUE, &hKey );
   if ( lRet != ERROR_SUCCESS )
   {
      RegCloseKey(hKey);
      return FALSE;
   }

   // FYI: RegisteredOrganization is the organization name
   lRet = RegQueryValueEx( hKey, aS("RegisteredOwner"), NULL, NULL,
      (LPBYTE) aOwner, &dwBufLen);

   if ( (lRet != ERROR_SUCCESS) || (dwBufLen > 255) )
   {
      RegCloseKey(hKey);
      return FALSE;
   }

   RegCloseKey( hKey );

   return aOwner;
}


aCHAR *LEnvironment::aget_path(aCHAR *fname, aCHAR *ext, aCHAR *subdir)
{
   // NOTE - Caller must delete the path!
   FILE    *fp;
   aCHAR    path[_MAX_PATH];

   aCHAR    wdrive[_MAX_DRIVE];
   aCHAR    wdir[_MAX_DIR];
   aCHAR    wfname[_MAX_FNAME];
   aCHAR    wext[_MAX_EXT];

   LPathString psfname(fname);
   psfname.AddExt(ext);

   aCHAR *pathstr;
   int len;
   LPathString ps;
   ps = psfname;

   // first try right here
   fp = Lfopen(ps, aS("r"));
   if (fp)
   {
      fclose(fp);
      len = ps.Length();
      LNEWZ(pathstr, aCHAR[len+1]);
      Lstrcpy(pathstr, ps);
      return(pathstr);
   }

   // try the EXE directory 
   GetModuleFileNameAW(NULL, path, _MAX_PATH);
   L_splitpath(path, wdrive, wdir, wfname, wext);
   ps = (LPathString)wdrive;
   ps += wdir;

   ps += aS("..\\");
   ps += subdir;
   ps += aS("\\");
   ps += psfname;
   fp = Lfopen(ps, aS("r"));
   if (fp)
   {
      fclose(fp);
      len = ps.Length();
      LNEWZ(pathstr, aCHAR[len+1]);
      Lstrcpy(pathstr, ps);
      return(pathstr);
   }

#if defined(LIB_DLL)
   // try the DLL directory 
   GetModuleFileNameAW(g_hInstance, path, _MAX_PATH);
   L_splitpath(path, wdrive, wdir, wfname, wext);
   ps = (LPathString)wdrive;
   ps += wdir;

   ps += aS("..\\");
   ps += subdir;
   ps += aS("\\");
   ps += psfname;
   fp = Lfopen(ps, aS("r"));
   if (fp)
   {
      fclose(fp);
      len = ps.Length();
      LNEWZ(pathstr, aCHAR[len+1]);
      Lstrcpy(pathstr, ps);
      return(pathstr);
   }
#endif  // LIB_DLL

   return NULL;
}

void LEnvironment::computer_name(char* name, int len)
{
   DWORD buflen = len - 1;
	GetComputerNameA(name, &buflen);
	return;
}

/*
char *LEnvironment::hardware_signature()
{
	int info[4];
	char* sig = new char[9];

	__cpuid(info, 0);
	if (info[0] >= 1)	{
		__cpuid(info, 1);
	}

	sprintf(sig, "%08X", info[0]);
	return sig;
}
*/


#elif defined(UNIX)
FILE *LEnvironment::asys_fopen(aCHAR *fname, aCHAR *mode, aCHAR *subdir)
{
   FILE    *fp;
   aCHAR   path[_MAX_PATH];
   aCHAR    *amzi_path;

   // is it here?
   fp = Lfopen(fname, mode);
   if (fp) return(fp);

   // lets check the amzi directory
   amzi_path = amzi_directory();    // try from the env variable
   if (amzi_path != NULL) {
      Lstrcpy(path, amzi_path);
      delete amzi_path;
      Lstrcat(path, aS("/"));
      Lstrcat(path, subdir);
      Lstrcat(path, aS("/"));
      Lstrcat(path, fname);

      fp = Lfopen(path, mode);
      if (fp) return(fp);
   }
   
   return NULL;
}


aCHAR *LEnvironment::amzi_directory()
{
   aCHAR *pathstr;
   LPathString ps;
   aCHAR    *amzi_path;
   // first see if we're running Eclipse
   if (is_ide())
   {
   		ps = application_path();
         if (!ps) return NULL;
   		ps = ps.GetPath();
   		ps += aS("/../../../../apls/");
   }
   else if (is_command()) {
      ps = application_path();
      if (!ps) return NULL;
      ps = ps.GetPath();
      ps += aS("/../");
   }
   else
   {
      amzi_path = Lgetenv_dup(aS("AMZI_DIR"));    // try from the env variable
      if (! amzi_path) return NULL;
      ps = amzi_path;
      ps += aS("/");
      delete amzi_path;
   }
 
   pathstr = new aCHAR[ps.Length() + 1];
   Lstrcpy(pathstr, ps);
   return pathstr;
}

bool LEnvironment::is_ide()
{
   if (Lstrstr(application_path(), aS("eclipse")))
      return true;
   else
      return false;
}

bool LEnvironment::is_command()
{
   aCHAR *ap = application_path();
   if (Lstrstr(ap, aS("alis")) ||
       Lstrstr(ap, aS("acmp")) ||
       Lstrstr(ap, aS("alnk")) ||
       Lstrstr(ap, aS("activate")) )
      return true;
   else
      return false;
}

#if defined(OSX)
aCHAR *LEnvironment::application_path()
{
  char app_path[_MAX_PATH];  // must be bigger than PROC_PIDPATHINFO_SIZE 1024
  int ret;
  ret = proc_pidpath(getpid(), app_path, sizeof(app_path));
  if (ret <= 0)
  	return NULL;
	LPathString ps;
	ps = app_path;
	aCHAR *pathstr = new aCHAR[ps.Length() + 1];
	Lstrcpy(pathstr, ps);
	return pathstr;
}
#else
aCHAR *LEnvironment::application_path()
{
   char app_path[_MAX_PATH];  // must be bigger than PROC_PIDPATHINFO_SIZE 1024
   int ret;
   char tmp[64];
   int len = sizeof(app_path);
   sprintf(tmp, "/proc/%d/exe", getpid());
   int bytes = readlink(tmp, app_path, len-1);
   if (bytes >= len - 1) bytes = len - 1;
   if (bytes <= 0)
      return NULL;
   app_path[bytes] = '\0';
   LPathString ps;
   ps = app_path;
   aCHAR *pathstr = new aCHAR[ps.Length() + 1];
   Lstrcpy(pathstr, ps);
   return pathstr;
}
#endif OSX

aCHAR *LEnvironment::get_user_name()
{
   int len = 120;
   aCHAR *user_name;
   LNEWZ(user_name, aCHAR[len+1]);
   Lstrcpy(user_name, aS("nobody"));
   return user_name;
}

void LEnvironment::computer_name(char* name, int len)
{
	gethostname(name, len-1);
	return;
}


aCHAR *LEnvironment::aget_path(aCHAR *fname, aCHAR *ext, aCHAR *subdir)
{
   // NOTE - Caller must delete the path!
   FILE    *fp;
   aCHAR    path[_MAX_PATH];

   aCHAR    wdrive[_MAX_DRIVE];
   aCHAR    wdir[_MAX_DIR];
   aCHAR    wfname[_MAX_FNAME];
   aCHAR    wext[_MAX_EXT];

   aCHAR    *amzi_path;

   LPathString psfname(fname);
   psfname.AddExt(ext);
   aCHAR *pathstr;
   int len;
   LPathString ps;
   ps = psfname;
   // first try right here
   fp = Lfopen(ps, aS("r"));
   if (fp)
   {
      fclose(fp);
      len = ps.Length();
      LNEWZ(pathstr, aCHAR[len+1]);
      Lstrcpy(pathstr, ps);
      return(pathstr);
   }

   amzi_path = amzi_directory();    // try from the env variable

   if (amzi_path == NULL)
      return(NULL);

   ps = (LPathString)amzi_path;
   ps += aS("/");
   ps += subdir;
   ps += aS("/");
   ps += psfname;
   //Lstrcpy(path, amzi_path);
   delete amzi_path;
   //Lstrcat(path, aS("/"));
   //Lstrcat(path, subdir);
   //Lstrcat(path, aS("/"));
   //Lstrcat(path, fname);

   //fp = Lfopen(path, aS("r"));
   fp = Lfopen(ps, aS("r"));
   if (fp)
   {
      fclose(fp);
      //len = Lstrlen(path);
      len = ps.Length();
      LNEWZ(pathstr, aCHAR[len+1]);
      Lstrcpy(pathstr, ps);
      return(pathstr);
   }

   return NULL;
}

/*
//To use cpuid in GCC, it is normally easiest to just define a macro, like this:

#define cpuid(func,ax,bx,cx,dx)\
	__asm__ __volatile__ ("cpuid":\
	"=a" (ax), "=b" (bx), "=c" (cx), "=d" (dx) : "a" (func));

//In the above, you simply put whatever function number you want in for func, and put in 4 variables that will get the output values of eax, ebx, ecx, and edx, respectively.

char *LEnvironment::hardware_signature()
{
#ifdef BSD
   return "111111";
#else
	int a,b,c,d;
	char sig[35];

	cpuid(0,a,b,c,d);

	if (a >= 1)
	{
		cpuid(1,a,b,c,d);
	}

	sprintf(sig, "%08X %08X %08X %08X", a,b,c,d);

	return sig;
#endif
}
*/

#endif                                         //asys_fopen flavors

// Get a character for immediate response in a listener
// type dialog (or debugger).  This is implementing
// the key$b/1 predicate, used internally by the alib
// predicate respkey/1.  It will not be called if the
// user program implements its own keyb/1.

#if defined(MSC)
int LEnvironment::e_keyZb()
{                                             // e_keyZb
   int a;

   a = _getch();
   putchar(a);   // let the user see what was typed
   return a;
}

#elif defined(LINUX) || defined(SOLARIS) || defined(HPUX)
int LEnvironment::e_keyZb()
{                                             // e_keyZb
   if (! b_tty_initialized)
      e_inittty();

   int c;
   termios  ots, ts;
   int rc;

   rc = tcgetattr(STDIN_FILENO, &ots);
   ts = ots;
   ts.c_lflag &= ~ICANON;
   ts.c_cc[VMIN] = 1;
   ts.c_cc[VTIME] = 0;
   rc = tcsetattr(STDIN_FILENO, TCSANOW, &ts);
   c = fgetc(stdin);
   rc = tcsetattr(STDIN_FILENO, TCSANOW, &ots);
   return c;
}

#else
int LEnvironment::e_keyZb()
{                                             // e_keyZb
   return -1;                                 // will cause a fail           
}
#endif

// At least on the K desktop, the backspace doesn't do its
// thing, so we reset stdin here.
// Actually it appears as if the flag setting of ICANON does all the right stuff.
// It's the 'canonical' flag making bs del etc. work as expected.
#if defined(LINUX) || defined(BSD)
#define CTRLCHAR(ch) ((ch)&0x1f)

termios before_amzi;

void LEnvironment::e_inittty()
{
   if (b_tty_initialized)
      return;

   termios ts;
   int rc;

   rc = tcgetattr(STDIN_FILENO, &ts);
   before_amzi = ts;
   //ts.c_cc[VERASE] = CTRLCHAR('H');
   ts.c_lflag |= ICANON;
   //ts.c_cc[VERASE] = BS;
   rc = tcsetattr(STDIN_FILENO, TCSANOW, &ts);

   b_tty_initialized = true;
}

void LEnvironment::e_resettty()
{
   if (! b_tty_initialized)
      return;

   tcsetattr(STDIN_FILENO, TCSANOW, &before_amzi);
}

#else
void LEnvironment::e_inittty()
{
}

void LEnvironment::e_resettty()
{
}
#endif


//---------------------------------------------------------
// Functions which differ in their Unicode flavors
//

#if defined(_UNICODE)

#if defined(MSC)
long LEnvironment::e_wtol(const wchar_t *s)
{                                               // e_atol
   return _wtol(s);
}
#else
long LEnvironment::e_wtol(const wchar_t *s)
{
   char *as = new_mbs_dup(s);
   long d = atol(as);
   delete as;
   return d;
}
#endif

double LEnvironment::e_wtof(const wchar_t *s)
{                                               // e_atof
   char *as = new_mbs_dup(s);
   double d = atof(as);
   delete as;
   return d;
}

#if defined(MSC) || defined(HPUX)
// MS didn't accept standard wcstok, opting for ANSI compatibility
wchar_t *LEnvironment::e_wcstok(wchar_t *s1, const wchar_t *s2) 
{                                               // e_wcstok
   return wcstok(s1, s2);
}
// used when no wcstok available
#elif defined(WALRUS)
// not really prime time, but BSD on Jim's machine seems to missing wcstok,
// and this will only work for ASCII applications, which is all we intend to run
// on Jim's machine.  dcm
char *as1;
wchar_t *LEnvironment::e_wcstok(wchar_t *s1, const wchar_t *s2) 
{
// e_wcstok
//printf("in e_wcstok\n");
//printf("  s1: %ls :\n", s1);
//if (s1 != NULL) {
//int l = wcslen(s1);
//int i;
//printf(" length of s1 = %d\n", l);
//for (i=0; i<l; i++) {
//   printf("%x.", (int)s1[i]);
//}
//printf("...\n");
//}
   if (s1 != NULL) {
      as1 = new_mbs_dup(s1);
      }
   char *as2 = new_mbs_dup(s2);
//   int len = wcslen(s1);
   char *aresult;
//printf("  as1: %s :\n", as1);   
//printf("  as2: %s :\n", as2);   
   if (s1 != NULL) aresult = strtok(as1, as2);
   else aresult = strtok(NULL, as2);
//printf("result: %s :\n", aresult);
   if (aresult == NULL) return NULL;
   int len = strlen(aresult);
//printf("  len: %d :\n", len);
   wchar_t *wresult = new wchar_t[len+2];
//printf("about to mbstowcs\n");
   mbstowcs(wresult, aresult, len+1);
//printf("wide result: %ls :\n", wresult);
   return wresult;
}

#endif


// Why is just this function missing from the HPUX wcs library?
// Maybe we're missing something here.  Sun has some problem
// as well, maybe its the need for the const_cast?
//
#if defined(HPUX) || defined(SOLARIS)
wchar_t *LEnvironment::e_wcsstr(const wchar_t *s1, const wchar_t *s2)
{                                          // e_wcsstr
   wchar_t *r;
   int n2 = wcslen(s2);
   int n1 = wcslen(s1);
   for(r = const_cast<wchar_t *> (s1); r <= s1 + n1 - n2; r++)
   {
      if (wcsncmp(r, s2, n2) == 0)
         return r;
   }
   return NULL;
}
#endif

#if defined(MSC)
int LEnvironment::e_wsystem(const wchar_t *s)
{                                              // e_wsystem
   return _wsystem(s);
}
#else
int LEnvironment::e_wsystem(const wchar_t *s)
{
   char *as = new_mbs_dup(s);
   int rc = system(as);
   delete as;
   return rc;
}
#endif

#if defined(MSC)
// NT is Unicode, but Win9x is not, so this function
// uses a runtime determination to do the right thing
FILE *LEnvironment::e_wfopen(const wchar_t *fname, const wchar_t *mode)
{                                               // e_wfopen
   if (m_osver == W95)
   {
      char *afname = new_mbs_dup(fname);
      char *amode = new_mbs_dup(mode);
      //BUGLOGWR("afname");
      FILE* fp = fopen(afname, amode);
      delete afname;
      delete amode;
      return fp;
   }
   else
      return _wfopen(fname, mode);
}

#else
FILE *LEnvironment::e_wfopen(const wchar_t *fname, const wchar_t *mode)
{
   char *afname = new_mbs_dup(fname);
   char *amode = new_mbs_dup(mode);
   FILE *f = fopen(afname, amode);
   delete afname;
   delete amode;
   return f;
}

#endif

#if defined(MSC)
wchar_t *LEnvironment::e_wgetenv_dup(const wchar_t *evar)
{                                                 // e_wgetenv_dup
   wchar_t *env = _wgetenv(evar);
   if (env == NULL)
      return NULL;
   wchar_t *buf;
   LNEWZ(buf, wchar_t[wcslen(env)+1]);
   wcscpy(buf, env);
   return buf;
}
#else
wchar_t *LEnvironment::e_wgetenv_dup(const wchar_t *evar)
{
   // NOTE - caller must delete returned buffer
   char *aevar = new_mbs_dup(evar);
   char *env = getenv(aevar);
   if (env == NULL)
      return NULL;
   wchar_t *buf = new_wcs_dup(env);
   delete aevar;
   return buf; 
}
#endif

#if defined(LINUX) || defined(HPUX)
size_t LEnvironment::e_wcsftime(wchar_t *buf, size_t maxsize, 
                                const wchar_t *fmt, const struct tm *timptr)
{                                               // e_wcsftime
   char *abuf;
   LNEWZ(abuf, char[maxsize]);
   char *afmt = new_mbs_dup(fmt);
   size_t rc = strftime(abuf, maxsize, afmt, timptr);
   mbstowcs(buf, abuf, maxsize);
   delete abuf;
   delete afmt;
   return rc;
}
#endif

  // Wide character file i/o not defined on some systems, and this
  // whole area is a mess.  MS tries to outsmart you, and determines
  // whether a file is open in text or binary mode.  If its text, it
  // reads an mb char and converts it; if its binary, it reads a real
  // int16.
  //
  // The Unix world seems to be not much better, in that it
  // always assumes the file is in text mode and only reads and writes
  // in mb mode, converting to wide as necessary.
  //
  // The best fix is probably to write our own that do the right thing
  // and implement them as part of our own file class that knows whether
  // or not the file in question is unicode.  For now, we hack the unix
  // solution in for Linux.

// This whole family was written for Linux, so see if it
// stays together over other ports.
#if defined(LINUX)
wint_t LEnvironment:: e_fgetwc(FILE *f)
{
   return (wint_t)fgetc(f);
}


wint_t LEnvironment::e_ungetwc(wint_t c, FILE *f)
{
   return (wint_t)ungetc((int)c, f);
}

wint_t LEnvironment::e_fputwc(wint_t c, FILE *f)
{
   return (wint_t)fputc((int)c, f);
}

int LEnvironment::e_fputws(const wchar_t *sw, FILE *f)
{
   int l = wcslen(sw);
   if (l == 0)
      return 0;
   l++;
   char* s;
   LNEWZ(s, char[l*2]);
   if (wcstombs(s, sw, l*2) >= l*2)
      s[l*2-1] = 0;
   int rc = fputs(s, f);
   delete s;
   return rc;
}

wchar_t* LEnvironment::e_fgetws(wchar_t *sw, int n, FILE *f)
{
   char* s;
   LNEWZ(s, char[n+1]);
   if (NULL == fgets(s, n, f))
      return NULL;
   int l = strlen(s);
   mbstowcs(sw, s, l);
   sw[l] = 0;
   delete s;
   return sw;
}
#endif // LINUX

//--------------------------------------------------
// The wprintf family, all derived from vsnwpf()
// implemented in lenv_wpf.cpp.
//
#if defined(LINUX) || defined(HPUX)
int LEnvironment::e_wprintf(const wchar_t *fmt, ...)
{
   va_list args;
   int rc;
   
   va_start(args, fmt);
   rc = e_vfwprintf(stdout, fmt, args);
   va_end(args);
   return rc;
}
#endif

#if defined(MSC)
int LEnvironment::e_swprintf(wchar_t *buf, size_t buflen, 
                             const wchar_t *fmt, ...)
{
   va_list args;
   int rc;
   
   va_start(args, fmt);
   rc = _vsnwprintf(buf, buflen, fmt, args);
   va_end(args);
   return rc;
}
#elif defined(GNU)
int LEnvironment::e_swprintf(wchar_t *buf, size_t buflen, 
                             const wchar_t *fmt, ...)
{
   va_list args;
   int rc;

   va_start(args, fmt);
   rc = vsnwpf(buf, buflen, fmt, args);
   va_end(args);
   return rc;
}
#endif

#if defined(LINUX) || defined(HPUX) || defined(SOLARIS) || defined(MACOSX)
int LEnvironment::e_vswprintf(wchar_t *buf, const wchar_t *fmt, va_list args)
{
   int rc;
   rc = vsnwpf(buf, 1024, fmt, args);
   return rc;
}
#endif

#if defined(LINUX) || defined(HPUX) || defined(SOLARIS) || defined(MACOSX)
int LEnvironment::e_vfwprintf(FILE *fp, const wchar_t *fmt, va_list args)
{
   int rc;
   wchar_t buf[1024];
   rc = vsnwpf(buf, 1024, fmt, args);
   e_fputws(buf, fp);
   return rc;
}
#endif

// This safer, but non-standard 'n' version is not being used
// for portability reasons.  But commented out for now in case
// we change our minds.

//#if defined(MSC)
//int LEnvironment::e_vsnwprintf(wchar_t *buf, int n, const wchar_t *fmt, va_list args)
//{
//   return _vsnwprintf(buf, n, fmt, args);
//}
//#elif defined(LINUX)
//int LEnvironment::e_vsnwprintf(wchar_t *buf, int n, const wchar_t *fmt, va_list args)
//{
//   int rc;
//   rc = vsnwpf(buf, n, fmt, args);
//   return rc;
//}
//#endif


// This version of printf not used for now, but just commented
// out in case we need them some day

//#if defined(LINUX)
//int LEnvironment::e_fwprintf(FILE *fp, const wchar_t *fmt, ...)
//{
//   va_list args;
//   int rc;
//   
//   va_start(args, fmt);
//   rc = e_vfwprintf(fp, fmt, args);
//   va_end(args);
//   return rc;
//}
//#endif



#else  // ANSI follows

// ANSI version, in keeping with our wide version, that uses the
// caller's buffer to receive the value.  Make sure to delete
// the environment when done.
char *LEnvironment::e_getenv_dup(const char *evar)
{
   char *env = getenv(evar);
   if (env == NULL)
      return NULL;
   char *buf;
   LNEW(buf, char[strlen(env)+1], aS("environment"));
   strcpy(buf, env);
   return buf;
}

// Making ANSI use of this function use safer form, but ignoring
// the num for now.
int LEnvironment::e_sprintf(char *buf, size_t num, const char *fmt, ...)
{
   va_list args;
   int rc;
   
   va_start(args, fmt);
   rc = vsprintf(buf, fmt, args);
   va_end(args);
   return rc;
}

#endif  // _UNICODE or not

