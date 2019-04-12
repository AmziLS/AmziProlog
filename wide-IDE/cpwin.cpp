//-----------------------------------------------------------
// Extended Windows Predicates, MFC version
//

#include "stdafx.h"
#include <direct.h>
#include <io.h>
#include "amzi.h"
#include "conview.h"
#include "proprog.h"
#include "cpwin.h"

/*
TF EXPFUNC p_keyb(ENGid);

PRED_INIT winPreds[] =
{
// keyboard predicates 
   {_T("keyb"), 1, p_keyb},
   {NULL, 0, NULL}
};


TF EXPFUNC p_keyb(ENGid eid)
{
// get the ASCII value for the next key struck
//   use the listener window's GetCh method

   int   a;
   a = (int)(g_pConView->GetCh());
   if (! lsUnifyParm(eid, 1, cINT, &a)) return FALSE;
   return TRUE;
}
*/

///////////////////////////////////////////////////////////
// Utility Functions
//

void slashslash2(_TCHAR* sout, const _TCHAR* sin)

/* Any string of _TCHARacters sent to Prolog is read using the
   Prolog reader, which interprets a backslash as an escape
   _TCHARacter.  So, this means if you really want a backslash,
   such as in a file path, then it has to be a double
   backslash.  This function makes that conversion. */
{
   while(*sin)
   {
      if (*sin == _T('\\'))
      {
         *sout++ = *sin;
         *sout++ = *sin++;
      }
      else
         *sout++ = *sin++;
   }
   *sout = *sin;
   return;
}

void slashslash1(_TCHAR* s)

/* Same as slashslash2 except conversion is done in place.
   Therefor, the input buffer must be large enough to handle
   the expanded string. */
{
   int   nslash=0;
   int   len=0;
   int   i;
   _TCHAR* s1;
   _TCHAR* s2;
   
   /* Count the slashes, then copy in place from right to
      left so we don't write on ourself. */
      
   s1 = s;
   while(*s1)
   {
      len++;
      if (*s1++ == _T('\\')) nslash++;
   }
   
   s2 = s + len + nslash;
   s1 = s + len;
   
   for(i=0; i<=len; i++)
   {
      if (*s1 == _T('\\'))
      {
         *s2-- = *s1;
         *s2-- = *s1--;
      }
      else
         *s2-- = *s1--;
   }
   
   return;
}
