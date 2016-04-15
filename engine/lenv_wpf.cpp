/************************************************************\
*
* lenv_wpf.c -- a wide character vsnprintf
*
* Copyright (c) 1992-2009 by Amzi! inc.  All Rights Reserved.
*
* Pete Humphrey's implementation for those environments
* that don't have one of their own.  Use to build the
* other missing wide character printf functions.
*
* $Log: lenv_wpf.cpp,v $
* Revision 1.1.1.1  2003/09/11 02:15:11  dennis
* Starting release 7.0
*
* Revision 1.5  2002/05/15 16:59:08  dennis
* Final fixes for last 6.1 build, 80
*
* Revision 1.4  2002/05/05 17:29:15  dennis
* more fixes to wcstombs, need to added ending null if string
* didn't fit
*
* Revision 1.3  2002/03/27 02:34:08  dennis
* fixed problems with writing %*g format in lenv_wpf.cpp, need the
* * for implementing decimal_places flag.
*
* Revision 1.2  2002/03/27 02:15:34  dennis
* maybe made wpf work with * in format?
*
* Revision 1.1.1.1  2000/12/29 02:18:06  dennis
* moved to a6
*
* Revision 1.3  2000/02/11 23:41:44  dennis
* some revendian changes, Solaris linker still not working
*
* Revision 1.2  2000/02/08 22:32:55  dennis
* environment-specific changes for HP-UX
*
* Revision 1.1  2000/01/31 14:13:11  dennis
* Cleaned up system dependent code, creating new module, lenv,
* that has it all, or at least most of it.
*
*
\************************************************************/

#include "inc.h"
#include "pch.h"

#if defined(UNIX)
// First a bunch of do_formats to be used by the main
// function later.

void do_format(int val, char *fmt, wchar_t *obuf)
{
   char buf[64];
   sprintf(buf, fmt, val);
   mbstowcs(obuf, buf, 32);
}

void do_format(double val, char *fmt, wchar_t *obuf)
{
   char buf[64];
   sprintf(buf, fmt, val);
   mbstowcs(obuf, buf, 32);
}

void do_format(int cnt, double val, char *fmt, wchar_t *obuf)
{
   char buf[64];
   sprintf(buf, fmt, cnt, val);
   mbstowcs(obuf, buf, 32);
}

void do_format(void *val, char *fmt, wchar_t *obuf)
{
   char buf[64];
   sprintf(buf, fmt, val);
   mbstowcs(obuf, buf, 32);
}

void do_format(wchar_t *val, char *fmt, wchar_t *obuf)
{
   int w1, w2, left;
   int len, pad;

   if (val == NULL)
      val = L"(nil)";
   
   w1 = w2 = left = 0;
   len = wcslen(val);
   
   fmt++;
   
   if (*fmt == '-')
   {
      fmt++;
      left = 1;
   }
   if (sscanf(fmt, "%d.%d", &w1, &w2) != 2)
      sscanf(fmt, "%d", &w1);
   if (w1 == 0)
      w1 = len;
   if (w2 == 0)
      w2 = w1;
   pad = w1 - len;
   if (pad > 0  &&  !left)
   {
      int i;
      for (i = 0;  i < pad;  i++)
         *(obuf++) = L' ';
      *obuf = 0;
   }
   if (len > w2)
   {
      int i;
      for (i = 0;  i < w2;  i++)
         *(obuf++) = val[i];
      *obuf = 0;
   }
   else
   {
      wcscpy(obuf, val);
      obuf += wcslen(obuf);
      if (pad > 0  &&  left)
      {
         int i;
         for (i = 0;  i < pad;  i++)
            *(obuf++) = L' ';
         *obuf = 0;
      }
   }
}

int LEnvironment::vsnwpf(wchar_t *buf, int buflen, const wchar_t *fmt, va_list args)
{
    const wchar_t *ptr;
    wchar_t *optr;
    int result = 0;

    ptr = fmt;
    optr = buf;
    while(*ptr)
    {
        if (*ptr != L'%')
        {
            *(optr++) = *(ptr++);
            continue;
        }
        // we are looking at %
        if (ptr[1] == L'%')
        {
            // escaped percent sign
            *(optr++) = *ptr;
            ptr += 2;
            continue;
        }

        char format[1024];
        wchar_t wformat[1024], *ptr2;
        int ival;
        double dval;
        void *vval;
        wchar_t *wcval;

        // get the whole format spec
        ptr2 = wformat;
        *(ptr2++) = *(ptr++);
        while (*ptr  &&  wcschr(L"dioxXucfeEGgps", *ptr) == NULL)
            *(ptr2++) = *(ptr++);
        if (*ptr)
            *(ptr2++) = *(ptr++);
        *ptr2 = 0;
        if (wcstombs(format, wformat, sizeof(format)) >= sizeof(format))
           format[sizeof(format)-1] = 0;
        switch(*(ptr2-1))
        {
            case 'd':
            case 'i':
            case 'o':
            case 'x':
            case 'X':
            case 'u':
            case 'c':
                ival = va_arg(args, int);
                do_format(ival, format, optr);
                break;
            case 'f':
            case 'e':
            case 'E':
            case 'g':
            case 'G':
                if (strchr(format, '*'))  // a count in format
                {
                   ival = va_arg(args, int);
                   dval = va_arg(args, double);
                   do_format(ival, dval, format, optr);
                }
                else
                {
                   dval = va_arg(args, double);
                   do_format(dval, format, optr);
                }
                break;
            case 'p':
                vval = va_arg(args, void*);
                do_format(vval, format, optr);
                break;
            case 's':
                wcval = va_arg(args, wchar_t *);
                do_format(wcval, format, optr);
                break;
        }
        result++;
        optr += wcslen(optr);
    }
    *optr = 0;
    return result;
}
#endif // UNIX
