/**************************************************************
*
* lstring.cpp - Logic Server LString Class
*
***************************************************************/ 

#include "inc.h"
#include "pch.h"

LString::LString( const LString &ls, int max, ... )
{                        // Construct an lstring from a format string and args

   aCHAR *temp;
   LNEWZ(temp, aCHAR[max]);
   va_list args;

   va_start(args, max);
// The 'n' version is non-standard, for portability
// we might opt for the more dangerous standard.
// choose one or the other:
//   Lvsnprintf(temp, max, ls.m_rep->m_chars, args);
   Lvsprintf(temp, ls.m_rep->m_chars, args);
   va_end(args);

   LNEWZ(m_rep, LStringRep(temp));
   delete temp;
}

/* seems not necessary due to casting, even causes problems...
// The ostream operator << cannot be defined in
// the class LString, because the first argument
// for an operator is the thing being called.  So,
// it is defined separately like this.  In order for
// this to work, operator<< must have been declared
// a friend of LString.   This function returns the
// ostream so it can be linked with other <<s.
Lostream& operator<<( Lostream &os, const LString &ls )
{
   os << ls.Buffer();
   return os;
}

*/

bool operator<(LString a, LString b)
{
   if (a.operator<(b)) return true;
   else return false;
}

// When you enter a=b, you are really entering the
// function a.operator=(b).  In other words, a = b
// sends a message to 'a' to change its value to 'b'.
//
// The input is a reference, or synonym, for the string
// being passed in, which is passed in as if by value.
// It is const because it is not nice to change arguments
// that are being passed by what looks like value but is
// actually by address.
//
// If this operator= were to simply perform the assignment
// function, then it would return void.  But real assignement
// operators also return the value of the assignment, so
// = can be chained together, such as a = b = c, or
// if (NULL == (a = b)).  To accomodate this use, the
// operator returns the object called, or 'a' in our examples.
// Again, the easiest way to return it is with a synonym for
// the value of the 'this' pointer.
LString& LString::operator=( const LString &ls )
{
   // First guard against self assignment
   if (&ls == this)
      return *this;

   // Inc the arguments ref count and dec the ref count we were using.  
   // Then swap the old rep for the new.
   //ls.m_rep->m_count++;
   //if (--m_rep->m_count <= 0)
   //   delete m_rep;
   //m_rep = ls.m_rep;

   // Don't pass the pointer for assignment, give assignee
   // a new clean copy, so if they modify it, it doesn't change
   // the original

   if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(ls.m_rep->m_chars));

   return *this;
}

LString& LString::operator=( const aCHAR* s )
{
   if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(s));
   return *this;
}

LString LString::operator+( const LString &ls )
{
   aCHAR* temp;
   LNEWZ(temp, aCHAR[Length() + ls.Length() + 1]);
   Lstrcpy(temp, m_rep->m_chars);
   Lstrcat(temp, ls.m_rep->m_chars);
   LString retval(temp);
   delete [] temp;
   return retval;
}

void LString::operator+=( const LString &ls )
{                 // Unlike operator+, += changes the value of this Lstring.
   aCHAR *temp;

   LNEWZ( temp, aCHAR[Length() + ls.Length() + 1]);
   Lstrcpy(temp, m_rep->m_chars);
   Lstrcat(temp, ls.m_rep->m_chars);

   if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(temp));

   delete[] temp;
}

// Overloads the array operator and returns the
// character in question.  But note that it returns
// a reference, or synonym, for m_buffer[i], which
// means that it can be used as an l-value as well.
// Ex.  LString ls("1234");  ls[2] = 'x';
aCHAR& LString::operator[]( int i )
{

   if (i < 0)
      i = 0;
   if (i > Length())
      i = Length();

   return m_rep->m_chars[i];
}

LString LString::Substring(int start, int length) const
{
   aCHAR *temp;

   // Force reasonable values for start and length.
   int len = Length();
   if (start < 0)
      start = 0;
   if (start >= len)
      return LString(aS(""));

   if (start + length > len)
      length = len - start;
   LNEWZ(temp, aCHAR[length + 1]);
   for (int i=0; i < length; i++)
      temp[i] = m_rep->m_chars[start + i];
   temp[length] = EOS;
   LString retval(temp);
   delete temp;
   return retval;
}

int LString::Strchr(aCHAR c) const
{
   int i, len=Length();

   for (i=0; i < len; i++)
      if (m_rep->m_chars[i] == c)
         break;
   if (i == len)
      i = -1;
   return i;
}

int LString::Strchrss(aCHAR c, int start, int end) const
{
  int i;

   if (start > Length())
      start = Length();
   if (end < start)
      end = start;
   for (i=start; i < end; i++)
      if (m_rep->m_chars[i] == c)
         break;
   return i;
}

int LString::Strrchr(aCHAR c) const
{
   int i, len=Length();

   for (i=len; i>=0; i--)
      if (m_rep->m_chars[i] == c)
         break;
   return i;
}

int LString::Strstr(LString const &s) const
{
   aCHAR* p;

   p = Lstrstr(m_rep->m_chars, s.m_rep->m_chars);
   return (int)(p ? p - m_rep->m_chars : -1);
}

int LString::Stricmp(LString const &s) const
{
   aCHAR* p1 = m_rep->m_chars;
   aCHAR* p2 = s.m_rep->m_chars;
   aCHAR c1,c2;

   c1 = *p1++;
   c2 = *p2++;
   while (c1 != EOS && c2 != EOS)
   {
      c1 = tolower(c1);
      c2 = tolower(c2);
      if (c1 < c2)
         return -1;
      if (c1 > c2)
         return 1;
      c1 = *p1++;
      c2 = *p2++;
   }
   if (c1 == c2)
      return 0;
   else
      return c1 == EOS ? -1: 1;
}

void LString::TrimWhiteSpace()
{
   aCHAR *temp;
   int len = Length();

   LNEWZ(temp, aCHAR[len + 1]);

   int i=0;
   while (Lisspace(m_rep->m_chars[i]) && i < len)
      i++;
   int start = i;

   i = len-1;
   while (Lisspace(m_rep->m_chars[i]) && i > start)
      i--;
   int end = i;

   for (i = start; i <= end; i++)
      temp[i-start] = m_rep->m_chars[i];
   temp[i-start] = EOS;

   if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(temp));

   delete temp;
}

void LString::toLower()
{
   aCHAR *temp;
   int len = Length();
   LNEWZ(temp, aCHAR[len+1]);
   int i;
   for (i=0; i<len; i++)
      temp[i] = tolower(m_rep->m_chars[i]);
   temp[len] = EOS;
      if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(temp));
   delete temp;
}

void LString::First(int n)
{
   aCHAR *temp;
   int len = Length();

   if (n >= len)
      return;

   LNEWZ(temp, aCHAR[len + 1]);

   int i;
   for (i=0; i<n; i++)
      temp[i] = m_rep->m_chars[i];
   temp[i] = EOS;

   if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(temp));

   delete temp;
}

// fill in an ascii character string value
// for debugging and the like.  don't use
// for real because of memory leak it causes.
char *LString::toString(int len)
{
   char *buf;
   LNEWZ(buf, char[len+1]);
   if ((int)wcstombs(buf, m_rep->m_chars, len) >= len)
      buf[len-1] = 0;
   return buf;
}

// LTokenString implementation - it simply keeps
// an index into the string indicating where to
// start looking for the next token.  I think
// using indexes is best, given that we'll have
// to use the special MBCS routines for MBCS
// support.

LString LTokenString::GetTokenChar(LString delim)
{
   int hit = Length();
   int h;
   int lend = delim.Length();

   for (int i=0; i < lend; i++)
   {
      h = Strchrss(delim[i], m_index, hit);
      if (h < hit)
         hit = h;
   }
   LString retval = Substring(m_index, hit-m_index);
   m_index = hit+1;
   return retval;
}

LString LTokenString::GetTokenStr(LString delim)
{                                    // Parse a token based on a full string.
   return LString(aS(""));
}


// LPathString implementation

LPathString::LPathString(const aCHAR *s)
{
   if (s) { LNEWZ(m_rep, LStringRep(s)); }
   else { LNEWZ(m_rep, LStringRep(aS(""))); }

   // Fix all the slashes so GetFileName et al work
#ifdef UNIX
   aCHAR  bad_slash = aS('\\');
   aCHAR  good_slash = aS('/');
#else
   aCHAR  bad_slash = aS('/');
   aCHAR  good_slash = aS('\\');
#endif

   aCHAR  *c = m_rep->m_chars;
   while (*c++)
      if ( *c == bad_slash )
         *c = good_slash;
}

const LString LPathString::GetDrive()
{
   int i = Strchr(':');

   if (i < 0)
      return LString(aS(""));

   LString temp=Substring(0,i+1);

   return temp;
}

const LString LPathString::GetExt()
{
   int len = Length();
   int dot = Strrchr(aS('.'));
#ifdef UNIX
   int slash = Strrchr(aS('/'));
#else
   int slash = Strrchr(aS('\\'));
#endif
   // If there's a dot in the last part, then there
   // already is a file extension.
   if (dot > slash)
      return Substring(dot+1, (len-(dot+1)));
   else
      return LString(aS(""));
}

const LString LPathString::GetPath()
{
   int len = Length();
#ifdef UNIX
   int last_slash = Strrchr(aS('/'));
#else
   int last_slash = Strrchr(aS('\\'));
#endif
   int colon = Strchr(aS(':'));
   if (colon == len)
      colon = 0;
   else
      colon++;
   if (last_slash <= 0)
      return LString(aS(""));
   else
      return Substring(colon, last_slash-colon+1);
}

LString LPathString::GetFileName()
{
   int len = Length();
   int dot = Strrchr(aS('.'));
#ifdef UNIX
   int last_slash = Strrchr(aS('/'));
#else
   int last_slash = Strrchr(aS('\\'));
#endif
   // If there's a dot in the last part, then there
   // already is a file extension.
   if (dot > last_slash)
      return Substring(last_slash+1, (dot-(last_slash+1)));
   else
      return Substring(last_slash+1, (len-(last_slash+1)));
}

void LPathString::AddExt(LString ext)
{
   int len = Length();
   // Find the last dot and last slash
   int dot = Strrchr(aS('.'));
#ifdef UNIX
   int slash = Strrchr(aS('/'));
#else
   int slash = Strrchr(aS('\\'));
#endif
   // If there's a dot in the last part, then there
   // already is a file extension.
   if (dot > slash)
      return;
   *this += ext;
}

void LPathString::ForceExt(LString ext)
{
   int dot = Strrchr(aS('.'));
   if (dot == -1)
      AddExt(ext);
   else
   {
      First(dot);
      *this += ext;
   }
}

void LPathString::TiltSlashes()
{
#ifdef UNIX
   aCHAR  bad_slash = aS('\\');
   aCHAR  good_slash = aS('/');
#else
   aCHAR  bad_slash = aS('/');
   aCHAR  good_slash = aS('\\');
#endif

   aCHAR  *c = m_rep->m_chars;
   while (*c++)
      if ( *c == bad_slash )
         *c = good_slash;
}

LDateString::LDateString() : LString()
{
   time_t   t;
   tm*      st;

   aCHAR *temp;
   LNEWZ(temp, aCHAR[60]);

   time(&t);
   st = localtime(&t);

   Lstrftime(temp, 60, aS("%B %d, %Y"), st);

   if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(temp));

   delete[] temp;
}

LTimeString::LTimeString() : LString()
{
   time_t   t;
   tm*      st;

   aCHAR *temp;
   LNEWZ(temp, aCHAR[60]);

   time(&t);
   st = localtime(&t);

   Lstrftime(temp, 60, aS("%I:%M %p"), st);

   if (--m_rep->m_count <= 0)
      delete m_rep;
   LNEWZ(m_rep, LStringRep(temp));

   delete[] temp;
}
