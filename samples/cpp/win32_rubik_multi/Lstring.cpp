// Logic Server LString Class

#include "stdafx.h"
#include "inc.h"

#include <stdarg.h>

LString::LString( const LString &ls, int max, ... )
// Construct an lstring from a format string and args
{
	_TCHAR *temp = new _TCHAR[max];
	va_list args;

	va_start(args, max);
	Lvsnprintf(temp, max, ls.m_rep->m_chars, args);
	va_end(args);

	m_rep = new LStringRep(temp);
	delete [] temp;
}

// The ostream operator << cannot be defined in
// the class LString, because the first argument
// for an operator is the thing being called.  So,
// it is defined separately like this.  In order for
// this to work, operator<< must have been declared
// a friend of LString.	This function returns the
// ostream so it can be linked with other <<s.
ostream& operator<<( ostream &os, const LString &ls )
{
	os << ls.Buffer();
	return os;
}

LString& LString::operator=( const LString &ls )
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
{
	// First guard against self assignment
	if (&ls == this)
		return *this;

	// Increment the arguments reference count and decrement
	// the reference count we used to be using.  Then swap
	// the old rep for the new.
	ls.m_rep->m_count++;
	if (--m_rep->m_count <= 0)
		delete m_rep;
	m_rep = ls.m_rep;
	return *this;
}

LString& LString::operator=( const _TCHAR* s )
{
	if (--m_rep->m_count <= 0)
		delete m_rep;
	m_rep = new LStringRep(s);
	return *this;
}

LString LString::operator+( const LString &ls )
{
	_TCHAR* temp = new _TCHAR[Length() + ls.Length() + 1];
	Lstrcpy(temp, m_rep->m_chars);
	Lstrcat(temp, ls.m_rep->m_chars);
	LString retval(temp);
	delete [] temp;
	return retval;
}

void LString::operator+=( const LString &ls )
// Unlike operator+, += changes the value of
// this Lstring.
{
	_TCHAR *temp;

	temp = new _TCHAR[Length() + ls.Length() + 1];
	Lstrcpy(temp, m_rep->m_chars);
	Lstrcat(temp, ls.m_rep->m_chars);

	if (--m_rep->m_count <= 0)
		delete m_rep;
	m_rep = new LStringRep(temp);

	delete[] temp;
}

_TCHAR& LString::operator[]( int i )
// Overloads the array operator and returns the
// character in question.  But note that it returns
// a reference, or synonym, for m_buffer[i], which
// means that it can be used as an l-value as well.
// Ex.  LString ls("1234");  ls[2] = 'x';
{
	if (i < 0)
		i = 0;
	if (i > Length())
		i = Length();

	return m_rep->m_chars[i];
}

LString LString::Substring(int start, int length) const
{
	_TCHAR *temp;

	// Force reasonable values for start and length.
	int len = Length();
	if (start < 0)
		start = 0;
	if (start >= len)
		return LString(_T(""));
	if (start + length > len)
		length = len - start;

	temp = new _TCHAR[length + 1];
	for (int i=0; i < length; i++)
		temp[i] = m_rep->m_chars[start + i];
	temp[length] = '\0';
	LString retval(temp);
	delete [] temp;
	return retval;
}

int LString::Strchr(_TCHAR c) const
{
	int len=Length();
	for (int i=0; i < len; i++)
		if (m_rep->m_chars[i] == c)
			break;
	if (i == len)
		i = -1;
	return i;
}

inline int LString::Strchrss(_TCHAR c, int start, int end) const
{
	if (start > Length())
		start = Length();
	if (end < start)
		end = start;
	for (int i=start; i < end; i++)
		if (m_rep->m_chars[i] == c)
			break;
	return i;
}

int LString::Strrchr(_TCHAR c) const
{
	int len=Length();
	for (int i=len; i>=0; i--)
		if (m_rep->m_chars[i] == c)
			break;
	return i;
}

void LString::TrimWhiteSpace()
{
	_TCHAR *temp;

	int len = Length();
	temp = new _TCHAR[len + 1];

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
	temp[i-start] = '\0';

	if (--m_rep->m_count <= 0)
		delete m_rep;
	m_rep = new LStringRep(temp);

	delete[] temp;
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
// Parse a token based on a full string.
{
	return LString(_T(""));
}


// LPathString implementation

LString LPathString::GetDrive()	const
{
	int i = Strchr(':');

	if (i < 0)
		return LString(_T(""));

	LString temp=Substring(0,i+1);

	return temp;
}

void LPathString::AddExt(LString ext)
{
	int len = Length();
	// Find the last dot and last slash
	int dot = Strrchr('.');
#ifdef UNIXbased
	int slash = Strrchr('/');
#else
	int slash = Strrchr('\\');
#endif
	// If there's a dot in the last part, then there
	// already is a file extension.
	if (dot > slash)
		return;
	*this += ext;
}