// Logic Server LString Class

#ifndef LSTRING_H
#define LSTRING_H

// This string class uses Microsoft's notation for
// strings that allows code to be compiled for
// various character codes, being single byte,
// multibyte MBCS, and Unicode.  The header file
// def.h has #IFDEFs that remove the Microsoft
// notation for non MS platforms.

#if defined(P32) && defined(WINbased)
#include <tchar.h>

#define Lstrlen    _tcslen
#define Lstrcmp    _tcscmp
#define Lstrncmp   _tcsncmp
#define Lstrcpy    _tcscpy
#define Lstrncpy   _tcsncpy
#define Lstrstr    _tcsstr
#define Lstrcat    _tcscat
#define Lstrcspn   _tcscspn
#define Lstrchr    _tcschr
#define Lstrrchr   _tcsrchr
#define Lstrtok    _tcstok

#define Lisspace   _istspace

#define Latol      _ttol

#define Lfgets     _fgetts
#define Lvsnprintf _vsntprintf

#define Lmemset    memset
#define Lmemcpy    memcpy

#else

#define Lstrlen    strlen
#define Lstrcmp    strcmp
#define Lstrncmp   strncmp
#define Lstrcpy    strcpy
#define Lstrncpy   strncpy
#define Lstrstr    strstr
#define Lstrcat    strcat
#define Lstrcspn   strcspn
#define Lstrchr    strchr
#define Lstrrchr   strrchr
#define Lstrtok    strtok

#define Lisspace   isspace

#define Latol      atol

#define Lfgets     fgets
#define Lvsnprintf _vsnprintf

#define Lmemset    memset
#define Lmemcpy    memcpy

#endif


#define NL  _T("\n")
#define SP  _T(" ")

typedef   _TCHAR *   STRptr;
typedef   STRptr *   STRhnd;

#define S(x) LString(_T(x))  // shorthand LString creation

const MAX_LSTRING = 4096;   // maximum expected string, longer is an error

class LString;

// From Coplien's 'Advanced C++' - This string
// implementation uses the 'reference counting
// idiom' to avoid unnecessary copying of internals
// during the creation of temporary LStrings.  It
// uses a separate LStringRep class that contains
// a reference pointer.  When a copy is made of a
// LString, the reference count of the internal
// representation is simply incremented.  Destruction
// decrements.  When it gets to 0, then the
// LStringRep is finally deleted.
class LStringRep
{
friend class LString;
private:
	_TCHAR* m_chars;
	int     m_count;
private:
	LStringRep( const _TCHAR* s )
	{
		Lstrcpy(m_chars = new _TCHAR[Lstrlen(s)+1], s);
		m_count = 1;
	}
	~LStringRep() { delete[] m_chars; }
};

// The LString class follows Coplien's 'canonical
// orthodox form.'  This means it has a default
// constructor, a copy constructor, overriding of
// the assignment operator and a destructor.
//
// It additionally has an override of the _TCHAR*
// declaration so that LStrings can be used in
// function calls where _TCHAR* is used.
class LString
{
private:
	LStringRep *m_rep;
public:

	// Constructors and Destructors

	LString()
	{	m_rep = new LStringRep(_T("")); }
	// Const in the argument list means the argument
	// will not be changed.
	LString( const _TCHAR *s )
	{	m_rep = new LStringRep(s); }
	LString( const LString &ls )
	{	m_rep = ls.m_rep;
		m_rep->m_count++; }
	// Construct an LString from a format specification
	// and its variable arguments.  Int is the maximum
	// size of the resulting LString.
	LString( const LString &, int, ... );
	virtual ~LString()
	{	if (--m_rep->m_count <= 0) delete m_rep; }

	// Functions and Operators

	// Const on the function means the function will
	// not change member variables.  This is important
	// because only const functions can be called
	// on const instances of the object, and const
	// instances are frequently created by functions
	// that take objects by reference.
	int Length() const
	{	return Lstrlen(m_rep->m_chars); }
	// First const on _TCHAR* means the pointer can't be
	// modified, 2nd means contents can't be modified
	// and third means this function doesn't modify them.
	const _TCHAR* const Buffer() const
	{	return m_rep->m_chars; }
	// Overloading the stream operators must be done
	// by non-member functions, so they need to be
	// declared friends to get at what they need.
	friend ostream& operator<<( ostream&, const LString& );
	// Overload _TCHAR*() so that anywhere a _TCHAR*() appears
	// it will take an lstring as well.  Note that it
	// takes any _TCHAR*(), not just const, so it can
	// be used in sprintf() and friends.
	operator _TCHAR*() const { return m_rep->m_chars; }
	LString& operator=( const LString & );
	LString& operator=( const _TCHAR * );
	LString operator+( const LString & );
	void operator+=( const LString & );
	_TCHAR& operator[]( int );
	int operator==( const LString &ls )
	{	return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars) ? 0 : 1; }
	int operator>( const LString &ls )
	{	return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars)>0 ? 1 : 0; }
	int operator<( const LString &ls )
	{	return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars)<0 ? 1 : 0; }
	int operator!=( const LString &ls )
	{	return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars) ? 1 : 0; }
	LString Substring(int, int) const;
	int Strchr(_TCHAR) const;
	// Same as Strchr, but only look at characters between a
	// starting and ending index.
	int Strchrss(_TCHAR, int, int) const;
	int Strrchr(_TCHAR) const;
	void TrimWhiteSpace();
};

// Derive a string class designed to be tokenized.
class LTokenString : public LString
{
private:
	int m_index;
public:
	LTokenString( const _TCHAR *s ) : LString(s) { m_index = 0; }
	void ResetIndex() { m_index = 0; }
	// Get next token delimited by any character in argument.
	LString GetTokenChar(LString);
	// Get next token delimited by full string in argument.
	LString GetTokenStr(LString);
};

// Derive a string class for file paths.
class LPathString : public LString
{
public:
	LPathString() : LString() {};
	LPathString( const _TCHAR *s ) : LString(s) {};
	LPathString& operator=( const LString &s )
	{	LString::operator=(s); return *this; }
	LString GetDrive() const;
	// Add a default extension
	void AddExt(LString);
};

#endif LSTRING_H