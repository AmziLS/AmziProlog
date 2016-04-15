//-*-C++-*-
/****************************************************************************
* Logic Server LString Class
*
*****************************************************************************/

#ifndef LSTRING_H
#define LSTRING_H

#define S(x) LString(aS(x))  // shorthand LString creation

const int MAX_LSTRING = 4096;   // maximum expected string, longer is an error

class LString;

// LString has a '<' but we need the function defined as
// well for STL classes that order LStrings.
bool operator<(LString a, LString b);

// From Coplien's 'Advanced C++' - 
// This string implementation uses the 'reference counting idiom' 
// to avoid unnecessary copying of internals during the creation of 
// temporary LStrings.  
// It uses a separate LStringRep class that contains a reference pointer.  
// When a copy is made of an LString the reference count of the internal
// representation is simply incremented.  Destruction decrements.  
// When it gets to 0, then the LStringRep is finally deleted.

// WARNING - this means that one can change the copy and change the
// original.  Take care.

class LStringRep
{
friend class LString;
friend class LTimeString;
friend class LDateString;
friend class LPathString;
friend class LAtom;
protected:
  aCHAR* m_chars;                       // actual C-type (UNICODE) string
  int     m_count;
protected:
  LStringRep( const aCHAR* s )
  {
	  m_chars = new aCHAR[Lstrlen(s)+1];
     Lstrcpy(m_chars, s);
	  m_count = 1;
  }
#ifdef _UNICODE
  LStringRep( const char* s )
  {
	 int len = (int)strlen(s) + 1;
    m_chars = new aCHAR[len];
	 mbstowcs(m_chars, s, len);
	 m_count = 1;
  }
#endif
  ~LStringRep() { delete m_chars; }
};

// The LString class follows Coplien's 'canonical orthodox form.'  
// This means it has a default constructor, a copy constructor, 
// overriding of the assignment operator and a destructor.
//
// It additionally has an override of the aCHAR* declaration 
// so that LStrings can be used in function calls where aCHAR* is used.

class LString
{
protected:
  LStringRep *m_rep;                     // reference to string reference
public:

  // Constructors and Destructors
  
  LString()
  { LNEWZ(m_rep, LStringRep(aS(""))); }
  
  LString( const aCHAR *s )
  {
     if (s) { LNEWZ(m_rep, LStringRep(s)); }
     else { LNEWZ(m_rep, LStringRep(aS(""))); }
  }

#ifdef _UNICODE
  LString( const char *s)
  { if (s) { LNEWZ(m_rep, LStringRep(s)); }
  else { LNEWZ(m_rep, LStringRep(aS(""))); } }
#endif

  LString( const LString &ls )
  {   m_rep = ls.m_rep; m_rep->m_count++; }
  // Construct an LString from a format spec and its variable args.  
  // Int is the maximum size of the resulting LString.
  LString( const LString &, int, ... );
  virtual ~LString()
  {   if (--m_rep->m_count <= 0) delete m_rep; }
  
  // Functions and Operators
  
  // Const on the function means the function will not change member vars.  
  // This is important because only const functions can be called on const 
  // instances of the object, and const instances are frequently created 
  // by functions that take objects by reference.

  int Length() const
  {   return (int)Lstrlen(m_rep->m_chars); }

  // First const on aCHAR* means the pointer can't be modified, 
  // 2nd means contents can't be modified
  // and third means this function doesn't modify them.
  const aCHAR* const Buffer() const
  {   return m_rep->m_chars; }

/* seems not necessary, due to casting, even causes some problems...
  // Overloading the stream operators must be done by non-member functions, 
  // so they need to be declared friends to get at what they need.
  friend Lostream & operator<<( Lostream &, const LString& );
*/

  // Overload aCHAR*() so that anywhere a aCHAR*() appears
  // it will take an lstring as well.  
  // Note that it takes any aCHAR*(), not just const, 
  // so it can be used in sprintf() and friends.
  operator aCHAR*() const { return m_rep->m_chars; }

  // Note assigment changed, so that assignee has clean copy
  // of internal m_rep m_chars.
  LString& operator=( const LString & );
  LString& operator=( const aCHAR * );

  LString operator+( const LString & );
  void operator+=( const LString & );

  aCHAR& operator[]( int );

  int operator==( const LString &ls )
  {   return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars) ? 0 : 1; }

  int operator==( const aCHAR *s )
  {   return Lstrcmp(m_rep->m_chars, s) ? 0 : 1; }

  int operator>( const LString &ls )
  {   return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars)>0 ? 1 : 0; }

  int operator<( const LString &ls )
  {   return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars)<0 ? 1 : 0; }
  //int operator<( const LString &ls )
  //{   return _wcsicmp(m_rep->m_chars, ls.m_rep->m_chars)<0 ? 1 : 0; }

  int operator!=( const LString &ls )
  {   return Lstrcmp(m_rep->m_chars, ls.m_rep->m_chars) ? 1 : 0; }

  LString Substring(int, int) const;
  int Strchr(aCHAR) const;
  int Strchrss(aCHAR, int, int) const;         // gets chars twixt indices.
  int Strrchr(aCHAR) const;
  int Strstr(LString const &s) const;
  int Stricmp(LString const &s) const;
  void TrimWhiteSpace();
  void First(int n);             // Make string just the first n characters
  char *toString(int);
  char *toString()
  {   return toString(256); }
  void toLower();
};

class LTokenString : public LString
{                            // Derive a string class designed to be tokenized.
private:
   int m_index;
public:
   LTokenString( const aCHAR *s ) : LString(s) { m_index = 0; }
   void ResetIndex() { m_index = 0; }
   // Get next token delimited by any character in argument.
   LString GetTokenChar(LString);
   // Get next token delimited by full string in argument.
   LString GetTokenStr(LString);
};

// NOTE - LPathString allows for change in place of its contents,
// which means that if it was created as a copy from another LString,
// it can change that original as well.
class LPathString : public LString
{   // Derive a string class for file paths.

public:
   LPathString() : LString() {};
   LPathString( const aCHAR *s );
   LPathString& operator=( const LString &s )
      {   LString::operator=(s); return *this; }
   //LPathString& operator=( const LPathString &s )
   //   { *this = (LString &) s;  return *this; }
   //LPathString& operator=( const aCHAR* s )
   //   { *this = (LString &) s;  return *this; }
   LPathString& operator=( const LPathString &s )
      { LString::operator=(s);  return *this; }
   LPathString& operator=( const aCHAR* s )
      { LString::operator=(s);  return *this; }
   const LString GetDrive();
   const LString GetExt();
   const LString GetPath();
   LString GetFileName();
   // Add a default extension
   void AddExt(LString);
   void ForceExt(LString);
   void TiltSlashes();  // makes the slashes go the right way for OS
};

class LDateString : public LString
{
public:
   LDateString();
};

class LTimeString : public LString
{
public:
   LTimeString();
};

#endif //LSTRING_H


