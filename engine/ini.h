/*************************************************************
* 
*   ini.h -- Class LIni header, providing ini file services
* 
*   Copyright (c) 1992-2009 by Amzi! inc., All rights reserved
* 
*************************************************************/

#ifndef INI_H
#define INI_H

// The LIni class creates initial values for the various
// ini parameters from defaults.  It can then update the
// values from a .ini file or an input string.
//
// The input string is of the form heap=10000 local=...
// where a space is the delimiter between a-v pairs.
// This will probably have to be changed for the
// long file names with embedded blanks.
enum TROPT { TOFF, TON, TINIT };
enum LUNDEFFLAG { LFAIL, LERROR };

enum valtype { ENDV, UINTCV, INTCV, BOOLV, STRINGV, TRACEV,
         DECIMALSV, FLOATSV, PATHV, UNDEFV };
enum DECIMALS_ {real_, float_};
enum FLOATS_ {single_, double_};

struct IniTab
{
   aCHAR* attr;
   valtype type;
   void    *val;
};

class LIni
{
private:
   LEngine *m_peng;
   enum { INI_BUFLEN = 128   };
   LPathString m_inifile;
   IniTab *m_pit;

public:
   uintC   heap;
   uintC   local;
   uintC   control;
   uintC   trail;
   uintC   readbuffer;
   uintC   readdepth;
   uintC   maxvars;
   //uintC   thingblksz;
   uintC   macroheapsz;
   //uintC   maxmem;
   uintC   destbuf;
   uintC   srcbuf;
   uintC   chunksize;
   uintC   maxfiles;
   uintC   gcthingfreq;
   intC    maxclauses;
   intC    outputdepth;
#ifdef UNIX
   uintC   maxmemory;
#endif
   //intC    maxatoms;
   intC    heapbumper;
   TROPT   tracing;
   LFLAG   string_esc;
   LFLAG   apitrace;
   LFLAG   banner;
   //LFLAG   dbgc;
   LFLAG   breaks;
   LPathString logfile;
   LPathString lsxload;

   LFLAG   occurs_check;
   LFLAG   double_quote_strings;
   LFLAG   preprocessor;
   LFLAG   vars_sort_equal;
   LFLAG   debug64_cut;
   LFLAG   upper_case_atoms;
   LFLAG   vba;
   LFLAG   utf8io;
   LUNDEFFLAG   undefined_predicate;
   intC   decimal_places;
   DECIMALS_ decimals;
   FLOATS_  floats;
   intC    debug_port;
   LString debug_host;

public:
   LIni();
   ~LIni();

   void setEng(LEngine *peng)
   {  m_peng = peng; }
   void FileSet(LPathString);
   void StringSet(LString);
   void Dump();

private:
   void edit();
   void get_ini_file(FILE* fp);
   void get_ini_string(LString is);
   void set_av(LString attr, LString val);
   int  scampi(LString s1, LString s2);
};

#endif //INI_H
