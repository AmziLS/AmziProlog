/*************************************************************
*
*  ini.cpp -- Class LIni implementation,
*      providing ini file services
*
*  Copyright (c) 1992-2009 by Amzi! inc., All rights reserved
*
* 
*************************************************************/

#include "inc.h"
#include "pch.h"

const int ATOM_TABLE_SZ = 32768;

LIni::LIni()
{
   heap       = 500000;  // heap
   local      =  50000;  // local
   control    = 100000;  // control stack
   trail      =  50000;  // trail
   readbuffer =  50000;  // read buffer size
   readdepth  =    512;  // max depth of temp parse stack
   // not really settable - problem - compiler uses last (not first groan)
   // XVar for temporary use, and that's hardwired in..., see assemb.pro,
   // so maxvars IS 4096.  period.
   maxvars    =   4096;  // number of X variables
   //thingblksz =  16000;  // number of things in a thing block
   macroheapsz =  5000;  // space for macros
   //maxmem     =      0;  // max memory, 0 means no max
   // these two are just initial values, dynamically
   // modified by load, so not user setable anymore,
   // although still supported for compatibility
   destbuf    =   4000;  // buffer holding loaded predicates
   srcbuf     =   2000;  // buffer holding loading source predicates
   gcthingfreq = 10000;  // frequency to gc things
   //gcthingfreq = 0;  // frequency to gc things

//#if defined(P16)
//   chunksize = 16000;
//#elif defined(P32)
//   chunksize = 32000;
//#elif defined(P64)
//   chunksize = 64000;
//#endif

   maxclauses = 5000;                    // loadable clauses  ray(500)
   outputdepth = 500;
#ifdef UNIX
   maxmemory = 0;                      // maximum unix memory for process
#endif
   maxfiles = 64;                      // load modules
   //maxatoms   = ATOM_TABLE_SZ;
   heapbumper = 10;              // the percentage of heap used as gc bumper
   tracing    = TOFF;                    // tracing
   string_esc = LON;                     // stringesc
//   apitrace   = LOFF;                    // apitrace
   apitrace   = LOFF;                    // apitrace
   banner     = LON;                     // amzibanner
   breaks     = LON;                     // enable break button
   //dbgc       = LOFF;                    // database garbage collection
   //logfile    = aS("/Users/dennis/Work/temp/amzilog.txt");                  // logfile
   logfile    = aS("");                  // logfile
   lsxload    = aS("");                  // lsxload

   occurs_check = LOFF;
   double_quote_strings = LOFF;
   preprocessor = LON;
   vars_sort_equal = LOFF;
   debug64_cut = LOFF;
   upper_case_atoms = LOFF;
   vba = LOFF;
   utf8io = LOFF;
   undefined_predicate = LFAIL;
   debug_port = 8000;
   debug_host = aS("localhost");
   decimal_places = -1;
   decimals = float_;
   floats = single_;

   IniTab it[] =
   {
      {aS("heap"),       UINTCV, &heap},
      {aS("h"),          UINTCV, &heap},
      {aS("local"),      UINTCV, &local},
      {aS("l"),          UINTCV, &local},
      {aS("control"),    UINTCV, &control},
      {aS("c"),          UINTCV, &control},
      {aS("trail"),      UINTCV, &trail},
      {aS("t"),          UINTCV, &trail},
      {aS("readbuffer"), UINTCV, &readbuffer},
      {aS("rb"),         UINTCV, &readbuffer},
      {aS("readdepth"),  UINTCV, &readdepth},
      {aS("rd"),         UINTCV, &readdepth},
      //{aS("maxvars"),    UINTCV, &maxvars},
      //{aS("mv"),         UINTCV, &maxvars},
      //{aS("thingblksz"), UINTCV, &thingblksz},
      //{aS("tb"),         UINTCV, &thingblksz},
      {aS("macroheapsz"),UINTCV, &macroheapsz},
      {aS("mh"),         UINTCV, &macroheapsz},
      //{aS("maxmem"),     UINTCV, &maxmem},
      //{aS("mm"),         UINTCV, &maxmem},
      {aS("destbuf"),    UINTCV, &destbuf},
      {aS("db"),         UINTCV, &destbuf},
      {aS("srcbuf"),     UINTCV, &srcbuf},
      {aS("sb"),         UINTCV, &srcbuf},
      //{aS("chunksize"),  UINTCV, &chunksize},
      //{aS("ch"),         UINTCV, &chunksize},
      {aS("gcthingfreq"),  UINTCV, &gcthingfreq},
      {aS("gcf"),         UINTCV, &gcthingfreq},
      {aS("maxclauses"),  INTCV, &maxclauses},
      {aS("mc"),          INTCV, &maxclauses},
      {aS("outputdepth"),  INTCV, &outputdepth},
      {aS("od"),          INTCV, &outputdepth},
#ifdef UNIX
      {aS("maxmemory"),  INTCV, &maxmemory},
      {aS("mm"),          INTCV, &maxmemory},
#endif
      {aS("maxfiles"),   INTCV, &maxfiles},
      {aS("mf"),          INTCV, &maxfiles},
      //{aS("maxatoms"),    INTCV, &maxatoms},
      //{aS("ma"),          INTCV, &maxatoms},
      {aS("heapbumper"),  INTCV, &heapbumper},
      {aS("hb"),          INTCV, &heapbumper},
      {aS("tracing"),    TRACEV, &tracing},
      {aS("tr"),         TRACEV, &tracing},
      {aS("string_esc"),  BOOLV, &string_esc},
      {aS("se"),          BOOLV, &string_esc},
      {aS("apitrace"),    BOOLV, &apitrace},
      {aS("at"),          BOOLV, &apitrace},
      {aS("banner"),      BOOLV, &banner},
      {aS("breaks"),      BOOLV, &breaks},
      //{aS("dbgc"),        BOOLV, &dbgc},
      {aS("logfile"),   PATHV, &logfile},
      {aS("lf"),        PATHV, &logfile},
      {aS("lsxload"),   PATHV, &lsxload},
      {aS("loadlsx"),   PATHV, &lsxload},
      {aS("ll"),        PATHV, &lsxload},
      {aS("occurs_check"),          BOOLV, &occurs_check},
      {aS("oc"),                    BOOLV, &occurs_check},
      {aS("double_quote_strings"),  BOOLV, &double_quote_strings},
      {aS("dqs"),                   BOOLV, &double_quote_strings},
      {aS("preprocessor"),          BOOLV, &preprocessor},
      {aS("prep"),                  BOOLV, &preprocessor},
      {aS("vars_sort_equal"),       BOOLV, &vars_sort_equal},
      {aS("vse"),                   BOOLV, &vars_sort_equal},
      {aS("debug64_cut"),           BOOLV, &debug64_cut},
      {aS("dbgc"),                  BOOLV, &debug64_cut},
      {aS("upper_case_atoms"),      BOOLV, &upper_case_atoms},
      {aS("uca"),                   BOOLV, &upper_case_atoms},
      {aS("vba"),                   BOOLV, &vba},
      {aS("utf8io"),                BOOLV, &utf8io},
      {aS("undefined_predicate"),   UNDEFV, &undefined_predicate},
      {aS("up"),                    UNDEFV, &undefined_predicate},
      {aS("decimal_places"),        INTCV, &decimal_places},
      {aS("dp"),                    INTCV, &decimal_places},
      {aS("decimals"),              DECIMALSV, &decimals},
      {aS("dn"),                    DECIMALSV, &decimals},
      {aS("floats"),                FLOATSV, &floats},
      {aS("fl"),                    FLOATSV, &floats},
      {aS("debug_port"),            INTCV, &debug_port},
      {aS("dbgp"),                  INTCV, &debug_port},
      {aS("debug_host"),            STRINGV, &debug_host},
      {aS("dbgh"),                  STRINGV, &debug_host},
      {aS(""), ENDV, NULL}
   };

   int i, len_it;

   for (i=0; it[i].type != ENDV; i++) ;
   len_it = i;

   LNEW(m_pit, IniTab[len_it+1], aS("initialization"));
   //if (m_pit == NULL)
   //   pXCPT->Error(outofmemE, aS("IniTable"));

   for (i=0; i <= len_it; i++)
      m_pit[i] = it[i];
}

LIni::~LIni()
{
   delete[] m_pit;
}

void LIni::FileSet(LPathString ifile)
{
   FILE    *fp;

//Lprintf(aS("in FileSet\n"));
   m_inifile = ifile;
   m_inifile.ForceExt(aS(".cfg"));
   BUGLOGWR("About to open .cfg files");

//Lprintf(aS("about to Lasys_fopen\n"));
   if ((fp = Lasys_fopen(aS("amzi.cfg"), aS("r"), aS("config"))) != NULL)
   {
//Lprintf(aS("Lasys_fopen worked I guess\n"));
      BUGLOGWR("amzi.cfg opened");
      get_ini_file(fp);
      fclose(fp);
   }
//Lprintf(aS("about to Lfopen\n"));
   if ((fp = Lfopen(m_inifile, aS("r"))) )
   {
#ifdef BUG_LOG
      BUGLOGWR("other.cfg opened");
      wcstombs(g_bugbuf, m_inifile, G_BUGBUF_SIZE);
      BUGLOGWR(g_bugbuf);
#endif
      get_ini_file(fp);
      fclose(fp);
   }
//Lprintf(aS("about to edit\n"));
   edit();
//Lprintf(aS("leaving FileSet\n"));
}

void LIni::StringSet(LString av_string)
{                  // calls set_av with attribute-value pairs from av_string
   FILE    *fp;
   if ((fp = Lasys_fopen(aS("amzi.cfg"), aS("r"), aS("config"))) != NULL)
   {
      BUGLOGWR("amzi.cfg opened");
      get_ini_file(fp);
      fclose(fp);
   }

   int len = (int)Lstrlen(av_string);

   aCHAR* buf;
   LNEW(buf, aCHAR[len+1], aS("temporary"));
   //if (buf == NULL)
   //   pXCPT->Error(outofmemE, aS("ini string"));
   Lstrcpy(buf, av_string);

   aCHAR  *attr, *val;
   aCHAR  *attr_delim = aS("=");
   aCHAR  *val_delim = aS(", \n\t");

   attr = Lstrtok(buf, attr_delim);
   while (attr )
     {
       val = Lstrtok(NULL, val_delim);
       set_av(attr, val);
       attr = Lstrtok(NULL, attr_delim);
     }

   delete[] buf;
   edit();
}

void LIni::Dump()
{
#ifdef LANDFILL
   DUMP << aS("heap=")       << heap       << NL;
   DUMP << aS("local=")      << local      << NL;
   DUMP << aS("control=")    << control    << NL;
   DUMP << aS("trail=")      << trail      << NL;
   DUMP << aS("readbuffer=") << readbuffer << NL;
   DUMP << aS("readdepth=")  << readdepth  << NL;
   DUMP << aS("maxvars=")    << maxvars    << NL;
   DUMP << aS("gcthingfreq=") << gcthingfreq << NL;
   DUMP << aS("macroheapsz=") << macroheapsz << NL;
   //DUMP << aS("maxmem=")     << maxmem     << NL;
   DUMP << aS("destbuf=")    << destbuf    << NL;
   DUMP << aS("srcbuf=")     << srcbuf     << NL;
   DUMP << aS("chunksize=")  << chunksize  << NL;
   DUMP << aS("maxclauses=") << maxclauses << NL;
   DUMP << aS("maxfiles=") << maxfiles << NL;
   //DUMP << aS("maxatoms=")   << maxatoms   << NL;
   DUMP << aS("heapbumper=") << heapbumper << NL;
   DUMP << aS("logfile=")    << logfile    << NL;
   DUMP << aS("lsxload=")    << lsxload    << NL;
   DUMP <<   aS("tracing=")    << tracing    << NL;
   DUMP <<   aS("string_esc=") << string_esc << NL;
   DUMP <<   aS("apitrace=")   << apitrace   << NL;
   DUMP <<   aS("banner=")     << banner     << NL;
   DUMP <<   aS("occurs_check")     << occurs_check     << NL;
   DUMP <<   aS("double_quote_strings")     << double_quote_strings     << NL;
   DUMP <<   aS("preprocessor=")     << preprocessor     << NL;
   DUMP <<   aS("vars_sort_equal=")     << vars_sort_equal     << NL;
   DUMP <<   aS("debug64_cut=")     << debug64_cut     << NL;
   DUMP <<   aS("upper_case_atoms=")     << upper_case_atoms     << NL;
   DUMP <<   aS("vba=")     << vba     << NL;
   DUMP <<   aS("utf8io=")     << utf8io     << NL;
   DUMP <<   aS("decimal_places=")     << decimal_places     << NL;
   DUMP <<   aS("decimals=")     << decimals     << NL;
   DUMP <<   aS("floats=")     << floats     << NL;
   DUMP <<   aS("debug_port=")  << debug_port << NL;
   DUMP <<   aS("debug_host=")  << debug_host << NL;
   //DUMP <<   aS("dbgc=")       << dbgc       << NL;
#endif
}

// Private member functions of LIni

void LIni::edit()
// check for correct values for various parameters.
// at this time there is no editing to be done.
{
   // MaxVars cannot be set less than the default.  This MUST
   // be coordinated with cclause.pro, which uses 4096 slot.
   // Can't really be bigger either.
   //if (maxvars < 4096)
   maxvars = 4096;
   return;
}

void LIni::get_ini_file(FILE* fp)
{
   aCHAR  buffer[INI_BUFLEN];
   aCHAR  delim[] = aS(" =,\n\t");
   aCHAR  *attr, *val;
   //aCHAR  attr[INI_BUFLEN], val[INI_BUFLEN];

   //for (int i = 0; i < INI_BUFLEN; i++) buffer[i] = 0;
   //Lprintf(aS("In get_ini_file buffer: %s\n"), buffer);
   //for (int i=0; i < INI_BUFLEN; i++) Lprintf(aS("%x"), buffer[i]);

   while (NULL != Lfgets(buffer, INI_BUFLEN, fp))
   {
#ifdef BUG_LOG
      wcstombs(g_bugbuf, buffer, G_BUGBUF_SIZE);
      BUGLOGWR(g_bugbuf);
#endif
//Lprintf(aS("Buffer = %s\n"), buffer);
      if (Lstrchr(aS(";#/[%"), buffer[0])) 
        continue;
//Lprintf(aS("and now the Lstrtok\n"));
      if ((attr = Lstrtok(buffer, delim)) == NULL) 
        continue;
//for (int i=0; i < INI_BUFLEN; i++) Lprintf(aS("%x "), buffer[i]);

      //Lstrcpy(attr, xattr);
      if ((val = Lstrtok(NULL, delim)) == NULL) 
        continue;
      //Lstrcpy(val, xval);

//Lprintf(aS("calling set_av(%s, %s\n)"), attr, val);
      set_av(attr, val);
   }
   edit();
}

void LIni::set_av(LString attr, LString val)
{
  int i, status;
  
//Lprintf(aS("attr = %s, val = %s\n"), (aCHAR*)attr, (aCHAR*)val);

  attr.TrimWhiteSpace();
  val.TrimWhiteSpace();
  
  for (i=0; m_pit[i].type != ENDV; i++)
    {
      status = scampi(attr, m_pit[i].attr);               // ray
      if (! status)
        {
          switch(m_pit[i].type)
            {
            case UINTCV:
              *(uintC*)m_pit[i].val = (uintC)Latol(val);
              break;
            case INTCV:
              *(intC*)m_pit[i].val = (intC)Latol(val);
              break;
            case BOOLV:
              if      (! scampi(val, aS("TRUE")))  *(TF*)m_pit[i].val = TRUE;
              else if (! scampi(val, aS("ON")))    *(TF*)m_pit[i].val = TRUE;
              else if (! scampi(val, aS("FALSE"))) *(TF*)m_pit[i].val = FALSE;
              else if (! scampi(val, aS("OFF")))   *(TF*)m_pit[i].val = FALSE;
              else ;
              break;
            case UNDEFV:
              if      (! scampi(val, aS("FAIL")))  *(LUNDEFFLAG*)m_pit[i].val = LFAIL;
              else if (! scampi(val, aS("ERROR"))) *(LUNDEFFLAG*)m_pit[i].val = LERROR;
              else ;
              break;
            case TRACEV:
              if     (! scampi(val, aS("TRUE")))  *(intC*)m_pit[i].val = TON;
              else if(! scampi(val, aS("ON")))    *(intC*)m_pit[i].val = TON;
              else if(! scampi(val, aS("FALSE"))) *(intC*)m_pit[i].val = TOFF;
              else if(! scampi(val, aS("OFF")))   *(intC*)m_pit[i].val = TOFF;
              else 
                if (! scampi(val, aS("INIT")))  *(intC*)m_pit[i].val = TINIT;
              else ;
              break;
            case STRINGV:
              *(LString*)m_pit[i].val = val;
              break;
            case PATHV:
              *(LPathString*)m_pit[i].val = val;
              ((LPathString*)(m_pit[i].val))->TiltSlashes();
              break;
            case DECIMALSV:
              if      (! scampi(val, aS("real")))  *(TF*)m_pit[i].val = real_;
              else if (! scampi(val, aS("float"))) *(TF*)m_pit[i].val = float_;
              else ;
              break;
            case FLOATSV:
              if      (! scampi(val, aS("single")))  *(TF*)m_pit[i].val = single_;
              else if (! scampi(val, aS("double"))) *(TF*)m_pit[i].val = double_;
              else ;
              break;
            default:
              break;
            }
          break;
        }
    }
  if (m_pit[i].type == ENDV)
    pXCPT->Error(unknowniniE, (STRptr)attr);
}


int LIni::scampi(LString s1, LString s2)
{                                      // string compare, case insensitive 
   int i;

   for (i=0; tolower(s1[i]) == tolower(s2[i]); i++)
      if (s1[i] == EOS) 
        return 0;

   return(s1[i] - s2[i]);
}








