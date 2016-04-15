
/**************************************************************************
* 
* cpdebug.h - flags that turn debugging on and off in various modules.
* 
**************************************************************************/

#ifndef DEBUG_H
#define DEBUG_H

#ifdef P64
#define   PP_SIZE           16   /* width of print field for pointers */
#else
#define   PP_SIZE            8   /* width of print field for pointers */
#endif

#define BUGOUT  std::cout     // temporary print statements, easy to find later

/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*          Debugging Options          */
/*_____________________________________*/


//#define LASSERT(exp, msg)

#define STUB(x) g_xcpt->Error(stubE, x)
//#define STUB(x) std::cout << x

// For debugging, this flag causes the creation
// of a dump file (landfill).  Various other flags
// control what sort of information is written to
// the dump file.

// Note that the landfill is either a wostream
// or ostream, depending on the platform.  sio defines
// an overload of << that writes *wchar_t to an ostream
// and visa versa.

#ifdef _DEBUG
#define LANDFILL
#define noPROBE
#else
//#define noLANDFILL
#define noLANDFILL
#define noPROBE
#endif

//#define LANDFILL
//#define noPROBE

// Debugging Flags
#define noBUG_SYNC

#ifdef LANDFILL

#define LASSERT(exp, msg) if (!(exp)) g_xcpt->Error(internalE, msg)

#define FILL(s) DUMP << m_peng << ": " << s << NL << FLUSH;
#define PAUSE   /*errInfoMsg("PAUSE")*/

#define noTIMEX    // used to catch loops in main cdexec loop

#ifdef TIMEX
extern float time1;
extern float time2;
#endif

// no landfill

#else

#define LASSERT(exp, msg)
#define FILL(s)
#define PAUSE

#endif

#endif //DEBUG_H
