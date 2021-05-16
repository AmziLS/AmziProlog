//--------------------------------------------------------------------
// link.h - header for Windows front end on Amzi! linker
//

#ifdef _WIN32
//#define EXPFUNC
//#define CLNKPAS __stdcall
#define CLNKPAS __cdecl
typedef int (CLNKPAS *pfLINK)(void(*)(_TCHAR*), int, _TCHAR **);
#else
//#define EXPFUNC __export
#define CLNKPAS __pascal
typedef int (CLNKPAS *pfLINK)(void(*)(_TCHAR*), int, _TCHAR **);
#endif


class CConView;

class CLink : public CDocument
{
   DECLARE_DYNCREATE(CLink)
private:
   HINSTANCE m_hLinkDLL;
   pfLINK    m_pfL;

public:
   CLink();
   ~CLink();
   BOOL Run(CConView* pCV);
   BOOL Run(CConView* pCV, CString sFile);
   BOOL LinkProject(CConView* pCV);
};

