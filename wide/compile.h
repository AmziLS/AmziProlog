//---------------------------------------------------------------------
// compile.h - header for Amzi! compiler
//

class CCompile : public CProProg
{
public:
   CCompile();
   ~CCompile();
   // Overrides
   BOOL Initialize(const _TCHAR* pszPathName);
   BOOL Run();
   BOOL Run(CString sFile);
   BOOL CompileProject();
};

