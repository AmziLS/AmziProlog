// prodoc.h : interface of the CProDoc class
//
/////////////////////////////////////////////////////////////////////////////

class CProProg;

class CProDoc : public CDocument
{
protected: // create from serialization only
   CProDoc();
   DECLARE_DYNCREATE(CProDoc)

// Attributes
private:
   CProProg *m_pCProProg;

// Operations
public:
   void AttachProg(CProProg* pProg) { m_pCProProg = pProg; }
   CProProg* GetProg() { return(m_pCProProg); }
   
// Implementation
public:
   virtual ~CProDoc();
   virtual void Serialize(CArchive& ar);   // overridden for document i/o
#ifdef _DEBUG
   virtual void AssertValid() const;
   virtual void Dump(CDumpContext& dc) const;
#endif

protected:
//   virtual BOOL OnNewDocument();

// Generated message map functions
protected:
   //{{AFX_MSG(CProDoc)
      // NOTE - the ClassWizard will add and remove member functions here.
      //    DO NOT EDIT what you see in these blocks of generated code !
   //}}AFX_MSG
   DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

