// txtrmdoc.h : interface of the CTxTermDoc class
//
/////////////////////////////////////////////////////////////////////////////

class CTxTermFrame;		                

class CTxTermDoc : public CDocument
{
protected: // create from serialization only
	CTxTermDoc();
	DECLARE_DYNCREATE(CTxTermDoc)

// Attributes
public:
CTxTermFrame 		*m_View; 	// the only view 

// Operations
public:
virtual BOOL CanCloseFrame( CFrameWnd* pFrame ) {return FALSE;};

// Implementation
public:
	virtual ~CTxTermDoc();
	virtual void Serialize(CArchive& ar);   // overridden for document i/o
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	virtual BOOL OnNewDocument();


// Generated message map functions
protected:
	//{{AFX_MSG(CTxTermDoc)
	afx_msg void OnFileClose();
	afx_msg void OnUpdateFileNew(CCmdUI* pCmdUI);
	afx_msg void OnFileSave();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// Global variables

extern CTxTermDoc 	*pTermDoc;        

/////////////////////////////////////////////////////////////////////////////
