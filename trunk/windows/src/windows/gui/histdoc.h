// histdoc.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CHistDoc document

class 			CXEditView;		// forward declaration...                

class CHistDoc : public CDocument
{
	DECLARE_SERIAL(CHistDoc)
protected:
	CHistDoc();			// protected constructor used by dynamic creation

private:

// Attributes
public:
CXEditView 		*m_View; 		// the only view 

// Operations
public:
	virtual BOOL CanCloseFrame( CFrameWnd* pFrame ) {return FALSE;};

// Implementation
protected:
	virtual ~CHistDoc();
	virtual void Serialize(CArchive& ar);	// overridden for document i/o
	virtual	BOOL OnNewDocument();

	// Generated message map functions
protected:
	//{{AFX_MSG(CHistDoc)
	afx_msg void OnFileSave();
	afx_msg void OnUpdateFileOpen(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileNew(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// Global variables

extern CHistDoc *pHistDoc;        

/////////////////////////////////////////////////////////////////////////////
