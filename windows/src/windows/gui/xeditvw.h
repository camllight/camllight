// xeditvw.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CXEditView view

class CXEditView : public CEditView
{
	DECLARE_DYNCREATE(CXEditView)
protected:
	CXEditView();			// protected constructor used by dynamic creation

// Attributes  
private:
	CXEditView	*m_RedirectedTo;
	
public:
	int			m_FlushSize,
				m_MaxSize;         
	BOOL		m_IsReadOnly;
	CEdit       *m_Edit;

// Overridden to fix a bug in CSplitter::CreateView, which transmits
// AFX_WS_DEFAULT_VIEW & ~WS_BORDER to the client to be created. Then,
// CEditView::Create uses dwStyleDefault only if this style 
// is == to AFX_WS_DEFAULT_VIEW, which cannot be the case...

	BOOL PreCreateWindow(CREATESTRUCT& cs);

// Operations
public:                         
	void SetReadOnly(BOOL bReadOnly);
	void RedirectInputTo(CXEditView *destView);
	void AppendText(LPCSTR buf); 
	void SetText(char *buf); 
	void SelectAll();
	void Copy();
	void Paste();
	void GotoEnd();
	void Newline();
	
// Implementation
protected:
	virtual ~CXEditView(); 

	// Generated message map functions
protected:
	//{{AFX_MSG(CXEditView)
	afx_msg void OnEditPaste();
	afx_msg void OnEditSelectall();
	afx_msg void OnChar(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnSetFocus(CWnd* pOldWnd);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// Global variables

extern CXEditView 	*pInputView, *pOutputView, *pHistView, *pTermView;        

/////////////////////////////////////////////////////////////////////////////
