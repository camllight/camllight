// txtrmfr.h : header file
//     

/////////////////////////////////////////////////////////////////////////////
// CTxTermFrame = frame with splitter

#ifndef __AFXEXT_H__
#include <afxext.h>
#endif

class CXEditView;                

class CTxTermFrame : public CMDIChildWnd
{
	DECLARE_DYNCREATE(CTxTermFrame)
protected:
	CTxTermFrame();			// protected constructor used by dynamic creation

// Attributes       
private:
protected:
	CSplitterWnd	m_wndSplitter;
public:
	CXEditView 		*m_OutputView, 	// the topmost pane 
					*m_InputView;   

// Operations
public:

// Implementation
public:
	virtual ~CTxTermFrame();  
	virtual BOOL OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext);
	virtual	BOOL PreCreateWindow( CREATESTRUCT& cs );

	// Generated message map functions
	//{{AFX_MSG(CTxTermFrame)
	afx_msg void OnFileInclude();
	afx_msg void OnFileCompile();
	afx_msg void OnFileLoad();
	afx_msg void OnFileLoadObject();
	afx_msg void OnFileSave();
	afx_msg void OnFileOpen();
	afx_msg void OnFileNew();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
