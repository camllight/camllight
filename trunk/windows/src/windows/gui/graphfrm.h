// graphfrm.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CGraphFrame frame

class CGraphFrame : public CMDIChildWnd
{
	DECLARE_DYNCREATE(CGraphFrame)
protected:
	CGraphFrame();           // protected constructor used by dynamic creation

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGraphFrame)
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CGraphFrame();
	BOOL PreCreateWindow( CREATESTRUCT& cs );

	// Generated message map functions
	//{{AFX_MSG(CGraphFrame)
	afx_msg void OnClose();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
