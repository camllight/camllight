// testdlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CTestDlg dialog

class CTestDlg : public CDialog
{
// Construction
public:
	CTestDlg(CCAMLWinApp *theApp, CWnd* pParent = NULL);   // standard constructor

// Dialog Data
	//{{AFX_DATA(CTestDlg)
	enum { IDD = IDD_TSTDLG };
	//}}AFX_DATA
	UINT&	m_Input;


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CTestDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:

	// Generated message map functions
	//{{AFX_MSG(CTestDlg)
		// NOTE: the ClassWizard will add member functions here
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
