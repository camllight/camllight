// prefsdlg.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CPrefsDlg dialog

class CPrefsDlg : public CDialog
{
// Construction
public:
	CPrefsDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CPrefsDlg)
	enum { IDD = IDD_PREFS };
	UINT	m_OutputMaxSize;
	UINT	m_OutputFlushSize;
	UINT	m_HistMaxSize;
	UINT	m_HistFlushSize;
	BOOL	m_bShowHistory;
	BOOL	m_bAutoScroll;
	BOOL	m_bSaveWorkspace;
	int		m_ReturnAction;
	int		m_EnterAction;
	int		m_CtrlReturnAction;
	//}}AFX_DATA

// Implementation
protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support

	// Generated message map functions
	//{{AFX_MSG(CPrefsDlg)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
