// prefsdlg.cpp : implementation file
//

#include "stdafx.h"
#include "camlwin.h"
#include "prefsdlg.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPrefsDlg dialog

CPrefsDlg::CPrefsDlg(CWnd* pParent)	
	: CDialog(CPrefsDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPrefsDlg)
	m_OutputMaxSize = 0;
	m_OutputFlushSize = 0;
	m_HistMaxSize = 0;
	m_HistFlushSize = 0;
	m_bShowHistory = FALSE;
	m_bAutoScroll = FALSE;
	m_bSaveWorkspace = FALSE;
	m_ReturnAction = 0;
	m_EnterAction = 0;
	m_CtrlReturnAction = 0;
	//}}AFX_DATA_INIT
}

/*OtherConstr(CCAMLWinApp *theApp, CWnd* pParent) 
	: m_OutputMaxSize(theApp->m_OutputMaxSize) 
{
	CDialog(CPrefsDlg::IDD, pParent);
	//{{AAFX_DDATA_IINIT(CPrefsDlg)
	m_OutputFlushSize = 0;
	m_HistMaxSize = 0;
	m_HistFlushSize = 0;
	m_bShowHistory = FALSE;
	m_bAutoScroll = FALSE;
	m_bSaveWindowPos = FALSE;
	m_SendCmd = 0;
	m_NLCmd = 0;
	//}}AFX_DATA_INIT
}
*/


void CPrefsDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPrefsDlg)
	DDX_Text(pDX, IDC_OUTPUT_MAX_SIZE, m_OutputMaxSize);
	DDV_MinMaxUInt(pDX, m_OutputMaxSize, 0, 31);
	DDX_Text(pDX, IDC_OUTPUT_FLUSH_SIZE, m_OutputFlushSize);
	DDV_MinMaxUInt(pDX, m_OutputFlushSize, 0, 31);
	DDX_Text(pDX, IDC_HIST_MAX_SIZE, m_HistMaxSize);
	DDV_MinMaxUInt(pDX, m_HistMaxSize, 0, 31);
	DDX_Text(pDX, IDC_HIST_FLUSH_SIZE, m_HistFlushSize);
	DDV_MinMaxUInt(pDX, m_HistFlushSize, 0, 31);
	DDX_Check(pDX, IDC_SHOW_HISTORY, m_bShowHistory);
	DDX_Check(pDX, IDC_AUTO_SCROLL, m_bAutoScroll);
	DDX_Check(pDX, IDC_SAVE_WINDOW_POS, m_bSaveWorkspace);
	DDX_Radio(pDX, IDC_RET_ACTION, m_ReturnAction);
	DDX_Radio(pDX, IDC_ENTER_ACTION, m_EnterAction);
	DDX_Radio(pDX, IDC_CTRL_RET_ACTION, m_CtrlReturnAction);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CPrefsDlg, CDialog)
	//{{AFX_MSG_MAP(CPrefsDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CPrefsDlg message handlers
