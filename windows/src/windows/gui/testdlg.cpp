// testdlg.cpp : implementation file
//

#include "stdafx.h"
#include "camlwin.h"
#include "testdlg.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTestDlg dialog


CTestDlg::CTestDlg(CCAMLWinApp *theApp, CWnd* pParent)
	: CDialog(CTestDlg::IDD, pParent),
	  m_Input(theApp->m_OutputMaxSize)
{
	//{{AFX_DATA_INIT(CTestDlg)
//	m_Input = 0;
	//}}AFX_DATA_INIT
}


void CTestDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CTestDlg)
	DDX_Text(pDX, IDC_EDIT1, m_Input);
	DDV_MinMaxUInt(pDX, m_Input, 0, 100);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CTestDlg, CDialog)
	//{{AFX_MSG_MAP(CTestDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CTestDlg message handlers
