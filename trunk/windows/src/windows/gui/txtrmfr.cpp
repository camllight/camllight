// txtrmfr.cpp : implementation file
//

#include "afxwin.h"	
#include "stdafx.h"
#include "camlwin.h"
#include "txtrmfr.h"
#include "xeditvw.h"

#include <string.h>

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CTxTermFrame

IMPLEMENT_DYNCREATE(CTxTermFrame, CMDIChildWnd)

CTxTermFrame::CTxTermFrame()
{
}                                            

CTxTermFrame::~CTxTermFrame()
{
}

BOOL CTxTermFrame::OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext)
{                                     
	// create a splitter with 2 rows, 1 column
	if (!m_wndSplitter.CreateStatic(this, 2, 1, 
									WS_CHILD | WS_VISIBLE | WS_HSCROLL))
	{
		TRACE("Failed to CreateStaticSplitter\n");
		return FALSE;
	}

	// add the first splitter pane - a default (Read-Only) xeditview in row 0
	if (!m_wndSplitter.CreateView(0, 0,
		pContext->m_pNewViewClass, CSize(80, 240), pContext))
	{
		TRACE("Failed to create first pane\n");
		return FALSE;
	}

	// add the second splitter pane - another default  xeditview in column 1
	if (!m_wndSplitter.CreateView(1, 0,
		pContext->m_pNewViewClass, CSize(80, 80), pContext))
	{
		TRACE("Failed to create second pane\n");
		return FALSE;
	}       
	
	m_OutputView = (CXEditView*)m_wndSplitter.GetPane(0,0);
	m_InputView = (CXEditView*)m_wndSplitter.GetPane(1,0);     
	CAMLInput = m_InputView;
	CAMLPrinter = m_OutputView;        
	
	CAMLInput->ShowScrollBar(SB_HORZ,FALSE);
	CAMLPrinter->ShowScrollBar(SB_HORZ,FALSE);

	// set the Printer as Read-Only and redirect its input towards the input pane... 
	m_OutputView->SetReadOnly(TRUE);
	m_OutputView->RedirectInputTo(m_InputView);
    
	return TRUE;
}

BOOL CTxTermFrame::PreCreateWindow( CREATESTRUCT& cs )
{
//	LONG style = ~(WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX);
//	cs.style = cs.style & style;

	return CMDIChildWnd::PreCreateWindow(cs);
}

BEGIN_MESSAGE_MAP(CTxTermFrame, CMDIChildWnd)
	//{{AFX_MSG_MAP(CTxTermFrame)
	ON_COMMAND(ID_FILE_INCLUDE, OnFileInclude)
	ON_COMMAND(ID_FILE_COMPILE, OnFileCompile)
	ON_COMMAND(ID_FILE_LOAD, OnFileLoad)
	ON_COMMAND(ID_FILE_LOADOBJECT, OnFileLoadObject)
	ON_COMMAND(ID_FILE_SAVE, OnFileSave)
	ON_COMMAND(ID_FILE_OPEN, OnFileOpen)
	ON_COMMAND(ID_FILE_NEW, OnFileNew)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CTxTermFrame message handlers

#define	SOURCE_FILTER			"Source Files (*.ml) | *.ml ||"
#define	SOURCE_INTERFACE_FILTER	"Source Files (*.ml) | *.ml |Interface Files (*.mli) | *.mli ||"
#define	OBJECT_FILTER			"Object Files (*.zo) | *.zo ||"

static void do_file(char *command, CString title, 
					LPCTSTR defExt, LPCTSTR fileName, LPCTSTR filter)
{	
	CFileDialog aDlg( 
		TRUE, 										// BOOL bOpenFileDialog, 
		defExt, 									// LPCTSTR lpszDefExt = NULL,
		fileName,									// LPCTSTR lpszFileName = NULL, 
		OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT, 
		filter);

	aDlg.m_ofn.lpstrTitle=title;

	int res = aDlg.DoModal();
	if(res == IDOK) {
		CString path_name=CString(command) + " \"" + aDlg.GetPathName() + "\";;";
		char *buf = path_name.GetBuffer(256);
		for(unsigned int i=0;i<strlen(buf);i++)
			if(buf[i]=='\\') buf[i]='/';
		SendCmdToCAML(buf);
		path_name.ReleaseBuffer();
	}

}

void CTxTermFrame::OnFileInclude() 
{
	do_file("include",CString("Include file..."),".ml","*.ml",SOURCE_FILTER);

}

void CTxTermFrame::OnFileCompile() 
{
	do_file("compile",CString("Compile file..."),".ml","*.ml",SOURCE_INTERFACE_FILTER);	
}

void CTxTermFrame::OnFileLoad() 
{
	do_file("load",CString("Load file..."),".ml","*.ml",SOURCE_FILTER);
}

void CTxTermFrame::OnFileLoadObject() 
{
	do_file("load_object",CString("Load object file..."),".zo","*.zo",OBJECT_FILTER);
}

char buf[31*1024];

void CTxTermFrame::OnFileOpen() 
{
	if(AfxMessageBox("Caution: this will destroy the contents of the Input pane",MB_OKCANCEL)!=IDCANCEL) {
		CFileDialog aDlg( 
			TRUE, 										// BOOL bOpenFileDialog, 
			NULL, 										// LPCTSTR lpszDefExt = NULL,
			"*.ml",										// LPCTSTR lpszFileName = NULL, 
			OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT 
			);

		aDlg.m_ofn.lpstrTitle="Open File...";

		int res = aDlg.DoModal();
		if(res == IDOK) {
			CString path_name=aDlg.GetPathName();
			CStdioFile h(path_name,CFile::modeRead | CFile::shareDenyWrite | CFile::typeBinary);
			unsigned int len = h.Read(buf,sizeof(buf));
			buf[len]=0;
			h.Close();
			CAMLInput->SetText(buf);
			}
	}
}

void CTxTermFrame::OnFileSave() 
{
	theApp.FileSave(CAMLInput,"Save Input as:");
}

/*
void CTxTermFrame::OnDestroy() 
{
	
	// m_wndSplitter.DeleteView(0,0);
	// m_wndSplitter.DeleteView(0,0);

	CMDIChildWnd::OnDestroy();
}
*/

void CTxTermFrame::OnFileNew() 
{
	OnFileNew();
	
}
