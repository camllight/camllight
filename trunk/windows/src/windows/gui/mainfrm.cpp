// mainfrm.cpp : implementation of the CMainFrame class
//

#include "stdafx.h"
#include "camlwin.h"

#include <stdlib.h>

#include "mainfrm.h"
#include "graphdoc.h"
#include "graphvw.h"
#include "xeditvw.h"
#include "signal.h"   

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CMainFrame

IMPLEMENT_DYNAMIC(CMainFrame, CMDIFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CMDIFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_COMMAND(ID_CAML_INTERRUPT, OnCamlInterrupt)
	ON_COMMAND(ID_CAML_GC, OnCamlGc)
	ON_COMMAND(ID_DISPLAY_ARRANGEWINDOWS_STANDARD, OnDisplayStdPositions)
	ON_COMMAND(ID_CAML_SHOWHISTORY, OnCamlShowHistory)
	ON_COMMAND(ID_FILE_CLOSE, OnFileClose)
	ON_UPDATE_COMMAND_UI(ID_FILE_CLOSE, OnUpdateFileClose)
	ON_COMMAND(ID_CAML_SEND, OnCamlSend)
	ON_UPDATE_COMMAND_UI(ID_CAML_DEBUG, OnUpdateCamlDebug)
	ON_COMMAND(ID_CAML_DEBUG, OnCamlDebug)
	ON_COMMAND(ID_CONTEXT_HELP, OnContextHelp)
	ON_COMMAND(ID_CAML_SHOWGRAPHICS, OnCamlShowGraphics)
	ON_UPDATE_COMMAND_UI(ID_CAML_SHOWGRAPHICS, OnUpdateCamlShowGraphics)
	ON_UPDATE_COMMAND_UI(ID_CAML_SHOWHISTORY, OnUpdateCamlShowHistory)
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
	// Global help commands
	ON_COMMAND(ID_HELP_INDEX, CMDIFrameWnd::OnHelpIndex)
//	ON_COMMAND(ID_HELP_USING, CMDIFrameWnd::OnHelpUsing)
//	ON_COMMAND(ID_HELP, CMDIFrameWnd::OnHelp)
//	ON_COMMAND(ID_CONTEXT_HELP, CMDIFrameWnd::OnContextHelp)
	ON_COMMAND(ID_DEFAULT_HELP, CMDIFrameWnd::OnHelpIndex)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// arrays of IDs used to initialize control bars

// toolbar buttons - IDs are command buttons
static UINT BASED_CODE buttons[] =
{
	// same order as in the bitmap 'toolbar.bmp'
	ID_FILE_OPEN,	// or INCLUDE
	ID_FILE_SAVE,
		ID_SEPARATOR,
	ID_EDIT_CUT,
	ID_EDIT_COPY,
	ID_EDIT_PASTE,
		ID_SEPARATOR,
	ID_FILE_PRINT,
	ID_APP_ABOUT,
	ID_HELP_INDEX,
};

static UINT BASED_CODE indicators[] =
{
	ID_SEPARATOR,           // status line indicator
	ID_INDICATOR_CAPS,
	ID_INDICATOR_NUM,
	ID_INDICATOR_SCRL,
};

/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

CMainFrame::CMainFrame()
{
	// TODO: add member initialization code here
}

CMainFrame::~CMainFrame()
{
}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CMDIFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;

	if (!m_wndToolBar.Create(this) ||
		!m_wndToolBar.LoadBitmap(IDR_MAINFRAME) ||
		!m_wndToolBar.SetButtons(buttons,
		  sizeof(buttons)/sizeof(UINT)))
	{
		TRACE("Failed to create toolbar\n");
		return -1;      // fail to create
	}

	if (!m_wndStatusBar.Create(this) ||
		!m_wndStatusBar.SetIndicators(indicators,
		  sizeof(indicators)/sizeof(UINT)))
	{
		TRACE("Failed to create status bar\n");
		return -1;      // fail to create
	}

	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CMDIFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CMDIFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers

extern BOOL SendToCAML;


void CMainFrame::OnClose()
{	
	SendCmdToCAML("quit();;");
	SendToCAML = TRUE;
}

extern "C" void handle_signal (unsigned char*, int);
extern "C" unsigned char raise_break_exn[];
extern "C" int async_signal_mode;

extern "C" void gr_lineto(int, int);
extern "C" void gr_moveto(int, int);

void CMainFrame::OnCamlInterrupt() 
{		
	 if (!async_signal_mode)
	 	handle_signal(raise_break_exn, 1);
}

void CMainFrame::OnCamlGc() 
{
	SendCmdToCAML("gc__full_major();;");
}

void CMainFrame::OnDisplayStdPositions() 
{
	theApp.SetStdPositions();	
}

void CMainFrame::OnCamlShowHistory() 
{
	theApp.m_bShowHistory=!theApp.m_bShowHistory;
	if(theApp.m_bShowHistory)
		CAMLHistory->GetParent()->ShowWindow(SW_SHOWNORMAL);
	else
		CAMLHistory->GetParent()->ShowWindow(SW_HIDE);	
		
//	if(CAMLHistory->GetParent()->IsIconic())
//		CAMLHistory->GetParent()->ShowWindow(SW_RESTORE);	
}

void CMainFrame::OnFileClose() 
{	
}

void CMainFrame::OnUpdateFileClose(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable(FALSE);	
}

void CMainFrame::OnCamlSend() 
{
	SendToCAML = TRUE;	
} 

void CMainFrame::OnUpdateCamlDebug(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(theApp.m_bDebugMode);	
}

void CMainFrame::OnCamlDebug() 
{
	theApp.m_bDebugMode=!(theApp.m_bDebugMode);
	if(theApp.m_bDebugMode)
		SendCmdToCAML("debug_mode true;;");
	else
		SendCmdToCAML("debug_mode false;;");
	SendToCAML = TRUE;

}

void CMainFrame::OnContextHelp() 
{
	AfxMessageBox("Sorry, no contextual help available");
}

extern CGraphDoc 	*pGraphDoc;        

void CMainFrame::OnCamlShowGraphics() 
{
	theApp.m_bShowGraphics=!(theApp.m_bShowGraphics);
	if(theApp.m_bShowGraphics)
		pGraphDoc->m_View->GetParent()->ShowWindow(SW_SHOWNORMAL);
	else
		pGraphDoc->m_View->GetParent()->ShowWindow(SW_HIDE);
}

void CMainFrame::ShowGraphics() 
{
	theApp.m_bShowGraphics=FALSE;
	OnCamlShowGraphics();
}

void CMainFrame::HideGraphics() 
{
	theApp.m_bShowGraphics=TRUE;
	OnCamlShowGraphics();
}

void CMainFrame::OnUpdateCamlShowGraphics(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(theApp.m_bShowGraphics);	
}

void CMainFrame::OnUpdateCamlShowHistory(CCmdUI* pCmdUI) 
{
	pCmdUI->SetCheck(theApp.m_bShowHistory);	
}
