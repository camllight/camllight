// xeditvw.cpp : implementation file
//

#include "stdafx.h"
#include "camlwin.h"
#include "xeditvw.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CXEditView

IMPLEMENT_DYNCREATE(CXEditView, CEditView)

CXEditView::CXEditView()
{             
	m_Edit = &GetEditCtrl();

	m_RedirectedTo = NULL;
}

CXEditView::~CXEditView()
{
}


BEGIN_MESSAGE_MAP(CXEditView, CEditView)
	//{{AFX_MSG_MAP(CXEditView)
	ON_COMMAND(ID_EDIT_PASTE, OnEditPaste)
	ON_COMMAND(ID_EDIT_SELECTALL, OnEditSelectall)
	ON_WM_CHAR()
	ON_WM_SETFOCUS()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
              
             
/////////////////////////////////////////////////////////////////////////////
// Global variables


/////////////////////////////////////////////////////////////////////////////
// CXEditView operations


void CXEditView::SetReadOnly(BOOL bReadOnly = TRUE)
{               
	m_IsReadOnly = TRUE;
	m_Edit->SetReadOnly(bReadOnly);
}

void CXEditView::RedirectInputTo(CXEditView *destView)
{
	m_RedirectedTo = destView;
}

/////////////////////////////////////////////////////////////////////////////
// CXEditView overrided members

// class name for control creation
static const TCHAR szClassName[] = _T("EDIT");

BOOL CXEditView::PreCreateWindow(CREATESTRUCT& cs)
{
	ASSERT(cs.lpszClass == NULL);
	cs.lpszClass = szClassName;

	// map default CView style to default CEditView style
/*	if (cs.style == AFX_WS_DEFAULT_VIEW)
		cs.style = dwStyleDefault;
*/
	cs.style = dwStyleDefault;
  	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CXEditView message handlers
 
/*
void CXEditView::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	if (m_RedirectedTo != NULL){
	    m_RedirectedTo->SetFocus();
		m_RedirectedTo->OnKeyDown(nChar, nRepCnt, nFlags);
		}
	else
		CEditView::OnKeyDown(nChar, nRepCnt, nFlags);
}

void CXEditView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	if (m_RedirectedTo != NULL){
	    m_RedirectedTo->SetFocus(); 
		m_RedirectedTo->OnChar(nChar, nRepCnt, nFlags);
		}
	else
		CEditView::OnChar(nChar, nRepCnt, nFlags);
}

void CXEditView::OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	if (m_RedirectedTo != NULL){
	    m_RedirectedTo->SetFocus(); 
		m_RedirectedTo->OnKeyUp(nChar, nRepCnt, nFlags);
		}
	else
		CEditView::OnKeyUp(nChar, nRepCnt, nFlags);
}

void CXEditView::OnSetFocus(CWnd* pOldWnd)
{
	CEditView::OnSetFocus(pOldWnd);
	if (m_RedirectedTo != NULL){
	    m_RedirectedTo->m_Edit->SetSel(-1,0); // SetFocus(); 
		};
}
*/

void CXEditView::OnEditPaste()
{
	if (m_RedirectedTo != NULL){
	    m_RedirectedTo->SetFocus(); 
	    m_RedirectedTo->OnEditPaste(); 
		}
	else
		CEditView::OnEditPaste();
}          

                                
void CXEditView::OnEditSelectall()
{
	m_Edit->SetSel(0,-1);	
}

/////////////////////////////////////////////////////////////////////////////
// CXEditView public operations

void CXEditView::Copy()
{	
	m_Edit->Copy(); 
}	
	
void CXEditView::Paste()
{	
	m_Edit->SetReadOnly(FALSE);
	m_Edit->Paste();
	m_Edit->SetReadOnly(TRUE);
}	
	
void CXEditView::GotoEnd()
{	
	int lg = m_Edit->GetWindowTextLength(); 
	m_Edit->SetSel(lg,lg);
}	
	
void CXEditView::Newline()
{
	char *buf=(char*)malloc(4);
	sprintf(buf,"%c%c",13,10);
	CAMLInput->GotoEnd();
	CAMLInput->m_Edit->ReplaceSel(buf);
	free(buf);
}

void CXEditView::SelectAll()
{
	m_Edit->SetSel(0,-1);	
}

void CXEditView::AppendText(LPCSTR buf)
{
	int oldSize = m_Edit->GetWindowTextLength();
	if(oldSize > m_MaxSize*1024)
		m_Edit->SetSel(0,m_FlushSize*1024);
	m_Edit->ReplaceSel("");
	GotoEnd();
	m_Edit->ReplaceSel(buf);
}				  

void CXEditView::SetText(char *buf)
{
	SelectAll();
	m_Edit->ReplaceSel(buf);
}
	 

// provisoire... 
extern BOOL SendToCAML;

void CXEditView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags)
{ 

	// Extended key (Fn/Num) : (nFlags >> 8),
	// ALT : (nFlags >> 13), 
	// Ctrl+RET : nChar = 10
#define Extended 	(nFlags >> 8)
#define AltDown 	(nFlags >> 13)
#define CtrlReturn  (nChar == 10)

	if(    ( (this == CAMLInput) && (nChar == 13))
		&& (   ( !Extended && (theApp.m_ReturnAction == 0))					// RETURN
			|| ( Extended  && (theApp.m_EnterAction == 0))	)				// ENTER
		|| ( CtrlReturn && (theApp.m_CtrlReturnAction == 0))		) 		// CTRL+RETURN
		SendToCAML = TRUE; 

	if(!SendToCAML)
		CEditView::OnChar(nChar, nRepCnt, nFlags); 
}


void CXEditView::OnSetFocus(CWnd* pOldWnd)
{
	CEditView::OnSetFocus(pOldWnd);
	if (m_RedirectedTo != NULL){
	    m_RedirectedTo->m_Edit->SetFocus(); 
		};
}

