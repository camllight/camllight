// graphfrm.cpp : implementation file
//

#include "stdafx.h"
#include "camlwin.h"
#include "graphfrm.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CGraphFrame

IMPLEMENT_DYNCREATE(CGraphFrame, CMDIChildWnd)

CGraphFrame::CGraphFrame()
{
}

CGraphFrame::~CGraphFrame()
{
}


BOOL CGraphFrame::PreCreateWindow( CREATESTRUCT& cs )
{
	// LONG style = ~(WS_SYSMENU | WS_MINIMIZEBOX | WS_MAXIMIZEBOX);
	// cs.style = cs.style & style;

	return CMDIChildWnd::PreCreateWindow(cs);
}

BEGIN_MESSAGE_MAP(CGraphFrame, CMDIChildWnd)
	//{{AFX_MSG_MAP(CGraphFrame)
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CGraphFrame message handlers

void CGraphFrame::OnClose() 
{
}
