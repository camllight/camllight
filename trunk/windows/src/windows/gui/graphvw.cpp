// graphvw.cpp : implementation file
//

#include "stdafx.h"
#include "camlwin.h"
#include "graphvw.h"
#include "graphdoc.h"

#include <stdlib.h>

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

extern CGraphDoc *CAMLGraph;
extern CCAMLWinApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CGraphView

IMPLEMENT_DYNCREATE(CGraphView, CScrollView)

CGraphView::CGraphView()
{
}

CGraphView::~CGraphView()
{
	delete m_DC;
}


BEGIN_MESSAGE_MAP(CGraphView, CScrollView)
	//{{AFX_MSG_MAP(CGraphView)
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_CHAR()
	ON_WM_MOUSEMOVE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CXEditView overridden members

// class name for control creation
static const TCHAR szClassName[] = _T("GRAPHVIEW");

BOOL CGraphView::PreCreateWindow( CREATESTRUCT& cs )
{
	cs.style &= ~WS_SYSMENU;
	return CScrollView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CGraphView drawing

void CGraphView::OnInitialUpdate()
{
	CSize sizeTotal, sizePage, sizeLine;

	CGraphDoc *pDoc = (CGraphDoc*)GetDocument();
	sizeTotal = pDoc->GetDocSize();
	sizePage.cx = sizePage.cy = sizeTotal.cx / 10;
	sizeLine.cx = sizeLine.cy = sizePage.cx / 10;
	SetScrollSizes(MM_TEXT, sizeTotal, sizePage, sizeLine);

	m_DC = new CClientDC(this);
	pDoc->InitDocumentDC(m_DC);

 	CScrollView::OnInitialUpdate();

}

void CGraphView::OnDraw(CDC* pDC)
{
	// pDC->SelectObject(theApp.m_Font);
	CBitmap * oldbmp = CAMLGraph->m_OffScreenDC->SelectObject(CAMLGraph->m_bmp);
	pDC->BitBlt(0,0,CAMLGraph->m_sizeX,CAMLGraph->m_sizeY,CAMLGraph->m_OffScreenDC,0,0,SRCCOPY);
    CAMLGraph->m_OffScreenDC->SelectObject(oldbmp);
}
 
void CGraphView::ForceRedraw()
{
	CDC *pDC=GetDC();
	OnDraw(pDC);
	ReleaseDC(pDC);
}
 
/////////////////////////////////////////////////////////////////////////////
// CGraphView diagnostics

#ifdef _DEBUG
void CGraphView::AssertValid() const
{
	CScrollView::AssertValid();
}

void CGraphView::Dump(CDumpContext& dc) const
{
	CScrollView::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CGraphView message handlers

void CGraphView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo) 
{
	// TODO: Add your specialized code here and/or call the base class
	
	CScrollView::OnPrepareDC(pDC, pInfo);
}

extern int charCode, 
           button,
		   mouse_x, mouse_y,
		   GraphEvent;

#define	GR_BUTTON_DOWN	1
#define	GR_BUTTON_UP	2
#define	GR_KEY_DOWN		3
#define	GR_MOUSE_MOTION	4

void CGraphView::OnLButtonDown(UINT nFlags, CPoint point) 
{
	GraphEvent = GR_BUTTON_DOWN;
	button = (nFlags & MK_LBUTTON);		

	CScrollView::OnLButtonDown(nFlags, point);
}

void CGraphView::OnLButtonUp(UINT nFlags, CPoint point) 
{
	GraphEvent = GR_BUTTON_UP;
	button = (nFlags & MK_LBUTTON);		
	
	CScrollView::OnLButtonUp(nFlags, point);
}

void CGraphView::OnChar(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	GraphEvent = GR_KEY_DOWN;
	charCode = nChar;
	
	CScrollView::OnChar(nChar, nRepCnt, nFlags);
}

void CGraphView::OnMouseMove(UINT nFlags, CPoint point) 
{
	GraphEvent = GR_MOUSE_MOTION;
	button = (nFlags & MK_LBUTTON);		
	mouse_x = point.x; 
	mouse_y = point.y;
	 
	CScrollView::OnMouseMove(nFlags, point);
}
