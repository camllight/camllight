// graphdoc.cpp : implementation file
//

#include "stdafx.h"
#include "camlwin.h"

#include "xeditvw.h"
#include "txtrmdoc.h"
#include "txtrmfr.h"
#include "mainfrm.h"
#include "graphdoc.h"
#include "graphvw.h"

#include "math.h"

/* from ::runtime: */
// These headers are ANSI-style
#define __STDC__
extern "C" {
#include "..\..\RUNTIME\alloc.h"
#include "..\..\RUNTIME\memory.h"
#include "..\..\RUNTIME\fail.h"
#include "..\..\RUNTIME\signals.h"
#include "..\..\RUNTIME\str.h"
}
#undef __STDC__

#define Short_val(x)	((short) Long_val(x))
 
#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

#include "colors.h"

/////////////////////////////////////////////////////////////////////////////
// CGraphDoc

IMPLEMENT_DYNCREATE(CGraphDoc, CDocument)

CGraphDoc::CGraphDoc()
{
	m_DC = NULL;

	CDC *tmpDC = ((CMainFrame *)theApp.m_pMainWnd)->GetDC();
	m_sizeX=tmpDC->GetDeviceCaps(HORZRES);
	m_sizeY=tmpDC->GetDeviceCaps(VERTRES);
	m_sizeDoc=CSize(m_sizeX, m_sizeY);
	((CMainFrame *)theApp.m_pMainWnd)->ReleaseDC(tmpDC);

	m_logBrush = new LOGBRUSH;
	m_logBrush->lbStyle = BS_SOLID;
	m_logBrush->lbHatch = HS_CROSS;
	m_logBrush->lbColor = BLACK;

	m_logPen   = new LOGPEN;
	m_logPen->lopnStyle=PS_SOLID;
	m_logPen->lopnWidth.x=1;
	m_logPen->lopnColor=BLACK;
}

BOOL CGraphDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;
	return TRUE;
}

CDC *activeDC;
CPoint sp, cp;

void CGraphDoc::InitDocumentDC(CClientDC *pDC)
{
	m_DC = pDC;
	m_OffScreenDC=new CDC;
	m_OffScreenDC->CreateCompatibleDC(pDC);

	m_bmp=new CBitmap;
	m_bmp->CreateCompatibleBitmap(pDC,m_sizeX,m_sizeY);

	m_OffScreenDC->SetBkColor(BLUE);

	m_pen=new CPen;
	VERIFY(m_pen->CreatePen(PS_SOLID, 1, BLACK));

	m_brush=new CBrush;
	VERIFY(m_brush->CreateSolidBrush(BLACK));

	// The default font is that used by the toplevel itself:
	m_lf = theApp.m_lf;

	m_font = new(CFont);
	VERIFY(m_font->CreateFontIndirect(&m_lf));

	// The default background color id set to White:
	m_BkColor = WHITE; // RGB(192,192,192);
	m_OffScreenDC->SetBkColor(m_BkColor);
	m_DC->SetBkColor(m_BkColor);

	activeDC = NULL;

 	m_tempDC=new CDC;
 	VERIFY(m_tempDC->CreateCompatibleDC(pDC));
}

CGraphDoc::~CGraphDoc()
{
	m_bmp->DeleteObject();
	delete m_bmp;

	m_pen->DeleteObject();
	delete m_pen;

	m_font->DeleteObject();
	delete m_font;

	m_brush->DeleteObject();
	delete m_brush;

	delete m_logBrush;
	delete m_logPen;

	delete m_tempDC;
	delete m_OffScreenDC;
}

BEGIN_MESSAGE_MAP(CGraphDoc, CDocument)
	//{{AFX_MSG_MAP(CGraphDoc)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPEN, OnUpdateFileOpen)
	ON_UPDATE_COMMAND_UI(ID_FILE_SAVE, OnUpdateFileSave)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()



/////////////////////////////////////////////////////////////////////////////
// Global variables

CGraphDoc 	*pGraphDoc;        


/////////////////////////////////////////////////////////////////////////////
// CGraphDoc diagnostics

#ifdef _DEBUG
void CGraphDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CGraphDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CGraphDoc serialization

void CGraphDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
}

/////////////////////////////////////////////////////////////////////////////
// The interface with CAML

extern CGraphDoc 	*CAMLGraph;        

#define UD(y)			( m_sizeY - 1 - (y)) 
#define convert_y(y)	( UD(Short_val(y)) )
// Conversion from absolute to relative coordinates:
#define CX_(x)			x - sp.x
#define CY_(y)			y - sp.y

void SelectDC(CDC *pDC)
{
	CAMLGraph->m_oldbmp = CAMLGraph->m_OffScreenDC->SelectObject(CAMLGraph->m_bmp);
	CAMLGraph->m_oldpen = pDC->SelectObject(CAMLGraph->m_pen);	
	CAMLGraph->m_oldfont = pDC->SelectObject(CAMLGraph->m_font);	
	CAMLGraph->m_oldbrush = pDC->SelectObject(CAMLGraph->m_brush);	
	if(pDC != CAMLGraph->m_OffScreenDC) {
		sp=CAMLGraph->m_View->GetScrollPosition();
		cp=CAMLGraph->m_OffScreenDC->GetCurrentPosition();
		pDC->MoveTo(cp.x-sp.x,cp.y-sp.y);
	} else {
        sp.x=0;
        sp.y=0;
    }	
	activeDC = pDC;
}

void UnselectDC()
{
	activeDC->SelectObject(CAMLGraph->m_oldpen);
	activeDC->SelectObject(CAMLGraph->m_oldfont);
	activeDC->SelectObject(CAMLGraph->m_oldbrush);
	CAMLGraph->m_OffScreenDC->SelectObject(CAMLGraph->m_oldbmp);
	activeDC = NULL;
}

void graphic_fail(char *msg)
{
	raise_with_arg(GRAPHIC_FAILURE_EXN, copy_string(msg));
}

#define GRAPHPRIM(method,prim,args,call) \
	extern "C" value prim args \
	{ return CAMLGraph->method call; } \
	value CGraphDoc::##method args

// Each graphical order is executed in the memory DC and in the current physical DC,
// in order to keep the view up-to-date.

GRAPHPRIM( ClearGraph, gr_clear_graph, (), () )
{
	CBrush*	oldbr, *br;
	br = new CBrush(m_BkColor);
	SelectDC(m_OffScreenDC);
	oldbr = m_OffScreenDC->SelectObject(br);
	m_OffScreenDC->FillRect(CRect(0,0,m_sizeX,m_sizeY), br);
	m_OffScreenDC->SelectObject(oldbr);
	UnselectDC();
	br->DeleteObject();
	delete br;
	// We force the view to be updated 
	m_View->ForceRedraw();

	return Val_unit;
}

extern CTxTermDoc *pTermDoc;

GRAPHPRIM( CloseGraph, gr_close_graph, (), () )
{
	((CMainFrame *)(theApp.m_pMainWnd))->HideGraphics();
	CAMLInput->GetParent()->RedrawWindow(NULL,NULL,RDW_INVALIDATE | RDW_UPDATENOW | RDW_ERASE | RDW_INTERNALPAINT);
	return Val_unit;
}

GRAPHPRIM( SizeX, gr_size_x, (), () )
{
	return Val_long(m_sizeX);
}

GRAPHPRIM( SizeY, gr_size_y, (), () )
{
	return Val_long(m_sizeY);
}

GRAPHPRIM( SetColor, gr_set_color, (value col), (col) )
{
	long color=Long_val(col);

// In graphics.ml, rgb is logically defined as rgb r g b = r lsl 16 + g lsl 8 + b
// but this is in the reverse order in Windows...
// I make a conversion here instead of changing lib/graphics.ml.

	int  r = (color & 0xFF0000) >> 16, 
		 g = (color & 0x00FF00) >> 8 , 
		 b =  color & 0x0000FF;
	COLORREF c = RGB(r,g,b);

	// The current pen...
	m_logPen->lopnColor = c;
	m_pen->DeleteObject();
	VERIFY(m_pen->CreatePenIndirect(m_logPen));

	// The current brush...
	m_logBrush->lbColor = c;
	m_brush->DeleteObject();
	VERIFY(m_brush->CreateBrushIndirect(m_logBrush));

	// And finally the color of the text:
	m_DC->SetTextColor(c);
	m_OffScreenDC->SetTextColor(c);

	return Val_unit;
}

GRAPHPRIM( OpenGraph, gr_open_graph, (value mode), (mode) )
{
	POINT bottom = {0, m_sizeY};
	int x0, y0, width0, height0, nconv;

	m_View->ScrollToPosition(bottom);
	SetColor(BLACK);
	gr_clear_graph();
	nconv = sscanf(String_val(mode), " %d x %d + %d + %d", &width0, &height0, &x0, &y0);
	if (nconv < 4) {
		x0 = 10;
		y0 = 10;
	}
	if (nconv < 2) {
		width0 = 600;
		height0 = 400;
	}
	m_View->GetParent()->MoveWindow(x0, y0, width0, height0);
	((CMainFrame *)(theApp.m_pMainWnd))->ShowGraphics();
	m_View->GetParent()->BringWindowToTop();
	m_View->SetFocus();

	return Val_unit;
}

GRAPHPRIM( PointColor, gr_point_color, (value x, value y), (x, y) )
{
	SelectDC(m_OffScreenDC);
	COLORREF color = m_OffScreenDC->GetPixel( Short_val(x), UD(Short_val(y)) );

	unsigned long b = (unsigned long)((color & 0xFF0000) >> 16), 
		 		  g = (unsigned long)((color & 0x00FF00) >> 8) , 
		 		  r = (unsigned long)(color & 0x0000FF);
	UnselectDC();
  	return Val_long( (r<<16) + (g<<8) + b);
}

GRAPHPRIM( MoveTo, gr_moveto, (value x, value y), (x, y) )
{
	m_OffScreenDC->MoveTo(Int_val(x),convert_y(y));
	return Val_unit;
}

GRAPHPRIM( CurrentPoint, gr_current_point, (), () )
{
	value res = alloc_tuple(2);
  	CPoint p = m_OffScreenDC->GetCurrentPosition();
	long x = p.x,
		 y = p.y;

	Field(res, 0) = Val_long(x);
	Field(res, 1) = Val_long(UD(y));

	return res;
}

GRAPHPRIM( LineTo, gr_lineto, (value x, value y), (x, y) )
{
	SelectDC(m_DC);
	activeDC->LineTo(Int_val(x)-sp.x,convert_y(y)-sp.y);
	UnselectDC();

	SelectDC(m_OffScreenDC);
	activeDC->LineTo(Int_val(x)-sp.x,convert_y(y)-sp.y);
	UnselectDC();

	return Val_unit;
}

GRAPHPRIM( Plot, gr_plot, (value x, value y), (x, y) )
{
	gr_moveto(x,y);
	gr_lineto(Val_int(Int_val(x)+1),y);
	return Val_unit;
}

#define PI 3.14159265359
#define deg2rad(x) ((x) * PI / 180.0)

GRAPHPRIM( DrawOrFillArc, gr_draw_or_fill_arc, (value *argv, int argc, BOOL fill), (argv, argc, fill) )
{
	int x, y, r_x, r_y, start, end;

	r_x = Int_val(argv[2]);
	r_y = Int_val(argv[3]);
	if ((r_x < 0) || (r_y < 0))
		invalid_argument("draw_arc: radius must be positives");
	x     = Int_val(argv[0]);
	y     = Int_val(argv[1]);
	start = Int_val(argv[4]);
	end   = Int_val(argv[5]);

	int	x1, y1, x2, y2, x3, y3, x4, y4;
	// Upper-left corner of bounding rect.
	x1=	x - r_x;
	y1=	y + r_y;
	// Lower-right corner of bounding rect.
	x2=	x + r_x;
	y2=	y - r_y;
	// Starting point
	x3=x + (int)(100 * cos(deg2rad(start))); 
	y3=y + (int)(100 * sin(deg2rad(start))); 
	// Ending point
	x4=x + (int)(100 * cos(deg2rad(end))); 
	y4=y + (int)(100 * sin(deg2rad(end))); 

	SelectDC(m_DC);
	if( fill )
		activeDC->Pie(x1-sp.x, UD(y1)-sp.y, x2-sp.x, UD(y2)-sp.y, x3-sp.x, UD(y3)-sp.y, x4-sp.x, UD(y4)-sp.y);
	else
		activeDC->Arc(x1-sp.x, UD(y1)-sp.y, x2-sp.x, UD(y2)-sp.y, x3-sp.x, UD(y3)-sp.y, x4-sp.x, UD(y4)-sp.y);
	UnselectDC();

	SelectDC(m_OffScreenDC);
	if( fill )
		activeDC->Pie(x1-sp.x, UD(y1)-sp.y, x2-sp.x, UD(y2)-sp.y, x3-sp.x, UD(y3)-sp.y, x4-sp.x, UD(y4)-sp.y);
	else
		activeDC->Arc(x1-sp.x, UD(y1)-sp.y, x2-sp.x, UD(y2)-sp.y, x3-sp.x, UD(y3)-sp.y, x4-sp.x, UD(y4)-sp.y);
	UnselectDC();

	return Val_unit;
}

GRAPHPRIM( DrawArc, gr_draw_arc, (value *argv, int argc), (argv, argc) )
{
	return gr_draw_or_fill_arc(argv, argc, FALSE);
}

GRAPHPRIM( DrawChar, gr_draw_char, (value c), (c) )
{		   
	CString buf((char)Int_val(c));

	CSize extent;

	SelectDC(m_DC);
	extent = activeDC->GetTextExtent( buf, 1);
	activeDC->TextOut(cp.x-sp.x,cp.y-sp.y-m_lf.lfHeight,
					  buf,1);
	UnselectDC();

	SelectDC(m_OffScreenDC);
	activeDC->TextOut(cp.x-sp.x,cp.y-sp.y-m_lf.lfHeight,
					  buf,1);
	activeDC->MoveTo(cp.x+extent.cx,cp.y);
	UnselectDC();

	return Val_unit;
}

GRAPHPRIM( DrawString, gr_draw_string, (value s), (s) )
{
	mlsize_t len = string_length(s);
	if (len > 32767) len = 32767;
	CSize extent;

	SelectDC(m_DC);
	extent = activeDC->GetTextExtent( String_val(s), len);
	activeDC->TextOut(cp.x-sp.x,cp.y-sp.y-m_lf.lfHeight,
					  String_val(s),len);

	UnselectDC();

	SelectDC(m_OffScreenDC);
	activeDC->TextOut(cp.x-sp.x,cp.y-sp.y-m_lf.lfHeight,
					  String_val(s),len);
	activeDC->MoveTo(cp.x+extent.cx,cp.y);
	UnselectDC();

	return Val_unit;
}

GRAPHPRIM( TextSize, gr_text_size, (value s), (s) )
{
	mlsize_t len = string_length(s);
	if (len > 32767) len = 32767;
	CSize extent;

	SelectDC(m_DC);
	extent = activeDC->GetTextExtent( String_val(s), len);
	UnselectDC();

	value	res = alloc_tuple(2);
	Field(res, 0) = Val_long(extent.cx);
	Field(res, 1) = Val_long(extent.cy);

	return res;
}

GRAPHPRIM( FillRect, gr_fill_rect, (value vx, value vy, value vw, value vh), (vx, vy, vw, vh) )
{
	// (x,y)=top bottom
	int	x, y, w, h;
	x=Int_val(vx);
	y=Int_val(vy);
	w=Int_val(vw);
	h=Int_val(vh);

	SelectDC(m_DC);
	activeDC->FillRect(CRect(x-sp.x,UD(y+h-1)-sp.y,x+w-sp.x,UD(y-1)-sp.y), m_brush);
	UnselectDC();

	SelectDC(m_OffScreenDC);
	activeDC->FillRect(CRect(x-sp.x,UD(y+h-1)-sp.y,x+w-sp.x,UD(y-1)-sp.y), m_brush);
	UnselectDC();

	return Val_unit;
}

GRAPHPRIM( FillArc, gr_fill_arc, (value *argv, int argc), (argv, argc) )
{
	return gr_draw_or_fill_arc(argv, argc, TRUE);
	return Val_unit;
}

GRAPHPRIM( FillPoly, gr_fill_poly, (value vect), (vect) )
{
	int n_points, i;
	n_points = Wosize_val(vect);
	if (n_points < 3)
		graphic_fail("fill_poly: not enough points");

	POINT	*p,
			*poly = (POINT *)malloc(n_points*sizeof(POINT));

	SelectDC(m_DC);
	p = poly;
	for( i = 0; i < n_points; i++ ){
		p->x = Int_val(Field(Field(vect,i),0));
		p->y = UD(Int_val(Field(Field(vect,i),1)))-sp.y;
		p++;
	}
	activeDC->Polygon(poly,n_points);
	UnselectDC();

	SelectDC(m_OffScreenDC);
	p = poly;
	for( i = 0; i < n_points; i++ ){
		p->x = Int_val(Field(Field(vect,i),0));
		p->y = UD(Int_val(Field(Field(vect,i),1)))-sp.y;
		p++;
	}
	activeDC->Polygon(poly,n_points);
	UnselectDC();

	free(poly);

	return Val_unit;
}

int BDown, BUp , KeyPress, Motion , Poll, charCode,
	GraphEvent,
	button,
	LookGraphEvent;
int mouse_x, mouse_y;

int b_down = 0,
	b_up = 0,
	key_press = 0,
  	motion = 0,
  	poll = 0;

#define	GR_BUTTON_DOWN	1
#define	GR_BUTTON_UP	2
#define	GR_KEY_DOWN		3
#define	GR_MOUSE_MOTION 4

GRAPHPRIM( WaitEvent, gr_wait_event, (value l), (l) )
{
	value result;

	b_down = 0,
	b_up = 0,
	key_press = 0,
	motion = 0,
	poll = 0;

	// enter_blocking_section ();
	while (l != Atom (0)){
	  switch (Tag_val (Field (l, 0))){
	  case 0: b_down = 1; break;
	  case 1: b_up = 1; break;
	  case 2: key_press = 1; break;
	  case 3: motion = 1; break;
	  case 4: poll = 1; break;
	  }
	  l = Field (l, 1);
	}
	
	LookGraphEvent = 1;
	GraphEvent = 0;

    if (key_press && charCode != 0) {
	    GraphEvent = GR_KEY_DOWN;
	} else {
	    while (1) {
		    if( ::PeekMessage(&(theApp.m_msgCur), NULL, NULL, NULL, PM_NOREMOVE) ) {
			    GraphEvent = 0;
				charCode = 0;
				theApp.PumpMessage();
		    }
			// GraphEvent is updated by CAMLGraph's message handlers.
		    if (poll || motion && GraphEvent == GR_MOUSE_MOTION
					 || pending_signal
		        	 || b_down && GraphEvent == GR_BUTTON_DOWN
		        	 || b_up && GraphEvent == GR_BUTTON_UP
					 || key_press && GraphEvent == GR_KEY_DOWN)
		      break;
		}
	}
	LookGraphEvent = 0;

	result = alloc_tuple (5);

	// We get the mouse position:  
	POINT  mousePos;
	if( !GetCursorPos(&mousePos) )
		graphic_fail("Wait_Event: could not get mouse position");
	// We get the position of the bottom left corner of the
	// client area of CAMLGraph view, and then determine the
	// position of the mouse in CAMLGraph's coordinates system.
	RECT	clientRect;
	m_View->GetWindowRect(&clientRect);
	CPoint	sp=m_View->GetScrollPosition();

	// Coordinates of the origin in screen system:
	int X_org = clientRect.left - sp.x,
		Y_org = m_sizeY - sp.y + clientRect.top;
	// Coordinates of the mouse in logical system:
	int X_mouse = mousePos.x - X_org,
	    Y_mouse = Y_org - mousePos.y;

	Field (result, 0) = Val_int (X_mouse);
	Field (result, 1) = Val_int (Y_mouse);
	Field (result, 2) = Atom (button); // TRUE if button is down
	if (GraphEvent == GR_KEY_DOWN){
	  Field (result, 3) = Atom (1);
	  Field (result, 4) = Val_int (charCode);
	}else{
	    Field (result, 3) = Atom (0);
	    Field (result, 4) = Val_int (0);
	}
  // leave_blocking_section ();
  if (GraphEvent == GR_KEY_DOWN && !poll) charCode = 0;

  return result;
}

GRAPHPRIM( Sound, gr_sound, (value freq, value duration), (freq, duration) )
{
	long f = Long_val (freq);
	long d = Long_val (duration);
	Beep((DWORD)f,(DWORD)d);
  	return Val_unit;
}

struct image {
  final_fun final;
  int w;
  int h;
  CBitmap * data;
  CBitmap * mask;
};

#define Width(i) (((struct image *) i)->w)
#define Height(i) (((struct image *) i)->h)
#define Data(i) (((struct image *) i)->data)
#define Mask(i) (((struct image *) i)->mask)

extern "C" static void finalize_image (value i)
{
	delete Data(i);
	if (Mask(i) != NULL) delete Mask(i);
}

GRAPHPRIM( CreateImage, gr_create_image, (value w, value h), (w, h) )
{
	if (Int_val (w) < 0 || Int_val (h) < 0)
	    graphic_fail("create_image: width and height must be positive");
	CBitmap * cbm = new CBitmap;
	cbm->CreateCompatibleBitmap(m_DC, Int_val(w), Int_val(h));
	value res = alloc_shr(sizeof(struct image) / sizeof(value), Final_tag);
	Final_fun (res) = finalize_image;
	Width (res) = Int_val(w);
	Height (res) = Int_val(h);
	Data (res) = cbm;
	Mask (res) = NULL;
	return res;
}

GRAPHPRIM( BlitImage, gr_blit_image, (value i, value x, value y), (i, x, y) )
{
	CBitmap * oldBmp = m_tempDC->SelectObject(Data(i));
	int xsrc = Int_val(x);
	int ysrc = UD(Int_val(y) + Height(i) - 1); 
	m_tempDC->BitBlt(0, 0, Width(i), Height(i),
								m_OffScreenDC, xsrc, ysrc, SRCCOPY);
	m_tempDC->SelectObject(oldBmp);
	return Val_unit;
}

GRAPHPRIM( DrawImage, gr_draw_image, (value i, value x, value y), (i, x, y) )
{
	int xdst = Int_val(x);
	int ydst = UD(Int_val(y) + Height(i) - 1); 
	if (Mask(i) == NULL) {
      CBitmap * oldBmp = m_tempDC->SelectObject(Data(i));
	  m_OffScreenDC->BitBlt(xdst, ydst, Width(i), Height(i),
								       m_tempDC, 0, 0, SRCCOPY);
	  m_tempDC->SelectObject(oldBmp);
	} else {
      CBitmap * oldBmp = m_tempDC->SelectObject(Mask(i));
	  m_OffScreenDC->BitBlt(xdst, ydst, Width(i), Height(i),
								       m_tempDC, 0, 0, SRCAND);
	  m_tempDC->SelectObject(Data(i));
 	  m_OffScreenDC->BitBlt(xdst, ydst, Width(i), Height(i),
								       m_tempDC, 0, 0, SRCPAINT);
  	  m_tempDC->SelectObject(oldBmp);
	}

	SelectDC(m_DC);
	m_DC->BitBlt(xdst-sp.x, ydst-sp.y, Width(i), Height(i), 
	         				m_OffScreenDC, xdst, ydst, SRCCOPY);
	UnselectDC();

  	return Val_unit;
}

GRAPHPRIM( MakeImage, gr_make_image, (value matrix), (matrix) )
{
	int width, height;
	value img;
	height = Wosize_val(matrix);
	if (height == 0) {
		width = 0;
	} else {
		width = Wosize_val(Field(matrix, 0));
		for (int i = 1; i < height; i++) {
	  		if (width != (int) Wosize_val(Field(matrix, i)))
				graphic_fail("make_image: non-rectangular matrix");
		}
	}
	Push_roots(r, 1)
	r[0] = matrix;
	img = gr_create_image(Val_int(width), Val_int(height));
	matrix = r[0];
	Pop_roots();

	int has_transp = 0;
    CBitmap * oldBmp = m_tempDC->SelectObject(Data(img));
    for (int i = 0; i < height; i++) {
      for (int j = 0; j < width; j++) {
        int col = Long_val (Field (Field (matrix, i), j));
	    if (col == -1){
	      has_transp = 1;
		  m_tempDC->SetPixel(j, i, 0);
	    } else {
		  int red = (col >> 16) & 0xFF;
		  int green = (col >> 8) & 0xFF;
		  int blue = col & 0xFF;
		  m_tempDC->SetPixel(j, i, RGB(red, green, blue));
		}
	  }
	}
	m_tempDC->SelectObject(oldBmp);
	if (has_transp) {
        CBitmap * cbm = new CBitmap;
	    cbm->CreateCompatibleBitmap(m_DC, width, height);
		Mask(img) = cbm;
		CBitmap * oldBmp = m_tempDC->SelectObject(Mask(img));
		for (int i = 0; i < height; i++) {
		  for (int j = 0; j < width; j++) {
		    int col = Long_val (Field (Field (matrix, i), j));
			m_tempDC->SetPixel(j, i, col == -1 ? 0xFFFFFF : 0);
		  }
		}
		m_tempDC->SelectObject(oldBmp);
	}
	return img;
}

static value alloc_int_vect(mlsize_t size)
{
	value res;
	mlsize_t i;
	
	if (size == 0) return Atom(0);
	if (size <= Max_young_wosize) {
		res = alloc(size, 0);
	} else {
		res = alloc_shr(size, 0);
	}
	for (i = 0; i < size; i++) {
		Field(res, i) = Val_long(0);
	}
	return res;
}

GRAPHPRIM( DumpImage, gr_dump_image, (value img), (img) )
{
	int height = Height(img);
	int width = Width(img);
	value matrix;
	int i, j;

	Push_roots(r, 2);
	r[0] = img;
    r[1] = alloc_int_vect (height);
  	for (i = 0; i < height; i++) {
    	modify (&Field (r[1], i), alloc_int_vect (width));
  	}
	img = r[0];
	matrix = r[1];
	Pop_roots();

    CBitmap * oldBmp = m_tempDC->SelectObject(Data(img));
	for (i = 0; i < height; i++) {
		for (j = 0; j < width; j++) {
			int col = m_tempDC->GetPixel(j, i);
			int blue = (col >> 16) & 0xFF;
			int green = (col >> 8) & 0xFF;
			int red = col & 0xFF;
			Field(Field(matrix, i), j) = Val_long((red << 16) + (green << 8) + blue);
		}
	}
	m_tempDC->SelectObject(oldBmp);
	if (Mask(img) != NULL) {
	    CBitmap * oldBmp = m_tempDC->SelectObject(Mask(img));
		ASSERT(oldBmp != NULL);
		for (i = 0; i < height; i++) {
			for (j = 0; j < width; j++) {
				if (m_tempDC->GetPixel(j, i) != 0)
					Field(Field(matrix, i), j) = Val_long(-1);
			}
		}
 		m_tempDC->SelectObject(oldBmp);
	}
	return matrix;
}

/////////////////////////////////////////////////////////////////////////////
// CGraphDoc commands

void CGraphDoc::OnUpdateFileOpen(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( FALSE );
}

void CGraphDoc::OnUpdateFileSave(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( FALSE );
}
