// graphdoc.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CGraphDoc document

class	CGraphView;

typedef	long value;

class CGraphDoc : public CDocument
{
protected:
	CGraphDoc();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CGraphDoc)

// Attributes
public:
	CGraphView	*m_View;
	CBitmap		*m_bmp;		// the bitmap that stores the current image
	CPen		*m_pen, 
				*m_dummyPen;
	CBrush		*m_brush,
				*m_dummyBrush;
	CFont		*m_font, 
				*m_dummyFont;
	CClientDC	*m_DC;
	CDC			*m_OffScreenDC, 
			    *m_tempDC;
	CSize		m_sizeDoc;
	int			m_sizeX, m_sizeY;
	LOGBRUSH	*m_logBrush;
	LOGPEN		*m_logPen;
	LOGFONT		m_lf;
	COLORREF	m_BkColor;

// Operations
public:
	void InitDocumentDC(CClientDC *pDC);
	CSize GetDocSize() { return m_sizeDoc; }
	// The interface with CAML graphics primitives:
	value ClearGraph();
	value OpenGraph(value mode);
	value CloseGraph();
	value SizeX();
	value SizeY();
	value SetColor(value col);
	value PointColor(value x, value y);
	value MoveTo(value x, value y);
	value CurrentPoint();
	value LineTo(value x, value y);
	value Plot(value x, value y);
	value DrawOrFillArc(value *argv, int argc, BOOL fill);
	value DrawArc(value *argv, int argc);
	value DrawChar(value c);
	value DrawString(value s);
	value TextSize(value s);
	value FillRect(value vx, value vy, value vw, value vh);
	value FillArc(value *argv, int argc);
	value FillPoly(value vect);
	value WaitEvent(value l);
	value Sound(value freq, value duration);
	value CreateImage(value w, value h);
	value BlitImage(value i, value x, value y);
	value DrawImage(value i, value x, value y);
	value MakeImage(value matrix);
	value DumpImage(value img);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGraphDoc)
	protected:
	virtual BOOL OnNewDocument();
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CGraphDoc();
	virtual void Serialize(CArchive& ar);   // overridden for document i/o
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
protected:
	//{{AFX_MSG(CGraphDoc)
	afx_msg void OnUpdateFileOpen(CCmdUI* pCmdUI);
	afx_msg void OnUpdateFileSave(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// Global variables

extern CGraphDoc 	*pGraphDoc;        

