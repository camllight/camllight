// mainfrm.h : interface of the CMainFrame class
//
/////////////////////////////////////////////////////////////////////////////

class CMainFrame : public CMDIFrameWnd
{
	DECLARE_DYNAMIC(CMainFrame)
public:
	CMainFrame();

// Attributes					
public:

// Operations
public:
	void ShowGraphics();
	void HideGraphics();

// Implementation
public:
	virtual ~CMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:  // control bar embedded members
	CStatusBar  m_wndStatusBar;
	CToolBar    m_wndToolBar;

// Generated message map functions
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnCamlInterrupt();
	afx_msg void OnCamlGc();
	afx_msg void OnDisplayStdPositions();
	afx_msg void OnCamlShowHistory();
	afx_msg void OnFileClose();
	afx_msg void OnUpdateFileClose(CCmdUI* pCmdUI);
	afx_msg void OnCamlSend();
	afx_msg void OnUpdateCamlDebug(CCmdUI* pCmdUI);
	afx_msg void OnCamlDebug();
	afx_msg void OnContextHelp();
	afx_msg void OnCamlShowGraphics();
	afx_msg void OnUpdateCamlShowGraphics(CCmdUI* pCmdUI);
	afx_msg void OnUpdateCamlShowHistory(CCmdUI* pCmdUI);
	afx_msg void OnClose();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
