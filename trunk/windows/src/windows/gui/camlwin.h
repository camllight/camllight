// camlwin.h : main header file for the CAMLWIN application
//             

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols


/////////////////////////////////////////////////////////////////////////////
// CCAMLWinApp:
// See camlwin.cpp for the implementation of this class
//

class CPrefsDlg;
class CXEditView;

class CCAMLWinApp : public CWinApp
{ 
private:

	// Command-line
	LPSTR		m_CmdLine;
	int 		m_argc;
	char 		*m_argv[48];

	BOOL 		m_WantToExit;
	void		*m_JmpBuf;

public:

	BOOL		m_bDebugMode,
				m_bShowGraphics;
	// Workspace settings (both default and current are written in 
	// the .INI file). The bSaveSettings from the current determines
	// which set to use.

	// - Default
	RECT	m_mainRect;
	RECT	m_termRect;
	RECT	m_histRect;
	RECT	m_graphRect;

	LOGFONT 	m_lf, m_defLf;

	int		m_defWidth, m_defHeigth;

	UINT	m_defOutputMaxSize;
	UINT	m_defOutputFlushSize;
	UINT	m_defHistMaxSize;
	UINT	m_defHistFlushSize;
	BOOL	m_defbShowHistory;
	BOOL	m_defbAutoScroll;
	BOOL	m_defbSaveWorkspace;
	int		m_defReturnAction;	  	// 0 = Send, 1 = NL
	int		m_defCtrlReturnAction;
	int		m_defEnterAction;

	CFont	*m_defFont;

	// - Current
	int		m_Width, m_Heigth;

	UINT	m_OutputMaxSize;
	UINT	m_OutputFlushSize;
	UINT	m_HistMaxSize;
	UINT	m_HistFlushSize;
	BOOL	m_bShowHistory;
	BOOL	m_bAutoScroll;
	BOOL	m_bSaveWorkspace;
	int		m_ReturnAction;	  		// 0 = Send, 1 = NL
	int		m_CtrlReturnAction;
	int		m_EnterAction;

	CFont	*m_Font;

	//		The Dialog box
	CPrefsDlg	*m_PrefsDlg;

	BOOL LoadSettings();
	BOOL SaveSettings();
	BOOL SetDefaultSettings();
	
public:  
	void SetStdPositions();
	void SetStdProportions();
	void SetFont();
	void WantToExit(BOOL bFlag=TRUE){ m_WantToExit = bFlag;};
	void OnAppExit();

	void FileSave(CXEditView *pWnd, char *title);

	CCAMLWinApp();   

	int DoRead(char * text, int len);
	int DoExit();
	void LookEvent(unsigned long delay);


// Overrides
	virtual BOOL InitInstance();
	virtual int Run();

// Implementation

	//{{AFX_MSG(CCAMLWinApp)
	afx_msg void OnAppAbout();
	afx_msg void OnCamlPrefs();
	afx_msg void OnFileNew();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef CAMLWIN
extern CCAMLWinApp theApp;
#endif

/////////////////////////////////////////////////////////////////////////////
extern "C" int DoExit(int retcode);
extern "C" void LookEvent(unsigned long delay);
extern "C" int DoRead(char *text, int len);
class CXEditView;
extern "C" void DoWrite(CXEditView *window, char *text);

extern "C" void SendCmdToCAML(char *command);
extern "C" void SetInput(char *buf);

extern CXEditView *CAMLPrinter, *CAMLInput, *CAMLHistory;

/////////////////////////////////////////////////////////////////////////////
