// camlwin.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"

#define CAMLWIN

#include "camlwin.h"

#include "mainfrm.h"
#include "txtrmdoc.h"
#include "txtrmfr.h"
#include "xeditvw.h"
#include "histdoc.h"
#include "graphdoc.h"
#include "graphfrm.h"
#include "graphvw.h"
#include "prefsdlg.h"

#include "afxdisp.h"           

#include "stdlib.h"
#include "io.h"
#include "fcntl.h"
#include "setjmp.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif


/////////////////////////////////////////////////////////////////////////////
// CCAMLWinApp

BEGIN_MESSAGE_MAP(CCAMLWinApp, CWinApp)
	//{{AFX_MSG_MAP(CCAMLWinApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
	ON_COMMAND(ID_CAML_PREFS, OnCamlPrefs)
	ON_COMMAND(ID_FILE_NEW, OnFileNew)
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
	// Standard print setup command
	ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCAMLWinApp construction

CCAMLWinApp::CCAMLWinApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CCAMLWinApp object

CCAMLWinApp NEAR theApp;             

// CAMLPrinter is the topmost pane of the TxTermFrame
CXEditView		*CAMLPrinter, *CAMLInput, *CAMLHistory;
CGraphDoc		*CAMLGraph;

/////////////////////////////////////////////////////////////////////////////
// CCAMLWinApp initialization

extern "C" void gr_clear_graph();

BOOL CCAMLWinApp::InitInstance()
{

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	SetDialogBkColor();        // Set dialog background color to gray
//	LoadStdProfileSettings();  // Load standard INI file options (including MRU)
	LoadSettings();
	                                     
	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	CMultiDocTemplate 	*pTermDocTemplate,
						*pHistDocTemplate,
						*pGraphDocTemplate;
	      
	// The "terminal": a splitter frame with two text edit views
	pTermDocTemplate = new CMultiDocTemplate(
		IDR_CAML_TERM, // IDR_TXTTRMTYPE,
		RUNTIME_CLASS(CTxTermDoc),
		RUNTIME_CLASS(CTxTermFrame),        // Instead of CMDIChildWnd, standard MDI child frame
		RUNTIME_CLASS(CXEditView));
	AddDocTemplate(pTermDocTemplate);

	// The "history": a (Read-Only) text edit view
	pHistDocTemplate = new CMultiDocTemplate(
		IDR_CAML_HIST, // IDR_TXTTRMTYPE,
		RUNTIME_CLASS(CHistDoc),
		RUNTIME_CLASS(CMDIChildWnd), 	// standard MDI child frame
		RUNTIME_CLASS(CXEditView));
	AddDocTemplate(pHistDocTemplate);

	// The "graphpane": a simple view
	pGraphDocTemplate = new CMultiDocTemplate(
		IDR_CAML_GRAPH, // IDR_TXTTRMTYPE,
		RUNTIME_CLASS(CGraphDoc),
		RUNTIME_CLASS(CGraphFrame), 	// standard MDI child frame
		RUNTIME_CLASS(CGraphView));
	AddDocTemplate(pGraphDocTemplate);

	// create main MDI Frame window
	CMainFrame* pMainFrame = new CMainFrame;
	if (!pMainFrame->LoadFrame(IDR_MAINFRAME))
		return FALSE;
	m_pMainWnd = pMainFrame;

	m_pMainWnd->DragAcceptFiles();

	// enable file manager drag/drop and DDE Execute open
	EnableShellOpen();
	RegisterShellFileTypes();

	// create the Preferences Dialog box
	m_PrefsDlg=new(CPrefsDlg);

   	// create the CAML text terminal window  
    pTermDoc = (CTxTermDoc *)pTermDocTemplate->OpenDocumentFile(NULL,TRUE); 
	pTermDoc->SetTitle("Terminal");
	
	POSITION pos = pTermDoc->GetFirstViewPosition();
	pTermDoc->m_View = (CTxTermFrame *)(pTermDoc->GetNextView( pos ));

   	// create the CAML history window  
    pHistDoc = (CHistDoc *)pHistDocTemplate->OpenDocumentFile(NULL,TRUE); 
	pHistDoc->SetTitle("History");
	// CAMLHistory = (void *)pHistDoc;
	
	pos = pHistDoc->GetFirstViewPosition();
	pHistDoc->m_View = (CXEditView *)(pHistDoc->GetNextView( pos ));
	CAMLHistory = pHistDoc->m_View;
	
	// the History is Read-Only... 
	CAMLHistory->SetReadOnly(TRUE);
    CAMLHistory->RedirectInputTo(CAMLInput);
    
   	// create the CAML graphics window  
    pGraphDoc = (CGraphDoc *)pGraphDocTemplate->OpenDocumentFile(NULL,TRUE); 
	pGraphDoc->SetTitle("Graphics");
	pos = pGraphDoc->GetFirstViewPosition();
	pGraphDoc->m_View = (CGraphView *)(pGraphDoc->GetNextView( pos ));
	CAMLGraph = pGraphDoc;
	gr_clear_graph();

	if(!m_bShowGraphics)
		pGraphDoc->m_View->GetParent()->ShowWindow(SW_HIDE);
	if(!m_bShowHistory)
		pHistDoc->m_View->GetParent()->ShowWindow(SW_HIDE);
	
    // and we do not want to exit...
    m_WantToExit = FALSE;     
    
    //////////////////////////////////
	// The main window has been initialized, so show and update it.
                  
	m_pMainWnd->SetWindowPos(NULL,m_mainRect.left,m_mainRect.top,m_mainRect.right-m_mainRect.left,
   							m_mainRect.bottom-m_mainRect.top,SWP_NOZORDER);
	SetStdPositions();

	pMainFrame->ShowWindow(m_nCmdShow);
	pMainFrame->UpdateWindow();
	
	CAMLInput->SetFocus();

	SetFont();
	CAMLInput->SetFont(m_Font);	
	CAMLPrinter->SetFont(m_Font);	
	CAMLHistory->SetFont(m_Font);	
	
	// that's all !
	return TRUE;
}

// Pfff... m_lpCmdLine can not be set without launching the app
// from Start/Execute or creating a shortcut to the command-line.
// We prefer to read this command-line in the .INI file.
// Since it is only a char*, we split it into a char** 
#define	SEND	0
#define	NL		1
BOOL CCAMLWinApp::LoadSettings()
{
	m_bDebugMode=FALSE;
	m_bShowGraphics=FALSE;
	m_bShowHistory=TRUE;

	CString	dummy = GetProfileString("General","CmdLine","");

	// For the "GUI-based" toplevel, 
	// camlrun c:\caml7\lib\camltop -stdlib c:\caml7\lib

	int lg = dummy.GetLength();
	m_CmdLine=new CHAR[lg+1];
	strcpy( m_CmdLine, dummy );

	int i=0, j=0;
	m_argv[j++] = m_CmdLine;

	while(i<lg){	// CmdLine[lg]=0
		while( (i<lg) && (m_CmdLine[i]!=' ') && (m_CmdLine[i]!=' ')) i++;
		if(i!=lg){
			m_CmdLine[i++]=0;
			m_argv[j++]=m_CmdLine+i;
		}
	}
	m_argc=j;

	m_pszHelpFilePath = strdup (GetProfileString("General","HelpFile",m_pszHelpFilePath));

	m_defLf.lfEscapement 		= 0;
	m_defLf.lfOrientation 		= 0;
	m_defLf.lfWeight 			= FW_DONTCARE;
	m_defLf.lfStrikeOut 		= FALSE;
	m_defLf.lfCharSet 			= ANSI_CHARSET;
	m_defLf.lfOutPrecision 		= OUT_DEFAULT_PRECIS;
	m_defLf.lfClipPrecision 	= CLIP_DEFAULT_PRECIS;
	m_defLf.lfQuality 			= DEFAULT_QUALITY;
	m_defLf.lfPitchAndFamily 	= FIXED_PITCH + FF_DONTCARE;

	m_lf = m_defLf;

	// Preferences
	// - Default values:
    m_defWidth 				= GetProfileInt("Window sizes","DefCamlWidth",440);
    m_defHeigth 			= GetProfileInt("Window sizes","DefCamlHeigth",280);

	m_defOutputMaxSize 		= GetProfileInt("Window sizes","DefOutputMaxSize",30);
	m_defOutputFlushSize 	= GetProfileInt("Window sizes","DefOutputFlushSize",10);
	m_defHistMaxSize 		= GetProfileInt("Window sizes","DefHistMaxSize",30);
	m_defHistFlushSize 		= GetProfileInt("Window sizes","DefHistFlushSize",10);
	m_defbShowHistory 		= GetProfileInt("Window sizes","DefShowHistory",TRUE);
	m_defbAutoScroll 		= GetProfileInt("Window sizes","DefAutoScroll",TRUE);
	m_defbSaveWorkspace		= GetProfileInt("Window sizes","DefSaveWorkspace",TRUE);
	m_defReturnAction		= GetProfileInt("Window sizes","DefReturnAction",NL);
	m_defCtrlReturnAction	= GetProfileInt("Window sizes","DefCtrlReturnAction",SEND);
	m_defEnterAction		= GetProfileInt("Window sizes","DefEnterAction",SEND);

	m_defLf.lfHeight 		= GetProfileInt("Font","DefHeigth",0);	
	m_defLf.lfWidth 		= GetProfileInt("Font","DefWidth",0);
	m_defLf.lfItalic 		= (BYTE)GetProfileInt("Font","DefItalic",0);
	m_defLf.lfUnderline 	= (BYTE)GetProfileInt("Font","DefUnderline",0);
	CString faceName 		= GetProfileString("Font","DefFace name");  
	sprintf(m_defLf.lfFaceName,faceName.GetBuffer(2));

	// - Current values
	m_bSaveWorkspace		= GetProfileInt("Window sizes","SaveWorkspace",TRUE);
	if(1){ // m_bSaveWorkspace){
	    m_mainRect.left		= GetProfileInt("Window sizes","Main window Left",40);
	    m_mainRect.right	= GetProfileInt("Window sizes","Main window Right",300);
	    m_mainRect.top		= GetProfileInt("Window sizes","Main window Top",40);
	    m_mainRect.bottom	= GetProfileInt("Window sizes","Main window Bottom",200);

		m_OutputMaxSize 	= GetProfileInt("Window sizes","OutputMaxSize",30);
		m_OutputFlushSize 	= GetProfileInt("Window sizes","OutputFlushSize",10);
		m_HistMaxSize 		= GetProfileInt("Window sizes","HistMaxSize",30);
		m_HistFlushSize 	= GetProfileInt("Window sizes","HistFlushSize",10);
		m_bShowHistory 		= GetProfileInt("Window sizes","ShowHistory",TRUE);
		m_bAutoScroll 		= GetProfileInt("Window sizes","AutoScroll",TRUE);
		m_ReturnAction		= GetProfileInt("Window sizes","ReturnAction",NL);
		m_CtrlReturnAction	= GetProfileInt("Window sizes","CtrlReturnAction",SEND);
		m_EnterAction		= GetProfileInt("Window sizes","EnterAction",SEND);

		m_lf.lfHeight 		= GetProfileInt("Font","Heigth",0);	
		m_lf.lfWidth 		= GetProfileInt("Font","Width",0);
		m_lf.lfItalic 		= (BYTE)GetProfileInt("Font","Italic",0);
		m_lf.lfUnderline 	= (BYTE)GetProfileInt("Font","Underline",0);
		CString faceName 		= GetProfileString("Font","Face name","FixedSys");  
		sprintf(m_defLf.lfFaceName,faceName.GetBuffer(2));
	} else SetDefaultSettings();
	return TRUE;

}                
#undef NL
#undef SEND    

BOOL CCAMLWinApp::SaveSettings()
{
// void GetWindowRect( LPRECT lpRect ) const;

	BOOL saveOK = WriteProfileInt("Window sizes","SaveWorkspace",m_bSaveWorkspace);

	if (m_bSaveWorkspace){
		m_pMainWnd->GetWindowRect(&m_mainRect);

		// Preferences: we just need to save the current values
	    saveOK = saveOK
	    	    && WriteProfileInt("Window sizes","Main window Left",m_mainRect.left)
				&& WriteProfileInt("Window sizes","Main window Right",m_mainRect.right)
				&& WriteProfileInt("Window sizes","Main window Top",m_mainRect.top)
				&& WriteProfileInt("Window sizes","Main window Bottom",m_mainRect.bottom)
				&& WriteProfileInt("Window sizes","OutputMaxSize",m_OutputMaxSize)
		 		&& WriteProfileInt("Window sizes","OutputFlushSize",m_OutputFlushSize)
		 		&& WriteProfileInt("Window sizes","HistMaxSize",m_HistMaxSize)
		 		&& WriteProfileInt("Window sizes","HistFlushSize",m_HistFlushSize)
		 		&& WriteProfileInt("Window sizes","ShowHistory",m_bShowHistory)
		 		&& WriteProfileInt("Window sizes","AutoScroll",m_bAutoScroll)
		 		&& WriteProfileInt("Window sizes","ReturnAction",m_ReturnAction)
		 		&& WriteProfileInt("Window sizes","CtrlReturnAction",m_CtrlReturnAction)
		 		&& WriteProfileInt("Window sizes","EnterAction",m_EnterAction)

		// Misc. settings
						  				// LONG
		 		&& WriteProfileInt("Font","Heigth",m_lf.lfHeight)	
		 		&& WriteProfileInt("Font","Width",m_lf.lfWidth)
				    	  				// BYTE
		 		&& WriteProfileInt("Font","Italic",m_lf.lfItalic)
		 		&& WriteProfileInt("Font","Underline",m_lf.lfUnderline);
		if (!saveOK)
			AfxMessageBox("Could not write the workspace...");
	}
	return TRUE;

}                    

BOOL CCAMLWinApp::SetDefaultSettings()
{
	
    m_Width 				= m_defWidth;                
    m_Heigth 				= m_defHeigth;               
							                           
	m_OutputMaxSize 		= m_defOutputMaxSize;
	m_OutputFlushSize 		= m_defOutputFlushSize;
	m_HistMaxSize 			= m_defHistMaxSize;
	m_HistFlushSize 		= m_defHistFlushSize;
	m_bShowHistory 			= m_defbShowHistory;
	m_bAutoScroll 			= m_defbAutoScroll;
	m_bSaveWorkspace		= m_defbSaveWorkspace;
	m_ReturnAction			= m_defReturnAction;
	m_CtrlReturnAction		= m_defCtrlReturnAction;
	m_EnterAction			= m_defEnterAction;

	m_lf					= m_defLf;

	return TRUE;
}

void CCAMLWinApp::SetFont()
{
	m_Font = new(CFont);
	m_Font->CreateFontIndirect(&m_lf);
}

void CCAMLWinApp::SetStdPositions()
{
	SetStdProportions();
}

void CCAMLWinApp::SetStdProportions()
{
	RECT rect;
    m_pMainWnd->GetClientRect(&rect);                
							  
	if(CAMLHistory->GetParent()->IsIconic())
    	pTermDoc->m_View->GetParent()->GetParent()->SetWindowPos(NULL,
    									5,
    									5,
    									rect.right,
    									rect.bottom,
									    SWP_NOZORDER);
	else{
    	CAMLHistory->GetParent()->SetWindowPos(NULL,
    										rect.right*2/3+5,
	    									5,
	    									rect.right/3-10,
	    									rect.bottom-55,
	    									SWP_NOZORDER);

	    pTermDoc->m_View->GetParent()->GetParent()->SetWindowPos(NULL,
	    									5,
	    									5,
	    									rect.right*2/3-5,
	    									rect.bottom-55,
										    SWP_NOZORDER);
	}
#define RECT_WIDTH(r)  (r.right  - r.left)
#define RECT_HEIGHT(r) (r.bottom - r.top)
	RECT	cliRect, frRect;
	CAMLHistory->GetParent()->GetWindowRect(&frRect);
	CAMLHistory->GetParent()->GetClientRect(&cliRect);
	CAMLGraph->m_View->GetParent()->SetWindowPos(NULL,25,25,
	   CAMLGraph->m_sizeX+RECT_WIDTH(frRect)-(cliRect.right),
	   CAMLGraph->m_sizeY+RECT_HEIGHT(frRect)-(cliRect.bottom),
//	   min(rect.right-50,CAMLGraph->m_sizeX),
//	   min(rect.bottom-50,CAMLGraph->m_sizeY),
	   SWP_NOZORDER);	 
	CAMLGraph->m_View->ScrollToPosition(CPoint(0,CAMLGraph->m_sizeY));
}


/////////////////////////////////////////////////////////////////////////////
// Main running routine until application exits
           
BOOL SendToCAML=FALSE;

extern "C" int caml_main(int argc, char *argv[]);

int empty_main()
{
	char buffer[1024];
	void *foo=malloc(1996);
	int len;
	do{
		len=DoRead(buffer,1024);
		buffer[len]=0;
		DoWrite(CAMLPrinter,buffer);
	}
	while(*buffer!='q');
	return DoExit(0);
}

// #define DBG

int CCAMLWinApp::Run()						
{   
	if (m_pMainWnd == NULL && AfxOleGetUserCtrl())
	{   
		// Not launched /Embedding or /Automation, but has no main window!
		TRACE0("Warning: 'm_pMainWnd' is NULL in CWinApp::Run"
				" - quitting application\n");
		::PostQuitMessage(0);
	}
	// need to reserve the file handles 0, 1 and 2, which have special
	// meaning both in lib/io.ml and ui.cpp
	int fd_stdin = _open("NUL", _O_RDONLY, 0);
	if (fd_stdin != 0) { _dup2(fd_stdin, 0); _close(fd_stdin); }
	int fd_stdout = _open("NUL", _O_WRONLY, 0);
	if (fd_stdout != 1) { _dup2(fd_stdout, 1); _close(fd_stdout); }
	int fd_stderr = _open("NUL", _O_WRONLY, 0);
	if (fd_stderr != 2) { _dup2(fd_stderr, 2); _close(fd_stderr); }

	m_JmpBuf=(void*)malloc(sizeof(jmp_buf));
	if(setjmp(*(jmp_buf *)m_JmpBuf)==0){
#ifdef DBG
		empty_main();
#else
		caml_main(m_argc,m_argv);
#endif
		return 0;
	}else{
		free(m_JmpBuf);
		return ExitInstance();
	}
}                       

int CCAMLWinApp::DoExit()
{
	SaveSettings();
	// Do not forget to close all the open documents !
	pHistDoc->SetModifiedFlag(FALSE);
	pTermDoc->SetModifiedFlag(FALSE);
	pGraphDoc->SetModifiedFlag(FALSE);
	pHistDoc->OnCloseDocument();
	pTermDoc->OnCloseDocument();
	pGraphDoc->OnCloseDocument();
	m_Font->DeleteObject();
	delete m_Font;
	delete m_PrefsDlg;
	delete m_CmdLine;
	free((char *) m_pszHelpFilePath);

	// It's time now to call DestroyWindow, since OnClose can't.
	m_pMainWnd->DestroyWindow();
	delete m_pMainWnd;

	longjmp(*((jmp_buf *)m_JmpBuf),1);
	return 1;
}

void SendToHistory()
{
}

#define EndedByNewLine(buf) (buf[strlen(buf)-2]==13 && buf[strlen(buf)-1]==10)
UINT	bufLen;
BOOL	InputReadyToSend=FALSE;
char	*inputBuf,
		*inputBufRoot;

int CCAMLWinApp::DoRead(char *text, int len)
{   
	if( InputReadyToSend ){
		int res = min(len,bufLen);
		strncpy(text,inputBuf,res);
		inputBuf+=res;
		bufLen-=res;
		if(bufLen<=0){
			free(inputBufRoot);
			InputReadyToSend=FALSE;
		}
		return res;
	}
	else {
		CAMLInput->GetParent()->GetParent()->BringWindowToTop();                                 
		SendToCAML=FALSE;
		// Acquire and dispatch messages until a "Send to Caml" or "Exit" command is received.
		while (m_WantToExit==FALSE)
		{
			LONG lIdleCount = 0;
			// check to see if we can do idle work
			while (!::PeekMessage(&m_msgCur, NULL, NULL, NULL, PM_NOREMOVE) &&
				OnIdle(lIdleCount++))
			{
				// more work to do
			}     

			// either we have a message, or OnIdle returned false
			if (SendToCAML || m_WantToExit || !PumpMessage())
				break;                
		}      
		if (SendToCAML){
			bufLen=((CXEditView *)CAMLInput)->GetBufferLength();
			InputReadyToSend = TRUE;
			inputBufRoot=(char*)malloc(bufLen+3);
			inputBuf=inputBufRoot;
			((CXEditView *)CAMLInput)->m_Edit->GetWindowText(inputBuf,bufLen+1);
			if(!EndedByNewLine(inputBuf)){
				inputBuf[bufLen]=13;							
				inputBuf[bufLen+1]=10;
				inputBuf[bufLen+2]=0;
				bufLen+=2;
				CAMLInput->Newline();
			}
			// We copy the input to the printer and the history
			CAMLInput->SelectAll();
			CAMLInput->Copy();
			CAMLPrinter->Paste();
			CAMLHistory->Paste();
			SendToHistory();

			return DoRead(text,len);
		}else
			return m_WantToExit;
	}
}       

void CCAMLWinApp::LookEvent(unsigned long delay)
{   
	if(::PeekMessage(&m_msgCur, NULL, NULL, NULL, PM_NOREMOVE) )
		PumpMessage();
}       


/////////////////////////////////////////////////////////////////////////////
// Exported functions (towards ui.cpp)

extern "C" int DoExit(int retcode)
{
	AfxMessageBox("See you later...");
	return theApp.DoExit();
}

extern "C" int DoRead(char *text, int len)
{
	return theApp.DoRead(text,len);
}

extern "C" void DoWrite(CXEditView *window, char *text)
{   
	window->AppendText(text);
}              

extern "C" void LookEvent(unsigned long delay)
{
	theApp.LookEvent(delay);
}

extern "C" void SendCmdToCAML(char *command)
{
	CAMLInput->SetText(command);
	if(!EndedByNewLine(command)) 
		CAMLInput->Newline();
	SendToCAML = TRUE;
}

extern "C" void SetInput(char *buf)
{
	CAMLInput->SetText(buf);
}

void CCAMLWinApp::FileSave(CXEditView *pWnd, char *title)
{
	CFileDialog aDlg( 
		FALSE, 										// BOOL bOpenFileDialog, 
		NULL, 										// LPCTSTR lpszDefExt = NULL,
		"*.ml",										// LPCTSTR lpszFileName = NULL, 
		OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT 
		);

	aDlg.m_ofn.lpstrTitle=title;

	int res = aDlg.DoModal();
	if(res == IDOK) {
		CString path_name=aDlg.GetPathName();
		char buf[31*1024];
		int textLen = pWnd->m_Edit->GetWindowText(buf,sizeof(buf));
		CStdioFile h(path_name,CFile::modeCreate | CFile::modeWrite | CFile::shareDenyWrite | CFile::typeText);
		h.WriteString(buf);
		h.Close();}
}


/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

// Implementation
protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//{{AFX_MSG(CAboutDlg)
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

// App command to run the dialog
void CCAMLWinApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}             


/////////////////////////////////////////////////////////////////////////////
// CCAMLWinApp commands

void CCAMLWinApp::OnCamlPrefs() 
{
	#define T(X) m_PrefsDlg->X = X;
	T(m_OutputMaxSize)
	T(m_OutputFlushSize)
	T(m_HistMaxSize)
	T(m_HistFlushSize)
	T(m_bShowHistory)
	T(m_bAutoScroll)
	T(m_bSaveWorkspace)
	T(m_ReturnAction)
	T(m_CtrlReturnAction)
	T(m_EnterAction)
	#undef T
	int retcode, contentsOK;
	do{ 
		retcode=m_PrefsDlg->DoModal();
		contentsOK=   (retcode==IDCANCEL)
			       || ( (  m_PrefsDlg->m_ReturnAction 
				         + m_PrefsDlg->m_CtrlReturnAction 
				         + m_PrefsDlg->m_EnterAction > 0 )
				      && (  m_PrefsDlg->m_ReturnAction 
				         + m_PrefsDlg->m_CtrlReturnAction 
				         + m_PrefsDlg->m_EnterAction <= 2) );
		if (!contentsOK)
			AfxMessageBox("Incorrect configuration\n(No \"Send to Caml\" command or no \"New Line\" command)");

	} while( !contentsOK );   
	if( retcode==IDOK ){
		#define T(X) X=m_PrefsDlg->##X;
		T(m_OutputMaxSize)
		T(m_OutputFlushSize)
		T(m_HistMaxSize)
		T(m_HistFlushSize)
		T(m_bShowHistory)
		T(m_bAutoScroll)
		T(m_bSaveWorkspace)
		T(m_ReturnAction)
		T(m_CtrlReturnAction)
		T(m_EnterAction)
		#undef T

		CAMLPrinter->m_FlushSize 	= m_OutputFlushSize; 
		CAMLPrinter->m_MaxSize 		= m_OutputMaxSize;

		CAMLHistory->m_FlushSize 	= m_HistFlushSize; 
		CAMLHistory->m_MaxSize 		= m_HistMaxSize;

		// We reflect the changes of the visibility of the History window:
		if(m_bShowHistory)
			pHistDoc->m_View->GetParent()->ShowWindow(SW_SHOW);
		else
			pHistDoc->m_View->GetParent()->ShowWindow(SW_HIDE);
	
	}
}


void CCAMLWinApp::OnFileNew() 
{
	CWinApp::OnFileNew();
	
}
