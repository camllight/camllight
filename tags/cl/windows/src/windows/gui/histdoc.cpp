// histdoc.cpp : implementation file
//

#include "stdafx.h"
#include "xeditvw.h"
#include "camlwin.h"
#include "histdoc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHistDoc

IMPLEMENT_SERIAL(CHistDoc, CDocument, 0 /* schema number*/ )

CHistDoc::CHistDoc()
{
}

BOOL CHistDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;
	return TRUE;
}

CHistDoc::~CHistDoc()
{
}


BEGIN_MESSAGE_MAP(CHistDoc, CDocument)
	//{{AFX_MSG_MAP(CHistDoc)
	ON_COMMAND(ID_FILE_SAVE, OnFileSave)
	ON_UPDATE_COMMAND_UI(ID_FILE_OPEN, OnUpdateFileOpen)
	ON_UPDATE_COMMAND_UI(ID_FILE_NEW, OnUpdateFileNew)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// Global variables

CHistDoc *pHistDoc;        

/////////////////////////////////////////////////////////////////////////////
// CHistDoc serialization

void CHistDoc::Serialize(CArchive& ar)
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
// CHistDoc commands

void CHistDoc::OnFileSave() 
{
	theApp.FileSave(CAMLHistory,"Save history as:");
}


void CHistDoc::OnUpdateFileOpen(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( FALSE );
}

void CHistDoc::OnUpdateFileNew(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( FALSE );	
}
