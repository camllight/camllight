// txtrmdoc.cpp : implementation of the CTxTermDoc class
//

#include "stdafx.h"
#include "xeditvw.h"
#include "camlwin.h"

#include "txtrmdoc.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CTxTermDoc

IMPLEMENT_DYNCREATE(CTxTermDoc, CDocument)

BEGIN_MESSAGE_MAP(CTxTermDoc, CDocument)
	//{{AFX_MSG_MAP(CTxTermDoc)
	ON_COMMAND(ID_FILE_CLOSE, OnFileClose)
	ON_UPDATE_COMMAND_UI(ID_FILE_NEW, OnUpdateFileNew)
	ON_COMMAND(ID_FILE_SAVE, OnFileSave)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Global variables

CTxTermDoc 	*pTermDoc;        

/////////////////////////////////////////////////////////////////////////////
// CTxTermDoc construction/destruction

CTxTermDoc::CTxTermDoc()
{
	// TODO: add one-time construction code here
}

CTxTermDoc::~CTxTermDoc()
{
}

BOOL CTxTermDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CTxTermDoc serialization

void CTxTermDoc::Serialize(CArchive& ar)
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
// CTxTermDoc diagnostics

#ifdef _DEBUG
void CTxTermDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CTxTermDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CTxTermDoc commands

void CTxTermDoc::OnFileClose() 
{
}

void CTxTermDoc::OnFileSave() 
{
	theApp.FileSave(CAMLInput,"Save Input as:");
	
}

void CTxTermDoc::OnUpdateFileNew(CCmdUI* pCmdUI) 
{
	pCmdUI->Enable( FALSE );	
}

