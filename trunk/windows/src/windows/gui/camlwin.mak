# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Camlwin - Win32 BoundsCheck
!MESSAGE No configuration specified.  Defaulting to Camlwin - Win32\
 BoundsCheck.
!ENDIF 

!IF "$(CFG)" != "Camlwin - Win32 Debug" && "$(CFG)" !=\
 "Camlwin - Win32 Release" && "$(CFG)" != "Camlwin - Win32 BoundsCheck"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "camlwin.mak" CFG="Camlwin - Win32 BoundsCheck"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Camlwin - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "Camlwin - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Camlwin - Win32 BoundsCheck" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "Camlwin - Win32 Release"
MTL=mktyplib.exe
RSC=rc.exe
CPP=cl.exe

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# PROP BASE Use_MFC 1
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "WinDebug"
# PROP Intermediate_Dir "WinDebug"
OUTDIR=.\WinDebug
INTDIR=.\WinDebug

ALL : "$(OUTDIR)\camlwin.exe" "$(OUTDIR)\camlwin.bsc"

CLEAN : 
	-@erase "$(INTDIR)\camlwin.obj"
	-@erase "$(INTDIR)\camlwin.pch"
	-@erase "$(INTDIR)\CAMLWIN.res"
	-@erase "$(INTDIR)\camlwin.sbr"
	-@erase "$(INTDIR)\graphdoc.obj"
	-@erase "$(INTDIR)\graphdoc.sbr"
	-@erase "$(INTDIR)\graphfrm.obj"
	-@erase "$(INTDIR)\graphfrm.sbr"
	-@erase "$(INTDIR)\graphvw.obj"
	-@erase "$(INTDIR)\graphvw.sbr"
	-@erase "$(INTDIR)\histdoc.obj"
	-@erase "$(INTDIR)\histdoc.sbr"
	-@erase "$(INTDIR)\mainfrm.obj"
	-@erase "$(INTDIR)\mainfrm.sbr"
	-@erase "$(INTDIR)\prefsdlg.obj"
	-@erase "$(INTDIR)\prefsdlg.sbr"
	-@erase "$(INTDIR)\STDAFX.OBJ"
	-@erase "$(INTDIR)\STDAFX.SBR"
	-@erase "$(INTDIR)\txtrmdoc.obj"
	-@erase "$(INTDIR)\txtrmdoc.sbr"
	-@erase "$(INTDIR)\txtrmfr.obj"
	-@erase "$(INTDIR)\txtrmfr.sbr"
	-@erase "$(INTDIR)\ui.obj"
	-@erase "$(INTDIR)\ui.sbr"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\xeditvw.obj"
	-@erase "$(INTDIR)\xeditvw.sbr"
	-@erase "$(OUTDIR)\camlwin.bsc"
	-@erase "$(OUTDIR)\camlwin.exe"
	-@erase "$(OUTDIR)\camlwin.ilk"
	-@erase "$(OUTDIR)\camlwin.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(OUTDIR)" :

"$(OUTDIR)\camlwin.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)

.c{$(CPP_OBJS)}.obj:

.c{$(CPP_SBRS)}.sbr:

.cpp{$(CPP_OBJS)}.obj:

.cpp{$(CPP_SBRS)}.sbr:

.cxx{$(CPP_OBJS)}.obj:

.cxx{$(CPP_SBRS)}.sbr:

# ADD BASE CPP /nologo /G3 /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /YX /c
# ADD CPP /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fr /c
CPP_PROJ=/nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=.\WinDebug/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x40c /d "_DEBUG"
# ADD RSC /l 0x40c /d "_DEBUG"
RSC_PROJ=/l 0x40c /fo"$(INTDIR)/CAMLWIN.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/camlwin.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\camlwin.sbr" \
	"$(INTDIR)\graphdoc.sbr" \
	"$(INTDIR)\graphfrm.sbr" \
	"$(INTDIR)\graphvw.sbr" \
	"$(INTDIR)\histdoc.sbr" \
	"$(INTDIR)\mainfrm.sbr" \
	"$(INTDIR)\prefsdlg.sbr" \
	"$(INTDIR)\STDAFX.SBR" \
	"$(INTDIR)\txtrmdoc.sbr" \
	"$(INTDIR)\txtrmfr.sbr" \
	"$(INTDIR)\ui.sbr" \
	"$(INTDIR)\xeditvw.sbr"

"$(OUTDIR)\camlwin.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 oldnames.lib /nologo /stack:0x11240 /subsystem:windows /debug /machine:IX86
# ADD LINK32 ..\runtime\libcaml.lib /nologo /stack:0x11240 /subsystem:windows /debug /machine:IX86
LINK32_FLAGS=..\runtime\libcaml.lib /nologo /stack:0x11240 /subsystem:windows\
 /incremental:yes /pdb:"$(OUTDIR)/camlwin.pdb" /debug /machine:IX86\
 /def:".\CAMLWIN.DEF" /out:"$(OUTDIR)/camlwin.exe" 
DEF_FILE= \
	".\CAMLWIN.DEF"
LINK32_OBJS= \
	"$(INTDIR)\camlwin.obj" \
	"$(INTDIR)\CAMLWIN.res" \
	"$(INTDIR)\graphdoc.obj" \
	"$(INTDIR)\graphfrm.obj" \
	"$(INTDIR)\graphvw.obj" \
	"$(INTDIR)\histdoc.obj" \
	"$(INTDIR)\mainfrm.obj" \
	"$(INTDIR)\prefsdlg.obj" \
	"$(INTDIR)\STDAFX.OBJ" \
	"$(INTDIR)\txtrmdoc.obj" \
	"$(INTDIR)\txtrmfr.obj" \
	"$(INTDIR)\ui.obj" \
	"$(INTDIR)\xeditvw.obj"

"$(OUTDIR)\camlwin.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# PROP BASE Use_MFC 1
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "toto"
# PROP BASE Intermediate_Dir "toto"
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "WinRelease"
# PROP Intermediate_Dir "WinRelease"
OUTDIR=.\WinRelease
INTDIR=.\WinRelease

ALL : "$(OUTDIR)\camlwin.exe" "$(OUTDIR)\camlwin.bsc"

CLEAN : 
	-@erase "$(INTDIR)\camlwin.obj"
	-@erase "$(INTDIR)\camlwin.pch"
	-@erase "$(INTDIR)\CAMLWIN.res"
	-@erase "$(INTDIR)\camlwin.sbr"
	-@erase "$(INTDIR)\graphdoc.obj"
	-@erase "$(INTDIR)\graphdoc.sbr"
	-@erase "$(INTDIR)\graphfrm.obj"
	-@erase "$(INTDIR)\graphfrm.sbr"
	-@erase "$(INTDIR)\graphvw.obj"
	-@erase "$(INTDIR)\graphvw.sbr"
	-@erase "$(INTDIR)\histdoc.obj"
	-@erase "$(INTDIR)\histdoc.sbr"
	-@erase "$(INTDIR)\mainfrm.obj"
	-@erase "$(INTDIR)\mainfrm.sbr"
	-@erase "$(INTDIR)\prefsdlg.obj"
	-@erase "$(INTDIR)\prefsdlg.sbr"
	-@erase "$(INTDIR)\STDAFX.OBJ"
	-@erase "$(INTDIR)\STDAFX.SBR"
	-@erase "$(INTDIR)\txtrmdoc.obj"
	-@erase "$(INTDIR)\txtrmdoc.sbr"
	-@erase "$(INTDIR)\txtrmfr.obj"
	-@erase "$(INTDIR)\txtrmfr.sbr"
	-@erase "$(INTDIR)\ui.obj"
	-@erase "$(INTDIR)\ui.sbr"
	-@erase "$(INTDIR)\xeditvw.obj"
	-@erase "$(INTDIR)\xeditvw.sbr"
	-@erase "$(OUTDIR)\camlwin.bsc"
	-@erase "$(OUTDIR)\camlwin.exe"
	-@erase "$(OUTDIR)\camlwin.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(OUTDIR)" :

"$(OUTDIR)\camlwin.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)

.c{$(CPP_OBJS)}.obj:

.c{$(CPP_SBRS)}.sbr:

.cpp{$(CPP_OBJS)}.obj:

.cpp{$(CPP_SBRS)}.sbr:

.cxx{$(CPP_OBJS)}.obj:

.cxx{$(CPP_SBRS)}.sbr:

# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /FR /c
# ADD CPP /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /Fr /c
CPP_PROJ=/nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRelease/
CPP_SBRS=.\WinRelease/
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x40c /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo"$(INTDIR)/CAMLWIN.res" /d "NDEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/camlwin.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\camlwin.sbr" \
	"$(INTDIR)\graphdoc.sbr" \
	"$(INTDIR)\graphfrm.sbr" \
	"$(INTDIR)\graphvw.sbr" \
	"$(INTDIR)\histdoc.sbr" \
	"$(INTDIR)\mainfrm.sbr" \
	"$(INTDIR)\prefsdlg.sbr" \
	"$(INTDIR)\STDAFX.SBR" \
	"$(INTDIR)\txtrmdoc.sbr" \
	"$(INTDIR)\txtrmfr.sbr" \
	"$(INTDIR)\ui.sbr" \
	"$(INTDIR)\xeditvw.sbr"

"$(OUTDIR)\camlwin.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 oldnames.lib /nologo /stack:0x10240 /subsystem:windows /machine:IX86
# ADD LINK32 ..\runtime\libcaml.lib /nologo /stack:0x40000 /subsystem:windows /debug /machine:IX86 /nodefaultlib:"libc"
# SUBTRACT LINK32 /pdb:none
LINK32_FLAGS=..\runtime\libcaml.lib /nologo /stack:0x40000 /subsystem:windows\
 /incremental:no /pdb:"$(OUTDIR)/camlwin.pdb" /debug /machine:IX86\
 /nodefaultlib:"libc" /def:".\CAMLWIN.DEF" /out:"$(OUTDIR)/camlwin.exe" 
DEF_FILE= \
	".\CAMLWIN.DEF"
LINK32_OBJS= \
	"$(INTDIR)\camlwin.obj" \
	"$(INTDIR)\CAMLWIN.res" \
	"$(INTDIR)\graphdoc.obj" \
	"$(INTDIR)\graphfrm.obj" \
	"$(INTDIR)\graphvw.obj" \
	"$(INTDIR)\histdoc.obj" \
	"$(INTDIR)\mainfrm.obj" \
	"$(INTDIR)\prefsdlg.obj" \
	"$(INTDIR)\STDAFX.OBJ" \
	"$(INTDIR)\txtrmdoc.obj" \
	"$(INTDIR)\txtrmfr.obj" \
	"$(INTDIR)\ui.obj" \
	"$(INTDIR)\xeditvw.obj"

"$(OUTDIR)\camlwin.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# PROP BASE Use_MFC 1
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Camlwin_"
# PROP BASE Intermediate_Dir "Camlwin_"
# PROP BASE Target_Dir ""
# PROP Use_MFC 1
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Camlwin_"
# PROP Intermediate_Dir "Camlwin_"
# PROP Target_Dir ""
OUTDIR=.\Camlwin_
INTDIR=.\Camlwin_

ALL : "$(OUTDIR)\camlwin.exe" "$(OUTDIR)\camlwin.bsc"

CLEAN : 
	-@erase "$(INTDIR)\camlwin.obj"
	-@erase "$(INTDIR)\camlwin.pch"
	-@erase "$(INTDIR)\CAMLWIN.res"
	-@erase "$(INTDIR)\camlwin.sbr"
	-@erase "$(INTDIR)\graphdoc.obj"
	-@erase "$(INTDIR)\graphdoc.sbr"
	-@erase "$(INTDIR)\graphfrm.obj"
	-@erase "$(INTDIR)\graphfrm.sbr"
	-@erase "$(INTDIR)\graphvw.obj"
	-@erase "$(INTDIR)\graphvw.sbr"
	-@erase "$(INTDIR)\histdoc.obj"
	-@erase "$(INTDIR)\histdoc.sbr"
	-@erase "$(INTDIR)\mainfrm.obj"
	-@erase "$(INTDIR)\mainfrm.sbr"
	-@erase "$(INTDIR)\prefsdlg.obj"
	-@erase "$(INTDIR)\prefsdlg.sbr"
	-@erase "$(INTDIR)\STDAFX.OBJ"
	-@erase "$(INTDIR)\STDAFX.SBR"
	-@erase "$(INTDIR)\txtrmdoc.obj"
	-@erase "$(INTDIR)\txtrmdoc.sbr"
	-@erase "$(INTDIR)\txtrmfr.obj"
	-@erase "$(INTDIR)\txtrmfr.sbr"
	-@erase "$(INTDIR)\ui.obj"
	-@erase "$(INTDIR)\ui.sbr"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(INTDIR)\xeditvw.obj"
	-@erase "$(INTDIR)\xeditvw.sbr"
	-@erase "$(OUTDIR)\camlwin.bsc"
	-@erase "$(OUTDIR)\camlwin.exe"
	-@erase "$(OUTDIR)\camlwin.ilk"
	-@erase "$(OUTDIR)\camlwin.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fr /c
# ADD CPP /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /Fr /c
CPP_PROJ=/nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Camlwin_/
CPP_SBRS=.\Camlwin_/
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x40c /d "_DEBUG"
# ADD RSC /l 0x40c /d "_DEBUG"
RSC_PROJ=/l 0x40c /fo"$(INTDIR)/CAMLWIN.res" /d "_DEBUG" 
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/camlwin.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\camlwin.sbr" \
	"$(INTDIR)\graphdoc.sbr" \
	"$(INTDIR)\graphfrm.sbr" \
	"$(INTDIR)\graphvw.sbr" \
	"$(INTDIR)\histdoc.sbr" \
	"$(INTDIR)\mainfrm.sbr" \
	"$(INTDIR)\prefsdlg.sbr" \
	"$(INTDIR)\STDAFX.SBR" \
	"$(INTDIR)\txtrmdoc.sbr" \
	"$(INTDIR)\txtrmfr.sbr" \
	"$(INTDIR)\ui.sbr" \
	"$(INTDIR)\xeditvw.sbr"

"$(OUTDIR)\camlwin.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
# ADD BASE LINK32 ..\runtime\libcaml.lib /nologo /stack:0x11240 /subsystem:windows /debug /machine:IX86
# ADD LINK32 ..\runtime\libcaml.lib /nologo /stack:0x11240 /subsystem:windows /debug /machine:IX86
LINK32_FLAGS=..\runtime\libcaml.lib /nologo /stack:0x11240 /subsystem:windows\
 /incremental:yes /pdb:"$(OUTDIR)/camlwin.pdb" /debug /machine:IX86\
 /def:".\CAMLWIN.DEF" /out:"$(OUTDIR)/camlwin.exe" 
DEF_FILE= \
	".\CAMLWIN.DEF"
LINK32_OBJS= \
	"$(INTDIR)\camlwin.obj" \
	"$(INTDIR)\CAMLWIN.res" \
	"$(INTDIR)\graphdoc.obj" \
	"$(INTDIR)\graphfrm.obj" \
	"$(INTDIR)\graphvw.obj" \
	"$(INTDIR)\histdoc.obj" \
	"$(INTDIR)\mainfrm.obj" \
	"$(INTDIR)\prefsdlg.obj" \
	"$(INTDIR)\STDAFX.OBJ" \
	"$(INTDIR)\txtrmdoc.obj" \
	"$(INTDIR)\txtrmfr.obj" \
	"$(INTDIR)\ui.obj" \
	"$(INTDIR)\xeditvw.obj"

"$(OUTDIR)\camlwin.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "Camlwin - Win32 Debug"
# Name "Camlwin - Win32 Release"
# Name "Camlwin - Win32 BoundsCheck"

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\CAMLWIN.RC
DEP_RSC_CAMLW=\
	".\RES\CAMLWIN.ICO"\
	".\res\camlwin.rc2"\
	".\RES\HIST.ICO"\
	".\RES\TERM.ICO"\
	".\RES\TOOLBAR.BMP"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"


"$(INTDIR)\CAMLWIN.res" : $(SOURCE) $(DEP_RSC_CAMLW) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"


"$(INTDIR)\CAMLWIN.res" : $(SOURCE) $(DEP_RSC_CAMLW) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"


"$(INTDIR)\CAMLWIN.res" : $(SOURCE) $(DEP_RSC_CAMLW) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\STDAFX.CPP
DEP_CPP_STDAF=\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yc"STDAFX.H"
# ADD CPP /Yc"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yc"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\STDAFX.OBJ" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\STDAFX.SBR" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\camlwin.pch" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yc"STDAFX.H"
# ADD CPP /Yc"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yc"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\STDAFX.OBJ" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\STDAFX.SBR" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\camlwin.pch" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yc"STDAFX.H"
# ADD CPP /Yc"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yc"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\STDAFX.OBJ" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\STDAFX.SBR" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\camlwin.pch" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\camlwin.cpp
DEP_CPP_CAMLWI=\
	".\camlwin.h"\
	".\graphdoc.h"\
	".\graphfrm.h"\
	".\graphvw.h"\
	".\histdoc.h"\
	".\mainfrm.h"\
	".\prefsdlg.h"\
	".\stdafx.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\camlwin.obj" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\camlwin.sbr" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\camlwin.obj" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\camlwin.sbr" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\camlwin.obj" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\camlwin.sbr" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mainfrm.cpp
DEP_CPP_MAINF=\
	".\camlwin.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
	".\mainfrm.h"\
	".\stdafx.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\mainfrm.obj" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\mainfrm.sbr" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\mainfrm.obj" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\mainfrm.sbr" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\mainfrm.obj" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\mainfrm.sbr" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\txtrmdoc.cpp
DEP_CPP_TXTRM=\
	".\camlwin.h"\
	".\stdafx.h"\
	".\txtrmdoc.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\txtrmdoc.obj" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\txtrmdoc.sbr" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\txtrmdoc.obj" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\txtrmdoc.sbr" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\txtrmdoc.obj" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\txtrmdoc.sbr" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\txtrmfr.cpp
DEP_CPP_TXTRMF=\
	".\camlwin.h"\
	".\stdafx.h"\
	".\txtrmfr.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\txtrmfr.obj" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\txtrmfr.sbr" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\txtrmfr.obj" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\txtrmfr.sbr" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\txtrmfr.obj" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\txtrmfr.sbr" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\xeditvw.cpp
DEP_CPP_XEDIT=\
	".\camlwin.h"\
	".\stdafx.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\xeditvw.obj" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\xeditvw.sbr" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\xeditvw.obj" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\xeditvw.sbr" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\xeditvw.obj" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\xeditvw.sbr" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\histdoc.cpp
DEP_CPP_HISTD=\
	".\camlwin.h"\
	".\histdoc.h"\
	".\stdafx.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\histdoc.obj" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\histdoc.sbr" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\histdoc.obj" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\histdoc.sbr" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\histdoc.obj" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\histdoc.sbr" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\prefsdlg.cpp
DEP_CPP_PREFS=\
	".\camlwin.h"\
	".\prefsdlg.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\prefsdlg.obj" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\prefsdlg.sbr" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\prefsdlg.obj" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\prefsdlg.sbr" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\prefsdlg.obj" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\prefsdlg.sbr" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ui.cpp
DEP_CPP_UI_CP=\
	".\camlwin.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\ui.obj" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\ui.sbr" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c\
 $(SOURCE) \
	

"$(INTDIR)\ui.obj" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\ui.sbr" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fr"$(INTDIR)/" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\ui.obj" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

"$(INTDIR)\ui.sbr" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CAMLWIN.DEF

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphvw.cpp
DEP_CPP_GRAPH=\
	".\camlwin.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"


"$(INTDIR)\graphvw.obj" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"

"$(INTDIR)\graphvw.sbr" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"


"$(INTDIR)\graphvw.obj" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"

"$(INTDIR)\graphvw.sbr" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"


"$(INTDIR)\graphvw.obj" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"

"$(INTDIR)\graphvw.sbr" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphdoc.cpp
DEP_CPP_GRAPHD=\
	"..\..\RUNTIME\alloc.h"\
	"..\..\RUNTIME\fail.h"\
	"..\..\RUNTIME\memory.h"\
	"..\..\RUNTIME\signals.h"\
	"..\..\RUNTIME\str.h"\
	".\..\..\..\config\m.h"\
	".\..\..\..\config\s.h"\
	".\..\..\RUNTIME\config.h"\
	".\..\..\RUNTIME\freelist.h"\
	".\..\..\RUNTIME\gc.h"\
	".\..\..\RUNTIME\major_gc.h"\
	".\..\..\RUNTIME\minor_gc.h"\
	".\..\..\RUNTIME\misc.h"\
	".\..\..\RUNTIME\mlvalues.h"\
	".\camlwin.h"\
	".\colors.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
	".\mainfrm.h"\
	".\stdafx.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"


"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"

"$(INTDIR)\graphdoc.sbr" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"


"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"

"$(INTDIR)\graphdoc.sbr" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"


"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"

"$(INTDIR)\graphdoc.sbr" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphfrm.cpp
DEP_CPP_GRAPHF=\
	".\camlwin.h"\
	".\graphfrm.h"\
	".\stdafx.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"


"$(INTDIR)\graphfrm.obj" : $(SOURCE) $(DEP_CPP_GRAPHF) "$(INTDIR)"

"$(INTDIR)\graphfrm.sbr" : $(SOURCE) $(DEP_CPP_GRAPHF) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"


"$(INTDIR)\graphfrm.obj" : $(SOURCE) $(DEP_CPP_GRAPHF) "$(INTDIR)"

"$(INTDIR)\graphfrm.sbr" : $(SOURCE) $(DEP_CPP_GRAPHF) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"


"$(INTDIR)\graphfrm.obj" : $(SOURCE) $(DEP_CPP_GRAPHF) "$(INTDIR)"

"$(INTDIR)\graphfrm.sbr" : $(SOURCE) $(DEP_CPP_GRAPHF) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
