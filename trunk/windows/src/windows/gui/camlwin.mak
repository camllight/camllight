# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
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
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

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
	-@erase ".\WinDebug\vc40.pdb"
	-@erase ".\WinDebug\camlwin.pch"
	-@erase ".\WinDebug\vc40.idb"
	-@erase ".\WinDebug\camlwin.bsc"
	-@erase ".\WinDebug\graphfrm.sbr"
	-@erase ".\WinDebug\graphdoc.sbr"
	-@erase ".\WinDebug\mainfrm.sbr"
	-@erase ".\WinDebug\txtrmfr.sbr"
	-@erase ".\WinDebug\graphvw.sbr"
	-@erase ".\WinDebug\prefsdlg.sbr"
	-@erase ".\WinDebug\STDAFX.SBR"
	-@erase ".\WinDebug\ui.sbr"
	-@erase ".\WinDebug\xeditvw.sbr"
	-@erase ".\WinDebug\txtrmdoc.sbr"
	-@erase ".\WinDebug\histdoc.sbr"
	-@erase ".\WinDebug\camlwin.sbr"
	-@erase ".\WinDebug\camlwin.exe"
	-@erase ".\WinDebug\histdoc.obj"
	-@erase ".\WinDebug\camlwin.obj"
	-@erase ".\WinDebug\graphfrm.obj"
	-@erase ".\WinDebug\graphdoc.obj"
	-@erase ".\WinDebug\mainfrm.obj"
	-@erase ".\WinDebug\txtrmfr.obj"
	-@erase ".\WinDebug\graphvw.obj"
	-@erase ".\WinDebug\prefsdlg.obj"
	-@erase ".\WinDebug\STDAFX.OBJ"
	-@erase ".\WinDebug\ui.obj"
	-@erase ".\WinDebug\xeditvw.obj"
	-@erase ".\WinDebug\txtrmdoc.obj"
	-@erase ".\WinDebug\CAMLWIN.res"
	-@erase ".\WinDebug\camlwin.ilk"
	-@erase ".\WinDebug\camlwin.pdb"

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
	".\WinDebug\graphfrm.sbr" \
	".\WinDebug\graphdoc.sbr" \
	".\WinDebug\mainfrm.sbr" \
	".\WinDebug\txtrmfr.sbr" \
	".\WinDebug\graphvw.sbr" \
	".\WinDebug\prefsdlg.sbr" \
	".\WinDebug\STDAFX.SBR" \
	".\WinDebug\ui.sbr" \
	".\WinDebug\xeditvw.sbr" \
	".\WinDebug\txtrmdoc.sbr" \
	".\WinDebug\histdoc.sbr" \
	".\WinDebug\camlwin.sbr"

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
	".\WinDebug\histdoc.obj" \
	".\WinDebug\camlwin.obj" \
	".\WinDebug\graphfrm.obj" \
	".\WinDebug\graphdoc.obj" \
	".\WinDebug\mainfrm.obj" \
	".\WinDebug\txtrmfr.obj" \
	".\WinDebug\graphvw.obj" \
	".\WinDebug\prefsdlg.obj" \
	".\WinDebug\STDAFX.OBJ" \
	".\WinDebug\ui.obj" \
	".\WinDebug\xeditvw.obj" \
	".\WinDebug\txtrmdoc.obj" \
	".\WinDebug\CAMLWIN.res"

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
	-@erase ".\WinRelease\camlwin.exe"
	-@erase ".\WinRelease\camlwin.obj"
	-@erase ".\WinRelease\camlwin.pch"
	-@erase ".\WinRelease\prefsdlg.obj"
	-@erase ".\WinRelease\graphfrm.obj"
	-@erase ".\WinRelease\mainfrm.obj"
	-@erase ".\WinRelease\txtrmfr.obj"
	-@erase ".\WinRelease\graphvw.obj"
	-@erase ".\WinRelease\STDAFX.OBJ"
	-@erase ".\WinRelease\txtrmdoc.obj"
	-@erase ".\WinRelease\graphdoc.obj"
	-@erase ".\WinRelease\xeditvw.obj"
	-@erase ".\WinRelease\ui.obj"
	-@erase ".\WinRelease\histdoc.obj"
	-@erase ".\WinRelease\CAMLWIN.res"
	-@erase ".\WinRelease\camlwin.pdb"
	-@erase ".\WinRelease\camlwin.bsc"
	-@erase ".\WinRelease\STDAFX.SBR"
	-@erase ".\WinRelease\camlwin.sbr"
	-@erase ".\WinRelease\mainfrm.sbr"
	-@erase ".\WinRelease\txtrmdoc.sbr"
	-@erase ".\WinRelease\txtrmfr.sbr"
	-@erase ".\WinRelease\xeditvw.sbr"
	-@erase ".\WinRelease\histdoc.sbr"
	-@erase ".\WinRelease\prefsdlg.sbr"
	-@erase ".\WinRelease\ui.sbr"
	-@erase ".\WinRelease\graphvw.sbr"
	-@erase ".\WinRelease\graphdoc.sbr"
	-@erase ".\WinRelease\graphfrm.sbr"

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
	".\WinRelease\STDAFX.SBR" \
	".\WinRelease\camlwin.sbr" \
	".\WinRelease\mainfrm.sbr" \
	".\WinRelease\txtrmdoc.sbr" \
	".\WinRelease\txtrmfr.sbr" \
	".\WinRelease\xeditvw.sbr" \
	".\WinRelease\histdoc.sbr" \
	".\WinRelease\prefsdlg.sbr" \
	".\WinRelease\ui.sbr" \
	".\WinRelease\graphvw.sbr" \
	".\WinRelease\graphdoc.sbr" \
	".\WinRelease\graphfrm.sbr"

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
	".\WinRelease\camlwin.obj" \
	".\WinRelease\prefsdlg.obj" \
	".\WinRelease\graphfrm.obj" \
	".\WinRelease\mainfrm.obj" \
	".\WinRelease\txtrmfr.obj" \
	".\WinRelease\graphvw.obj" \
	".\WinRelease\STDAFX.OBJ" \
	".\WinRelease\txtrmdoc.obj" \
	".\WinRelease\graphdoc.obj" \
	".\WinRelease\xeditvw.obj" \
	".\WinRelease\ui.obj" \
	".\WinRelease\histdoc.obj" \
	".\WinRelease\CAMLWIN.res"

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
	-@erase ".\Camlwin_\vc40.pdb"
	-@erase ".\Camlwin_\camlwin.pch"
	-@erase ".\Camlwin_\vc40.idb"
	-@erase ".\Camlwin_\camlwin.bsc"
	-@erase ".\Camlwin_\graphdoc.sbr"
	-@erase ".\Camlwin_\graphvw.sbr"
	-@erase ".\Camlwin_\camlwin.sbr"
	-@erase ".\Camlwin_\xeditvw.sbr"
	-@erase ".\Camlwin_\prefsdlg.sbr"
	-@erase ".\Camlwin_\histdoc.sbr"
	-@erase ".\Camlwin_\ui.sbr"
	-@erase ".\Camlwin_\txtrmfr.sbr"
	-@erase ".\Camlwin_\mainfrm.sbr"
	-@erase ".\Camlwin_\STDAFX.SBR"
	-@erase ".\Camlwin_\txtrmdoc.sbr"
	-@erase ".\Camlwin_\graphfrm.sbr"
	-@erase ".\Camlwin_\camlwin.exe"
	-@erase ".\Camlwin_\mainfrm.obj"
	-@erase ".\Camlwin_\STDAFX.OBJ"
	-@erase ".\Camlwin_\txtrmdoc.obj"
	-@erase ".\Camlwin_\graphfrm.obj"
	-@erase ".\Camlwin_\graphdoc.obj"
	-@erase ".\Camlwin_\graphvw.obj"
	-@erase ".\Camlwin_\camlwin.obj"
	-@erase ".\Camlwin_\xeditvw.obj"
	-@erase ".\Camlwin_\prefsdlg.obj"
	-@erase ".\Camlwin_\histdoc.obj"
	-@erase ".\Camlwin_\ui.obj"
	-@erase ".\Camlwin_\txtrmfr.obj"
	-@erase ".\Camlwin_\CAMLWIN.res"
	-@erase ".\Camlwin_\camlwin.ilk"
	-@erase ".\Camlwin_\camlwin.pdb"

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
	".\Camlwin_\graphdoc.sbr" \
	".\Camlwin_\graphvw.sbr" \
	".\Camlwin_\camlwin.sbr" \
	".\Camlwin_\xeditvw.sbr" \
	".\Camlwin_\prefsdlg.sbr" \
	".\Camlwin_\histdoc.sbr" \
	".\Camlwin_\ui.sbr" \
	".\Camlwin_\txtrmfr.sbr" \
	".\Camlwin_\mainfrm.sbr" \
	".\Camlwin_\STDAFX.SBR" \
	".\Camlwin_\txtrmdoc.sbr" \
	".\Camlwin_\graphfrm.sbr"

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
	".\Camlwin_\mainfrm.obj" \
	".\Camlwin_\STDAFX.OBJ" \
	".\Camlwin_\txtrmdoc.obj" \
	".\Camlwin_\graphfrm.obj" \
	".\Camlwin_\graphdoc.obj" \
	".\Camlwin_\graphvw.obj" \
	".\Camlwin_\camlwin.obj" \
	".\Camlwin_\xeditvw.obj" \
	".\Camlwin_\prefsdlg.obj" \
	".\Camlwin_\histdoc.obj" \
	".\Camlwin_\ui.obj" \
	".\Camlwin_\txtrmfr.obj" \
	".\Camlwin_\CAMLWIN.res"

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
	".\RES\TERM.ICO"\
	".\RES\HIST.ICO"\
	".\RES\TOOLBAR.BMP"\
	".\res\camlwin.rc2"\
	

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

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

DEP_CPP_CAMLWI=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\mainfrm.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\xeditvw.h"\
	".\histdoc.h"\
	".\graphdoc.h"\
	".\graphfrm.h"\
	".\graphvw.h"\
	".\prefsdlg.h"\
	
NODEP_CPP_CAMLWI=\
	".\SaveSettings"\
	
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

DEP_CPP_CAMLWI=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\mainfrm.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\xeditvw.h"\
	".\histdoc.h"\
	".\graphdoc.h"\
	".\graphfrm.h"\
	".\graphvw.h"\
	".\prefsdlg.h"\
	
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

DEP_CPP_CAMLWI=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\mainfrm.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\xeditvw.h"\
	".\histdoc.h"\
	".\graphdoc.h"\
	".\graphfrm.h"\
	".\graphvw.h"\
	".\prefsdlg.h"\
	
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
	".\stdafx.h"\
	".\camlwin.h"\
	".\mainfrm.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
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
	".\stdafx.h"\
	".\xeditvw.h"\
	".\camlwin.h"\
	".\txtrmdoc.h"\
	

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
	".\stdafx.h"\
	".\camlwin.h"\
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
	".\stdafx.h"\
	".\camlwin.h"\
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
	".\stdafx.h"\
	".\xeditvw.h"\
	".\camlwin.h"\
	".\histdoc.h"\
	

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
	".\stdafx.h"\
	".\camlwin.h"\
	".\prefsdlg.h"\
	

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
	".\stdafx.h"\
	".\camlwin.h"\
	

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

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

DEP_CPP_GRAPH=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\graphvw.h"\
	".\graphdoc.h"\
	
NODEP_CPP_GRAPH=\
	".\;"\
	

"$(INTDIR)\graphvw.obj" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"

"$(INTDIR)\graphvw.sbr" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

DEP_CPP_GRAPH=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\graphvw.h"\
	".\graphdoc.h"\
	

"$(INTDIR)\graphvw.obj" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"

"$(INTDIR)\graphvw.sbr" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

DEP_CPP_GRAPH=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\graphvw.h"\
	".\graphdoc.h"\
	

"$(INTDIR)\graphvw.obj" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"

"$(INTDIR)\graphvw.sbr" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphdoc.cpp

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

DEP_CPP_GRAPHD=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\xeditvw.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\mainfrm.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
	".\..\..\RUNTIME\alloc.h"\
	".\..\..\RUNTIME\memory.h"\
	".\..\..\RUNTIME\fail.h"\
	".\..\..\RUNTIME\signals.h"\
	".\..\..\RUNTIME\str.h"\
	".\colors.h"\
	".\..\..\RUNTIME\misc.h"\
	".\..\..\RUNTIME\mlvalues.h"\
	".\..\..\RUNTIME\config.h"\
	".\..\..\..\config\m.h"\
	".\..\..\..\config\s.h"\
	".\..\..\RUNTIME\gc.h"\
	".\..\..\RUNTIME\major_gc.h"\
	".\..\..\RUNTIME\minor_gc.h"\
	".\..\..\RUNTIME\freelist.h"\
	
NODEP_CPP_GRAPHD=\
	".\CAMLGraph"\
	

"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"

"$(INTDIR)\graphdoc.sbr" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

DEP_CPP_GRAPHD=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\xeditvw.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\mainfrm.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
	".\..\..\RUNTIME\alloc.h"\
	".\..\..\RUNTIME\memory.h"\
	".\..\..\RUNTIME\fail.h"\
	".\..\..\RUNTIME\signals.h"\
	".\..\..\RUNTIME\str.h"\
	".\colors.h"\
	".\..\..\RUNTIME\misc.h"\
	".\..\..\RUNTIME\mlvalues.h"\
	".\..\..\RUNTIME\config.h"\
	".\..\..\..\config\m.h"\
	".\..\..\..\config\s.h"\
	".\..\..\RUNTIME\gc.h"\
	".\..\..\RUNTIME\major_gc.h"\
	".\..\..\RUNTIME\minor_gc.h"\
	".\..\..\RUNTIME\freelist.h"\
	

"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"

"$(INTDIR)\graphdoc.sbr" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 BoundsCheck"

DEP_CPP_GRAPHD=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\xeditvw.h"\
	".\txtrmdoc.h"\
	".\txtrmfr.h"\
	".\mainfrm.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
	".\..\..\RUNTIME\alloc.h"\
	".\..\..\RUNTIME\memory.h"\
	".\..\..\RUNTIME\fail.h"\
	".\..\..\RUNTIME\signals.h"\
	".\..\..\RUNTIME\str.h"\
	".\colors.h"\
	".\..\..\RUNTIME\misc.h"\
	".\..\..\RUNTIME\mlvalues.h"\
	".\..\..\RUNTIME\config.h"\
	".\..\..\..\config\m.h"\
	".\..\..\..\config\s.h"\
	".\..\..\RUNTIME\gc.h"\
	".\..\..\RUNTIME\major_gc.h"\
	".\..\..\RUNTIME\minor_gc.h"\
	".\..\..\RUNTIME\freelist.h"\
	

"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"

"$(INTDIR)\graphdoc.sbr" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphfrm.cpp
DEP_CPP_GRAPHF=\
	".\stdafx.h"\
	".\camlwin.h"\
	".\graphfrm.h"\
	

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
