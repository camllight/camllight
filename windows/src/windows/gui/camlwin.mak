# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Camlwin - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Camlwin - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Camlwin - Win32 Debug" && "$(CFG)" !=\
 "Camlwin - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "camlwin.mak" CFG="Camlwin - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Camlwin - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE "Camlwin - Win32 Release" (based on "Win32 (x86) Application")
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
RSC=rc.exe
MTL=mktyplib.exe

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

ALL : "$(OUTDIR)\camlwin.exe"

CLEAN : 
	-@erase ".\WinDebug\vc40.pdb"
	-@erase ".\WinDebug\camlwin.pch"
	-@erase ".\WinDebug\vc40.idb"
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

# ADD BASE CPP /nologo /G3 /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /FR /YX /c
# ADD CPP /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\WinDebug/
CPP_SBRS=
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
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 oldnames.lib /nologo /stack:0x11240 /subsystem:windows /debug /machine:IX86
# ADD LINK32 ..\runtime\libcaml.lib /nologo /stack:0x11240 /subsystem:windows /debug /machine:IX86
LINK32_FLAGS=..\runtime\libcaml.lib /nologo /stack:0x11240 /subsystem:windows\
 /incremental:yes /pdb:"$(OUTDIR)/camlwin.pdb" /debug /machine:IX86\
 /def:".\CAMLWIN.DEF" /out:"$(OUTDIR)/camlwin.exe" 
DEF_FILE= \
	".\CAMLWIN.DEF"
LINK32_OBJS= \
	"$(INTDIR)/histdoc.obj" \
	"$(INTDIR)/camlwin.obj" \
	"$(INTDIR)/graphfrm.obj" \
	"$(INTDIR)/graphdoc.obj" \
	"$(INTDIR)/mainfrm.obj" \
	"$(INTDIR)/txtrmfr.obj" \
	"$(INTDIR)/graphvw.obj" \
	"$(INTDIR)/prefsdlg.obj" \
	"$(INTDIR)/STDAFX.OBJ" \
	"$(INTDIR)/ui.obj" \
	"$(INTDIR)/xeditvw.obj" \
	"$(INTDIR)/txtrmdoc.obj" \
	"$(INTDIR)/CAMLWIN.res"

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

ALL : "$(OUTDIR)\camlwin.exe"

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

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /MT /W3 /GX /O1 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /FR /c
# ADD CPP /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\WinRelease/
CPP_SBRS=
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
BSC32_SBRS=
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
	"$(INTDIR)/camlwin.obj" \
	"$(INTDIR)/prefsdlg.obj" \
	"$(INTDIR)/graphfrm.obj" \
	"$(INTDIR)/mainfrm.obj" \
	"$(INTDIR)/txtrmfr.obj" \
	"$(INTDIR)/graphvw.obj" \
	"$(INTDIR)/STDAFX.OBJ" \
	"$(INTDIR)/txtrmdoc.obj" \
	"$(INTDIR)/graphdoc.obj" \
	"$(INTDIR)/xeditvw.obj" \
	"$(INTDIR)/ui.obj" \
	"$(INTDIR)/histdoc.obj" \
	"$(INTDIR)/CAMLWIN.res"

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

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\CAMLWIN.RC
DEP_RSC_CAMLW=\
	".\RES\CAMLWIN.ICO"\
	".\RES\TERM.ICO"\
	".\RES\HIST.ICO"\
	".\RES\TOOLBAR.BMP"\
	".\RES\CAMLWIN.RC2"\
	

"$(INTDIR)\CAMLWIN.res" : $(SOURCE) $(DEP_RSC_CAMLW) "$(INTDIR)"
   $(RSC) $(RSC_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\STDAFX.CPP
DEP_CPP_STDAF=\
	".\STDAFX.H"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yc"STDAFX.H"
# ADD CPP /Yc"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yc"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\STDAFX.OBJ" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\camlwin.pch" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yc"STDAFX.H"
# ADD CPP /Yc"STDAFX.H"

BuildCmds= \
	$(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS"\
 /Fp"$(INTDIR)/camlwin.pch" /Yc"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE) \
	

"$(INTDIR)\STDAFX.OBJ" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

"$(INTDIR)\camlwin.pch" : $(SOURCE) $(DEP_CPP_STDAF) "$(INTDIR)"
   $(BuildCmds)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\camlwin.cpp
DEP_CPP_CAMLWI=\
	".\STDAFX.H"\
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
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\camlwin.obj" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\camlwin.obj" : $(SOURCE) $(DEP_CPP_CAMLWI) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\mainfrm.cpp
DEP_CPP_MAINF=\
	".\STDAFX.H"\
	".\camlwin.h"\
	".\mainfrm.h"\
	".\graphdoc.h"\
	".\graphvw.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\mainfrm.obj" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\mainfrm.obj" : $(SOURCE) $(DEP_CPP_MAINF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\txtrmdoc.cpp
DEP_CPP_TXTRM=\
	".\STDAFX.H"\
	".\xeditvw.h"\
	".\camlwin.h"\
	".\txtrmdoc.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\txtrmdoc.obj" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\txtrmdoc.obj" : $(SOURCE) $(DEP_CPP_TXTRM) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\txtrmfr.cpp
DEP_CPP_TXTRMF=\
	".\STDAFX.H"\
	".\camlwin.h"\
	".\txtrmfr.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\txtrmfr.obj" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\txtrmfr.obj" : $(SOURCE) $(DEP_CPP_TXTRMF) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\xeditvw.cpp
DEP_CPP_XEDIT=\
	".\STDAFX.H"\
	".\camlwin.h"\
	".\xeditvw.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\xeditvw.obj" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\xeditvw.obj" : $(SOURCE) $(DEP_CPP_XEDIT) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\histdoc.cpp
DEP_CPP_HISTD=\
	".\STDAFX.H"\
	".\xeditvw.h"\
	".\camlwin.h"\
	".\histdoc.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\histdoc.obj" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\histdoc.obj" : $(SOURCE) $(DEP_CPP_HISTD) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\prefsdlg.cpp
DEP_CPP_PREFS=\
	".\STDAFX.H"\
	".\camlwin.h"\
	".\prefsdlg.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\prefsdlg.obj" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\prefsdlg.obj" : $(SOURCE) $(DEP_CPP_PREFS) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\ui.cpp
DEP_CPP_UI_CP=\
	".\STDAFX.H"\
	".\camlwin.h"\
	

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\ui.obj" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /G3 /MTd /W4 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D\
 "_WINDOWS" /D "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c $(SOURCE)


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

# ADD BASE CPP /Yu"STDAFX.H"
# ADD CPP /Yu"STDAFX.H"

"$(INTDIR)\ui.obj" : $(SOURCE) $(DEP_CPP_UI_CP) "$(INTDIR)"\
 "$(INTDIR)\camlwin.pch"
   $(CPP) /nologo /MT /W4 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D\
 "_MBCS" /Fp"$(INTDIR)/camlwin.pch" /Yu"STDAFX.H" /Fo"$(INTDIR)/" /c $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\CAMLWIN.DEF

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphvw.cpp
DEP_CPP_GRAPH=\
	".\STDAFX.H"\
	".\camlwin.h"\
	".\graphvw.h"\
	".\graphdoc.h"\
	

"$(INTDIR)\graphvw.obj" : $(SOURCE) $(DEP_CPP_GRAPH) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphdoc.cpp

!IF  "$(CFG)" == "Camlwin - Win32 Debug"

DEP_CPP_GRAPHD=\
	".\STDAFX.H"\
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
	".\..\..\RUNTIME\MISC.H"\
	".\..\..\RUNTIME\MLVALUES.H"\
	".\..\..\RUNTIME\CONFIG.H"\
	".\..\..\..\config\M.H"\
	".\..\..\..\config\S.H"\
	".\..\..\RUNTIME\GC.H"\
	".\..\..\RUNTIME\MAJOR_GC.H"\
	".\..\..\RUNTIME\MINOR_GC.H"\
	".\..\..\RUNTIME\FREELIST.H"\
	
NODEP_CPP_GRAPHD=\
	".\;"\
	

"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "Camlwin - Win32 Release"

DEP_CPP_GRAPHD=\
	".\STDAFX.H"\
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
	".\..\..\RUNTIME\MISC.H"\
	".\..\..\RUNTIME\MLVALUES.H"\
	".\..\..\RUNTIME\CONFIG.H"\
	".\..\..\..\config\M.H"\
	".\..\..\..\config\S.H"\
	".\..\..\RUNTIME\GC.H"\
	".\..\..\RUNTIME\MAJOR_GC.H"\
	".\..\..\RUNTIME\MINOR_GC.H"\
	".\..\..\RUNTIME\FREELIST.H"\
	

"$(INTDIR)\graphdoc.obj" : $(SOURCE) $(DEP_CPP_GRAPHD) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\graphfrm.cpp
DEP_CPP_GRAPHF=\
	".\STDAFX.H"\
	".\camlwin.h"\
	".\graphfrm.h"\
	

"$(INTDIR)\graphfrm.obj" : $(SOURCE) $(DEP_CPP_GRAPHF) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
