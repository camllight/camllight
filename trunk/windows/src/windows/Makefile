all:
	cd runtime
	nmake
	cd ..\gui
        nmake -f camlwin.mak CFG="Camlwin - Win32 Release"


clean:
	cd gui
	del *.pdb
	cd windebug
	del *.aps
	del *.bsc
	del *.clw
	del *.exe
	del *.exp
	del *.idb
	del *.ilk
	del *.lib
	del *.obj
	del *.pch
	del *.pdb
	del *.sbr
	del *.vcw
	del *.wsp
	cd ..\winrelease
	del *.aps
	del *.bsc
	del *.clw
	del *.exe
	del *.exp
	del *.idb
	del *.ilk
	del *.lib
	del *.obj
	del *.pch
	del *.pdb
	del *.sbr
	del *.vcw
	del *.wsp
	cd ..\..\runtime
	del *.obj
	del *.lib
