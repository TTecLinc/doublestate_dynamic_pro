# Microsoft Developer Studio Project File - Name="hk_v24" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=hk_v24 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hk_v24.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hk_v24.mak" CFG="hk_v24 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hk_v24 - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "hk_v24 - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "hk_v24 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /browser /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /c
# ADD BASE RSC /l 0x407 /d "NDEBUG"
# ADD RSC /l 0x407 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "hk_v24 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x407 /d "_DEBUG"
# ADD RSC /l 0x407 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "hk_v24 - Win32 Release"
# Name "hk_v24 - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\aggrchecks.f90
DEP_F90_AGGRC=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\aggrdec.f90
DEP_F90_AGGRD=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\alexfuncs.f90
DEP_F90_ALEXF=\
	".\Debug\alexutils.mod"\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\alexutils.f90
DEP_F90_ALEXU=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\alg_gs.f90
DEP_F90_ALG_G=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\alg_gsqn.f90
DEP_F90_ALG_GS=\
	".\Debug\alexutils.mod"\
	".\Debug\params.mod"\
	".\Debug\paramsalg.mod"\
	".\Debug\paramsimsl.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\alg_qn.f90
DEP_F90_ALG_Q=\
	".\Debug\alexfuncs.mod"\
	".\Debug\alexutils.mod"\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\broyden.f90
DEP_F90_BROYD=\
	".\Debug\alexfuncs.mod"\
	".\Debug\alexutils.mod"\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\calibrhkcons.f90
# End Source File
# Begin Source File

SOURCE=.\calibrolg.f90
DEP_F90_CALIB=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\calibrpens.f90
DEP_F90_CALIBR=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\demographics.f90
DEP_F90_DEMOG=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\fsinglehh.f90
DEP_F90_FSING=\
	".\Debug\params.mod"\
	".\Debug\paramshh.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\guess.f90
DEP_F90_GUESS=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\house.f90
DEP_F90_HOUSE=\
	".\Debug\params.mod"\
	".\Debug\paramshh.mod"\
	".\Debug\paramsimsl.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\housegiven.f90
DEP_F90_HOUSEG=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\main.f90
DEP_F90_MAIN_=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\olg.f90
DEP_F90_OLG_F=\
	".\Debug\alexutils.mod"\
	".\Debug\params.mod"\
	".\Debug\paramsalg.mod"\
	".\Debug\paramsimsl.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\Params.f90
# End Source File
# Begin Source File

SOURCE=.\paramsalg.f90
DEP_F90_PARAM=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\paramshh.f90
DEP_F90_PARAMS=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\paramsimsl.f90
NODEP_F90_PARAMSI=\
	".\simsl.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\saving.f90
DEP_F90_SAVIN=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\solveolg.f90
DEP_F90_SOLVE=\
	".\Debug\params.mod"\
	".\Debug\paramsalg.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\testsettings.f90
DEP_F90_TESTS=\
	".\Debug\params.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\welfare.f90
DEP_F90_WELFA=\
	".\Debug\params.mod"\
	".\Debug\paramsimsl.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
