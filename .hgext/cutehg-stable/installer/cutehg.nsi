; TODO: Cleanup up this file, it is a bit of a mess

!include "MUI2.nsh"

!define VERSION "0.1"
!define VERSION_QUAD "0.1.0.0"
!define NAME "CuteHg"
!define NAMEL "cutehg"
!define SRCDIR "build\hg-cutehg\dist"

Name "${NAME}"
outFile "${NAME}-installer-${VERSION}.exe"
 
# define installation directory
InstallDir "$PROGRAMFILES\${NAME}"
InstallDirRegKey HKCU "Software\${NAME}" "Install_Dir"

;Request application privileges for Windows Vista
RequestExecutionLevel admin
SetCompressor lzma

XPStyle on

Var StartMenuFolder

#Page components
!insertmacro MUI_PAGE_DIRECTORY

;Start Menu Folder Page Configuration
!define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKCU"
!define MUI_STARTMENUPAGE_REGISTRY_KEY "Software\${NAME}" 
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
  
!insertmacro MUI_PAGE_STARTMENU Application $StartMenuFolder

!insertmacro MUI_PAGE_INSTFILES
  
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

;Languages
!insertmacro MUI_LANGUAGE "English"

;Version info
VIAddVersionKey /LANG=${LANG_ENGLISH} "ProductName" "${NAME}"
VIAddVersionKey /LANG=${LANG_ENGLISH} "Comments" "Qt4 Dialog Extension of Mercurial"
;VIAddVersionKey /LANG=${LANG_ENGLISH} "CompanyName" "Fake company"
;VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalTrademarks" "Test Application is a trademark of Fake company"
VIAddVersionKey /LANG=${LANG_ENGLISH} "LegalCopyright" "© 2009 Tom Burdick"
VIAddVersionKey /LANG=${LANG_ENGLISH} "FileDescription" "Qt4 Dialog Extension of Mercurial"
VIAddVersionKey /LANG=${LANG_ENGLISH} "FileVersion" "${VERSION}"
VIProductVersion "${VERSION_QUAD}"

#Page directory
#Page instfiles

#UninstPage uninstConfirm
#UninstPage instfiles

# start default section
section

    # set the installation directory as the destination for the following actions
    setOutPath $INSTDIR
 
    File ${SRCDIR}\hg.exe
    File ${SRCDIR}\library.zip
    File ${SRCDIR}\*.pyd
    File ${SRCDIR}\*.dll
    File ${SRCDIR}\mercurial.ini
    File ${SRCDIR}\w9xpopen.exe
    File /r ${SRCDIR}\mercurial

    ; Overlays
    File ${SRCDIR}\tortoiseoverlays-win32.msi
    File ${SRCDIR}\tortoiseoverlays-x64.msi

    # TODO: Detect 64 bit and register cutehg-64.dll instead
    RegDLL $INSTDIR\cutehg-32.dll

    WriteRegStr HKCU "SOFTWARE\${NAME}" "Install_Dir" "$INSTDIR"

    # create the uninstaller
    WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "DisplayName" "${NAME}"
    WriteRegStr HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "UninstallString" '"$INSTDIR\uninstall.exe"'
    WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoModify" 1
    WriteRegDWORD HKCU "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}" "NoRepair" 1
    WriteUninstaller "$INSTDIR\uninstall.exe"

sectionEnd

Section "Start Menu Shortcuts"

    !insertmacro MUI_STARTMENU_WRITE_BEGIN Application
    
    ;Create shortcuts
    CreateDirectory "$SMPROGRAMS\$StartMenuFolder"

    # This is only a test
    CreateShortCut "$SMPROGRAMS\$StartMenuFolder\${NAME}.lnk" "$INSTDIR\hg.exe" "cuteupdate"

    CreateShortCut "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk" "$INSTDIR\Uninstall.exe"
  
    !insertmacro MUI_STARTMENU_WRITE_END
  
SectionEnd

# uninstaller section start
section "uninstall"
 
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\${NAME}"
    DeleteRegKey /ifempty HKCU SOFTWARE\${NAME}

    UnRegDLL $INSTDIR\cutehg-32.dll

    delete "$INSTDIR\hg.exe"
    delete "$INSTDIR\library.zip"
    delete "$INSTDIR\*.dll"
    delete "$INSTDIR\*.pyd"
    delete "$INSTDIR\mercurial.ini"
    delete "$INSTDIR\w9xpopen.exe"
    delete "$INSTDIR\mercurial\*.*"

    delete "$INSTDIR\uninstall.exe"
    # $INSTDIR/mercurial is not cleaned up properly, need to find a way to do this as easy as possible
    RMDir "$INSTDIR\mercurial"
    RMDir "$INSTDIR"

    !insertmacro MUI_STARTMENU_GETFOLDER Application $StartMenuFolder
    Delete "$SMPROGRAMS\$StartMenuFolder\Uninstall.lnk"
    RMDir "$SMPROGRAMS\$StartMenuFolder"
 
# uninstaller section end
sectionEnd

# Make sure only one installer is active
Function .onInit
    System::Call 'kernel32::CreateMutexA(i 0, i 0, t "${NAME}Installer") i .r1 ?e'
    Pop $R0
 
    StrCmp $R0 0 +3
        MessageBox MB_OK|MB_ICONEXCLAMATION "The installer is already running."
        Abort
FunctionEnd
