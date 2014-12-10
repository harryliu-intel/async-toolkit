; Copyright 2002 Fulcrum Microsystems.  All rights reserved.
; $Id$
; $DateTime$
; $Author$

(defun UIRedraw ()
  ( UIInit )
  ( UIDrawUITableOnWindow 
    TheUITable
    ( getCurrentWindow ) ) )
;;;

(defun UIGetCellView ()
  (cond (
         (let (
               ( NonIconified
                 ( setof Window ( hiGetWindowList ) ( and 
                                                      ( getq Window cellView )
                                                      ( equal ( hiGetWindowState Window ) `mapped ) ) ) ) )
               (if ( equal ( length NonIconified ) 1 )
                   ( getq ( car NonIconified ) cellView ) ) ) )
        (
         ( getq 
           ( car ( exists Selected ( geGetSelectedSet )
                          ( getq Selected cellView ) ) )
           cellView ) )
        (
         ( geGetWindowCellView ) ) ) )

(defun UIGetUIFileHeader ( UIFile )
  (let (
        ( InPort ( infile UIFile ) )
        ( Expression nil ) )
    (when InPort 
      ( while ( or ( null Expression )
                   ( not ( listp Expression ) ) )
        ( setq Expression ( eval ( read InPort ) ) ) )
      ( close InPort )
      Expression ) ) )

(defun UIGetUIDirectoryHeader ( UIDir )
  ( UIGetUIFileHeader ( sprintf nil "%s/.menu" UIDir ) ) )

(defun UICreateUIIntermediateFromUIDirectory ( UIDirectory )
  (when ( isReadable UIDirectory )
    (defvar UIStack nil )
    (let ( 
          ( Header ( UIGetUIDirectoryHeader UIDirectory ) ) )
      ( UIPushUI ( UICreateMenu
                   ( car Header )
                   ( car Header ) ) )
      ( UICreateUIIntermediateFromUIDirectoryImpl
        UIDirectory )
      ( UIGetCurrentUI ) ) ) )

(defun UICreateUIIntermediateFromUIDirectoryImpl ( UIDirectory )
  (when ( isReadable UIDirectory )
    ( mapcar 
      ( lambda ( File )
        (cond (
               ( and ( isFile File )
                     ( equal ( car ( last ( parseString File "." ) ) ) "il" ) )
               (let ( 
                     ( Header ( UIGetUIFileHeader File ) ) )                
                 ; printf "file header: %L\n" Header )
                 ( UIPushUI ( UICreateMenuItem
                              ( cadr Header )
                              ( car Header )
                              ( sprintf nil "( %sCB )" ( cadr Header ) ) ) )
                 ( load File )
                 ( UIPopUI ) ) )
              ( 
               ( isDir File )
               (let ( 
                     ( Header ( UIGetUIDirectoryHeader File ) ) )
                 (if Header
                     (let ()
                       ( UIPushUI ( UICreateSubMenu
                                    ( cadr Header )
                                    ( car Header ) ) )
                       ( UICreateUIIntermediateFromUIDirectoryImpl
                         File )
                       ( UIPopUI ) )
                   ( printf "UI Error: No .menu file %s\n" File ) )
                   ) ) ) )
      ( mapcar 
        ( lambda ( File )
          ( sprintf nil "%s/%s" UIDirectory File ) )
        ( setof File ( getDirFiles UIDirectory )
                ( and  ( not ( equal File ".menu" ) )
                       ( not ( equal File "." ) )
                      ( not ( equal File ".." ) ) ) ) ) ) ) )

(defun UIReplaceWithEnv ( CDSConfigTable Key )
  (let (
        ( Value ( getShellEnvVar Key ) )
        ( ShellVar ( sprintf nil "${%s}" Key ) ) )
  (when Key
    ( rexCompile ShellVar )
    ( foreach 
      Key
      CDSConfigTable
      ( setarray 
        CDSConfigTable
        Key
        ( rexReplace 
          ( arrayref CDSConfigTable Key )
          Value 1 ) ) ) ) ) )

(defun UILoadDir ( Dir @rest argsRE )
  ( foreach
    File
    ( setof 
      File
      ( getDirFiles Dir )
      ( and 
        ( not ( equal File "." ) )
        ( not ( equal File ".." ) )
        ( not ( rexMatchp "~$" File ) )
        ( not ( rexMatchp "\\.rp$" File ) )
        ( forall RE argsRE ( rexMatchp RE File ) ) ) )
    ( load ( strcat Dir "/" File ) ) ) )
  
; 
; this function loads all of the files in the user's autoload directory and
; is currently not used and remains for reference
;

(defun UIAutoLoad ( CDSConfigTable )
 (let (
       ( DirectoryString ( ConfigFileGetValue CDSConfigTable "AUTOLOAD_DIRS" ) ) )
   (let (
         ( Directories  ( append ( parseString DirectoryString ":" ) ( list "autoload" ) ) ) )
     ( foreach Directory Directories
               (if ( isDir Directory )
                   ( foreach File ( setof File ( getDirFiles Directory )
                                          ( and ( not ( equal File "." ) )
                                                ( not ( equal File ".." ) ) ) )
                             ( load ( sprintf nil "%s/%s" Directory File ) ) ) ) ) ) )  t )

(defun UIInit ( @rest args )
  (let (
        ( VirtuosoHome ( getShellEnvVar "VIRTUOSO_HOME" ) )
        ( FulcrumPDKRoot ( getShellEnvVar "FULCRUM_PDK_ROOT" ) ) )
    
    (if VirtuosoHome
        ( printf "VIRTUOSO_HOME: %s\n" VirtuosoHome )
      (error "No $VIRTUOSO_HOME (root of virtuoso-integration package)" ) )
    (if FulcrumPDKRoot
        ( printf "FULCRUM_PDK_ROOT: %s\n" FulcrumPDKRoot )
      (error "No $FULCRUM_PDK_ROOT (root of fulcrum-*-pdk package installation)" ) )

    (let (
          ( LoadMe
            ( list
              ".cdsinit.user"
              ( strcat FulcrumPDKRoot
                       "/share/Fulcrum/autoload/bindkeys" )
              ( strcat VirtuosoHome
                       "/share/skill/ui/ui/build/share/autoload/bindkeys" ) 
              ( strcat VirtuosoHome
                       "/share/skill/ui/ui/build/share/autoload/bindkeys_hand" ) 
             ) )
          )

    (defvar TheCDSConfigTable ( makeTable `cdsconfig nil ) )
    (defvar TheUITable ( makeTable `ui nil ) )
    ( ConfigFileGetShellEnv TheCDSConfigTable )
    ( ConfigFileParseConfigFileToTable 
      TheCDSConfigTable
      "cds.config" )
    ( ConfigFileParseConfigFileToTable 
      TheCDSConfigTable
      ( strcat FulcrumPDKRoot "/share/Fulcrum/pdk.config" ) )
    ( load
      ( strcat FulcrumPDKRoot "/share/Fulcrum/pdkinfo.il" ) )
    ( load
      ( strcat FulcrumPDKRoot "/share/Fulcrum/businfo.il" ) )
    ( load
      ( strcat FulcrumPDKRoot "/share/Fulcrum/cableinfo.il" ) )

    ( UIReplaceWithEnv TheCDSConfigTable "VIRTUOSO_HOME" )
    ( UIReplaceWithEnv TheCDSConfigTable "FULCRUM_PDK_ROOT" )
    ( setarray TheCDSConfigTable "FULCRUM_PDK_ROOT" FulcrumPDKRoot )

    ( foreach File LoadMe (when ( isFile File ) ( load File ) ) )
    ; replaces UIAutoLoad, left as list for expansion if needed (AAG)
    (let (
       ( Directories  ( list "autoload" ) ) )
          ( foreach Directory Directories
              (if ( isDir Directory )
                  ( foreach File ( setof File ( getDirFiles Directory )
                            ( and ( not ( equal File "." ) )
                                     ( not ( equal File ".." ) ) ) )
                         ( load ( sprintf nil "%s/%s" Directory File ) ) ) ) ) )

    (when ( hiGraphicMode )
      ( createDir "temp" )
      ( foreach 
        Directory 
        ( setof 
          Directory 
          ( append 
            ( parseString 
              (cond ( ( arrayref TheCDSConfigTable "UI_DIRECTORY" ) ) ( "" ) )
              ":" )
            ( list ( strcat FulcrumPDKRoot "/share/Fulcrum/menu" )
                   ( strcat VirtuosoHome "/share/skill/ui/ui/build/share/Fulcrum" )
                   ( strcat VirtuosoHome "/share/skill/ui/ui/build/share/Nano" )
                   ( strcat VirtuosoHome "/share/skill/ui/ui/build/share/Floorplan" )
                   ( strcat VirtuosoHome "/share/skill/ui/ui/build/share/Hercules" )
                   ( strcat VirtuosoHome "/share/skill/ui/ui/build/share/Proteus" )
                   "usermenu" ) )
          ( isDir Directory ) )
        ( setarray 
          TheUITable
          Directory 
          ( UIGetNativeRootMenu 
            ( UICreateUIIntermediateFromUIDirectory Directory ) ) ) )
      (deUnRegUserTriggers "maskLayout" )
      (deUnRegUserTriggers "schematic" )
      (deRegUserTriggers "maskLayout" nil 'UIGetTheUITableNativeRootMenus nil )
      (deRegUserTriggers "schematic" nil 'UIGetTheUITableNativeRootMenus nil )
      (if (not (null (deGetAppInfo "maskLayoutXL")))
          (let ()
            (deUnRegUserTriggers "maskLayoutXL" )
            (deRegUserTriggers "maskLayoutXL" nil 'UIGetTheUITableNativeRootMenus nil ) ) ) ) ) ) )


(defun UIGetTheUITableNativeRootMenus ( Args )
  ( UIGetUITableNativeRootMenus TheUITable ) )

(defun UIGetUITableNativeRootMenus ( UITable )
  (let ( 
        ( NativeMenuList nil ) )
    ( foreach 
      Directory
      UITable
      (let (
            ( NativeMenu ( arrayref UITable Directory ) ) )
        (if NativeMenu
            ( setq NativeMenuList ( cons NativeMenu NativeMenuList ) ) ) ) )
    NativeMenuList ) )

(defun UIDrawUITableOnWindow ( UITable Window )
  ( foreach NativeMenu ( UIGetUITableNativeRootMenus UITable )
            ( UIDrawNativeRootMenu NativeMenu Window ) ) )

(defun UIDrawNativeRootMenu ( BannerMenu Window )
  (let (
        ( BannerIndex
          ( ListFindIndex 
            ( hiGetBannerMenus Window )
            ( getq BannerMenu hiMenuSym ) ) ) )
    (if BannerIndex
        ( hiDeleteBannerMenu           
          Window
          BannerIndex ) )        
    ( hiInsertBannerMenu
      Window
      BannerMenu
      (cond (
             BannerIndex )
            ( 
               1000 ) ) ) ) )

(defun UIGetNativeRootMenu ( UIRootComponent )
  (let (
        ( BannerMenu
         ( UICreateNative
           UIRootComponent
           `UIDefaultFactory
           t ) ) )
    BannerMenu ) )

(defun UICreateNative ( UIRootComponent UIFactory TopLevel )
  ; do children first
  (let (
        ( Children 
          ;the actual db structs
          ( mapcar 
            ( lambda ( UIComponent )
               ( UICreateNative UIComponent UIFactory nil ) )
            ;the UIs
            ( mapcar 
              `cadr 
              ( tableToList ( UIGetChildren UIRootComponent ) ) ) ) ) )
    (if ( or Children ( not TopLevel ) )
        ( apply UIFactory ( list UIRootComponent Children ) ) ) ) )


(defun UIDefaultFactory ( UIComponent Children )
  (let (
        ( Constructor ( UIGetConstructor UIComponent ) ) )
    (let (
          ( Args (if Children ( list Children ) ( list nil )  ) ) )
      ( apply ( car Constructor ) Args )
      ) ) )

;UI Stack
(defun UIPushUI ( UIComponent )
  ( setq UIStack ( cons UIComponent UIStack ) ) )

(defun UIPopUI ()
  (let (
        ( Element ( car UIStack ) ) )
    ( setq UIStack ( cdr UIStack ) )
    Element ) )

(defun UIGetCurrentUI ()
  ( car UIStack ) )

;;UI interface
(defun UISetName ( UIComponent Name )
  ( setarray UIComponent "name" Name ) )

(defun UIGetName ( UIComponent )
  ( arrayref UIComponent "name" ) )

(defun UISetConstructor ( UIComponent Constructor )
  ( setarray UIComponent "constructor" Constructor ) )

(defun UIGetConstructor ( UIComponent )
  ( arrayref UIComponent "constructor" ) )

(defun UICreateConstructor ( ConstructorFunc )
  ( list ConstructorFunc ) )

(defun UIGetChildren ( UIComponent )
  (let (
        ( Children 
          (cond (
                 ( arrayref UIComponent "children" ) )
                ( 
                 ( makeTable "children" nil ) ) ) ) )
    ( setarray UIComponent "children" Children )
    Children ) )

(defun UIAddChild ( UIComponent ChildUI )
  (when UIComponent 
    ( setarray ( UIGetChildren UIComponent ) ( UIGetName ChildUI ) ChildUI ) ) )


;;;UI Creation
(defun UICreateMenuItemCallBack ( FuncSymbol Func )
   ( putd FuncSymbol 
          ( ExpressionReplaceSymbolsWithValues
            `(lambda ( )
               ( apply Func nil ) )
            ( list `Func ) ) ) )

(defun UICreateAction ( FuncSymbol Func )
  ( putd FuncSymbol 
         ( ExpressionReplaceSymbolsWithValues
           `(lambda ( ) 
              ( apply Func nil ) )
           ( list `Func ) ) ) )

(defun UICreateComponent ( Name Constructor )
  (let (
        ( CurrentUI ( UIGetCurrentUI ) )
        ( NewUI ( makeTable `ui nil ) ) )
    ( UISetName NewUI Name )
    ( UISetConstructor NewUI Constructor )
    ( UIAddChild CurrentUI NewUI )
  NewUI ) )


(defun UICreateMenu ( Name Text )
  (let (
        ( Constructor ( UICreateConstructor 
                        ( ExpressionReplaceSymbolsWithValues
                          `( lambda ( Items )
                             ( hiCreatePulldownMenu
                               ( stringToSymbol Name )
                               Text 
                               sort( Items lambda((x y) strcmp(x~>hiItemText y~>hiItemText)<0))  
                               ) )
                          ( list `Name `Text ) ) ) ) )
    ( UICreateComponent 
      Name
      Constructor ) ) )

(defun UICreateSubMenu ( Name Text )
  (let (
        ( Constructor ( UICreateConstructor 
                        ( ExpressionReplaceSymbolsWithValues
                          `( lambda ( Items )
                             ( hiCreateSliderMenuItem
                               ?name ( stringToSymbol ( sprintf nil "%s_slider" Name ) )
                               ?itemText Text
                               ?subMenu ( hiCreatePulldownMenu
                                          ( stringToSymbol Name )
                                          Text
                                          sort( Items lambda((x y) strcmp(x~>hiItemText y~>hiItemText)<0)) ) ) )
                          ( list `Name `Text ) ) ) ) )
    ( UICreateComponent 
      Name
      Constructor ) ) )

(defun UICreateMenuItem ( Name Text CallBack )
  (let (
        ( Constructor ( UICreateConstructor 
                        ( ExpressionReplaceSymbolsWithValues
                          `( lambda ( Children )
                             ( hiCreateMenuItem
                               ?name ( stringToSymbol Name )
                               ?itemText Text
                               ?callback CallBack ) )
                          ( list `Name `Text `CallBack ) ) ) ) )
    ( UICreateComponent 
      Name
      Constructor ) ) )


(defun UILoadPreferences ( FormSymbol )
  (let (
        ( PreferencesDir 
          ( ddGetStartup ".UIPreferences" ) ) )
    (cond (
           ( stringp PreferencesDir )
           ( UILoadPreferencesImpl 
             PreferencesDir
             FormSymbol ) )
          (
           t ) ) ) )

(defun UILoadPreferencesImpl ( PreferencesDir
                               FormSymbol )
  (when ( isDir PreferencesDir )
    (let (
          ( PreferencesFile
            ( sprintf 
              nil
              "%s/%s"
              PreferencesDir        
              ( symbolToString FormSymbol ) ) )
          ( Form ( symeval FormSymbol ) ) )
      (let (
            ( KeyValue t )
            ( InPort ( infile PreferencesFile ) )
            )
        (while
            ( setq KeyValue
                   ( eval ( read InPort ) ) )
          (let (
                ( Field ( get Form ( car KeyValue ) ) )
                ( Value ( cadr KeyValue ) ) )
            (unless ( equal Field->value Value )
              Field->value = Value
              ) ) )
      ( close InPort )
 ) ) ) )

(defun UICreateForm ( Name
                      CallBack )
  ( UICreateComponent
    Name
    ( UICreateConstructor CallBack )) )

(defun UIWritePreferences ( FormSymbol )
  (let (
        ( PreferencesDir 
          ( sprintf
            nil
            "%s.UIPreferences"
            ( ddGetStartup "" ) ) ) )
    ( createDir 
      PreferencesDir  )
    ( UIWritePreferencesImpl 
      PreferencesDir
      FormSymbol )
) )

(defun UIWritePreferencesImpl ( PreferencesDir
                                FormSymbol )
  (let (
        ( Form ( symeval FormSymbol ) )
        ( PreferencesFile
          ( sprintf 
            nil
            "%s/%s"
            PreferencesDir        
            ( symbolToString FormSymbol ) ) ) )
    (let (
          ( OutPort ( outfile PreferencesFile "w" ) ) )
      ( foreach 
        Field
        ( getq Form fieldList )
        ( fprintf 
          OutPort
          "( list `%s %L)\n" 
          ( symbolToString Field )
          ( get Form Field )->value
        ) )
      ( close OutPort ) ) ) )

(defun UIPopUpDialog (string) 
            ( hiDisplayAppDBox
              ?name `UIWarnBox
              ?dboxBanner "Warning"  
              ?dboxText string
              ?buttonLayout 'Close
              ?help nil
             )
)
