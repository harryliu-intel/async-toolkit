#!/bin/bash

<<DOC
<html>
Sets up a cadence working directory for tools/users.  Files created include:
<dl>

<dt><b>.cdsinit</b>
<dd>This is automatically loaded at initialization time by cadence.  This is where we:

<ul>
<li> Load the <a href=autoload>autoload.il</a> so we can use all the <a href=skill.html>SKILL</a>
<li> Initialize the menus in the usermenu directory inside your cds working directory
<li> Initialize the Fulcrum menu from the <a href=package.html#ui>ui package</a>
<li> Read in the cds.config variables for use in ui tools.
</ul>

<dt><b>cds.lib</b>
<dd>Binds cadence libraries to the filesystem.  mkcdswd just references the cds.lib.generated from <tool cdsp4sync>.

<dt><b>cds.config</b>
<dd>Contains bash environment variable syntax key value pairs, specific to the user interface tools, including CAST_PATH,UI_DIRECTORY,TEMP.<br>

<dt><b>assura_tech.lib</b>
<dd>In the same syntax as the cds.lib, references the <a href=http://internal/eng/depot/hw/layout/tsmc13/pdk/main/Fulcrum/assura>assura</a> directory in the PDK installation.

</dl>

<p> Bindkeys are made available from several sources:</p>
<ul>
<li> <a href=/home/local/common/cadence/install/IC-50.33U3-Linux/doc/vlehelp/vlehelp.pdf>Standard cadence bindkeys</a> referenced in .cdsinit.user
<li> <a href=http://internal/eng/physical/cadence/cadence.pdf>Standard Fulcrum bindkeys</a> and <a href=$src/sw/cad/external-tools-integration/cadence/virtuoso/skill/ui/autoload/bindkey>more standard bindkeys</a>. See <bug 4210> as to why there are 2 versions of standard bindkeys. 
<li> <a href=$src/hw/layout/tsmc13/pdk/Fulcrum/bindkeys>PDK dependent bindkeys</a> loaded with <skill UIInit>.
</ul>

</html>
DOC

PATH="/usr/intel/bin:$PATH"

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

function usage() {
  cat<<USAGE
Usage: `basename $0`
    --target-dir=(location of cadence working directory)
    --dfII-dir=dir
    --fulcrum-pdk-root=dir
  [ --cast-path=dir ] (necessary for pins/synctonetlist,etc)
  [ --force ] (overwrite files when necessary)
  [ --p4-client=name ] (to automate p4 client)
  [ --temp ] (only for tool use)
  [ --user-template=dir ] (use menu/autoload from this dir)
USAGE
}

sedcmd=`which sed`
findcmd=`which find`
check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"" 2
check_executable_file "$findcmd" "Unable to find find in \"$PATH\"" 2

p4_client=
dfII_dir=
fulcrum_pdk_root=
cast_path=
cds_wd_target=
force=
interactive=1
user_template_cds_wd="$package_root/share/script/sh/setup/cds_wd_default_template"

for arg in $@ ; do
  
  case "$arg" in
  --p4-client=* )
    p4_client=`echo $arg | $sedcmd -e "s/--p4-client=//"`
    ;;
  --dfII-dir=* )
    dfII_dir=`echo $arg | $sedcmd -e "s/--dfII-dir=//"`
    ;;
  --fulcrum-pdk-root=* )
    fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  --cast-path=* )
    cast_path=`echo $arg | $sedcmd -e "s/--cast-path=//"`
    ;;
  --target-dir=* )
    cds_wd_target=`echo $arg | $sedcmd -e "s/--target-dir=//"`
    ;;
  --user-template=* )
    user_template_cds_wd=`echo $arg | $sedcmd -e "s/--user-template=//"`
    ;;
  --force )
    force=1
    ;;
  --temp )
    interactive=
    ;;
  --* )
    echo "Unknown option \"$arg\"."
    usage
    exit 2
    ;;
  esac
done

check_for_empty_arg "$package_root"  \
    "The packageroot variable must contain the location of the package installation." 2

check_for_empty_arg "$cds_wd_target" \
    "The cadence working directory was not specified." 2

check_for_empty_arg "$dfII_dir" \
    "The cadence dfII directory  was not specified." 2

check_for_empty_arg "$fulcrum_pdk_root" \
    "The pdk root was not specified." 2

check_writeable_dir "$cds_wd_target" \
    "Cadence Working Directory: \"$cds_wd_target\" is not a readable, writeable, directory."  1

conon_path "$cds_wd_target"
cds_wd_target="$ret"

if [ -n "$user_template_cds_wd" ] ; then  
  check_readable_dir "$user_template_cds_wd"                                                    \
      "Cadence Working Directory: \"$user_template_cds_wd\" is not a readable directory."  1
  conon_path "$user_template_cds_wd"
  user_template_cds_wd="$ret"
fi

check_readable_dir  "$fulcrum_pdk_root"                                          \
    "Fulcrum PDK: \"$fulcrum_pdk_root\" is not a readable directory."                  2
conon_path "$fulcrum_pdk_root"
fulcrum_pdk_root="$ret"

check_readable_dir  "$dfII_dir"                                          \
    "DFII dir: \"$dfII_dir\" is not a readable directory."                  2

if [ ! -r "$dfII_dir/cds.lib.generated" ] ; then
    echo "DFII dir \"$dfII_dir\" is not a valid dfII dir."
    echo "You may need to run cdsp4sync first."
fi

conon_path "$dfII_dir"
dfII_dir="$ret"

if [ -n "$target_dfII_dir" ] ; then
check_readable_dir  "$target_dfII_dir"                                          \
    "target DFII dir: \"$target_dfII_dir\" is not a readable directory."                  2
conon_path "$target_dfII_dir"
target_dfII_dir="$ret"
fi

# make directories
mkdir -p "$cds_wd_target/temp"
mkdir -p "$cds_wd_target/cdl"

# read the pdk.config file
pdk_config="$fulcrum_pdk_root/share/Fulcrum/pdk.config"
source "$package_root/share/script/sh/sh-lib/file/config.sh"
source "$pdk_config"

# display.drf
if [[ !( -e "$cds_wd_target/display.drf" ) || ( -n "$force" ) ]] ; then
cp -f "$fulcrum_pdk_root/share/Fulcrum/display.drf" "$cds_wd_target/display.drf"
fi

# cds.lib
# interactive mode - make a copy and warn if different
# force - overwrite
[ -n "$interactive" ] && [ -e "$cds_wd_target/cds.lib" ] && cp "$cds_wd_target/cds.lib" "$cds_wd_target/cds.lib.old"
if [[ !( -e "$cds_wd_target/cds.lib" ) || \
       ( -n "$interactive" ) || \
       ( -n "$force" ) ]] ; then
rm -f "$cds_wd_target/cds.lib"
cat <<EOF>"$cds_wd_target/cds.lib"
# Auto-generated - please edit cds.lib.user instead
SOFTINCLUDE $dfII_dir/cds.lib.generated
SOFTINCLUDE \${FULCRUM_PDK_ROOT}/share/Fulcrum/dfII/cds.lib
SOFTINCLUDE cds.lib.user
# Use following line instead to use fake gates/stacks
# SOFTINCLUDE \${FULCRUM_PDK_ROOT}/share/Fulcrum/dfII/cds.lib.fake
EOF
fi
[ -n "$interactive" ] && \
[ -z "$force" ] && \
[ -e "$cds_wd_target/cds.lib.old" ] && \
   ! ( diff "$cds_wd_target/cds.lib" "$cds_wd_target/cds.lib.old" ) && \
    echo Original "$cds_wd_target/cds.lib" saved to "$cds_wd_target/cds.lib.old" ... please merge manually

# .cdsinit
[ -n "$interactive" ] && [ -e "$cds_wd_target/.cdsinit" ] && cp "$cds_wd_target/.cdsinit" "$cds_wd_target/.cdsinit.old"
if [[ !( -e "$cds_wd_target/.cdsinit" ) || \
       ( -n "$interactive" ) || \
       ( -n "$force" ) ]] ; then
rm -f "$cds_wd_target/.cdsinit"
if [ -n "$interactive" ] ; then
cat <<SKILL>"$cds_wd_target/.cdsinit"
; Automatically generated 
; Please edit ".cdsinit.user" instead
; They will be automatically loaded
VirtuosoHome = ( getShellEnvVar "VIRTUOSO_HOME" )
(if VirtuosoHome
    ( load ( strcat VirtuosoHome "/share/skill/autoload.il" ) )
    (error "VIRTUOSO_HOME environment variable must be set to the virtuoso-integration package root!")
)
( UIInit )
SKILL
else
# for non interactive use, do a one off w/ explicit paths
# so we don't have to mess with environment variables
cat <<SKILL>"$cds_wd_target/.cdsinit"
( load "$package_root/share/skill/autoload.il" )
( load "$fulcrum_pdk_root/share/Fulcrum/pdkinfo.il" )
SKILL
fi
fi
[ -n "$interactive" ] && \
[ -z "$force" ] && \
[ -e "$cds_wd_target/.cdsinit.old" ] && \
   ! ( diff "$cds_wd_target/.cdsinit" "$cds_wd_target/.cdsinit.old" ) && \
    echo Original "$cds_wd_target/.cdsinit" saved to "$cds_wd_target/.cdsinit.old" ... please merge manually

# .cdsinit.user
if [[ ! ( -e "$cds_wd_target/.cdsinit.user" ) ]] ; then
rm -f "$cds_wd_target/.cdsinit.user"
cat <<SKILL>"$cds_wd_target/.cdsinit.user"
; This file is automatically loaded at startup
; If you want to make a change, put it in here, NOT in .cdsinit
; You can use ( UILoadDir Dir [regular expression] )
; to load all files in a Directory that match regular expression
( load ( prependInstallPath "samples/local/leBindKeys.il" ) )
( load ( prependInstallPath "samples/local/lxBindKeys.il" ) )
SKILL
fi


# assura_tech.lib
if [[ ! ( -e "$cds_wd_target/assura_tech.lib" ) || ( -n "$force" ) ]] ; then
rm -f "$cds_wd_target/assura_tech.lib"
cat <<EOF>"$cds_wd_target/assura_tech.lib"
DEFINE Assura_$technology \${FULCRUM_PDK_ROOT}/share/Fulcrum/assura
EOF
for dir in frc GNDVddShortCheck keepoutCheck fillCheck drc_m2345678 m2-8 offgrid wellnotch powerCheck dummyCheck; do
if [ -d "$fulcrum_pdk_root/share/Fulcrum/drc/$dir" ]; then
echo "DEFINE $dir \${FULCRUM_PDK_ROOT}/share/Fulcrum/drc/$dir" >> "$cds_wd_target/assura_tech.lib"
fi
done
fi

# cds.config
if [[ ( -n "$interactive" ) && \
      ( ( -n "$force" ) || ! ( -e "$cds_wd_target/cds.config" ) ) ]] ; then
# cds.config
rm -f "$cds_wd_target/cds.config"
cat <<EOF>"$cds_wd_target/cds.config"
# colon seperated path of menu root directories
UI_DIRECTORY=
# DFII_DIR - dfII directory
DFII_DIR=$dfII_dir
# CAST_PATH - standard colon seperated cast-path
CAST_PATH=$cast_path
# temporary directory used by tools
TEMP=$cds_wd_target/temp
CDL_DIR=$cds_wd_target/cdl
EOF
if [ -n "$p4_client" ] ; then
cat <<EOC>>"$cds_wd_target/cds.config"
#P4 CLIENT
P4CLIENT=$p4_client
EOC
fi
fi

# copy their cds_wd template usermenu/autoload
if [ -n "$force" ] ; then
    [ -d "$cds_wd_target/usermenu" ] && chmod -R +w "$cds_wd_target/usermenu"
    cmd="cp -rf"
else
    cmd="cp -ru"
fi

if [ -d "$user_template_cds_wd" ]; then
[ -r "$user_template_cds_wd/display.drf" ] && /bin/rm -f "$cds_wd_target/display.drf";
$cmd "$user_template_cds_wd"/* "$cds_wd_target"
fi
