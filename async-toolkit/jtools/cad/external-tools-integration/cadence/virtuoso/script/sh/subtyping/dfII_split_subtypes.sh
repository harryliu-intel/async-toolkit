#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

# Implements a dfII subtype spec, by copying cellviews around with the 
# filesystem
# The spec format is just lines of the following format
# SPLIT: fqcn fromsubtype tosubtype+
# COPY: fromfqcn tofqcn

arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}
sh_lib_dir="$package_root/share/script/sh/sh-lib"

local_working_dir="/tmp"

function exit_func() {
    if [ -n "$cmd_tmp_dir" ] ; then
        rm -rf "$cmd_tmp_dir"
    fi
    if [ -n "$lib_list_file" ] ; then
        rm -f "$lib_list_file"
    fi
    if [ -n "$src_dest_file" ] ; then
        rm -f "$src_dest_file"
    fi
}

trap exit_func EXIT

source "$sh_lib_dir/file/filecheck.sh"
source "$sh_lib_dir/file/conon.sh"
source "$sh_lib_dir/script/generate_script_with_libs.sh"

function usage() {
    echo "Usage: $0 "
    echo "  --split-spec=file"
    echo "  --src-dfII-dir=dir"
    echo "  --dest-dfII-dir=dir"
    echo "  --fulcrum-pdk-root=dir"
    echo "  [ --pedantic ]"
    echo "  [ --verbose ]"
    echo "  [ --force ]"
    echo "  [ --views=netlist,floorplan ]"
}

sedcmd=`which sed`
gawkcmd=`which gawk`
grepcmd=`which grep`
sortcmd=`which sort`
uniqcmd=`which uniq`
findcmd=`which find`
bashcmd=`which bash`

check_executable_file "$sedcmd" "Unable to find sed in \"$PATH\"." 2
check_executable_file "$gawkcmd" "Unable to find gawk in \"$PATH\"." 2
check_executable_file "$grepcmd" "Unable to fine grep in \"$PATH\"." 2
check_executable_file "$sortcmd" "Unable to find sort in \"$PATH\"." 2
check_executable_file "$uniqcmd" "Unable to find uniq in \"$PATH\"." 2
check_executable_file "$findcmd" "Unable to find find in \"$PATH\"." 2
check_executable_file "$bashcmd" "Unable to find bash in \"$PATH\"." 2

cds_sh_lib="$package_root/share/script/sh/util"
check_readable_dir "$cds_sh_lib" \
    "Cadence Shell Script Library: \"$cds_sh_lib\" is not a readable directory." 2
cds_sh_lib_files=`$findcmd "$cds_sh_lib" \! -type d `

for file in $cds_sh_lib_files ; do
    source "$file"
done


spilt_spec=
src_dfII_dir=
dest_dfII_dir=
pedantic=
verbose=
force=
views=netlist,floorplan
fulcrumPDKRoot=

for arg in $@ ; do 
    case "$arg" in
        --split-spec=* )
        split_spec=`echo $arg | $sedcmd -e "s/--split-spec=//"`
        ;;
        --src-dfII-dir=* )
        src_dfII_dir=`echo $arg | $sedcmd -e "s/--src-dfII-dir=//"`
        ;;
        --dest-dfII-dir=* )
        dest_dfII_dir=`echo $arg | $sedcmd -e "s/--dest-dfII-dir=//"`
        ;;
        --pedantic )
        pedantic="1"
        ;;
        --verbose )
        verbose="1"
        ;;
        --force )
        force="1"
        ;;
        --views=* )
        views=`echo $arg | $sedcmd -e "s/--views=//"`
        ;;
        --fulcrum-pdk-root=* )
        fulcrumPDKRoot=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
        ;;
        --* )
        echo "Unknown argument: \"$arg\"."
        usage
        exit 2
        ;;
    esac
done


check_for_empty_arg "$split_spec"                                                        \
    "You must specify the split specification file."                                     2
check_for_empty_arg "$src_dfII_dir"                                                      \
    "You must specify the location of directory containing all the existing dfII data."  2
check_for_empty_arg "$dest_dfII_dir"                                                     \
    "You must specify the location of directory containing all the new dfII data."       2
check_for_empty_arg "$views"                                                             \
    "You must specify the views you want copied"                                         2
check_for_empty_arg "$fulcrumPDKRoot"                                                    \
    "You must specify the location of the fulcrum PDK package installation."             2


check_readable_file "$split_spec"                                                        \
    "Split Specification File: \"$split_spec\" is not a readable file."                  1
conon_path "$split_spec"
split_spec="$ret"


check_readable_dir "$src_dfII_dir"                                                       \
    "Source DFII directory: \"$src_dfII_dir\" is not a readable directory."              1
conon_path "$src_dfII_dir"
src_dfII_dir="$ret"

check_readable_dir "$fulcrumPDKRoot"                                                     \
    "Fulcrum PDK: \"$fulcrumPDKRoot\" is not a readable directory."                      1
conon_path "$fulcrumPDKRoot"
fulcrumPDKRoot="$ret"


if [[ ! ( -d "$dest_dfII_dir" ) ]] ; then
    mkdir -p "$dest_dfII_dir"
fi

check_writeable_dir "$dest_dfII_dir"                                                               \
    "Destination DFII directory: \"$dest_dfII_dir\" did not exist and could not be created."       1
conon_path "$dest_dfII_dir"
dest_dfII_dir="$ret"


lib_commands_dir="$package_root/share/script/sh/cell-automation/lib_commands"
templates_dir="$package_root/share/script/sh/cell-automation/templates"

check_readable_dir "$lib_commands_dir"                                           \
    "commands directory: \"$lib_commands_dir\" is not a directory."                    2

mkcdslib_source="$lib_commands_dir/mkcdslib"
check_readable_file "$mkcdslib_source"                                           \
    "mkcdslib: \"$mkcdslib_source\" is not a readable file."                           2

blank_cds_library="$fulcrumPDKRoot/share/Fulcrum/blank-library"
check_readable_dir  "$blank_cds_library"                                         \
    "Blank Cadence Library: \"$blank_cds_library\" is not a readable directory."       2


cmd_tmp_dir=`mktemp -d $local_working_dir/dfII_split_subtypes.XXXXXX`
generate_command_script "$cmd_tmp_dir" "$mkcdslib_source" "$sh_lib_dir" "$bashcmd"
mkcdslib="$ret"

check_executable_file "$mkcdslib" \
    "mkcdslib: \"$mkcdslib\" is not readable, executable file."  2



src_dest_file=`mktemp $local_working_dir/dfII_split_subtypes.XXXXXX`

lib_list_file=`mktemp $local_working_dir/dfII_split_subtypes.XXXXXX`

parsed_views=`echo "$views" | $sedcmd -e "s/,/ /g"`

( cat $split_spec          | \
    $grepcmd -e "^SPLIT"             | \
    $gawkcmd -- '{ N=4; while($N != "" ) { print $2 "." $3 "#" $2 "." $N ; N++ } }' & \
    cat $split_spec          | \
    $grepcmd -e "^COPY"             | \
    $gawkcmd -- '{ N=3; while($N != "" ) { print $2 "#" $N ; N++ } }' ) | \
    $sortcmd | $uniqcmd > $src_dest_file


function get_dir_info {
    src_cell_subtype=$1
    dest_cell_subtypes="$2"

    for view in $parsed_views ; do
        get_cadence_cell_view_dir "$src_cell_subtype" "$view" "$src_dfII_dir"
        src_cell_view_dir="$ret"

        if [[ ! ( -r "$src_cell_view_dir" && -d "$src_cell_view_dir" ) ]] ; then
            if [ -n "$pedantic" ] ; then
                curr_error_str="\"$src_cell_view_dir\" is not a readable directory."
                if [ -n "$error_str" ] ; then
                    error_str="$error_str\n$curr_error_str"
                else
                    error_str="$curr_error_str"
                fi
            fi
        else
            for dest_cell_subtype in $dest_cell_subtypes ; do
                get_cadence_cell_view_dir "$dest_cell_subtype" "$view" "$dest_dfII_dir"
                dest_cell_view_dir="$ret"

                get_lib_name "$dest_cell_subtype"
                dest_lib_name="$ret"
                echo "$dest_lib_name" >>$lib_list_file
                
                if [[ -a "$dest_cell_view_dir" ]] ; then
                    if [[ "$src_cell_view_dir" != "$dest_cell_view_dir" ]] ; then
                        if [ -n "$force" ] ; then
                            dirs_to_delete="$dirs_to_delete $dest_cell_view_dir"
                        else
                            curr_error_str="\"$dest_cell_view_dir\" exists use --force to overwrite."
                            if [ -n "$error_str" ] ; then
                                error_str="$error_str\n$curr_error_str"
                            else
                                error_str="$curr_error_str"
                            fi
                        fi
                    fi
                else
                    dest_cell_view_dir_parent=`dirname $dest_cell_view_dir`
                    if [[ ( ! -d "$dest_cell_view_dir_parent" ) ]] ; then
                        dirs_to_create="$dirs_to_create $dest_cell_view_dir_parent"
                    fi
                fi
            done
        fi
    done
}

function copy_cell {
    src_cell_subtype=$1
    dest_cell_subtypes="$2"
    for view in $parsed_views ; do
        get_cadence_cell_view_dir "$src_cell_subtype" "$view" "$src_dfII_dir"
        src_cell_view_dir="$ret"
        if [[ -r "$src_cell_view_dir" && -d "$src_cell_view_dir" ]] ; then
            for dest_cell_subtype in $dest_cell_subtypes ; do
                get_cadence_cell_view_dir "$dest_cell_subtype" "$view" "$dest_dfII_dir"
                dest_cell_view_dir="$ret"

                if [[ "$src_cell_view_dir" != "$dest_cell_view_dir" ]] ; then
                    if [ -n "$verbose" ] ; then
                        echo "Copy \"$src_cell_view_dir\" to \"$dest_cell_view_dir\"." 
                    fi
                    cp -a "$src_cell_view_dir" "$dest_cell_view_dir"
                    $findcmd $dest_cell_view_dir | xargs chmod u+w
                fi
            done
        fi
    done
}

for src_dest in $(<$src_dest_file) ; do
    get_dir_info $(echo $src_dest | $sedcmd -e "y/#/ /" )
done

if [ -z "$error_str" ] ; then
    lib_list=`cat $lib_list_file | $sortcmd | $uniqcmd`
    for lib in $lib_list ; do
        if [ -n "$verbose" ] ; then
            echo "Ensuring that library \"$lib\" exists."
        fi
        $mkcdslib "--cds-wd=$dest_dfII_dir"                      \
            "--generated-libraries-root=$dest_dfII_dir"    \
            "--lib=$lib"                                   \
            "--blank-cds-library=$blank_cds_library"       \
            "--cadence-shell-library=$cds_sh_lib"
    done   
fi

dirs_to_create=$(echo $dirs_to_create | $sortcmd | $uniqcmd )
dirs_to_delete=$(echo $dirs_to_delete | $sortcmd | $uniqcmd )

if [ -z "$error_str" ] ; then
    if [ -n "$dirs_to_delete" ] ; then
        for dir_to_delete in $dirs_to_delete ; do
            if [ -n "$verbose" ] ; then
                echo "Delete \"$dir_to_delete\"."
            fi
            rm -r "$dir_to_delete"
        done
    fi

    if [ -n "$dirs_to_create" ] ; then
        for dir_to_create in $dirs_to_create ; do
            if [ -n "$verbose" ] ; then
                echo "Create \"$dir_to_create\"."
            fi
            mkdir -p "$dir_to_create"
        done
    fi

    if [ -z "$error_str" ] ; then
      for src_dest in $(<$src_dest_file) ; do
          copy_cell $(echo $src_dest | $sedcmd -e "y/#/ /" )
      done
    else
        echo -e "$error_str"
    fi
fi
