#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

function exit_func() {
  if [ -f "$strace_dump" ] ; then
    rm -f $strace_dump
  fi
}

trap exit_func EXIT


arch_bin_dir=${0%\/*}
package_root=${arch_bin_dir%\/*}

app_name=$appname$

os_type=`uname -s`

arch_type=`uname -m`

grepcmd=`which grep`
sedcmd=`which sed`


verbose=
jre_to_use="/usr/intel/pkgs/java/1.6.0.25-64";
jre_args=
max_heap_size=
min_heap_size=
run_debugger=
debug_suspend=1
terminal=
debug_env=0
class_cache=
strace_output=
strace_dep_target=
strace_regex='"*"'
d64=

command_args=
for arg in "$@" ; do
  
  case "$arg" in
  --script-verbose )
    verbose=1
    ;;
  --jre=* )
    jre_to_use=`echo $arg | $sedcmd -e "s/--jre=//"`
    ;;
  --jre-args=* )
    jre_args=`echo $arg | $sedcmd -e "s/--jre-args=//"`
    ;;
  --max-heap-size=* )
    max_heap_size=`echo $arg | $sedcmd -e "s/--max-heap-size=//"`
    ;;
  --min-heap-size=* )
    min_heap_size=`echo $arg | $sedcmd -e "s/--min-heap-size=//"`
    ;;
  --64 )
    d64=1
    ;;
  --strace-regex=* )
    strace_regex=`echo $arg | $sedcmd -e "s/--strace-regex=//"`
    ;;   
  --strace-output=* )
    strace_output=`echo $arg | $sedcmd -e "s/--strace-output=//"`
    ;;   
  --strace-dep-target=* )
    strace_dep_target=`echo $arg | $sedcmd -e "s/--strace-dep-target=//"`
    ;;   
  --run-debugger )
    run_debugger=1
    ;;
  --no-suspend-debugger )
    debug_suspend=0
    ;;
  --terminal=* )
    terminal=`echo $arg | $sedcmd -e "s/--terminal=//"`
    ;;
  --debug-environment )
    debug_env=1
    ;;
  --class-cache=* )
    class_cache=`echo $arg | $sedcmd -e "s/--class-cache=//"`
    ;;
  --fulcrum-pdk-root=* )
    fulcrum_pdk_root=`echo $arg | $sedcmd -e "s/--fulcrum-pdk-root=//"`
    ;;
  * )
    if [ -n "$command_args" ] ; then
      command_args="$command_args \"$arg\""
    else
      command_args="\"$arg\""
    fi
    ;;
  esac
done

if [ -d $package_root/$os_type-$arch_type/lib ] ; then
  libs_in_package=`find $package_root/$os_type-$arch_type/lib -follow -name "*.so" -type f`
else
  libs_in_package=
fi

if [ -n "$verbose" ] ; then
    echo "Libraries in package:"
    for lib in $libs_in_package ; do
	echo $lib
    done
    echo ""
fi

#case "$arch_type" in
#  i?86 )
#    preload_libs="$jre_to_use/lib/i386/libjsig.so $preload_libs"
#    ;;
#esac

pre_load_str=
pre_load_list=
for lib in $preload_libs ; do
    if [ -f $lib ] ; then
	pre_load_str="$lib:$pre_load_str"
	pre_load_list="$pre_load_list $lib"
    fi
done

if [ -n "$verbose" ] ; then
    echo "Libraries to preload:"
    for lib in $pre_load_list ; do
	echo $lib
    done
    echo ""
fi


jarroot="$package_root/share/java"
# antlr jar handling

antlr_jar=

if [ -z $ANTLR_JAR ] ; then
  antlr_jar=$jarroot/antlr-2.7.2.jar
else
  antlr_jar=$ANTLR_JAR
fi

if [ -n "$verbose" ] ; then
    echo "Antlr JAR file is \"$antlr_jar\"."
fi

# concurrent util jar handling

concurrent_jar=

if [ -z $CONCURRENT_JAR ] ; then
  concurrent_jar=$jarroot/concurrent.jar
else
  concurrent_jar=$CONCURRENT_JAR
fi

if [ -n "$verbose" ] ; then
    echo "Concurrent util JAR file is \"$concurrent_jar\"."
fi

# stringtemplate jar handling

stringtemplate_jar=

if [ -z $STRINGTEMPLATE_JAR ] ; then
  stringtemplate_jar=$jarroot/stringtemplate.jar
else
  stringtemplate_jar=$STRINGTEMPLATE_JAR
fi

if [ -n "$verbose" ] ; then
    echo "stringtemplate JAR file is \"$stringtemplate_jar\"."
fi

# tools jar handling

tools_jar=

if [ -z $TOOLS_JAR ] ; then
  tools_jar="$jarroot/tools.jar"
else
  tools_jar=$TOOLS_JAR
fi

if [ -n "$verbose" ] ; then
    echo "tools JAR file is \"$tools_jar\"."
fi

jars_in_package=

if [ -d "$jarroot" ] ; then
    jars_in_package=`find "$jarroot" -follow -name "*.jar" "(" -type f -o -type l ")"`
fi

if [ -n "$verbose" ] ; then
    echo "Jars in package:"
    for jar in $jars_in_package ; do
	echo $jar
    done
    echo ""
fi

class_path=$antlr_jar:$concurrent_jar:$stringtemplate_jar:$jdom_jar:$xerces_jar

if [ -n "$class_cache" ] ; then
  class_path="$class_path:$class_cache"
fi

for jar in $jars_in_package ; do
    class_path="$class_path:$jar"
done

if [ -n "$verbose" ] ; then
    echo "CLASSPATH is \"$class_path\"."
fi

if [ "$arch_type" == "x86_64" ]; then
    exec_str="LD_LIBRARY_PATH=$package_root/$os_type-$arch_type/lib:/usr/local/lib:/usr/lib64:/lib64"
else
    exec_str="LD_LIBRARY_PATH=$package_root/$os_type-$arch_type/lib:/usr/local/lib:/usr/lib:/lib"
fi

if [ -n "$pre_load_str" ] ; then
    exec_str="$exec_str LD_PRELOAD=$pre_load_str"
fi

if [ -n "$terminal" ] ; then
  TERM=$terminal
  export TERM
  exec_str="$exec_str TERM=$terminal"
fi

if [ -n "$strace_output" ] ; then
  strace_dump=`mktemp /tmp/strace.XXXXXX`
  if [ "$os_type" == "SunOS" ] ; then
     trace_cmd="truss -o $strace_dump -t open -t _exit"
  elif [ "$os_type" == "Linux" ] ; then
     trace_cmd="strace -o $strace_dump -e trace=open,_exit"
  fi
  exec_str="$exec_str $trace_cmd"
fi

if [[ "$debug_env" == "1" ]] ; then
  print_env_str="$exec_str printenv"
fi

exec_str="$exec_str $jre_to_use/bin/java"

if [ -n "$max_heap_size" ] ; then
    exec_str="$exec_str -Xmx$max_heap_size"
fi

if [[ -n "$d64" && "$os_type" == "SunOS" ]] ; then
    exec_str="$exec_str -d64"
fi

if [ -n "$min_heap_size" ] ; then
    exec_str="$exec_str -Xms$min_heap_size"
fi

if [ -n "$run_debugger" ] ; then

    debug_str="-Xdebug -Xnoagent -Xrunjdwp:transport=dt_socket,server=y"

    if [[ "$debug_suspend" == "1" ]] ; then
      debug_str="$debug_str,suspend=y"
    else
      debug_str="$debug_str,suspend=n"
    fi
    exec_str="$exec_str $debug_str"
fi

if [ -n "$class_path" ] ; then
    exec_str="$exec_str -classpath $class_path"
fi

if [ -n "$jre_args" ] ; then
    exec_str="$exec_str $jre_args"
fi

exec_str="$exec_str $app_name \"--package-root=$package_root\" $command_args"

if [ -n "$fulcrum_pdk_root" ] ; then
    exec_str="$exec_str --config=$fulcrum_pdk_root/share/Fulcrum/jauto/jauto.config --config=$fulcrum_pdk_root/share/Fulcrum/jauto/process.config"
else 
    echo "--fulcrum-pdk-root=<fulcrum_pdk_root> must be specified"
    exit 1
fi

if [ -n "$verbose" ] ; then
    echo "eval $exec_str"
fi

eval $exec_str
ret=$?

if [[ "$debug_env" == "1" ]] ; then
  if [ -n "$verbose" ] ; then
    echo "eval $print_env_str"
  fi
  eval $print_env_str
fi
  
if [ -n "$strace_output" ] ; then
  cat "$strace_dump" | $grepcmd "$strace_regex" | $sedcmd -e "s/open(\"\(.*\)\".*/\1/" > "$strace_output"
  ret=$(grep "^_exit" "$strace_dump" | sed -e "s/_exit(\([0-9]*\)).*/\1/" )
  if [ -n "$strace_dep_target" ] ; then
    echo -n 'AUTODEP_TARGET_FILES := ' > "$strace_dump"
    echo "$strace_dep_target"  | sed -e "s/\#/\\\#/g" >> "$strace_dump"
    echo -n '$(AUTODEP_TARGET_FILES): ' >> "$strace_dump"
    cat "$strace_output" | $grepcmd -v "$strace_dep_target" | sed -e "s/\#/\\\#/g" | awk '{print $1 "\\"}' >> "$strace_dump"
    echo '' >> "$strace_dump"
    cp "$strace_dump" "$strace_output"
  fi
fi

exit $ret
