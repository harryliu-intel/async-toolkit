#!/bin/bash

function config_get_value() {
    local config_file=$1
    local key=$2
    ret=`cat $config_file | grep "^$key=" | sed -e "s/$key=//"`
}

function config_get_all_values() {
    local config_file="$1"
    if [ -f "$config_file" ] ; then
      local prev_config="$config"
      source "$config_file"
      configs=`cat "$config_file" | grep "^config=" | sed -e "s/config=//"`
      for config in $configs ; do
        config_get_all_values "$config"
      done
      config="$prev_config"
    fi
}

function config_get_all_keys() {
    local config_file="$1"
    if [ -f "$config_file" ] ; then
      local prev_config="$config"
      local keys=`cat "$config_file" | grep ".*=" | grep -v "^config=" | sed -e "s/=.*//"`
      configs=`cat "$config_file" | grep "^config=" | sed -e "s/config=//"`
      for config in $configs ; do
          keys="$keys"
`config_get_all_keys "$config"`
      done
      config="$prev_config"
    fi
    echo "$keys"
}


function config_fill_template_from_config_file() {
    local template_file="$1"
    local config_file="$2"
     
    sedcmd=`which sed`
    grepcmd=`which grep`
    bashcmd=`which bash`

    local vars=`config_get_all_keys "$config_file"`
    vars=`echo $vars`
    local cmd="config_get_all_values \"$config_file\"; \
               cat \"$template_file\" | config_fill_template $vars"

    export vars cmd
    export -f config_get_all_values \
              config_fill_template
    
    $bashcmd -c "$cmd"
}


function config_fill_template_from_env() {
    local template_file=$1
    sedcmd=`which sed`
    grepcmd=`which grep`

    vars=$( $grepcmd '\$.*\$' $template_file | $sedcmd -e 's/[^$]*\$\([^$]*\)\$[^$]*/\1 /g' | xargs -n 1 echo | sort | uniq)
    
    cat $template_file | config_fill_template $vars
}

function config_fill_template() {
  sedcmd=`which sed`
  bashcmd=`which bash`
  vars=$@
  
  template_cmd="$sedcmd -e {}"
  for var in $vars ; do
    eval "val="\$$var""
    #escape / and "
    var=`echo $var | $sedcmd -e "s/\//\\\\\\\\\\//g" -e 's/\"/\\\\\\\\\\"/g' `
    val=`echo $val | $sedcmd -e "s/\//\\\\\\\\\\//g" -e 's/\"/\\\\\\\\\\"/g' `
    template_cmd="$template_cmd -e s/\\\\\\\$$var\\\\\\\$/\"$val\"/g "
  done
  
#  cmd=`mktemp /tmp/config.XXXXXX`
#  echo \#\!/bin/bash > $cmd
#  echo "$template_cmd" >> $cmd
#  chmod +x $cmd

#  $cmd
#  rm $cmd

  $bashcmd -c "$template_cmd"
}
