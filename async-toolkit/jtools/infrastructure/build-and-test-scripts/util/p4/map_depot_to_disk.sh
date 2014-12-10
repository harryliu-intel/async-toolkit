#!/bin/bash
# Copyright 2002 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

#Maps a perforce path to a path on the local file system using the specified client spec.
#Limitations:
#Can only map depot paths that are explicity mentioned in the client specification, not
#sub directories of paths mentioned in the client specification.
#Usage: map_depot_path_to_disk perforce_command client_spec_name depot_path
#Resulting filesystem path is in $ret.
function map_depot_path_to_disk() {
  local p4cmd="$1"
  local clientspecname="$2"
  local depotpath="$3"
  
  #Escape any regular expression speciall characters in the depot path.
  local escapeddepotpath=`echo $depotpath | $sedcmd -e "s/\./\\\./g" \
                                                    -e "s/\*/\\\*/g" \
                                                    -e "s/\[/\\\[/g" \
                                                    -e "s/\]/\\\]/g" \
                                                    -e "s/\^/\\\^/g"`

  #find the specified depot path in the client specification.
  local client_spec_root=`$p4cmd client -o $clientspecname | $grepcmd -v "#.*$" | $grepcmd -e "Root:" | $sedcmd -e "s/Root:[[:space:]]*//"`
  local spec_line=`$p4cmd client -o $clientspecname | $grepcmd -v "^-.\+" | $grepcmd -e "$escapeddepotpath"`
  local client_path=`echo $spec_line | $gawkcmd -- "{ print \\$2 }"`
  local escaped_client_spec_root=`echo $client_spec_root | $sedcmd -e "s/\//\\\\\\\\\\//g"`
  local disk_path=`echo $client_path | $sedcmd -e "s/\/\/$clientspecname\//$escaped_client_spec_root\//" \
                                                        -e "s/\.\.\.\$//"`
  ret="$disk_path"
}
