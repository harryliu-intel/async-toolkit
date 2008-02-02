#!/usr/bin/awk -f

#
# $Id$
#

function process_file(fn,extension,type) {
  cmd = "cat < " fn;

  newname = fn;
  sub(/\.f\.tmpl/,"_" extension ".f",newname);

  while (cmd | getline) {
    gsub(/ TYPE /, " " type " ");
    print $0 > newname;
  }
  close(cmd);
  close(newname);
}

BEGIN { 
  forfiles[0] = "mul_mtransposem.f.tmpl";
  forfiles[1] = "mulmv.f.tmpl";
  forfiles[2] = "lu2_backsubstitute.f.tmpl";

  for (i in forfiles) {
    process_file(forfiles[i], "sp", "real"); 
    process_file(forfiles[i], "dp", "double precision"); 
  }
}
