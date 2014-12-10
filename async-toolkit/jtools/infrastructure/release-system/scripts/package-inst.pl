#!/usr/intel/bin/perl
# Copyright 2006 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$

use strict;
use Getopt::Long;

my $filecmd="/usr/bin/file";

if ( -r "/usr/share/file/magic" and -f "/usr/share/file/magic" ) {
    $filecmd .= " -m /usr/share/file/magic";
}
elsif ( -r "/etc/magic" and  -f "/etc/magic") {
    $filecmd .- " -m /etc/magic";
}

my %qrshref = (
    "Linux-x86_64" => "lx24-amd64",
    "Linux-i686" => "glinux",
    "SunOS-sun4u" => "solaris64",
);

sub findqrsharch {
    $qrshref{$_[0]} if defined $qrshref{$_[0]};
    "";
}

sub usage {
    print STDERR <<EU;
$_[0] [--no-log] [--overwrite] package_file target_dir
EU
    exit 1;
}

sub error  {
    print STDERR "$_[0]\n";
    exit $_[1];
}

sub mkwrapper {
    local (*P);
    open (P, ">$_[0]/.wrapper");
    print P <<EW;
#!/bin/bash
aname=\`/bin/uname -sm\`
aname=\${aname/ /-}
dn=\${0%\\/*}
dn=\${dn%\\/*}
bn=\${0/*\\/}
exec "\$dn/\$aname/bin/\$bn" "\$@"
EW
    close P;
    system ("chmod","755","$_[0]/.wrapper");
}

$ENV{PATH}="/usr/intel/bin:$ENV{PATH}";

sub get_package_root {

    my $tar_file_name=$_[0];
    my $untar_dir=$_[1];

    my $package_root=`tar -tzf $tar_file_name | grep -e "\\.fulcrum-package-root"`;
    chomp $package_root;
    $package_root =~ s:/.fulcrum-package-root::;
    my $ret="";
    if ( "$package_root" ne "" ) {
      $ret="$untar_dir/$package_root";
    }
    $ret;
}

sub conon_path {

    my $the_path=$_[0];
    my $pwd = `pwd`;
    chomp $pwd;
    chdir $the_path;
    my $ret=`pwd`;
    chomp $ret;
    chdir $pwd;
    $ret =~ s:^/mnt/fulcrum/home/:/home/:;
    $ret;
}

my $keepLog=1;
my $overWrite=0;
my $stripexec=0;
my $verbose=0;

sub usage {
    print STDERR "$_[0]\n" if defined $_[0];
    print STDERR <<EU;
Usage: $0 [options] <package-file> <target-dir>
    --no-log          : generate no log file
    --overwrite       : replace existing extract
    --strip           : strip executables in target
    --verbose         : verbose mode
EU
    exit 1;
}

my %options = (
    "no-log" => sub { $keepLog = 0; },
    "overwrite" => \$overWrite,
    "strip" => \$stripexec,
    "verbose" => \$verbose,
);

GetOptions (%options) or usage $0;

usage "Wrong number of args" if ($#ARGV != 1);

my $package_file = shift;
my $target_dir = shift;

if ( $package_file eq "" or "$target_dir" eq "" ) {
    usage $0;
}
if ( ! -d $target_dir and ! -w $target_dir ) {
  error ("$target_dir is not a directory or is not writeable.",3);
}
if ( ! -f $package_file ) {
  error ("$package_file does not exist.",3);
}
umask 0;

my $pdk=0;
if ( $package_file =~ /-pdk/) {
    $pdk=1;
}
my $tsmc28=0;
if ($package_file =~ /tsmc28/) {
    $tsmc28=1;
    `chgrp nm28 $package_file`;
    `chmod 640 $package_file`;
}
my $aname=`/bin/uname -sm`;
chomp $aname;
$aname =~ s/ /-/g;
my $pkganame=$package_file;
$pkganame =~ s/.*\///;
$pkganame =~ s/-[^-]+-[^-]+$//;
$pkganame =~ s/.*-L/L/;
$pkganame =~ s/.*-S/S/;

# never strip Solaris
if ( $pkganame eq "SunOS-sun4u" ) {
    $stripexec=0;
}

$target_dir=conon_path $target_dir;

my $package_root=get_package_root ($package_file,$target_dir);

if ( "$package_root" eq "" ) {
  error "$package_file is not a package file.",3
}

my $logFile="$package_root/.package-inst-$pkganame.log";
if ( $pdk ) {
    $logFile="$package_root/.package-inst.log";
}
my $log="";
if ( -s "$logFile" ) {
    $log=`cat $logFile`;
}

if ( $overWrite ) {
    system ('/bin/rm','-rf',"$package_root");
    if ( -d "$package_root" ) {
        print STDERR "Couldn't rm -rf install directory...aborting\n";
        exit 2;
    }
}

#echo tar -C $target_dir -xzf $package_file
my @cmd=("tar","-C","$target_dir","-xzf","$package_file");
print STDERR "@cmd\n" if $verbose;
if ( ! system (@cmd) ) {

  $package_root = conon_path $package_root;

  open (LOG, ">$logFile");
  print LOG "$log\n";

  if ( $keepLog ) {
      my $buildIdent=`cat "$package_root/.fulcrum-package-root"`;
      chomp $buildIdent;
      my $date=`date +%Y-%m-%d-%H-%M-%S-%Z`;
      chomp $date;
      print LOG "$buildIdent installed $date\n";
  }
  close LOG;

  my @package_architectures=`find $package_root -maxdepth 1 -mindepth 1 -type d -name "*-*"`;
  chomp @package_architectures;

  my $sh="/bin/bash";
  my $perl="/usr/intel/bin/perl";

  foreach my $type ( "sh", "pl" ) {
      foreach my $arch (@package_architectures) {

          if ( -d "$arch/bin" ) {
              my @arch_scripts=`find "$arch/bin" -maxdepth 1 -type f -name "*.$type"`;
              chomp @arch_scripts;

              foreach my $script (@arch_scripts) {

                  my $target_script_name=$script;
                  $target_script_name =~ s/\.$type$//;

                  system ("sed -e 's=\\\$packageroot\\\$=$package_root=g' \\
                      -e 's=\\\$archpath\\\$=$arch=g' \\
                      -e 's=\\\$PACKAGE_[[:alnum:]_]\\+_DEFAULT\\\$==g' \\
                      -e 's=\\\$perl\\\$=$perl=' \\
                      -e 's=\\\$sh\\\$=$sh=' '$script' \\
                      > '$target_script_name'");

                  if (-x "$script" ) {
                      system "chmod '--reference=$script' '$target_script_name'; touch -r '$script' '$target_script_name'";
                  }
                  else {
                      system "chmod 775 '$target_script_name' ; touch -r '$script' '$target_script_name'";
                  }
                  unlink "$script";
              }
          }
      }
  }

  my $install_share_data="$package_root/install/share/data";
  my $install_share_bin="$package_root/install/share/bin";

  my @install_share_bin_files=();
  if ( -d "$install_share_bin" ) {
    @install_share_bin_files=`find $install_share_bin -type f -o -type l`;
    chomp @install_share_bin_files;
  }

  my $install_arch_data="$package_root/install/$pkganame/data";
  my $install_arch_bin="$package_root/install/$pkganame/bin";

  my @install_arch_bin_files=();
  if ( -d "$install_arch_bin" ) {
    @install_arch_bin_files=`find $install_arch_bin -type f -o -type l`;
    chomp @install_arch_bin_files;
  }

  my $install_fail=0;

  my $install_exit_code=0;

  my $arch_bin="$package_root/$pkganame/bin";
  my $bin="$package_root/bin";
  my $arch_data="$package_root/$pkganame/data";

  foreach my $install_file ( @install_share_bin_files , @install_arch_bin_files ) {
    if ( ! $install_fail ) {
      # bash needed below for solaris
      if ( $verbose ) {
              print STDERR "/bin/bash -c \"$install_file $package_root $install_share_bin $install_share_data $install_arch_bin $install_arch_data $0 $arch_bin $arch_data\"\n";
          $install_exit_code=
              system "/bin/bash -c \"$install_file $package_root $install_share_bin $install_share_data $install_arch_bin $install_arch_data $0 $arch_bin $arch_data\"";
      }
      else {
          $install_exit_code=
              system "/bin/bash -c \"$install_file $package_root $install_share_bin $install_share_data $install_arch_bin $install_arch_data $0 $arch_bin $arch_data\" &>/dev/null";
      }
      $install_fail=1 if $install_exit_code != 0;
    }
  }

  if ( $install_fail ) {
    system "/bin/rm -rf $package_root &>/dev/null";
    error ( "Installation failed: $install_exit_code",2);
  }
  else {
    if ( $stripexec ) {
      if ( "$pkganame" eq "$aname" ) {
        system "strip '$arch_bin/'* &>/dev/null";
      }
      else {
        my $ret=findqrsharch $pkganame;
        system "QB_LOCAL=0 QRSH_FLAGS='-l a=$ret,mem=48 -cwd' fulcrum qb strip '$arch_bin/'* &> /dev/null";
      }
    }
    if ( ! -d "$bin" and -d "$arch_bin" ) {
      system "mkdir -p '$bin'";
      mkwrapper "$bin"
    }
    if ( -d "$bin" ) {
      my @archfiles = `find "$arch_bin" -type f -o -type l`;
      chomp @archfiles;
      foreach my $f ( @archfiles ) {
          my $fl = $f;
          $fl =~ s/.*\///;
          if ( `$filecmd "$f" | grep -c 'ELF '` > 0 ) {
              unlink "$bin/$fl";
              system "cd '$bin'; ln -s .wrapper '$fl'";
          }
          else {
              if ( ! -e "$bin/$fl" ) {
                system "/bin/mv '$f' '$bin'";
              }
              system "cd '$arch_bin'; /bin/rm -f '$fl'; ln -s '../../bin/$fl'";
          }
      }
    }
    system "/bin/rm -rf $package_root/install";
    if ($tsmc28) {
        my $pdkroot=$package_root;
        $pdkroot =~ s/\/[^\/]+$//;
        `chgrp nm28 "$pdkroot"`;
        `chmod 2750 "$pdkroot"`;
        `chmod -R o-rwx,g-w "$package_root"`;
        `find "$package_root" -follow -type d -print0 | xargs -0r chmod 2750`;
    }
  }
  my $tooldir=$package_root;
  $tooldir =~ s/\/([^\/]+)$//;
  my $version=$1;
  opendir (D, $tooldir);
  my @versions=sort { $b - $a } ( grep (/^\d+$/, readdir(D)));
  closedir D;
  my $lastversion=$version;
  foreach my $n (0..$#versions) {
    if ($versions[$n] eq $version) {
        $lastversion=$versions[$n+1];
        last;
    }
  }
  if ($lastversion ne $version and length($version) == length($lastversion) and $version - $lastversion < 10000) {
      my @files=`find $tooldir/$version -noleaf -type f`;
      chomp @files;
      foreach my $file (@files) {
        my $ofile=$file;
        $ofile =~ s:/$version/:/$lastversion/:;
        if ( -e $ofile ) {
            my @s1=stat($file);
            my @s2=stat($ofile);
            my $match=1;
            $match=0 if $s1[1] == $s2[1];
            for my $n ( 7,9 ) {
                $match=0 if $s1[$n] != $s2[$n];
            }
            if ($match and $s1[7] > 102400) {
                my $cmp=`cmp "$ofile" "$file" | wc -l`;
                chomp $cmp;
                if ($cmp eq "0") {
                    unlink($file);
                    print STDERR "Linking $file\n" if $verbose;
                    link($ofile, $file);
                }
            }
        }
      }
  }
}
else {
  error ( "$package_file could not be extracted.",3);
}
