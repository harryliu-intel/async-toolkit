#!/usr/intel/bin/perl -l
# AAG
# $Id$
# $DateTime$

sub usage {
    print "$_[0] [--no-log] [--overwrite] package_file target_dir";
    exit 1;
}

sub error {
    print "$_[0]";
    exit $_[1];
}

sub conon_path {

    my $the_path=$_[0];
    my $pwd=`pwd`;
    chomp $pwd;
    chdir $the_path;
    my $ret=`pwd`;
    chomp $ret;
    chdir $pwd;
    $ret;
}

sub get_package_root {

    my ($tar_file_name,$untar_dir)=@_;
    my $root_entry=`tar -tzf $tar_file_name | grep -e "\.fulcrum-package-root"`;
    chomp $root_entry;
    my $package_root=$root_entry;
    $package_root =~ s/\/\.fulcrum-package-root//;
    my $ret;
    if ( "$package_root" ne "" ) {
      $ret="$untar_dir/$package_root";
    }
    else {
      $ret="";
    }
    $ret;
}

$ENV{PATH}="/usr/intel/bin:$ENV{PATH}";

my $keepLog=1;
my $overWrite=0;
my $package_file="";
my $target_dir="";

foreach $arg ( @ARGV ) {
    if ($arg eq "--no-log") {
        $keepLog="";
    }
    elsif ($arg eq "--overwrite") {
        $overWrite=1;
    }
    elsif ($arg =~ /^-/) {
        print "Unknown option \"$arg\".";
        usage $0
    }
    else {
        if ( "$package_file"  eq "" ) {
            $package_file=$arg
        }
        elsif ( "$target_dir" eq "" ) {
            $target_dir=$arg
        }
        else {
            print "Too many args";
            usage $0;
        }
    }
}

system "mkdir -p $target_dir" if ! -d $target_dir;
if ( "$package_file" eq "" or "$target_dir" eq "" ) {
    usage $0;
}
my $package_dir=$target_dir;
$package_dir=~s/\/tools$/\/packages/;
if ( ! -d "$package_dir" ) {
    system "mkdir -p '$package_dir' >/dev/null 2>&1";
}
if ( -d $target_dir and -w $target_dir ) {

  if ( -f $package_file ) {

    umask 0;

    $target_dir=conon_path $target_dir;
    $package_root=get_package_root $package_file,$target_dir;

    $logFile="$package_root/.package-inst.log";
    $log="";
    if ( -s "$logFile" ) {
        $log = `cat $logFile`;
    }

    if ( "$package_root" ne "" ) {
      if ( ! $overWrite ) {
        if (system "/bin/rm -rf '$package_root'") {
            print "Couldn't rm -rf install directory...aborting";
            exit 2;
        }
      }

      if (! system ("tar -C $target_dir -xzf $package_file") ) {

        $package_root=conon_path $package_root;

        open (LOG, ">$logFile");
        print LOG "$log";
        if ( $keepLog ) {
            my $buildIdent=`cat $package_root/.fulcrum-package-root`;
            chomp $buildIdent;
            my $date=`date +%Y-%m-%d-%H-%M-%S-%Z`;
            chomp $date;
            print LOG "$buildIdent installed $date";
        }
        close LOG;

        my $install_fail=0;

        my $install_exit_code=0;

        my $bin="$package_root/bin";

        if ( ! -d "$bin" ) {
            $install_fail=1;
            $install_exit_code=1;
        }

        if ( $install_fail ) {
          if (system ("/bin/rm -rf $package_root") ) {
              error "Installation failed: $install_exit_code",2;
          }
        }
        else {
          system "touch $package_root/.installed{,-Linux-{x86_64,i686}}";
          system "/bin/cp -p '$package_file' '$package_dir'";
        }
      }
      else {
        error "$package_file could not be extracted.",3;
      }
    }
    else {
      error "$package_file is not a package file.",3;
    }

  }
  else {
    error "$package_file does not exist.",3;
  }
}
else {
  error "$target_dir is not a directory or is not writeable.",3;
}
