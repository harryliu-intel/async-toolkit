#!/usr/intel/bin/perl

# Copyright Fulcrum Microsystems 2003
# $Id$
# $DateTime$
# $Author$
# NOTE: this has been changed so that the old version method will no
# longer work where the change/version number is obtained from the
# package file. Now, if no change is specified, it will be '0'.


use strict;

use Cwd;
use File::Basename;
use File::stat;
use File::Spec;
use FileHandle;
use Getopt::Long;

my $argchange = "0";
my $verbose=0;
my $links=0;
my $rootProjectDir="xxx";
my $buildSystemRoot="xxx";
my $java_common=0;

$ENV{PATH}="/usr/intel/bin:$ENV{PATH}";

sub printError {
    my ($msg)= @_;
    print STDERR $msg . "\n" if defined ($msg);
    print STDERR <<USAGE;
Usage: $0 [-d] [--change=#] target-application target-directory
    -d             : no tar file is generated
    --change=#     : use this change number instead of major.minor
                   : IMPORTANT: this change number is NOT checked!
    --java-commmon : use to save build time if x86_64 is one of built arch's
USAGE
exit 1;
}

sub compareStrFunc {
    my ( $str1, $str2 ) = @_;
    return $str1 eq $str2;
}

sub appendElementToOrderedSet {
    my ( $arraySetRef, $areSameFunc, $newElement ) = @_;

    my $inArray = 0;

    my $len = scalar( @{$arraySetRef} );

    my $i = 0;

    while ( ( $inArray == 0 ) and ( $i < $len ) ) {
        $inArray = &$areSameFunc( $newElement, $arraySetRef->[$i] );
        $i = $i + 1;
    }
    if ( ! $inArray ) {
        my @newArray = ( @{$arraySetRef}, $newElement );
        return \@newArray;
    }
    else {
        return $arraySetRef;
    }
}

sub checkDirPermissions {
    my ( $errorFuncRef, $dirName, $needWrite ) = @_;

    my $result=0;

    if ( -d $dirName ) {
        if ( -x $dirName ) {
            if ( -r $dirName ) {
                if ( -w $dirName ) {
                    $result=1;
                }
                else {
                    if ( $needWrite == 0 ) {
                        $result=1;
                    }
                    else {
                        &$errorFuncRef( "$dirName is not writeable." );
                    }
                }
            }
            else {
                &$errorFuncRef( "$dirName is not readable." );
            }
        }
        else {
            &$errorFuncRef( "$dirName is not executable." );
        }
    }
    else {
        &$errorFuncRef( "$dirName is not a directory." );
    }
    return $result;
}

sub parseAndValidateArgs {
    my ( $errorFuncRef, $specFileName, $targetDir, 
         $targetApp, $projectDir, $targetArch, $targetOS, $companyName ) = @_;

    my @argArray;

    if ( ! ( $targetApp eq "" ) ) {
        
        if ( -r $specFileName ) {
            if ( checkDirPermissions( $errorFuncRef, $targetDir, 1 ) == 1 ) {
                if ( checkDirPermissions( $errorFuncRef, $projectDir, 0 ) == 1 ) {
                    if ( $targetArch eq "" ) {
                        chop( $targetArch = `uname -m` );
                    }
                    if ( $companyName eq "" ) {
                        $companyName = "fulcrum" ;
                    }
                    if ( $targetOS eq "" ) {
                        chop( $targetOS = `uname -s` );
                    }

                    @argArray = ( $specFileName, $targetDir, $targetDir,
                                  $targetApp, $projectDir, $targetArch,
                                  $targetOS, $companyName, 1);
                }   
            }
        }
        else {
            &$errorFuncRef( "$specFileName does not exist." );
        }
    }
    else {
        &$errorFuncRef( "You have to specify a target application name.");
    }

    return @argArray;
    
}

sub canonicalizePath {
    my ( $thePath ) = @_;

    my @pathParts = File::Spec->splitpath( $thePath );

    my ( $volume, $directory, $file ) = @pathParts;

    my @dirParts = File::Spec->splitdir( $directory );

    my $numDirectories = scalar( @dirParts );

    my $i;

    my @canonDirParts;

    my $numCanonDirParts = 0;

    for ( $i=0; $i<$numDirectories; $i++ ) {
        if ( ! ( $dirParts[ $i ] eq "" ) ) {
            if ( $dirParts[ $i ] eq File::Spec->updir ) {
            
                if ( $numCanonDirParts != 0 ) {
                
                    $numCanonDirParts--;
                
                    @canonDirParts = @canonDirParts[0..($numCanonDirParts-1)];
                
                }
                else {
                    @canonDirParts = ( @canonDirParts, $dirParts[ $i ] );
                }
            
            }
            else{
                if ( ! ( $dirParts[ $i ] eq File::Spec->curdir ) ) {
                    @canonDirParts = ( @canonDirParts, $dirParts[ $i ] );
                    $numCanonDirParts++;
                }
            }
        }
            
    }

    my $canonDir = File::Spec->catdir( @canonDirParts );

    my $canonPath = File::Spec->catpath( $volume, $canonDir, $file );

    if ( File::Spec->file_name_is_absolute( $thePath ) ) {
        $canonPath = "/" . $canonPath;
    }

    return $canonPath;
    
}


sub getMajorAndMinorVersion {
    my ( $specFile, $specFileName, $errorFuncRef ) = @_;
    my @specLineArray;
    my $specFileLine;
    
    $specFileLine = $specFile->getline();
    $specFileLine =~ s/\s+/ /;

    @specLineArray = split( /\s/ , $specFileLine );
    my ( $majorVersion, $minorVersion ) = @specLineArray;

    if ( ( $majorVersion eq "" ) || ( $minorVersion eq "" ) ) {
        &$errorFuncRef( "The first line of $specFileName must contain the " 
                        . "major version number followed by white space "
                        . "followed by the minor version number" );
    }

    ( $majorVersion, $minorVersion );
}

sub buildData {
    my ( $specFile, $majorVersion, $minorVersion, $errorFuncRef, 
         $specFileName, $targetDir, $rootTargetDir,
         $targetApp, $projectDir, $targetArch, $targetOS, 
         $companyName, $isTopLevel, @destRegEx) = @_;
    
#    print STDERR "buildData($specFile, $majorVersion, $minorVersion, $errorFuncRef, $specFileName, $targetDir, $rootTargetDir, $targetApp, $projectDir, $targetArch, $targetOS, $companyName, $isTopLevel, @destRegEx)\n" if $verbose;
    my $dirName = dirname( $specFileName );
    #if (  File::Spec->canonpath( $dirName ) eq File::Spec->curdir ) {
        #$dirName = &cwd;
    #}

    my @specLineArray;
    my $specFileLine;

    my %data;
            
    my $targetBaseName = "$targetApp-$argchange";
    my $tarBuildDir = "$rootTargetDir/$targetBaseName-package-build";

    my $targetTarFileName =
        File::Spec->canonpath("$rootTargetDir/versions/$targetBaseName.tar.gz");
            
    if ( ! ( ( $majorVersion eq "" ) || ( $minorVersion eq "" ) ) ) {
        my %cpyMap;
        my @includeFiles;
        my @javaClasses;
        my @resources;
        my $condtrue = 1;
        my $hereDoc = 0;

        while ( $specFileLine = $specFile->getline()) {

            # here documents
            if($specFileLine =~ m/^<<(\w+)\s*/) {
                $hereDoc = $1;
                next;
            } elsif($hereDoc) { 
                $hereDoc = 0 if($specFileLine =~ m/^${hereDoc}\s*/ );
                next;
            }

            # if statements
            # this is very very simple. Does not allow nested if's nor does it warn
            # if you use nesting nor does it tell you if you have in incomplete line
            # nor an invalid os name $targetOS is the result of uname -s
            
            if($specFileLine =~ m/^[\W]*ifnos\W/) {
                my @specLineArray = split( /\s+/ , $specFileLine );
                if ($specLineArray[1] ne $targetOS) {
                    $condtrue=1;
                }
                else {
                    $condtrue=0;
                }
                next;
            }
            if($specFileLine =~ m/^[\W]*ifnarch\W/) {
                my @specLineArray = split( /\s+/ , $specFileLine );
                if ($specLineArray[1] ne $targetArch) {
                    $condtrue=1;
                }
                else {
                    $condtrue=0;
                }
                next;
            }
            if($specFileLine =~ m/^[\W]*ifos\W/) {
                my @specLineArray = split( /\s+/ , $specFileLine );
                if ($specLineArray[1] eq $targetOS) {
                    $condtrue=1;
                }
                else {
                    $condtrue=0;
                }
                next;
            }
            if($specFileLine =~ m/^[\W]*ifarch\W/) {
                my @specLineArray = split( /\s+/ , $specFileLine );
                if ($specLineArray[1] eq $targetArch) {
                    $condtrue=1;
                }
                else {
                    $condtrue=0;
                }
                next;
            }
            if($specFileLine =~ m/^[\W]*endif\W/) {
                $condtrue=1;
                next;
            }
            if($specFileLine =~ m/^[\W]*else\W/) {
                $condtrue = ! $condtrue;
                next;
            }
            # skip remaining comments
            next if($specFileLine =~ m/^[\W]*$/ ||
                    $specFileLine =~ m/^[\W]*[\#]+/ );
            
            next if(! $condtrue );
            $specFileLine =~ s/\s+/ /;
                    
            my @specLineArray = split( /\s+/ , $specFileLine );
    
            $specLineArray[0] =~ s/\s*//;
            $specLineArray[1] =~ s/\s*//;
            $specLineArray[0] =~ s/\$arch\//$targetOS-$targetArch\//;

            #include another .package file
            #include <package-file> <relative-path>
            if ( $specLineArray[0] eq "include" ) {
                my $includeFileName = $specLineArray[1];
                foreach my $regEx (@destRegEx) {
                    my ($a, $b) = split("->",$regEx);
                    $includeFileName =~ s/$a/$b/;
                }

                
                # poor man's recursion
                # setup state for recursion
                my $includeFilePath = $specLineArray[2];
                my $includeSpecFilePath =
                    canonicalizePath("$projectDir/$includeFileName");
                my $includeTargetDir =
                    canonicalizePath("$targetDir/$includeFilePath");
                my $includeProjectDir =
                    canonicalizePath("$projectDir/$includeFilePath");
                my @includeDestRegEx = (@destRegEx,splice(@specLineArray,3));

                my $includeSpecFile = new FileHandle;
                $includeSpecFile->open( "<$includeSpecFilePath" ) or die "Can't open $includeSpecFilePath";
                
                #Consume version numbers line.
                $includeSpecFile->getline();
                
                # recurse
                my $includeData = buildData($includeSpecFile,
                                             $majorVersion,
                                             $minorVersion,
                                             $errorFuncRef,
                                             $includeSpecFilePath,
                                             $includeTargetDir,
                                             $rootTargetDir,
                                             $targetApp,
                                             $includeProjectDir,
                                             $targetArch,
                                             $targetOS,
                                             $companyName,
                                             0,
                                             @includeDestRegEx);

                # append results
                my $includeCpyMap = $includeData->{"copymap"};
                my $includeIncludeFiles = $includeData->{"includefiles"};
                push @includeFiles,(@$includeIncludeFiles,$includeSpecFilePath);
                
                my @includeJavaClasses = @{ $includeData->{"javaclasses"}};
                foreach my $javaClassName ( @includeJavaClasses ) {
                    @javaClasses = @{ appendElementToOrderedSet( \@javaClasses,
                                                                 \&compareStrFunc,
                                                                 $javaClassName ) };
                }

                my @includeResources = @{ $includeData->{"resources"}};
                foreach my $resource ( @includeResources ) {
                    @resources = @{ appendElementToOrderedSet( \@resources,
                                                               \&compareStrFunc,
                                                               $resource )
                                  };
                }
                
                while( my ($key,$val) = each(%$includeCpyMap) ) {
                    $cpyMap{$key} = $val;
                }
            }
            elsif ( $specLineArray[0] eq "java") {
                if ($targetArch eq "x86_64" or ! $java_common) {
                    shift @specLineArray;
                    foreach my $javaClassName ( @specLineArray ) {
                        @javaClasses = @{ appendElementToOrderedSet( \@javaClasses,
                                                                     \&compareStrFunc,
                                                                     $javaClassName ) };
                    }
                }
            }
            elsif ( $specLineArray[0] eq "resource" ) {
                # A resource is a file that is to be added to the jar file.  It
                # comes from the project directory, and is specified by the
                # path to the file, and the path in the jar file.  The path to
                # the file must end with the path in the jar file.
                if ($targetArch eq "x86_64" or ! $java_common) {
                    my $resourceFile = $specLineArray[1];
                    my $resourcePath = $specLineArray[2];
                    my $fullPath = canonicalizePath("$projectDir/$resourceFile");
                    if ($fullPath =~ s/\Q$resourcePath\E$//) {
                        my $resource = "$fullPath $resourcePath";
                        @resources = @{ appendElementToOrderedSet( \@resources,
                                                                   \&compareStrFunc,
                                                                   $resource )
                                      };
                    } else {
                        &$errorFuncRef( "$fullPath does not end with $resourcePath." );
                    }
                }
            }
            else {
                my $dest = $specLineArray[0];
                foreach my $regEx (@destRegEx) {
                    my ($a, $b) = split("->",$regEx);
                    $dest =~ s/$a/$b/;
                }
                
                my $tarRelativePath =  "$targetApp/$argchange/$dest";
                my $cpySrc;
                if ( File::Spec->file_name_is_absolute( $specLineArray[1] ) ) {
                    $cpySrc = canonicalizePath($specLineArray[1]); 
                }
                else{
                    #If the fourth item on the line is a t, then the file is copied from
                    #the build target directory.  If the fourth item on the line is a p
                    #then copy the file from project directory instead.
                    if ( ( $specLineArray[3] eq "" ) || ( $specLineArray[3] eq "t" ) ) {
                        $cpySrc =
                            canonicalizePath("$targetDir/$specLineArray[1]" );
                    }
                    else {
                        $cpySrc =
                            canonicalizePath("$projectDir/$specLineArray[1]");
                    }
                }
                
                my $cpyTarget =
                    canonicalizePath("$tarBuildDir/$tarRelativePath");
                
                my $targetPerm;
                if ( $specLineArray[2] eq "" ) {
                    $targetPerm = "644";
                }
                else {
                    $targetPerm = $specLineArray[2] ;
                }
                
                my @cpyPair = ( $cpySrc, $targetPerm );

                @{$cpyMap{$cpyTarget}} = @cpyPair;
            }
        }
        if ( ( $#javaClasses >= 0 ) and ( $isTopLevel == 1 ) ) {
            my $dest = "share/java/" . $targetApp . ".jar";
            my $tarRelativePath =  "$targetApp/$argchange/$dest";
            my $cpySrc = canonicalizePath("$targetDir/$targetApp.jar");
            my $cpyTarget = canonicalizePath("$tarBuildDir/$tarRelativePath");
            my @cpyPair = ( $cpySrc, "664" );
            @{$cpyMap{$cpyTarget}} = @cpyPair;
        }

        $data{"targetdir"}    = $targetDir;
        $data{"appname"}      = $targetApp;
        $data{"tarbuilddir"}  = $tarBuildDir;
        $data{"tarfilename"}  = $targetTarFileName;
        $data{"versionstr"}   = $argchange;
        $data{"targetos"}     = $targetOS;
        $data{"targetarch"}   = $targetArch;
        $data{"copymap"}      = \%cpyMap;
        $data{"includefiles"} = \@includeFiles;
        $data{"javaclasses"}  = \@javaClasses;
        $data{"resources"}    = \@resources;
    }
    $specFile->close();
    
    return \%data;
}

sub printDep {
    my $dataRef = shift;
    my $targetDir = $dataRef->{"targetdir"};
    my $tarFileName = $dataRef->{"tarfilename"};
    my $cpyMap = $dataRef->{"copymap"};
    my $includeFiles = $dataRef->{"includefiles"};
    my $appName = $dataRef->{"appname"};
    my $versionStr = $dataRef->{"versionstr"};
    my @javaClasses = @{ $dataRef->{"javaclasses"} };

    print ".PHONEY: " . $appName . "-package allpackages\n";    

    print "$targetDir/$appName.tar.gz: ";

    # (map { $_[0] } values(%$cpyMap),@$includeFiles) {
    foreach my $cpySrc ( sort (map { @{$_}[0]; } values(%$cpyMap)),@$includeFiles) {
        if ( ! ( $cpySrc eq "" ) ) {
            $cpySrc =~ s/\#/\\\#/g;
            print " \\\n    " . $cpySrc;
        }
    }
    
    print "\n";

    
    print "$targetDir/$appName.java \\\n";
    print "$targetDir/$appName.package.d:";
    foreach my $includedFile ( @{$includeFiles} ) {
        print " \\\n    $includedFile";
    }
    print "\n";
}

sub printJava {
    my ( $dataRef ) = @_;
    my @classList = @{ $dataRef->{"javaclasses"} };

    

    my $targetDir = $dataRef->{"targetdir"};
    my $appName = $dataRef->{"appname"};
    
    my $outputFileName = "$targetDir/$appName.java";
    
    my $outputFile = new FileHandle;
    $outputFile->open( ">$outputFileName" ) or die "Can't open $outputFileName for writing.";
    
    my $counter = 0;

    print $outputFile "public final class $appName {\n";
    if ( $#classList >= 0 ) {
        for my $className ( @classList ) {
            print $outputFile "  public final java.lang.String Foo_$counter() { return $className.class.getName() ; }\n";
            $counter = $counter + 1;
        }
    }
    print $outputFile "}\n";
        
    $outputFile->close();

}

sub printResources {
    my ( $dataRef ) = @_;
    my @resources = @{ $dataRef->{"resources"} };
    my $targetDir = $dataRef->{"targetdir"};
    my $appName = $dataRef->{"appname"};
    my $outputFileName = "$targetDir/$appName.resources";
    my $outputFile = new FileHandle;
    $outputFile->open( ">$outputFileName" ) or
        die "Can't open $outputFileName for writing.";
    for my $resource ( @resources ) {
        print $outputFile "$resource\n";
    }
    $outputFile->close();
}

sub myCpyFile {
    my ( $srcFileName, $destFileName, $targetPerm ) = @_;
    
    my $destDir = dirname( $destFileName );

    `mkdir -p $destDir 2>/dev/null` if ( ! ( -d $destDir ) );
    if( -f $srcFileName ) {
        system("/bin/rm -f '$destFileName'; /bin/cp -p '$srcFileName' '$destFileName'; /bin/chmod $targetPerm '$destFileName'");
        return 0;
    }
    "Source file: $srcFileName doesn't exist!";
}

sub myLnkFile {
    my ( $srcFileName, $destFileName, $targetPerm ) = @_;
    
    my $destDir = dirname( $destFileName );

    print STDERR "mkdir -p '$destDir'\n" if $verbose and ! -d $destDir;
    `mkdir -p '$destDir'` if ( ! ( -d $destDir ) );
    if( -f $srcFileName ) {
        if (($destFileName =~ m:/bin/:) and ($destFileName =~ m:\.(sh|pl)$:)) {
            printf STDERR "RENAME $destFileName " if $verbose;
            $destFileName =~ s:\.[^/\.]*$::;
            print STDERR "$destFileName\n" if $verbose;
        }
        print STDERR "/bin/rm -f '$destFileName'; /bin/ln -s '$srcFileName' '$destFileName'\n" if $verbose;
        system("/bin/rm -f '$destFileName'; /bin/ln -s '$srcFileName' '$destFileName'");
        return 0;
    }
    "Source file: $srcFileName doesn't exist!";
}

sub makeTarFile {
   my $dataRef = shift;

   my $tarFileName = $dataRef->{"tarfilename"};
   my $cpyMap = $dataRef->{"copymap"}; 
   my $tarBuildDir = $dataRef->{"tarbuilddir"};
   my $appName = $dataRef->{"appname"};
   my $targetDir = $dataRef->{"targetdir"};
   my $versionStr = $dataRef->{"versionstr"};
   my @javaClasses = @{ $dataRef->{"javaclasses"} };

   while( my ($cpyDest,$cpyPair) = each(%$cpyMap) ) {
        my $cpySrc = @{$cpyPair}[0];
        my $cpyPerm = @{$cpyPair}[1];

       my $haspkgroot = `fgrep -c '\$packageroot\$' "$cpySrc"`;
       chomp $haspkgroot;
       if ( ( ! ( $cpySrc eq "" ) ) and ( ! ( $cpyDest eq "" ) ) ){
           if ($links and (($cpySrc =~ m:^$rootProjectDir:) or ($cpySrc =~ m:^$buildSystemRoot:)) and ! $haspkgroot ) {
               print STDERR "link $cpySrc $cpyDest ($rootProjectDir)\n"
                  if $verbose;
               my $ret = myLnkFile( $cpySrc, $cpyDest, $cpyPerm );
               die $ret if $ret;
           }
           else {
               print STDERR "Copy $cpySrc $cpyDest ($rootProjectDir)\n"
                  if $verbose;
               my $ret = myCpyFile( $cpySrc, $cpyDest, $cpyPerm );
               die $ret if $ret;
            }
       }
       else {
         if ( $cpySrc eq "" ) {
           if ( $cpyDest eq "" ) {
             print "cpySrc and cpyDest were empty.\n";
           }
           else {
             print "cpySrc was empty for $cpyDest\n";
           }
         }
         else {
           print "cpyDest was empty for $cpySrc\n";
         }
       }     
   }
   
   my $packageRootFile =
      "$tarBuildDir/$appName/$versionStr/.fulcrum-package-root";
   my $user = $ENV{USER};
   my $hostname = `hostname`; chomp $hostname;
   my $date = `date +%Y-%m-%d-%H-%M-%S-%Z`; chomp $date;
   my $buildType = "unofficial";

   umask 0;
   open BUILD_IDENT,">$packageRootFile";
   print BUILD_IDENT "$appName Build: $user-$hostname-$date-$buildType";
   close BUILD_IDENT;
   print "packageRootFile $packageRootFile\n";
   `chmod 664 '$packageRootFile'`;
   `find $tarBuildDir -type d -printf '"%p"\\n' | xargs chmod g+s`;
   `tar -czf '$targetDir/$appName.tar.gz' -C '$tarBuildDir' .`;
   `rm -rf '$tarBuildDir'`;
}


my $printErrorRef = \&printError;

my $doTarFile = 1;

GetOptions (
    "depend|d" => sub { $doTarFile = 0;},
    "change=s" => \$argchange,
    "verbose" => \$verbose,
    "links" => \$links,
    "root-project-dir=s" => \$rootProjectDir,
    "build-system-root=s" => \$buildSystemRoot,
    "java-common" => \$java_common,
) or printError;

$buildSystemRoot=$rootProjectDir if $buildSystemRoot eq "xxx";

if ($links) {
    die "--links requires --root-project-dir\n" if ($rootProjectDir eq "xxx");
    die "root-project-dir must be permanent and rooted\n\t$rootProjectDir\n"
        if (! ($rootProjectDir =~ m:^/:) or ($rootProjectDir =~ m:^/scratch:) or
            ($rootProjectDir =~ m:^/tmp:));
}
if ( $argchange eq "0" and defined ($ENV{BUILD_CHANGE_NUMBER})) {
    $argchange = $ENV{BUILD_CHANGE_NUMBER};
}

my @validArgs = parseAndValidateArgs( $printErrorRef, @ARGV );

my $specFile = new FileHandle;
my $specFileName = $validArgs[0];
$specFile->open( "<$specFileName" ) or die "Can't open $specFileName";

my ( $majorVersion, $minorVersion ) = getMajorAndMinorVersion( $specFile, $specFileName, $printErrorRef );
print STDERR "$majorVersion.$minorVersion\n" if $verbose;

my $theData = buildData( $specFile, $majorVersion, $minorVersion, $printErrorRef, @validArgs);

if ( $doTarFile ) {
    if ($links and ! ($validArgs[4] =~ m:^$rootProjectDir/:)) {
        die "$rootProjectDir $validArgs[4]\nroot-project-dir must be root path to projectDir\n";
    }
    makeTarFile( $theData );
}
else {
    printDep( $theData );
    printJava( $theData );
    printResources( $theData );
}
