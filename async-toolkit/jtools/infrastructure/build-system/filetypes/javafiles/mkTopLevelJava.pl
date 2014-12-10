#!/usr/intel/bin/perl
# Copyright 2003 Fulcrum Microsystems.  All rights reserved.
# $Id$
# $DateTime$
# $Author$
use strict;
use File::Spec;

sub PARSED_FILE_IMPORTS () { 0 }
sub PARSED_FILE_CLASSES () { 1 }
sub LOADED_FILE_LOADED_IMPORTS () { 2 }

sub dirName {
    my ( $fileName ) = @_;
    my ( $volume, $directory, $file ) = File::Spec->splitpath( $fileName );

    return File::Spec->catpath( ( $volume, $directory ) );
}
    
sub compareStrFunc {
    my ( $str1, $str2 ) = @_;
    return $str1 eq $str2;
}

sub parseFile {
    my ( $classListFileName ) = @_;
    
    my $classListFileDirName = dirName( $classListFileName );
    

    open( classListFile, "<$classListFileName" ) or die "Can't open $classListFileName";

    my @importList = ();
    my @classList = ();
    my $currLine;
    while ( $currLine = <classListFile> ) {

        
        $currLine =~ s/\s+/ /;
        my @currLineComponents = split( /\s+/ , $currLine );

        if ( scalar( @currLineComponents ) > 1 ) {
            my $directive = $currLineComponents[0];

            if ( $directive eq "import" ) {
                my $importTarget = $classListFileDirName . "/" . $currLineComponents[1];
                @importList = ( @importList, $importTarget );
                
            }
            else {
                print "\"$directive\" is an unknown directive.\n";
            }

        }
        else {
            $currLineComponents[0] =~ s/\s+//;
            if ( ! ( $currLineComponents[0] eq "" ) ) { 
                @classList = @{ appendElementToOrderedSet( \@classList, \&compareStrFunc, $currLineComponents[0] ) };  
            }
        }

    }

    my %result;

    $result{PARSED_FILE_IMPORTS()} = \@importList;
    $result{PARSED_FILE_CLASSES()} = \@classList;

    return \%result;

}


sub appendElementToOrderedSet {
    my ( $arraySetRef, $areSameFunc, $newElement ) = @_;

    my $inArray = 0;

    my $len = scalar( @{$arraySetRef} );

    my $i = 0;

    while ( ( $inArray == 0 ) &&
            ( $i < $len ) ) {
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

sub mergeParseFileResults {
    my ( $result0, $result1 ) = @_;

    my %result;

    my @importList = ( @{$result0->{PARSED_FILE_IMPORTS()}} , @{$result1->{PARSED_FILE_IMPORTS()}} ) ;

    $result{PARSED_FILE_IMPORTS()}=\@importList;

    my @classList = @{$result0->{PARSED_FILE_CLASSES()}};
    for my $className ( @{$result1->{PARSED_FILE_CLASSES()}} ) {
        @classList = @{ appendElementToOrderedSet( \@classList, \&compareStrFunc, $className ) };
    }
    
    $result{PARSED_FILE_CLASSES()}=\@classList;

    return \%result;

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


sub loadFile {

    my ( $fileName ) = @_;

    $fileName = canonicalizePath( $fileName );

    my $currParseResult;

    my %importedFiles;

    $currParseResult = parseFile( $fileName );

    do {

        my @importList = @{$currParseResult->{PARSED_FILE_IMPORTS()}};

        if ( scalar( @importList ) > 0 ) {
            my $fileToImport = canonicalizePath( pop( @importList ) );
            
            $currParseResult->{PARSED_FILE_IMPORTS()} = \@importList;
            if ( ! defined( $importedFiles{$fileToImport} ) ) {
                $importedFiles{$fileToImport} = 1;                
                my $newParseResult = parseFile( $fileToImport );
                $currParseResult = mergeParseFileResults( $newParseResult, $currParseResult );
            }
        }
    } while ( scalar( @{$currParseResult->{PARSED_FILE_IMPORTS()}} ) > 0 );

    my @loadedFiles = ( $fileName, keys( %importedFiles ) );
    $currParseResult->{LOADED_FILE_LOADED_IMPORTS()} = \@loadedFiles;

    return $currParseResult;
}

sub emitJava {
    my ( $outputFileName, $topLevelClassName, $classListRef ) = @_;

    my @classList = @{ $classListRef };

    my $counter = 0;

    open( outputFile, ">$outputFileName" ) or die "Can not open \"$outputFileName\" for writing.";

    print outputFile "public final class $topLevelClassName {\n";
    for my $className ( @classList ) {
        print outputFile "  public final java.lang.String Foo_$counter() { return $className.class.getName() ; }\n";
        $counter = $counter + 1;
    }
    print outputFile "}\n";
    close outputFile;
}

sub emitDeps {
    my ( $outputFileName, $loadResult ) = @_;

    my @loadedFiles = @{$loadResult->{LOADED_FILE_LOADED_IMPORTS()}};

    print "$outputFileName $outputFileName.d : ";
    for my $loadedFileName ( @loadedFiles ) {
        print " \\\n  $loadedFileName";
    }
    print "\n";
}

my $doDeps=0;

if ( $ARGV[0] eq "-d" ) {
    $doDeps = 1;
    shift @ARGV;
}

my $inputFile = $ARGV[0];

my $className = $ARGV[1];

my $outputFileName = $ARGV[2];

my $parseResult = loadFile( $inputFile );

if ( $doDeps ) {
    emitDeps( $outputFileName, $parseResult );
}
else {
    my $classListRef = $parseResult->{PARSED_FILE_CLASSES()};

    emitJava( $outputFileName, $className, $classListRef );
}
