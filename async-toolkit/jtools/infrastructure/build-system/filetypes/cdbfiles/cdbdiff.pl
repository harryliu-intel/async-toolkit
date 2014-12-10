#!/usr/intel/bin/perl -w

use File::Basename;

my $dep1     = $ARGV[0];
my $dep2     = $ARGV[1];


sub getCDBFilesFromDepFile {
    my ($depFile) = @_;
    my @files = split("\n", `cat "$depFile" | xargs -n 1 echo | grep '^\/' | grep -v "$depFile"`);
    return \@files;
}

sub getStub {
    my ($file,$dir) = @_;
    $file =~ s/\Q$dir\Q//;
    return $file;
}

sub getDFIIDirFromFile {
    my ($file) = @_;
    while (! ( $file eq "/" ) &&
           ! ( -e "$file/cds.lib.generated" )
           ) {
        $file = &dirname($file);
    }
    return $file;
}

my $files1 = &getCDBFilesFromDepFile($dep1);
my $files2 = &getCDBFilesFromDepFile($dep2);


if(!defined $files1->[0]) {
    print "No files in $dep1\n";
    exit 2;
}
if(!defined $files2->[0]) {
    print "No files in $dep2\n";
    exit 2;
}

my $dfIIDir1 = getDFIIDirFromFile($files1->[0]);
my $dfIIDir2 = getDFIIDirFromFile($files2->[0]);

%map1 = map { getStub($_,$dfIIDir1) => 1 } @{$files1};


foreach $file (@{$files2}) {
    my $stub = &getStub($file,$dfIIDir2);
    if( defined $map1{$stub} ) {
        my $otherFile = "${dfIIDir1}$stub";
        my $diff = system("cmp -s $file $otherFile");
        if($diff!=0) {
            print "$file and $otherFile differ\n";
            exit 2;
        }
        delete($map1{$stub});
    } else {
        print "$dep1 missing $stub\n";
        exit 2;
    }
}

    
foreach $stub (keys %map1) {
    print "$dep2 missing $stub\n";
    exit 2;
} 


