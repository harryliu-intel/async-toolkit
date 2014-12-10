#
# PerforceIntegration
#
# Commands for checking files in & out of Perforce
#

package Perforce;

BEGIN {
    use Exporter;
    our @ISA = qw(Exporter);
    our @EXPORT_OK = qw( getdirsfromp4client );
}

use strict;

sub getdirsfromp4client {
    my ($client,$dirs,$verbose) = @_;
    return if ! defined($client);
    my $root=`p4 -c '$client' info | awk '/^Client root:/ {print \$3}'`;
    chomp $root;
    if ( ! -d $root ) {
        warn "Apparent p4 client root ($root) is not a directory";
        return;
    }
    open (P4, "p4 -c '$client' client -o |");
    while (<P4>) {
        chomp;
        if ( ! defined($dirs->{cast_dir}) and m://depot/\S*/cast/:) {
            s/.*\s//;
            s:/cast/.*:/cast:;
            s://$client:$root:;
            s://:/:g;
            if ( -d $_) {
                $dirs->{cast_dir} = $_;
                print STDERR "Resetting cast-dir=$dirs->{cast_dir}"
                    if $verbose;
            }
        }
        elsif ( ! defined($dirs->{spec_dir}) and m://depot/\S*/spec/:) {
            s/.*\s//;
            s:/spec/.*:/spec:;
            s://$client:$root:;
            s://:/:g;
            if ( -d $_) {
                $dirs->{spec_dir} = $_;
                print STDERR "Resetting spec-dir=$dirs->{spec_dir}"
                    if $verbose;
            }
        }
        elsif ( ! defined($dirs->{spar_dir}) and m://depot/\S*/spar/:) {
            s/.*\s//;
            s:/spar/.*:/spar:;
            s://$client:$root:;
            s://:/:g;
            if ( -d $_) {
                $dirs->{spar_dir} = $_;
                print STDERR "Resetting spar-dir=$dirs->{spar_dir}"
                    if $verbose;
            }
        }
        elsif ( ! defined($dirs->{dfII_dir}) and m://depot/\S*/dfII/:) {
            s/.*\s//;
            s:/dfII/.*:/dfII:;
            s://$client:$root:;
            s://:/:g;
            if ( -d $_) {
                $dirs->{dfII_dir} = $_;
                print STDERR "Resetting dfII-dir=$dirs->{dfII_dir}"
                    if $verbose;
            }
        }
    }
    if (defined ($dirs->{cast_dir}) and defined ($dirs->{spec_dir})) {
        $dirs->{cast_path}="$dirs->{cast_dir}:$dirs->{spec_dir}";
    }
}

1;
