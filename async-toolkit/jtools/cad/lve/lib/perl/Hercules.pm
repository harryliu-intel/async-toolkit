package Hercules;

use strict;
use warnings;

BEGIN {
    use Exporter ();
    our @ISA = qw(Exporter);
}

sub get_device_names {
    my ($pdk_root) = @_;  # TODO: the list should be loaded from the PDK
    return [
        # list of _mac devices from /nfs/site/disks/local_tools/Avago28/tsmc/t-n28-cl-ls-002-e1_1_0_3d_20140124/profile/X_DEV.cmd
        'nch_15_mac',
        'nch_15od18_mac',
        'nch_18_mac',
        'nch_18ud12_mac',
        'nch_18ud15_mac',
        'nch_25_mac',
        'nch_25od33_mac',
        'nch_25ud18_mac',
        'nch_33_mac',
        'nch_chvt_mac',
        'nch_edc_mac',
        'nch_elvt_mac',
        'nch_hg_mac',
        'nch_hguhvt_mac',
        'nch_hia15_mac',
        'nch_hia18_mac',
        'nch_hia25_mac',
        'nch_hia_mac',
        'nch_hv18_mac',
        'nch_hvt_mac',
        'nch_io_lvt_mac',
        'nch_lvt18ud12_mac',
        'nch_lvt_mac',
        'nch_mac',
        'nch_mlvt_mac',
        'nch_na15_mac',
        'nch_na18_mac',
        'nch_na18ud15_mac',
        'nch_na25_mac',
        'nch_na25od33_mac',
        'nch_na25ud18_mac',
        'nch_na33_mac',
        'nch_na_mac',
        'nch_svt_sp_mac',
        'nch_udm18_mac',
        'nch_udm18ud15_mac',
        'nch_udm_mac',
        'nch_uhvt_mac',
        'nch_ulvt_mac',
        'nch_zvt_mac',
        'pch_15_mac',
        'pch_15od18_mac',
        'pch_18_mac',
        'pch_18ud12_mac',
        'pch_18ud15_mac',
        'pch_25_mac',
        'pch_25od33_mac',
        'pch_25ud18_mac',
        'pch_33_mac',
        'pch_alvt_mac',
        'pch_elvt_mac',
        'pch_hg_mac',
        'pch_hguhvt_mac',
        'pch_hv18_mac',
        'pch_hvt_mac',
        'pch_lvt_mac',
        'pch_mac',
        'pch_mlvt_mac',
        'pch_svt_sp_mac',
        'pch_udm18_mac',
        'pch_udm18ud15_mac',
        'pch_udm_mac',
        'pch_uhvt_mac',
        'pch_ulvt_mac',
        # need to append _mac suffix for macro-models
        'nchpd_dpsr',
        'nchpg_dpsr',
        'pchpu_dpsr',
        'nchpd_sr',
        'nchpg_sr',
        'pchpu_sr'
    ];
}

sub write_nettran_devmap {
    my ($file, $macro_models) = @_;
    open (my $fh, '>', $file) or die "Can't write to $file: $!";
    my @map = map { ( $_ =~ /_mac$/ ? () : "( \"$_\" \"${_}_mac\" )" ) }
                  @{$macro_models};
    local $" = "\n";
    print $fh <<EOF;
( ( model
@map
) )
EOF
    close $fh;
}

1;
