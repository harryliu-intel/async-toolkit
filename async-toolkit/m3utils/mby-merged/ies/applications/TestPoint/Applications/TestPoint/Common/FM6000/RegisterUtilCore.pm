# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

package Applications::TestPoint::Common::FM6000::RegisterUtilCore;
use strict;
use warnings;

use SDKScalars;

sub tpFM6000MapLogicalPortToPhysical
{
    my ($self, $sw, $logicalPort, $dimension2) = @_;

    my $chip = $self->{CHIP};
    my ($channel, $epl, $physPort, %void);

    %void = (type => "fm_int", value => 0);
    $chip->fmGetPortAttribute($sw,
                              $logicalPort,
                              $FM_PORT_SELECT_ACTIVE_MAC,
                              \%void);
    my $mac = $void{"value"};
    $chip->fmPlatformMapLogicalPortToPhysical($sw,
                                              $logicalPort,
                                              \$sw,
                                              \$physPort);
    $chip->fm6000MapPhysicalPortToEplChannel($sw, $physPort, \$epl, \$channel);
    return ($sw, $epl + $mac, map {$channel + $_} @{$dimension2});

}   # end tpFM6000MapLogicalPortToPhysical

1;
