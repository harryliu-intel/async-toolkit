# vim:et:sw=4:ts=4:tw=79:
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/MACCore.pm
# Creation Date:    04/06/07
# Description:      
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2011 Intel Corporation. All Rights Reserved. 
#
# The source code contained or described herein and all documents related
# to the source code ("Material") are owned by Intel Corporation or its
# suppliers or licensors. Title to the Material remains with Intel
# Corporation or its suppliers and licensors. The Material contains trade
# secrets and proprietary and confidential information of Intel or its
# suppliers and licensors. The Material is protected by worldwide copyright
# and trade secret laws and treaty provisions. No part of the Material may
# be used, copied, reproduced, modified, published, uploaded, posted,
# transmitted, distributed, or disclosed in any way without Intel's prior
# express written permission.
#
# No license under any patent, copyright, trade secret or other intellectual
# property right is granted to or conferred upon you by disclosure or 
# delivery of the Materials, either expressly, by implication, inducement,
# estoppel or otherwise. Any license under such intellectual property rights
# must be express and approved by Intel in writing.
###############################################################################

package Applications::TestPoint::Common::MACCore;
use strict;
use warnings;

#Loaded when chip family is detected
#use base qw(
#    Exporter
#    Applications::TestPoint::Common::FM2000::MACCore
#    Applications::TestPoint::Common::FM4000::MACCore
#    Applications::TestPoint::Common::FM6000::MACCore
#);

use SDKScalars;
use Applications::TestPoint::Common::Messages;
our @EXPORT = qw(
    tpHandleAddMac
    tpHandleDelMac
    tpHandleResetMac
    tpHandleShowMac
);

##@cmethod public int tpHandleAddMac(char   *macAddress,
#                                    int    vlanID,
#                                    char   *type,
#                                    char   *destMask)
#
# @desc         Handles adding a MAC address
#
# @param[in]    macAddress The MAC address to be added
#
# @param[in]    vlanID The VLAN ID to associate with the MAC address that is to
#               be added
#
# @param[in]    type The type of the MAC address that is to be added. Valid
#               values are "locked", "unlocked" and "vid2"
#
# @param[in]    data1 The value depends on type
# 
# @param[in]    data2 The value depends on type
# 
# @param[in]    data3 The value depends on type
# 
# @param[in]    data4 The value depends on type
#
# @param[in]    data5 The value depends on type

# @param[in]    data6 The value depends on type
# 
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
#
# @note         To perform platform dependent MAC address entry modifications,
#               a platform can implement the tpPlatformSetAddress method, which
#               has the following prototype:
#
#               @code
#               public void
#               tpPlatformSetAddress(SDK::fm_macAddressEntry *entriesW)
#               @endcode
sub tpHandleAddMac
{
    my ($self, $macAddress, $vlanID, $type, $data1, $data2, $data3, $data4,
        $data5, $data6, $data7, $data8) = @_;

    my $vlanID2;
    my $destType;
    my $port;
    my $trig;
    my $hasVid2;
    my $hasRemoteID;
    my $remoteID;
    my $hasRemoteMac;
    my $remoteMac;

    if (defined($type) && $type eq "vid2")
    {
        $hasVid2 = 1;
        $vlanID2 = $data1;
        $type = $data2;
        $destType = $data3; 
        $port = $data4;
        if (defined($data5))
        {
           if ($data5 eq "remoteID")
           {
              $hasRemoteID = 1;
              $remoteID = $data6;
           }
           else
           {
              return $FM_ERR_INVALID_ARGUMENT;
           }

           if (defined($data7))
           {
              if ($data7 eq "remoteMac")
              {
                 $hasRemoteMac = 1;
                 $remoteMac = $data8;
              }
              else
              {
                 return $FM_ERR_INVALID_ARGUMENT;
              }
           }
        }
    }
    else
    {
        $vlanID2 = 0;
        $destType = $data1;
        $port = $data2;
        if (defined($data3) && $data3 eq "remoteID")
        {
           $hasRemoteID = 1;
           $remoteID = $data4; 

           if (defined($data5) && $data5 eq "remoteMac")
           {
              $hasRemoteMac = 1;
              $remoteMac = $data6; 
           }
        }
        else
        {
           $trig = $data3;
        }
    }

    my $chip = $self->{CHIP};

    # validate number of parameters
    if (!defined($macAddress)
        || !defined($vlanID)
        || !defined($vlanID2)
        || !defined($type)
        || !defined($destType)
        || !defined($port))
    {
        if (defined($hasVid2))
        {
           if (defined($hasRemoteID))
           {
              if (defined($hasRemoteMac))
              {
                  print("Must specify <dmac> <vid> vid2 <vid2> <type> <destType> <port> <remoteID> <remoteMac>!\n");
              }
              else 
              {
                  print("Must specify <dmac> <vid> vid2 <vid2> <type> <destType> <port> <remoteID>!\n");
              }
           }
           else
           {
               print("Must specify <dmac> <vid> vid2 <vid2> <type> <destType> <port>!\n");
           }
        }
        else
        {
           if (defined($hasRemoteID))
           {
              if (defined($hasRemoteMac))
              {
                 print("Must specify <dmac> <vid> <type> <destType> <port> <remoteID> <remoteMac>!\n");
              }
              else 
              {
                 print("Must specify <dmac> <vid> <type> <destType> <port> <remoteID>!\n");
              }
           }
           else
           {
              print("Must specify <dmac> <vid> <type> <destType> <port>!\n");
           }
        }
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # convert trigger to int
    if (!defined($trig))
    {
        $trig = -1;
    }
    else
    {
        my $trigTmp = $trig;
        $trig = $self->str2intnum($trig);
        if (!defined($trig))
        {
           print "Invalid argument <$trigTmp>!\n"; 
           return $FM_ERR_INVALID_ARGUMENT;
        }
    }

    # validate the remote ID
    if (!defined($remoteID))
    {
       $remoteID = 0;
    }
    else
    {
       my $remoteIdTmp = $remoteID;
       $remoteID = $self->str2intnum($remoteID);
       if (!defined($remoteID))
       {
          print "Invalid argument <$remoteIdTmp>!\n"; 
          return $FM_ERR_INVALID_ARGUMENT;
       }
       elsif ( $remoteID < 0 || $remoteID > 4095 )
       {
           print "Invalid remote ID <$remoteID>, must be between 0 and 4095\n"; 
           return $FM_ERR_INVALID_ARGUMENT;
       }
    }

    # validate the remoteMac flag
    if (!defined($remoteMac))
    {
       $remoteMac = "off";
    }
    elsif ($remoteMac ne "on" && $remoteMac ne "off")
    {
       print "Invalid remoteMac  <$remoteMac>, must be 'on' or 'off'\n"; 
       return $FM_ERR_INVALID_ARGUMENT;
    }

    # validate the destination type
    $destType = lc($destType);
    if ( ($destType ne "lag") && ($destType ne "port") )
    {
        print("Invalid destination type.  Use \"port\" or \"lag\".\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # validate the dmac
    if ($self->validateL2Address($macAddress) != $FM_OK)
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Do not allow new MAC addresses to use non-existing VLAN IDs.
    $vlanID = $self->str2intnum($vlanID);
    if (!defined($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!$self->_tpIsValidVLAN($vlanID))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

#   my @globalPortList = ();
#    my $switchNum;
#    my $logicalLag;
#    if ($destType eq "lag")
#    {
#        # Validate lag number
#        $logicalLag = $self->str2intnum($port);
#        if (!defined($logicalLag))
#        {
#            print($TP_MSG_ERR_LAG_INVALID_SCALAR);
#            return $FM_ERR_INVALID_ARGUMENT;
#        }
#
#        ($switchNum, $logicalLag) =
#                          $self->tpPlatformMapGlobalToLogicalLAG($logicalLag);
#
#        if (!$self->_tpIsValidLAG($switchNum, $logicalLag))
#        {
#            # the lag does not appear on this switch
#            print($TP_MSG_ERR_LAG_INVALID_SCALAR);
#            return $FM_ERR_INVALID_ARGUMENT;
#        }
#    }
#    else # "port" by default as destType has been validated
#    {
#        # build the global port list
##       @globalPortList = $self->validateList($port,
##                                     $self->tpPlatformGetFaceplatePortRange);
##
##       if (scalar(@globalPortList) == 0)
##       {
##           print($TP_MSG_ERR_PORT_INVALID_ARRAY);
##           return $FM_ERR_INVALID_ARGUMENT;
##       }
#    }

    # get the number of switches and prepare the entries array to be big 
    # enough for each switch
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $entriesW = [];
    map {$entriesW->[$_] = SDK::fm_macAddressEntry->new()} $self->tpGetSwitches;

    # prepare the entries to be added to the table
    my $status = $FM_OK;
    if ($destType eq "lag")
    {
        # Validate lag number
        my $lagNum = $self->str2intnum($port);
        if (!defined($lagNum))
        {
            print($TP_MSG_ERR_LAG_INVALID_SCALAR);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my ($switchNum, $logicalLag) = 
                          $self->tpPlatformMapGlobalToLogicalLAG($lagNum);

        if (!$self->_tpIsValidLAG($switchNum, $logicalLag))
        {
            # the lag does not appear on this switch
            print($TP_MSG_ERR_LAG_INVALID_SCALAR);
            return $FM_ERR_INVALID_ARGUMENT;
        }

        my $entryW = $entriesW->[$switchNum];

        # Get switch information from SDK
        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($switchNum, $info);

        if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000) ||
             ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) )
        {
            # validate the type
            if ($type ne "locked" && $type ne "unlocked"
                && $type ne "secureLocked" && $type ne "secureUnlocked")
            {
                printf("Must specify a valid type!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            # create the mac entry with the lag logical port
            my $lagLogPort;
            $chip->fmLAGNumberToLogicalPort($switchNum, 
                                            $logicalLag, 
                                            \$lagLogPort);
            my @ports = ( $lagLogPort );
            $status = $self->tpFM4000CreateMACEntry($switchNum,
                                                    $entryW,
                                                    $macAddress,
                                                    $vlanID,
                                                    $type,
                                                    \@ports, 
                                                    1);
            
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            # create the mac entry with the lag logical port
            my $lagLogPort;
            $chip->fmLAGNumberToLogicalPort($switchNum, 
                                            $logicalLag, 
                                            \$lagLogPort);
            my @ports = ( $lagLogPort );
            $status = $self->tpFM6000CreateMACEntry($switchNum,
                                                    $entryW,
                                                    $macAddress,
                                                    $vlanID,
                                                    $vlanID2,
                                                    $type,
                                                    \@ports,
                                                    1,
                                                    $remoteID,
                                                    $remoteMac);
            if ($status != $FM_OK)
            {
                return $status;
            }
        }

        elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
        {
            # validate the type
            if ($type ne "locked" && $type ne "unlocked")
            {
                printf("Must specify a valid type!\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }

            my $ports = [(0) x $FM_MAX_NUM_LAG_MEMBERS];
            my $numPorts;
            my $stat = $chip->fmGetLAGPortListExt($switchNum, $logicalLag,
                                                 \$numPorts, $ports,
                                                 $FM_MAX_NUM_LAG_MEMBERS);

            my $lowestPort = undef;
            for (my $i = 0; $i < $numPorts; $i++)
            {
                my $faceplatePort = 
                    $self->tpPlatformMapLogicalToFaceplatePort($switchNum,
                                                           $ports->[$i]);
                if (!defined $lowestPort)
                {
                    $lowestPort = $faceplatePort;
                }
                else
                {
                    if ($faceplatePort < $lowestPort)
                    {
                        $lowestPort = $faceplatePort;
                    }
                }
            }

            if (!defined $lowestPort)
            {
                printf("No ports found in LAG\n");
                return $FM_ERR_INVALID_ARGUMENT;
            }
            printf("\n");

            my @lagPorts = ($lowestPort);
            $status = $self->tpFM2000CreateMACEntry($switchNum,
                                                    $entryW,
                                                    $macAddress,
                                                    $vlanID,
                                                    $type,
                                                    \@lagPorts);
            
            if ($status != $FM_OK)
            {
                return $status;
            }
        }
    }
    else
    {
        my $validPortList = $FALSE;
        foreach my $switchNum ($self->tpGetSwitches)
        {
            my @portList = $self->validateExplicitList($TRUE, $port,
                              $self->tpPlatformGetLogicalPortList($switchNum,
                                                                  "local,lag"));
            if (scalar(@portList) == 0)
            {
                # None of the ports appear on this switch
                next;
            }
       
            $validPortList = $TRUE;

            my $entryW = $entriesW->[$switchNum];

            # Get switch information from SDK
            my $info = new SDK::fm_switchInfo();
            $chip->fmGetSwitchInfo($switchNum, $info);

            if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000) ||
                 ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) )
            {
                if ( (scalar(@portList) > 1) && 
                    ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_SWAG) )
                {
                    # because the value should be a scalar for swag.
                    print($TP_MSG_ERR_PORT_INVALID_SCALAR);
                    return $FM_ERR_INVALID_ARGUMENT;
                }

                if ($type ne "locked" && $type ne "unlocked"
                    && $type ne "secureLocked" && $type ne "secureUnlocked")
                {
                    printf("Must specify a valid type!\n");
                    return $FM_ERR_INVALID_ARGUMENT;
                }
                $status = $self->tpFM4000CreateMACEntry($switchNum,
                                                        $entryW,
                                                        $macAddress,
                                                        $vlanID,
                                                        $type,
                                                        \@portList);
            }
            elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
            {
                $status = $self->tpFM6000CreateMACEntry($switchNum,
                                                        $entryW,
                                                        $macAddress,
                                                        $vlanID,
                                                        $vlanID2,
                                                        $type,
                                                        \@portList,
                                                        0,
                                                        $remoteID,
                                                        $remoteMac);
            }
            elsif ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000)
            {
                if ($type ne "locked" && $type ne "unlocked")
                {
                    printf("Must specify a valid type!\n");
                    return $FM_ERR_INVALID_ARGUMENT;
                }
                $status = $self->tpFM2000CreateMACEntry($switchNum,
                                                        $entryW,
                                                        $macAddress,
                                                        $vlanID,
                                                        $type,
                                                        \@portList);
            }

            if ($status != $FM_OK)
            {
                printf("Switch %d: ", $switchNum) if ($switchCount > 1);
                printf("MAC address %s cannot be added: %s\n", $macAddress,
                       $status);
                return $status;
            }

        } # foreach my $switchNum ($self->tpGetSwitches)

        if ($validPortList ==  $FALSE)
        {
            print($TP_MSG_ERR_PORT_INVALID_ARRAY);
            return $FM_ERR_INVALID_ARGUMENT;
        }

    } # if (lc($destMask) eq "lag") else case

    # Perform platform dependent MAC address entry modifications.
    if (defined($self->can("tpPlatformSetAddress")))
    {
        $self->tpPlatformSetAddress($entriesW);
    }

    my $result = $FM_OK;
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();

        my $status = $FM_OK;
        if ($trig == -1) 
        {
            $status = $chip->fmAddAddress($switchNum,
                                          $entriesW->[$switchNum]);
        }
        else
        {
            $status = $chip->fmAddAddressToTable($switchNum,
                                                 $entriesW->[$switchNum], 
                                                 $trig,
                                                 $TRUE,
                                                 -1);
        }

        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            printf("MAC address %s cannot be added: %s!\n", $macAddress,
                    $status);
        }
    }
    return $result;
}

##@cmethod public int tpHandleDelMac(char *macAddress, int vlanID)
#
# @desc         Handles deleting a MAC address
#
# @param[in]    macAddress The MAC address to be deleted
#
# @param[in]    vlanID The VLAN ID associated with the MAC address to be
#               deleted
#               
# @param[in]    vlanID The second VLAN ID associated with the MAC address to be
#               deleted
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
# @return       FM_FAIL otherwise
sub tpHandleDelMac
{
    my ($self, $macAddress, $vlanID, $vlanID2) = @_;

    my $chip = $self->{CHIP};

    if (!defined($macAddress) || !defined($vlanID))
    {
        print("Must specify <mac address> <vid>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($macAddress)
        || $self->validateL2Address($macAddress) != $FM_OK)
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    # Do not perform strict VLAN ID checking to allow deletion of MAC addresses
    # with an associated VLAN ID that might no longer exists due to a
    # ``del vlan'' command.
    $vlanID = $self->str2intnum($vlanID);
    my ($minimum, $maximum) = $self->tpPlatformGetVLANRange;
    if (!defined($vlanID) || ($vlanID < $minimum || $vlanID > $maximum))
    {
        print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    my $entryW = SDK::fm_macAddressEntry->new();
    $entryW->{"macAddress"} = $macAddress;
    $entryW->{"vlanID"} = $vlanID;
    $entryW->{"vlanID2"} = $vlanID2;
    $entryW->{"type"} = 0;          # ignored
    $entryW->{"age"} = 0;           # ignored
    $entryW->{"destMask"} = 0;      # ignored

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmDeleteAddress($switchNum, $entryW);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            if ($status == $FM_ERR_ADDR_NOT_FOUND)
            {
                if (!defined $vlanID2)
                {
                    printf("%s with associated VLAN %d cannot be found\n",
                           $macAddress, $vlanID);
                }
                else
                {
                    printf("%s with associated VID1=%d and VID2=%d cannot be found\n",
                           $macAddress, $vlanID, $vlanID2);
                }
                next;
            }
            printf("MAC address %s cannot be deleted\n", $macAddress);
        }
    }
    return $result;
}

##@cmethod public int tpHandleResetMac(void)
#
# @desc         Handles resetting the MAC address table
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
sub tpHandleResetMac
{
    my ($self, $type) = @_;

    my $chip = $self->{CHIP};

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        my $status = $chip->fmDeleteAllAddresses($switchNum);
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL; 
            printf("Switch %d: ", $switchNum) if ($switchCount > 1);
            print("MAC address table cannot be reset!\n");
        }
        $chip->enableErrors();
    }
    return $result;
}

##@cmethod public int tpHandleShowMacStats()
#
# @desc         Handles showing MAC address stats
#
# @param[in]    macAddress The set of MAC addresses to be shown
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowMacStats
{
    my ($self) = @_;
    my $chip = $self->{CHIP};

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    my $macEntries;
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my ($nEntries);

        print("\n");
        if ($switchCount > 1)
        {
            printf("Switch %d:\n", $switchNum) if ($switchCount > 1);
            print("----------------------------------------------------------\n\n");
        }

        my %void = (type => 'fm_uint32', value => 0);
        $chip->disableErrors();
        my $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                       $FM_MAC_TABLE_SIZE,
                                                       \%void);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            print("MAC table size cannot be retreived!\n");
            next SWITCH;
        }
        $chip->disableErrors();
        my $maxEntries = $void{value} / $chip->fmSWIGGetMacEntrySize();
        $chip->enableErrors();
        $macEntries = [map {SDK::fm_macAddressEntry->new()} (1 .. $maxEntries)];

        $chip->disableErrors();
        $status = $chip->fmGetAddressTable($switchNum, \$nEntries, $macEntries);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            print("MAC address stats cannot be shown!\n");
            next SWITCH;
        }

        my ($nStatic, $nDynamic);

        $nStatic = 0;
        $nDynamic = 0;

        for (my $i = 0; $i < $nEntries; $i++)
        {
            my $entryW = $macEntries->[$i];

            if ($entryW->{"type"})
            {
                $nDynamic++;
            }
            else
            {
                $nStatic++;
            }
        }

        printf("Total number of entries   : %d\n", $nEntries); 
        printf("Number of static entries  : %d\n", $nStatic); 
        printf("Number of dynamic entries : %d\n", $nDynamic); 
    }
}

##@cmethod public int tpHandleShowMac(char *macAddress)
#
# @desc         Handles showing a set of MAC addresses
#
# @param[in]    macAddress The set of MAC addresses to be shown
# 
# @param[in]    vlanId The VLAN to which the MAC address belongs
# 
# @param[in]    vlanId2 The second VLAN to which the MAC address belongs
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowMac
{
    my ($self, $macAddress, $vlanId, $vlanId2) = @_;

    my $chip = $self->{CHIP};
    my $macEntries;

    if (!defined($macAddress))
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    my $showAll = $FALSE;
    if ($macAddress eq "all")
    {
        $showAll = $TRUE;
    }
    elsif ($macAddress eq "stats")
    {
        return $self->tpHandleShowMacStats();
    }
    else
    {
        if ($self->validateL2Address($macAddress) != $FM_OK)
        {
            print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
            return $FM_ERR_INVALID_ARGUMENT;
        }
        
        if (defined($vlanId))
        {
            if (!defined($vlanId2))
            {
                $vlanId2 = 0;
            }

            $vlanId = $self->str2intnum($vlanId);
            return $self->tpHandleShowSingleMac($macAddress, $vlanId, $vlanId2);
        }
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        my $nEntries = 0;

        $self->DumpMacAddressHeader($switchNum, $switchCount);

        my %void = (type => 'fm_uint32', value => 0);
        $chip->disableErrors();
        my $status = $chip->fmGetAddressTableAttribute($switchNum,
                                                       $FM_MAC_TABLE_SIZE,
                                                       \%void);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            print("MAC table size cannot be retreived!\n");
            next SWITCH;
        }
        $chip->disableErrors();
        my $maxEntries = $void{value} / $chip->fmSWIGGetMacEntrySize();
        $chip->enableErrors();
        $macEntries = [map {SDK::fm_macAddressEntry->new()} (1 .. $maxEntries)];

        $chip->disableErrors();
        $status = $chip->fmGetAddressTable($switchNum, \$nEntries, $macEntries);
        $chip->enableErrors();
        if ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("ERROR: %s\n", $status);
            print("MAC address table cannot be shown!\n");
            next SWITCH;
        }

        my ($minimum, $maximum) = $self->tpPlatformGetVLANRange;
        ENTRY: for (my $i = 0; $i < $nEntries; $i++)
        {
            my $entryW = $macEntries->[$i];
            
            if (!$showAll
                && ($entryW->{"macAddress"} ne $macAddress
                    || ($entryW->{"vlanID"} < $minimum
                        && $entryW->{"vlanID"} > $maximum)))
            {
                next ENTRY;
            }

            $self->DumpMacAddressEntry($switchNum, $entryW);
        }
        print("\n");
    }
    return $result;
}


##@cmethod public int tpHandleShowSingleMac(char *macAddress, int vlanId)
#
# @desc         Handles showing a single MAC address
#
# @param[in]    macAddress The MAC address to be shown
#
# @param[in]    vlanId The VLAN to which the MAC address belongs
# 
# @param[in]    vlanId2 The second VLAN to which the MAC address belongs
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleShowSingleMac
{
    my ($self, $macAddress, $vlanId, $vlanId2) = @_;

    my $chip = $self->{CHIP};

    if (!defined($macAddress))
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }
    
    if ($self->validateL2Address($macAddress) != $FM_OK)
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($vlanId))
    {
        print($TP_MSG_ERR_VLAN_INVALID_SCALAR);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!$self->_tpIsValidVLAN($vlanId))
    {
        print($TP_MSG_ERR_VLAN_INVALID_ID);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $result = $FM_OK;
    my $switchCount = scalar(@{[$self->tpGetSwitches]});
    
    SWITCH: foreach my $switchNum ($self->tpGetSwitches)
    {
        $chip->disableErrors();
        
        my $entryW = SDK::fm_macAddressEntry->new();
        my $status = $chip->fmGetAddressV2($switchNum,
                                           $macAddress,
                                           $vlanId,
                                           $vlanId2,
                                           $entryW);
        
        $chip->enableErrors();
        if ($status == $FM_ERR_ADDR_NOT_FOUND)
        {
            print("MAC address not found!\n");
            next SWITCH;
        }
        elsif ($status != $FM_OK)
        {
            $result = $FM_FAIL;
            printf("ERROR %s: %s\n", $status, $chip->fmErrorMsg($status));
            next SWITCH;
        }

        $self->DumpMacAddressHeader($switchNum, $switchCount);
        $self->DumpMacAddressEntry($switchNum, $entryW);
        print("\n");
    }
    
    return $result;
        
}    
        
        
##@cmethod private void DumpMacAddressHeader(fm_int switchNum, fm_int switchCount)
#
# @desc         Handles showing a single MAC address
#
# @param[in]    switchNum The switch number
#
# @param[in]    switchCount The number of switches being processed (in multiple
#               calls to this function.
#
sub DumpMacAddressHeader
{
    my ($self, $switchNum, $switchCount) = @_;
    my $chip = $self->{CHIP};

    print("\n");
    if ($switchCount > 1)
    {
        printf("Switch %d:\n", $switchNum);
        print("----------------------------------------------------------\n\n");
    }

    my $info = new SDK::fm_switchInfo();
    $chip->fmGetSwitchInfo($switchNum, $info);
    if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
    {
       printf("%-20s %-7s %-4s %-4s %-6s %-6s %-5s %-8s %-10s\n",
              "MAC", "Mode", "FID1", "FID2", "Type", "Value", "RemId",
              "Bank/Set", "Dest Mask");
       printf("%-20s %-7s %-4s %-4s %-6s %-6s %-5s %-8s %-8s\n", 
              "--------------------",
              "-------", "----", "----", "------", "------",
              "-----", "--------", "----------");
    }
    else
    {
       printf("%-20s %-7s %-4s %-4s %-6s %-6s %-5s %-8s %-10s\n",
              "MAC", "Mode", "FID1", "FID2", "Type", "Value", "Trig",
              "Bank/Set", "Dest Mask");
       printf("%-20s %-7s %-4s %-4s %-6s %-6s %-5s %-8s %-8s\n", 
              "--------------------",
              "-------", "----", "----", "------", "------",
               "-----", "--------", "----------");
    }
}

            
##@cmethod private void DumpMacAddressEntry(fm_int switchNum, fm_macAddressEntry entryW)
#
# @desc         Handles showing a single MAC address
#
# @param[in]    switchNum The switch number
#
# @param[in]    entryW The MAC table entry
#
sub DumpMacAddressEntry
{
    my ($self, $switchNum, $entryW) = @_;
    my $chip = $self->{CHIP};
    my $vid2 = $entryW->{"vlanID2"};

            
    my $addrType;
    
    TYPE: {
        $entryW->{"type"} eq $FM_ADDRESS_STATIC && do {
                                    $addrType = "Static";
                                    last TYPE;                                           
                                };

        $entryW->{"type"} eq $FM_ADDRESS_DYNAMIC && do {
                                    $addrType = "Dynamic";
                                    last TYPE;                                           
                                };

        $entryW->{"type"} eq $FM_ADDRESS_SECURE_STATIC && do {
                                    $addrType = "SecStat";
                                    last TYPE;                                           
                                };

        $entryW->{"type"} eq $FM_ADDRESS_SECURE_DYNAMIC && do {
                                    $addrType = "SecDyn";
                                    last TYPE;                                           
                                };
    }   # end of TYPE

    my $dest = $entryW->{"destMask"};

    if ($dest == 0xffffffff)
    {
        my $dport = $entryW->{"port"};
        my $ptype = "";
        $chip->fmGetLogicalPortType($switchNum, $dport, \$ptype);

        if($ptype == $FM_PORT_TYPE_PHYSICAL)
        {
            $ptype = "Local";
        }
        elsif ($ptype == $FM_PORT_TYPE_LAG)
        {
            $ptype = "LAG";
        }
        elsif ($ptype == $FM_PORT_TYPE_LBG)
        {
            $ptype = "LBG";
        }
        elsif ($ptype == $FM_PORT_TYPE_MULTICAST)
        {
            $ptype = "MC";
        }
        elsif ($ptype == $FM_PORT_TYPE_SPECIAL)
        {
            $ptype = "Specl";
        }
        elsif ($ptype == $FM_PORT_TYPE_CPU)
        {
            $ptype = "CPU";
        }
        elsif ($ptype == $FM_PORT_TYPE_CPU_MGMT)
        {
            $ptype = "MGMT";
        }
        elsif ($ptype == $FM_PORT_TYPE_REMOTE)
        {
            $ptype = "Remote";
        }
        else
        {
            $ptype = "?";
        }

        my $trig = -1;
        my $bank = -1;
        my $set = -1;

        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($switchNum, $info);
        if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000) ||
             ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000) ||
             ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_REMOTE_FM4000) )
        {

            $chip->fmGetAddressTrigger($switchNum, 
                                       $entryW->{"macAddress"},
                                       $entryW->{"vlanID"},
                                       \$trig);

            $vid2 = "NA";
        }

        if ($info->{'switchFamily'} != $FM_SWITCH_FAMILY_SWAG)
        {
            $chip->fmGetAddressBankSet($switchNum, 
                                       $entryW->{"macAddress"},
                                       $entryW->{"vlanID"},
                                       $entryW->{"vlanID2"},
                                       \$bank,
                                       \$set);
        }

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            my %void = (type => "fm_bool", value => 0);
            $chip->fmGetVlanAttribute($switchNum, 
                                      $entryW->{"vlanID"}, 
                                      $FM_VLAN_FID2_IVL,
                                      \%void);

            if ($void{value} == 0)
            {
                $vid2 = "-";
            }
        }

        if ( $ptype eq "Local" )
        {
            $dport = $self->tpPlatformMapLogicalToFaceplatePort($switchNum, 
                                                                $dport);
        }
        elsif ($ptype eq "LAG")
        {
            my $lagNumber;
            $chip->fmLogicalPortToLAGNumber($switchNum, 
                                            $dport,
                                            \$lagNumber);
            $dport = $lagNumber;
        }
        elsif ($ptype eq "Remote")
        {
            my $rglort;
            $chip->fmGetStackGlort($switchNum, 
                                            $dport,
                                            \$rglort);
            $dport = sprintf("0x%04x", $rglort);
        }

        printf("%-20s %-7s %-4d %-4s %-6s %-6s %-5s %-8s %s\n",
               $entryW->{"macAddress"},
               $addrType,
               $entryW->{"vlanID"},
               $vid2,
               $ptype,
               $dport,
               ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000) ?
                $entryW->{"remoteID"} : (($trig == -1) ? "-" : $trig),
               ($bank == -1) ? "-" :
               ("$set:" . sprintf("0x%x", $bank)),
               "-"
                );
    }
    else
    {
        my $trig = -1;
        my $bank = -1;
        my $set = -1;

        my $info = new SDK::fm_switchInfo();
        $chip->fmGetSwitchInfo($switchNum, $info);
        if ( ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM2000) ||
             ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM4000) ||
             ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_REMOTE_FM4000) )
        {

            $vid2 = "NA";
        }

        if ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000)
        {
            my %void = (type => "fm_bool", value => 0);
            $chip->fmGetVlanAttribute($switchNum, 
                                      $entryW->{"vlanID"}, 
                                      $FM_VLAN_FID2_IVL,
                                      \%void);

            if ($void{value} == 0)
            {
                $vid2 = "-";
            }
        }

        $chip->fmGetAddressTrigger($switchNum, 
                                   $entryW->{"macAddress"},
                                   $entryW->{"vlanID"},
                                   \$trig);

        $chip->fmGetAddressBankSet($switchNum, 
                                   $entryW->{"macAddress"},
                                   $entryW->{"vlanID"},
                                   $entryW->{"vlanID2"},
                                   \$bank,
                                   \$set);

        $dest = $self->stringifyPortMask($switchNum, $dest);

        printf("%-20s %-7s %-4d %-4s %-6s %-6s %-5s %-8s %s\n",
               $entryW->{"macAddress"},
               $addrType,
               $entryW->{"vlanID"},
               $vid2,
               "Local",
               "DMASK",
               ($info->{'switchFamily'} == $FM_SWITCH_FAMILY_FM6000) ?
                $entryW->{"remoteID"} : (($trig == -1) ? "-" : $trig),
               ($bank == -1) ? "-" :
               ("$set:" . sprintf("0x%x", $bank)),
               $dest);
    }

}




##@cmethod public int tpHandleSetTraceAddress(char   *macAddress)
#
# @desc         Handles setting the diagnostic trace MAC address
#
# @param[in]    macAddress The MAC address to be traced
#
# @return       FM_OK if successful
# @return       FM_ERR_INVALID_ARGUMENT if an invalid argument has been
#               supplied
sub tpHandleSetTraceAddress
{
    my ($self, $macAddress) = @_;

    my $chip = $self->{CHIP};

    if (!defined($macAddress))
    {
        print("Must specify <mac address>!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if ($self->validateL2Address($macAddress) != $FM_OK)
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $chip->fmDbgTraceMACAddress($macAddress);
    my $result = $FM_OK;
    return $result;
}



1;
