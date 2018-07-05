# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/RBridgeCore.pm
# Creation Date:    October 2, 2012
# Description:      TestPoint interface to RBridges
#
# INTEL CONFIDENTIAL
# Copyright 2007 - 2012 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::RBridgeCore;
use strict;
use warnings;
use Applications::TestPoint::Common::Messages;

use SDKScalars;
use Math::BigInt;


##@cmethod private int tpHandleShowRbridge(int *tunnelId)
#
# @desc         Displays one or more RBridge.
#
# @param[in]    tunnelId is the virtual tunnel ID to reference this Remote RBridge.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleShowRBridge
{
   my ($self, $tunnelId) = @_;
   my $api = $self->{'CHIP'};
   my $sw=0;
   my $nick = undef;
   my $status;
   my $numTunnelId;
   my $tunId;
   my $rbridge = SDK::fm_remoteRBridge->new();

   my @tunnelIdList = $self->validateList($tunnelId, 
                                          ($FM_FIRST_RBRIDGE_INDEX,
                                           $FM_MAX_REMOTE_RBRIDGE-1));

   if (!defined($tunnelId) || (scalar(@tunnelIdList) == 0))
   {
       print("Must specify a valid tunnel Id or tunnel ID range!\n");
       return $FM_ERR_INVALID_ARGUMENT;
   }

   print "\n";
   print "tunnel Id       NextHopMAC         Egress NickName\n";
   print "----------  ------------------    -----------------\n";

   if ($tunnelId eq "all")
   {
      $api->disableErrors();

      $status = $api->fmGetRBridgeFirst($sw, \$tunId, $rbridge);

      if ($status != $FM_OK)
      {
         if ($status == $FM_ERR_NO_MORE)
         {
            return $FM_OK;
         }

         printf("Invalid tunnelId: %d\n", $tunId);
         return $status;
      }

      printf("  %4d        %-20s     0x%04x\n", 
             $tunId, 
             $rbridge->{"address"},
             $rbridge->{"egressNick"});

      my $currentTunId= $tunId;
      my $nextTunId;

      while (1)
      {
         $status = $api->fmGetRBridgeNext($sw, 
                                          $currentTunId, 
                                          \$nextTunId,
                                          $rbridge);

         if ($status != $FM_OK)
         {
            if ($status == $FM_ERR_NO_MORE)
            {
               return $FM_OK;
            }

            printf("Invalid tunnelId: %d\n", $currentTunId);
            return $status;
         }

         printf("  %4d        %-20s     0x%04x\n", 
                $nextTunId, 
                $rbridge->{"address"},
                $rbridge->{"egressNick"});

         $currentTunId = $nextTunId;
      }

      $api->enableErrors();
   }
   else
   {

      foreach my $tunId (@tunnelIdList)
      {
         $status = $api->fmGetRBridgeEntry($sw, $tunId, $rbridge);

         if ($status != $FM_OK)
         {
            printf("Invalid tunnelId: %d\n", $tunId);
            return $status;
         }

         printf("  %4d        %-20s     0x%04x\n", 
                $tunId, 
                $rbridge->{"address"},
                $rbridge->{"egressNick"});
      }
   }

   return $FM_OK;

}   # end tpHandleShowRBridge



##@cmethod private int tpHandleShowRbridgeDistTree(int *tunnelId)
#
# @desc         Displays one or more RBridge Distribution Trees.
#
# @param[in]    tunnelId is the virtual tunnel ID to reference this 
#               Distribution Tree.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleShowRBridgeDistTree
{
   my ($self, $tunnelId) = @_;
   my $api = $self->{'CHIP'};
   my $sw=0;
   my $status;
   my $numTunnelId;
   my $tunId;
   my $distTree = SDK::fm_distTree->new();

   my @tunnelIdList = $self->validateList($tunnelId, 
                                          ($FM_FIRST_DISTRIBUTION_TREE_INDEX,
                                           $FM_MAX_DISTRIBUTION_TREE-1));

   if (!defined($tunnelId) || (scalar(@tunnelIdList) == 0))
   {
       print("Must specify a valid tunnel Id or tunnel ID range!\n");
       return $FM_ERR_INVALID_ARGUMENT;
   }

   print "\nDistribution Trees\n";
   print "tunnel Id    Egress NickName   Hop Count\n";
   print "----------  -----------------  ----------\n";

   if ($tunnelId eq "all")
   {
      $api->disableErrors();

      $status = $api->fmGetRBridgeDistTreeFirst($sw, \$tunId, $distTree);

      if ($status != $FM_OK)
      {
         if ($status == $FM_ERR_NO_MORE)
         {
            return $FM_OK;
         }

         printf("Invalid tunnelId: %d\n", $tunId);
         return $status;
      }

      printf("  %4d           0x%04x            %02d\n", 
             $tunId, 
             $distTree->{"nick"},
             $distTree->{"hopCnt"});

      my $currentTunId= $tunId;
      my $nextTunId;

      while (1)
      {
         $status = $api->fmGetRBridgeDistTreeNext($sw, 
                                                  $currentTunId, 
                                                  \$nextTunId,
                                                  $distTree);

         if ($status != $FM_OK)
         {
            if ($status == $FM_ERR_NO_MORE)
            {
               return $FM_OK;
            }

            printf("Invalid tunnelId: %d\n", $currentTunId);
            return $status;
         }

         printf("  %4d           0x%04x            %02d\n", 
                $nextTunId, 
                $distTree->{"nick"},
                $distTree->{"hopCnt"});

         $currentTunId = $nextTunId;
      }

      $api->enableErrors();
   }
   else
   {

      foreach my $tunId (@tunnelIdList)
      {
         $status = $api->fmGetRBridgeDistTree($sw, $tunId, $distTree);

         if ($status != $FM_OK)
         {
            printf("Invalid tunnelId: %d\n", $tunId);
            return $status;
         }

         printf("  %4d           0x%04x            %02d\n", 
                $tunId, 
                $distTree->{"nick"},
                $distTree->{"hopCnt"});
      }
   }

   return $FM_OK;

}   # end tpHandleShowRBridgeDistTree


##@cmethod private int tpHandleCreateRBridge(char *macAddress, 
#                                            int  *nickname)
#
# @desc         Creates a remote RBridge node.
#
# @param[in]    mac is the NextHop RBridge MAC used to reach this RBridge.
#
# @param[in]    nickname is the Remote RBridge NickName.
#
# @param[out]   Virtual tunnel ID to reference this Remote RBridge.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleCreateRBridge
{
    my $nargs = $#_;
    my ($self, $macAddress, $nickname) = @_;
    my $api = $self->{'CHIP'};
    

    if ($nargs != 2)
    {
        print "Invalid number of arguments\n";
        return $FM_FAIL;
    }

    if (!defined($macAddress)
        || $self->validateL2Address($macAddress) != $FM_OK)
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $nickname = hex($nickname) if ($nickname =~ /\D/);

    if ($nickname > 0xffff)
    {
        print "Must specify a valid 16-bit value nickname (0-0xffff)!\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $sw=0;
    my ($status);
    my $tunnelId = undef;
    my $rbridge = SDK::fm_remoteRBridge->new();
    $rbridge->{"address"} = $macAddress;
    $rbridge->{"egressNick"} = $nickname;


    $status = $api->fmCreateRBridge($sw, $rbridge, \$tunnelId);

    if ($status != $FM_OK)
    {
       print "RBridge could not be created!\n";
       return $status;
    }

   printf("RBridge created : tunnelID = %d\n", $tunnelId);
   return $FM_OK;

}   # end tpHandleCreateRBridge


##@cmethod private int tpHandleDeleteRBridge(int *tunnelId)
#
# @desc         Deletes a RBridge node.
#
# @param[in]    tunnelId is the virtual tunnel ID to reference this Remote RBridge.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleDeleteRBridge
{
   my ($self, $tunnelId) = @_;
   my $api = $self->{'CHIP'};


   if (!defined($tunnelId) || ($tunnelId > $FM_MAX_REMOTE_RBRIDGE))
   {
       print "Must specify a valid tunnel ID value!\n";
       return $FM_ERR_INVALID_ARGUMENT;
   }

   my $sw=0;
   my ($status);

   $status = $api->fmDeleteRBridge($sw, $tunnelId);

   if ($status != $FM_OK)
   {
      printf("RBridge tunnelId %d could not be deleted\n", $tunnelId);
      return $status;
   }

  printf("RBridge (tunnelID %d) deleted\n", $tunnelId);
  return $FM_OK;

}   # end tpHandleDeleteRBridge


##@cmethod private int tpHandleUpdateRBridge(int *tunnelId
#                                            char *macAddress, 
#                                            int  *nickname)
#
# @desc         Updates a remote RBridge node.
#
# @param[in]    tunnelId is the virtual tunnel ID to reference this 
#               RBridge.
#
# @param[in]    mac is the NextHop RBridge MAC used to reach this RBridge.
#
# @param[in]    nickname is the Remote RBridge NickName.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleUpdateRBridge
{
    my $nargs = $#_;
    my ($self, $tunnelId, $macAddress, $nickname) = @_;
    my $api = $self->{'CHIP'};
 
    if ($nargs != 3)
    {
        print "Invalid number of arguments\n";
        return $FM_FAIL;
    }

    my @tunnelIdList = $self->validateList($tunnelId, 
                                           ($FM_FIRST_RBRIDGE_INDEX,
                                            $FM_MAX_REMOTE_RBRIDGE-1));

    if (!defined($tunnelId) || (scalar(@tunnelIdList) == 0))
    {
        print("Must specify a valid tunnel Id!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($macAddress)
        || $self->validateL2Address($macAddress) != $FM_OK)
    {
        print($TP_MSG_ERR_MAC_INVALID_ADDRESS);
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $nickname = hex($nickname) if ($nickname =~ /\D/);

    if ($nickname > 0xffff)
    {
        print "Must specify a valid 16-bit value nickname (0-0xffff)!\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    my $sw=0;
    my ($status);
    my $rbridge = SDK::fm_remoteRBridge->new();
    $rbridge->{"address"} = $macAddress;
    $rbridge->{"egressNick"} = $nickname;


    $status = $api->fmUpdateRBridgeEntry($sw, $tunnelId, $rbridge);

    if ($status != $FM_OK)
    {
       print "RBridge could not be updated!\n";
       return $status;
    }

   printf("RBridge updated\n");
   return $FM_OK;

}   # end tpHandleUpdateRBridge


##@cmethod private int tpHandleCreateRBridgeDistTree(int *nickname)
#
# @desc         Creates a RBridge Distribution Tree.
#
# @param[in]    nickname is the 16-bit nickname associated to this distribution tree.
#
# @param[out]   Virtual tunnel ID to reference this Distribution Tree.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleCreateRBridgeDistTree
{
    my ($self, $nickname, $hopCnt) = @_;
    my $api = $self->{'CHIP'};

    $nickname = hex($nickname) if ($nickname =~ /\D/);
    
    if (!defined($nickname) || ($nickname > 0xffff))
    {
        print "Must specify a valid 16-bit value nickname (0-0xffff)!\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($hopCnt))
    {
       $hopCnt = 0x3f;
    }

    $hopCnt = hex($hopCnt) if ($hopCnt =~ /\D/);
    if ($hopCnt > 0x3f)
    {
       print "Must specify a valid Hop Count value (0-0x3f)!\n";
       return $FM_ERR_INVALID_ARGUMENT;
    }

    my $sw=0;
    my ($status);
    my $tunnelId = undef;
    my $distTree = SDK::fm_distTree->new();
    $distTree->{"nick"} = $nickname;
    $distTree->{"hopCnt"} = $hopCnt;

    $status = $api->fmCreateRBridgeDistTree($sw, $distTree, \$tunnelId);

    if ($status != $FM_OK)
    {
       print "Distribution Tree could not be created!\n";
       return $status;
    }

   printf("Distribution Tree created : tunnelID = %d\n", $tunnelId);
   return $FM_OK;

}   # end tpHandleCreateRBridgeDistTree


##@cmethod private int tpHandleDeleteRBridgeDistTree(int *tunnelId)
#
# @desc         Deletes a RBridge node.
#
# @param[in]    tunnelId is the virtual tunnel ID to reference this 
#               Distribution Tree.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleDeleteRBridgeDistTree
{
   my ($self, $tunnelId) = @_;
   my $api = $self->{'CHIP'};


   if (!defined($tunnelId) || ($tunnelId > $FM_MAX_DISTRIBUTION_TREE))
   {
       print "Must specify a valid tunnel ID value!\n";
       return $FM_ERR_INVALID_ARGUMENT;
   }

   my $sw=0;
   my ($status);

   $status = $api->fmDeleteRBridgeDistTree($sw, $tunnelId);

   if ($status != $FM_OK)
   {
      printf("Distribution Tree (tunnelId %d) could not be deleted\n", $tunnelId);
      return $status;
   }

  printf("Distribution Tree (tunnelID %d) deleted\n", $tunnelId);
  return $FM_OK;

}   # end tpHandleDeleteRBridgeDistTree



##@cmethod private int tpHandleUpdateRBridgeDistTree(int tunnelId,
#                                                    int *nickname,
#                                                    int hopCnt)
#
# @desc         Updates a RBridge Distribution Tree.
#
# @param[in]    tunnelId is the virtual tunnel ID to reference this 
#               Distribution Tree.
#
# @param[in]    nickname is the 16-bit nickname associated to this distribution tree.
#
# @return       FM_OK if successful
# @return       FM_FAIL otherwise
# 
sub tpHandleUpdateRBridgeDistTree
{
    my ($self, $tunnelId, $nickname, $hopCnt) = @_;
    my $api = $self->{'CHIP'};

    my @tunnelIdList = $self->validateList($tunnelId, 
                                           ($FM_FIRST_DISTRIBUTION_TREE_INDEX,
                                            $FM_MAX_DISTRIBUTION_TREE-1));

    if (!defined($tunnelId) || (scalar(@tunnelIdList) == 0))
    {
        print("Must specify a valid tunnel Id!\n");
        return $FM_ERR_INVALID_ARGUMENT;
    }

    $nickname = hex($nickname) if ($nickname =~ /\D/);
    
    if (!defined($nickname) || ($nickname > 0xffff))
    {
        print "Must specify a valid 16-bit value nickname (0-0xffff)!\n";
        return $FM_ERR_INVALID_ARGUMENT;
    }

    if (!defined($hopCnt))
    {
       $hopCnt = 0x3f;
    }

    $hopCnt = hex($hopCnt) if ($hopCnt =~ /\D/);
    if ($hopCnt > 0x3f)
    {
       print "Must specify a valid Hop Count value (0-0x3f)!\n";
       return $FM_ERR_INVALID_ARGUMENT;
    }

    my $sw=0;
    my ($status);
    my $distTree = SDK::fm_distTree->new();
    $distTree->{"nick"} = $nickname;
    $distTree->{"hopCnt"} = $hopCnt;

    $status = $api->fmUpdateRBridgeDistTree($sw, $tunnelId, $distTree);

    if ($status != $FM_OK)
    {
       print "Distribution Tree could not be updated!\n";
       return $status;
    }

   printf("Distribution Tree updated\n", $tunnelId);
   return $FM_OK;

}   # end tpHandleUpdateRBridgeDistTree


1;
