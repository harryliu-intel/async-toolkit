# vim:et:sw=4:ts=4:tw=79
# (No tabs, indent level is 4, text width is 79)

###############################################################################
# File:             Applications/TestPoint/Common/FunctionCore.pm
# Creation Date:    11/10/10
# Description:      Tespoint update support functions.
#
# INTEL CONFIDENTIAL
# Copyright 2010 - 2011 Intel Corporation. All Rights Reserved. 
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

package Applications::TestPoint::Common::UpdateCore;
use strict;
use warnings;

use base 'Exporter';
our @EXPORT = ('tpUpdateCommandSupport');

use SDKScalars;
use Applications::TestPoint::Common::Messages;

my $startupScriptIsOK;
my $startupScriptFile = "/usr/fulcrum_startup/startup_cf";
my $platform = $ENV{"PERL_PLATFORM"};


###############################################################################
#
#                               LOCAL FUNCTIONS
#
###############################################################################

##@cmethod private int ValidateScriptVersion()
#
# @desc         Check that startup_cf script version is 2.0 or higher.
#
# @param[in]    quiet: do not print messagess to the screen.
# 
# @return       FM_OK if the script is available and its version is 2.0 or
#                   higher.
# @return       FM_FAIL otherwise
sub ValidateScriptVersion
{
    my ($quiet) = @_;

    if (!defined $quiet)
    {
        $quiet = 0;
    }
    # validate the script version only once: save the result in $startupScriptIsOK
    # and use the saved value for the subsequent calls to this function.
    if (!defined($startupScriptIsOK))
    {
        # assume a bad script version
        $startupScriptIsOK = $FM_FAIL;

        # check that /usr/fulcrum_scripts/startup_cf is reachable.
        if (-e $startupScriptFile)
        {
            # open the file and check its version.
            # the mayor version num must be greater than '2'
            open(FH, "<", $startupScriptFile);
            while (<FH>)
            {
                # the expected string is: "# Version: x.y.z\n"
                if (/Version:/)
                {
                    my @tokens = split(/ /, $_);
                    my @versionNum = split(/\./,$tokens[2]);
                    # check mayor version number.
                    if ($versionNum[0] ne "" && $versionNum[0] >= 2)
                    {
                        $startupScriptIsOK = $FM_OK;
                    }
                    last;
                }
                elsif (chomp $_ eq "")
                {
                    # stop at the firts empty line.
                    last;
                }
            }
            close FH;
        }
        elsif (!$quiet)
        {
            # "/usr/fulcrum_startup/startup_cf" is not available.
            # This may happen when:
            #  -1. TestPoint is running using NFS.
            #  -2. There is an error with startup_cf and TestPoint was
            #      manually  launched.
            # print an error message and set $startupScriptIsOK to false.
            print($TP_MSG_ERR_UPDATE_SCRIPT_NOT_FOUND);
        }
    }
    # print an error message
    if ($startupScriptIsOK != $FM_OK && !$quiet)
    {
        print($TP_MSG_ERR_UPDATE_SCRIPT_INVALID_VERSION);
    }
    return $startupScriptIsOK;
}

##@cmethod private int ExecuteStartupCommand(char *commandLineParameters)
#
# @desc         Check that startup_cf script version is OK and execute
#               the given command.
#
# @param[in]    commandLineParameters :The parameters to pass to startup_cf
#
# @return       FM_OK if the script is OK and the execution was successfull.
# @return       FM_FAIL otherwise
sub ExecuteStartupCommand
{
    my ($parameters) = @_;
    my $err = $FM_FAIL;

    # argument validation
    if (defined($parameters))
    {
        # validate startup_cf version.
        if (&ValidateScriptVersion() == $FM_OK)
        {
            my $platformSpecification = "";

            # include platform specification.
            if (defined $platform && $platform ne "")
            {
                $platformSpecification = "-p $platform";
            }

            # call the script passing the given parameters
            if (system ("$startupScriptFile $platformSpecification $parameters") == 0)
            {
                $err = $FM_OK;
            }
        }
        print "\n";
    }
    return $err;
}

##@cmethod private void ShowRestartTpMessage()
#
# @desc         Show a message telling the user to restart TestPoint.
#               This is typically used after a successfull TestPoint image
#               installation
#
# @param[in]    void
#
# @return       void
sub ShowRestartTpMessage
{
    print "\nYou have to restart TestPoint to execute the new image\n";
}

###############################################################################
#
#                               PUBLIC FUNCTIONS
#
###############################################################################

##@cmethod public int tpUpdateCommandSupport(void)
#
# @desc         Check if the update command is supported.
#               "update" is supported when: 
#               1) the system is running from the CF.
#               2) the file startup_cf exists
#               3) the version of startup_cf is 2.0.0 or higher.
#
# @return       FM_OK if the update command is supported.
# @return       FM_FAIL otherwise
sub tpUpdateCommandSupport
{
    # call VeliadateScriptVersion with the 'quiet' flag set to true.
    return &ValidateScriptVersion(1);
}

##@cmethod public int tpHandleUpdateShowRepository(void)
#
# @desc         Show tarball repository content.
#               This feature is available only if startup_cf 2.0 or higher
#               is available.
#
# @param[in]    void
#
# @return       FM_OK script is OK and the execution was successfull.
# @return       FM_FAIL otherwise
sub tpHandleUpdateShowRepository
{
    # show repository content
    return &ExecuteStartupCommand("-t");
}

##@cmethod public int tpHandleUpdateShowImageInformation(void)
#
# @desc         Show information about the current TestPoint image.
#               This includes the platform, version number, date and the
#               name of the tarball used for the installation.
#
# @param[in]    void
#
# @return       FM_OK script is OK and the execution was successfull.
# @return       FM_FAIL otherwise
sub tpHandleUpdateShowImageInformation
{
    my $err = $FM_FAIL;

    if (-e "/fulcrum/TestPoint.info")
    {
        print "\n";
        open(FH, "<", "/fulcrum/TestPoint.info");
        while (<FH>)
        {
            print "$_";
        }
        print "\n";
        close FH;
        $err = $FM_OK;
    }
    else
    {
        print "\nTestPoint Image Information File not found\n";
    }
    return $err;
}

##@cmethod public int tpHandleUpdateInstallTpImage(char* tarballName)
#
# @desc         Install a new TestPoint image using the given tarball.
#               The tarball to use for the installation is taken from
#               the repository.
#               To execute the new TP image, the user must restart TP.
#               This feature is available only if startup_cf 2.0 or higher
#               is available.
#
# @param[in]    tarballName: The tarball to use for the installation.
# 
# @return       FM_OK script is OK and the execution was successfull.
# @return       FM_FAIL otherwise
sub tpHandleUpdateInstallTpImage
{
    my ($self, $tarballName) = @_;
    my $err = $FM_ERR_INVALID_ARGUMENT;

    # argument validation
    if (!defined($tarballName) || $tarballName eq "")
    {
        print($TP_MSG_ERR_UPDATE_INVALID_TARBALL_NAME);
    }
    else
    {
        my $option = "-i";

        # check if the tarball was specified using the reference number
        if ($tarballName =~ /^\d+$/)
        {
            # install image using the tarball reference number
            $option = "-I";
        }

        # install image
        $err = &ExecuteStartupCommand("$option $tarballName");

        if (!$err)
        {
            # If the installation was successfully completed, ask the user
            # to restart TestPoint
            &ShowRestartTpMessage();
        }        
    }
    return $err;
}

##@cmethod public int tpHandleUpdateRemoveTarball(int tarballNum)
#
# @desc         Remove the given tarball from the repository. The tarball is
#               specified by its reference number, which is given by from the
#               "show repository" command.
#               After a tarball was removed from the repository, the command
#               "show repository" is automatically called in order to show the
#               updated reference numbers.
#               This feature is available only if startup_cf 2.0 or higher
#               is available.
#
# @param[in]    tarballNum: The reference number of the tarball to remove. This
#                           number comes from the "Show repository" command
#
# @return       FM_OK script is OK and the execution was successfull.
# @return       FM_FAIL otherwise
sub tpHandleUpdateRemoveTarball
{
    my ($self, $tarballNum) = @_;
    my $err = $FM_ERR_INVALID_ARGUMENT;

    if (!defined($tarballNum) || $tarballNum !~ /^\d+$/) 
    {
        print ($TP_MSG_ERR_UPDATE_INVALID_TARBALL_NUMBER);
    }
    else
    {
        $err = &ExecuteStartupCommand("-r $tarballNum");
    }
    return $err;
}

##@cmethod public int tpHandleUpdateTftpDownload(char* tarballName,
#                                                char* serverIPaddr)
#
# @desc         Download the specified tarball from the given server using TFTP.
#               This feature is available only if startup_cf 2.0 or higher
#               is available.
#
# @param[in]    tarballName: The tarball to download.
# 
# @param[in]    serverIpAddr: The IP address of the TFTP server.
#
# @return       FM_OK script is OK and the execution was successfull.
# @return       FM_FAIL otherwise
sub tpHandleUpdateTftpDownload
{
    my ($self, $tarballName, $serverIpAddr) = @_;
    my $err = $FM_ERR_INVALID_ARGUMENT;

    if (!defined($tarballName) || $tarballName eq "")
    {
        print($TP_MSG_ERR_UPDATE_INVALID_TARBALL_NAME);
    }
    elsif (!defined($serverIpAddr) || $serverIpAddr eq "")
    {
        print($TP_MSG_ERR_UPDATE_INVALID_IP_ADDR);
    }
    else
    {
        $err = &ExecuteStartupCommand("-d $tarballName -s $serverIpAddr");
    }
    return $err;
}

1;
