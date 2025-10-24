
setenv GTR_ROOT /nfs/sc/disks/tfc_be_01/pdonehue/git_repos/applications.design-automation.memory.lamb/gtr
setenv GTR_TARGET /nfs/sc/disks/tfc_be_01/$USER/gtr

alias setup_virtuoso 'echo Make sure to do this from nbatch xterm; echo /p/hdk/bin/cth_psetup -p tfc -ward \$PWD -cfg tfc_ipde_n5.cth -tool ipde_all -quiet;echo \$SETUP_IPDE; echo setenv NETLISTER_SKIP_SRC 1; echo setenv NB_WASH_GROUPS "n5,tfc,user,bxd_cb,bxd_jbay,soc,n7,n7fe"; echo setenv MGC_REALTIME_HOME /p/hdk/cad/calibre/aoi_cal_2021.1_16.10;echo setenv GDMNOTLOADLIB 1'
alias setup_r2g 'echo ch2_xterm; echo /p/hdk/bin/cth_psetup -p tfc -ward \$PWD -cfg tfc_n3.cth -quiet ;echo \$SETUP_IPDE;'

alias setup_vmacold 'echo ch2_xterm; echo /p/hdk/bin/cth_psetup -p tfc -ward \$PWD -tool ipde_all -quiet;echo \$SETUP_IPDE; echo source /p/tfc/tools/barefoot/etc/barefoot.cshrc; echo module load calibre/2021.1_16.10; echo setenv NETLISTER_SKIP_SRC 1; echo setenv NB_WASH_GROUPS "n5,tfc,user,bxd_cb,bxd_jbay,soc,n7,n7fe"; echo setenv MGC_REALTIME_HOME /p/hdk/cad/calibre/aoi_cal_2021.1_16.10'
alias setup_liberate 'echo module load liberate/21.23.02'
alias setup_sis 'echo source /p/tfc/tools/barefoot/etc/barefoot.cshrc; echo module load siliconsmart/S-2021.06-SP1; echo setenv NB_WASH_GROUPS "n5,tfc,user,bxd_cb,bxd_jbay,soc,n7,n7fe"'
alias setup_swiss 'echo https://myhpc.swiss.intel.com'
alias nb_paul 'echo nbstatus jobs --target sc_normal --history 10m "user=~pdonehue"'



# EC Standard Login Environment -*- sh -*-
# user customized .cshrc
# $Source: /usr/cvs/cvsrep/ec_environ-1.0/release/user/cshrc.user,v $
# $Revision: 1.34 $

# use this logfile for debugging
echo "I: (.cshrc.$USER) [entering] `/bin/date`" >> $ec_env_error_log

#
# this file executed for every new shell/window
#
# ALL USER CHANGES GO IN THIS FILE:
#  append path components e.g. $HOME/bin
#  define custom aliases
#  set preferred printer
#  etc...
#

######################################################
# The following special features are not stored in the
# environment, but are activated by a text search in
# this file.
#
# 1) If your shell defined by the system is /bin/csh but
#    you want to force /usr/intel/bin/tcsh or /bin/tcsh
#    (when available), set the following value to 1.
#    Do not uncomment it; just edit the number:
#
#    leave this line commented # ECLOGIN_FORCE_TCSH=0
#
# 2) Automatic upgrades to .cshrc, .login, .profile may
#    happen at any time - one reason you should never edit
#    these files.  These updates are normally silent.
#    If you want to be notified when a new file is installed
#    automatically for you, set the following value to 1.
#
#    leave this line commented # ECLOGIN_UPGRADE_NOTICES=0
#
######################################################
#
# warning: do not place commands here if standard
#          output is produced; use the section for
#          LOGIN SHELLS guarded by a test for $prompt
#
######################################################

#
# System defaults have been set; commented
# examples show how you may wish to modify.
#

###
###-------->SHELL<--------
###

###-------->umask<--------
# The umask tells what permissions to
# REMOVE from newly created files.
# Information security requires the
# default to prevent world read/write.
# If you know none of your data is
# more/less confidential, you may
# want to change.
#
# umask 077	# prevent all group/world access
# umask 027	# default
# umask 022	# permit world read/search


###
###-------->ENVIRONMENT<--------
###


###-------->search path<--------

# append your personal bin - you may want to prepend
#  or place relative to other componts with -a/-b
#
modpath -q -f $HOME/bin
setenv FC_ROOT /p/hdk/cad/fusioncompiler/P-2019.03-SP5-T-20200901
modpath -q -f $FC_ROOT/bin

setenv KLAYOUT_BIN /nfs/sc/disks/tfc_be_01/pdonehue/tools/bin/klayout
modpath -q -f $KLAYOUT_BIN

# force "." to be last always
# (in case site/project has inserted before user's paths)
#
#modpath -q -f -d .
#modpath -q -f .

# OPTIONAL: eclogin by default will append "." later in
# the global execution flow.  Uncomment the next line to
# to prevent default addition of "."  Beware that automation
# scripts may depend on this, use with caution.
#
#set EC_NO_ADD_DOTPATH=1

###-------->printer<--------
# default printer; consult your site support
# for available printer names/locations
#
# setenv PRINTER my_printer_name
#setenv PRINTER "(not migrated)"

# LPDEST is normally same as printer
if ($?PRINTER) then
  setenv LPDEST  "$PRINTER"
endif

###-------->editor<--------
# default editor is probably /bin/vi (different on each OS)
#
# setenv EDITOR /usr/intel/bin/vim
# setenv EDITOR /usr/intel/bin/emacs
setenv EDITOR "/bin/vi" # (migrated during eclogin install)

###-------->X display<--------
# Display forwarding is automatic when you
# use ssh to access remote hosts.  If you
# never use multiple displays or always
# want to force a specific display, it may
# be set here.
#
# setenv DISPLAY mydesktop:0


###-------->symlink following behavior using $symlinks<--------
#
# This description is from the tcsh(1) manual:
#
#          symlinks (+)
#              Can be set to several different values to  control
#              symbolic link (`symlink') resolution:
#
#              If  set to `chase', whenever the current directory
#              changes to a directory containing a symbolic link,
#              it  is  expanded to the real name of the directory
#              to which the link points. This does not  work  for
#              the user's home directory; this is a bug.
#
#              If set to `ignore', the shell tries to construct a
#              current directory relative to the  current  direc-
#              tory before the link was crossed.  This means that
#              cd'ing through a symbolic link and then `cd ..'ing
#              returns  one  to the original directory. This only
#              affects builtin commands and filename  completion.
#
#              If  set  to  `expand', the shell tries to fix sym-
#              bolic links by actually expanding arguments  which
#              look  like  path  names. This affects any command,
#              not just builtins. Unfortunately,  this  does  not
#              work  for  hard-to-recognize  filenames,  such  as
#              those embedded in command options.  Expansion  may
#              be  prevented  by  quoting.  While this setting is
#              usually the most convenient, it is sometimes  mis-
#              leading  and  sometimes confusing when it fails to
#              recognize an argument which should be expanded.  A
#              compromise  is  to use `ignore' and use the editor
#              command normalize-path (bound by default to  ^X-n)
#              when necessary.

# ECLogin has default 'old' style behavior for historical reasons.
# If you like the modern behavior to show the path according to
# how you arrived there - and the ability to cd in reverse back
# to the origin through the link, then change this to 'ignore'.
#
set symlinks = chase

#
# Netbatch (optional)
#
# The following may have been automatically migrated
# at the time you installed eclogin
#
#setenv NBPOOL "(not migrated)"
#setenv NBQSLOT "(not migrated)"
#setenv NBCLASS "(not migrated)"


###
###-------->LOGIN SHELLS<--------
###
if ($?prompt) then
    ###
    ### This section runs for interactive shells only.
    ###

    ### example: you may wish to force the terminal
    # settings e.g. backspace.
    #
    # stty erase '^H' # normal
    # stty erase '^?' # Sun keyboard
    # !!! NOTE: before changing stty erase, also make sure you have the
    # !!!       eclogin version of .Xdefaults which works together with
    # !!!       the stty setting.


    ### default settings you may wish to change:
    #
    #set history=200	# command history remembered by the shell
    #set savehist=200	# command history saved after shell exits
    #set time=1		# show run-time stats of long-running processes
    #unset filec	# tcsh filename completion  (default on)
    #unset notify	# [t]csh bg job state change notices (default on)
    #
    #set autologout=480	# automatically clean up unused terminals (default off)
                        # WARNING: this will kill idle shell/windows after
                        # 480 minutes (8 hours), few users will want this.

    ### advanced tcsh options; see tcsh(1) and uncomment the ones you like
    if ($?tcsh) then

	# key bindings may be placed in a separate file
	if (-e $HOME/.bindings) source $HOME/.bindings

#	alias ls ls-F		# faster builtin ls
#	set watch=(1 any any)	# watch users logging in / out
#	set prompt="(%?)%B%S%M%s[%d.%T]%b%h%# "	# informational prompt
#	if ($?edit) set rprompt="%S#%s%c02"	# add cwd when it fits

#	# these next lines dynamically update the xterm title see xtset -h
#	setenv XTXTRA "since "`date "+%m/%d@%H:%M"`
#	alias res_t 'xtset -t %h:%d %e'	# reset titlebar
#	res_t			# reset title right now
#	# this is most efficient but loses when you exit a remote session
#	alias cwdcmd res_t
#	# this is less efficient because it runs too often, but it never fails
#	#alias precmd res_t

        #set   autocorrect  # tcsh command line spelling check/autocorrect
        #set   correct=cmd

    endif

    ### validation is enabled by default to help you detect errors.
    ### if you really want to suppress warnings/errors choose
    ### one of these:
    #
    # (default - do nothing)		   # show all msgs
    setenv EC_DISABLE_VAL project        # turn off project msgs
    setenv EC_DISABLE_VAL system         # turn off sysgem msgs
    setenv EC_DISABLE_VAL project,system # turn off all msgs
    #
    # if you disable these it is your responsibility to monitor
    # the log, normally in /tmp/eclogin-errors.<username>

endif

###
###-------->ALIASES<--------
###
# some sample aliases you may want to uncomment or supplement:
#
#alias cls     clear
#alias ll      ls -l
#alias lf      ls -F
#alias lr      ls -R
#alias h       history

#
# aliases may be kept in separate file if you wish
#
if ( -r $HOME/.aliases ) then
  source $HOME/.aliases
endif

echo "I: (.cshrc.$USER) [leaving] `/bin/date`" >> $ec_env_error_log
######################################################
#
# Caution: do not place commands here if standard
#          output is produced; use the section above
#          where LOGIN SHELLS are guarded by a test
#          for $prompt.
#
#          Also do not set the $prompt variable or
#          invoke aliases modifying $prompt except
#          within the section for LOGIN SHELLS above.
#
######################################################

# Setup ARC Environment
#if ( -e /p/psg/ctools/arc/current/arc.cshrc ) then
#source /p/psg/ctools/arc/current/arc.cshrc
#endif

#--------------------------------------------------------
# User customizations go below this line
#--------------------------------------------------------
# xwashmgr to manage groups
alias ls ls -F
alias arc_arc 'arc shell arc_wash/project/gdr'
alias arc_gdr 'arc shell project/gdr/gdr/1.0/phys/2020WW22,altera_tp/19.2.1.p17,altera_tdb/i10/3.7,icf_rapidesd/i10/20.1.2_patch,dmx/13.1,intel_clkrx_ip/P1275_81/2020WW24,dfx_tcb_sbox_scan/P1275_81/2019WW50s,icf_fill/P1275_81/qfill_2020WW10,intel_ux_ip/P1275_81/2020WW25,p4,my_cshrc'
alias arc_nd 'arc shell project/nadder/nd5/5.0/phys my_cshrc'

alias arc_nrr 'arc shell project/falcon/fm6revb/4.0/phys,dev/cheklee/altera_nrr/i10/19WW46q,altera_dmz_tech_fm/1.93,p4,my_cshrc'

alias arc_vlc 'arc shell project/vlc/vlcx/0.0/phys/2019WW51,altera_dmz_tech_fm/1.93,p4,my_cshrc'
alias arc_slr 'arc shell project/slr/branch/slr81main/0.0/phys,altera_dmz_tech_fm/1.93,p4,my_cshrc'

alias arc_nrr_t 'arc shell project/falcon/fm6revb/4.0/phys,dev/cheklee/altera_nrr/i10/19WW46q,altera_dmz_tech_fm/1.93,p4,synopsys_icc2/O-2018.06-SP5-T-20190917,my_cshrc'


alias arc_nrrrel 'arc shell project/falcon/fm6revb/4.0/phys,altera_dmz_tech_fm/1.79,p4,my_cshrc'
alias arc_r2 'arc shell project/falcon/fm6dot2/4.0/phys/2018WW41,icf_memory_ip/i10/2018WW41,altera_dmz_tech_fm/1.48,p4,my_cshrc'


alias arc_lsm 'arc shell project/falcon/fm6dot2/4.0/phys,icf_sip_idv/i10/2018WW45,p4,my_cshrc'
alias arc_lsm 'arc shell project/falcon/fm6dot2/4.0/phys/2019WW02,altera_icf_erc/i10/2019WW01,icf_sip_idv/i10/2018WW45f'

alias pgcp 'cp -auvx ppgrsync01.png.intel.com:'
alias p4 'env P4CONFIG=.p4config p4'

alias p4icm setenv P4CONFIG .icmconfig
alias p4nrr setenv P4CONFIG .p4config
alias emacs '/usr/bin/emacs -nw'
alias xterm 'xterm -bg black -fg yellow -sb'
alias cln '\rm -f *~ #*'
## need to launch in virtuoso directory
alias icfb 'arc submit -i "node/[memory>=12G]" lsfq=lxvirtuoso  virtuoso'

# protect file clobbering
alias rm 'rm -i'
alias mv 'mv -i'
alias cp 'cp -i'

#alias cd_nd 'cd /ice_ip/core/nadder/pdonehue'
#alias cd_rctold 'cd /nfs/site/disks/fm6_core_5/users/pdonehue/rct'
#alias cd_rct 'cd /nfs/site/disks/fm6_rctrel_1/users/rct'
#alias cd_rctarc 'cd /nfs/site/disks/fm6_rctarchive_1/users/pdonehue'
#alias cd_nrr 'cd /nfs/sc/disks/fm6_core_pdonehue/icmws/fmx_nrr_gold/cw_lib'

#alias cd_jbay 'cd /nfs/sc/disks/bfn_jbay_02/pdonehue'
alias cd_cb 'cd /nfs/sc/disks/bfn_pd_cb_07/pdonehue/pdonehue.cb-main'
alias cd_cb2 'cd /nfs/sc/disks/bfn_pd_cb_lv_01/pdonehue/pdonehue.cb-main'
alias cd_tfc 'cd /nfs/sc/disks/tfc_be_01/pdonehue'
alias cd_gtr 'cd /nfs/sc/disks/tfc_be_01/pdonehue/git_repos/applications.design-automation.memory.lamb/gtr'
alias cd_lib 'cd /nfs/sc/disks/tfc_be_01/pdonehue/lib_prep'
alias cd_lamb 'cd /nfs/sc/disks/tfc_be_01/pdonehue/n5_libs/silicon_smart'
alias cd_sis 'cd /nfs/sc/disks/tfc_be_01/pdonehue/n5_libs/silicon_smart/LDPQ_latch_char_setup/REGULAR/CHAR'
#alias cd_old_pylon 'cd /nfs/sc/disks/bfn_jbay_02/pdonehue/chipdev/hlp-work2'
#alias ch2_pesg_shell '/p/hdk/bin/cth_psetup -p pesg/2020.06SP -cfg n5p_h210_m15.cth'
#alias ch2_ward '/p/hdk/bin/cth_psetup -p tfc -ward $PWD'
#alias wash_pylon 'wash -n hlp grr76 bxd_jbay soc'

#alias cd_ivi 'cd /nfs/sc/disks/bfn_pd_cb_07/pdonehue/pdonehue.cb-main/pd/tm_cdm_data_word0'
#alias cd_arch 'cd /nfs/site/disks/fm6_rctarchive_1/users/pdonehue/icmws/fmx_nrr2/cw_lib/r2g2'
#alias cd_wrx 'cd /nfs/site/disks/km_core_pdonehue/icmws/km_dev/cw_lib/r2g2_dev_2020WW21'
#alias cd_gdr 'cd /nfs/site/disks/gdr_barakquad_2/users/pdonehue/icmws/gdr_main/r2g2'

#alias cd_ws 'cd /nfs/site/disks/fm6_core_2/users/pdonehue/icmws'
#alias tiny 'arc submit -i lsfq=ice_arc_large mem=24000 -- xterm'
#alias klayout '/nfs/site/disks/fm6_core_2/users/pdonehue/bin/klayout -e'

## two tabs 
set autoexpand

#setenv P4CLIENT pdonehue
#setenv P4PORT sj-perforce:1666
#setenv P4CONFIG .p4config
setenv P4DIFF tkdiff
setenv P4EDITOR '/usr/bin/emacs -nw'

#setenv P4MERGE /tools/perforce/2011.1/linux64/p4merge
#setenv P4PORT_PROXY sj-perforce-proxy:1998
#setenv P4PORT_MASTER sj-perforce:1666
#setenv P4PORT_PROXY_AUTO sj-perforce-proxy2:1998
#setenv P4TICKETS /home/pdonehue/.p4tickets

## r2g2
## r2g2 -init -dev
## for editing r2g2
## setenv P4CONFIG .p4config
## rtm_shell -gui
## rsync -av pdonehue@sj-rsync01: 
## samba.sc.intel.com for mounting home dir


# Prompt is set to user_name:hostname[history_event] time: tail_of_cwd %
set prompt = "%n:%m[%h] %U%@%u: %c %# "
set correct = command
set listmax = 10000000
set listmaxrows = 1000000
#alias openfix /p/psg/flows/common/altera_extraction_utilities/STI_2018WW29/bin/openfix.py
setenv NBCONF /usr/local/lib/netbatch
setenv NBPOOL sc_express

#alias nb_xterm 'setenv NBCONF /usr/local/lib/netbatch; setenv NBPOOL sc_express ; nbjob run --qslot /bfn/tofinocreek --mail "E 10" --class "SLES11SP4&&128G" xterm'
#alias nb_xterm 'setenv NBCONF /usr/local/lib/netbatch; setenv NBPOOL sc_express ; nbjob run --qslot /bfn/be --target sc_express --class "SLES12&&64G&&8C" xterm'
#alias ch2_xterm 'nbjob run --qslot /bfn/tofinocreek --mail "E 10" --class "SLES11SP4&&128G" xterm'
#alias ch2_xterm 'setenv NBCONF /usr/local/lib/netbatch; setenv NBPOOL sc_express ; nbjob run --qslot /bfn/be --target sc_express --class "SLES12&&64G" xterm'
#alias ch2_fusion '/p/hdk/cad/fusioncompiler/Q-2019.12-SP5/bin/fc_shell'
#alias ch2_pesg_setup '/p/hdk/bin/cth_psetup -p pesg/2020.09SP -cfg n5_h210_m15.cth'
#alias chward 'source $SETUP_R2G /p/hdk/cad/eou_utils/20.04.005/bin/setup_r2g -w  -tech h210_snps_m15 -t cth2020.09.SP1_n5'
#alias chward '$SETUP_R2G -w \!:1 -tech h210_snps_m15 -t cth2020.09.SP1_n5'
#alias ch2_ward '$SETUP_R2G -w $PWD -tech h210_snps_m15 -t cth2020.09.SP1_n5'
#alias ch2_setup '/p/hdk/bin/cth_psetup -p tfc -w $PWD'

#alias ch2_load_route_opt 'Ifc_shell -B \!:1 -I -T "import_ndm_design" -S outputs/route_opt/${block}.ndm -output_log_file logs/fc.import_design.\!:1.ropt.log'
#alias ch2_load_fplan 'Ifc_shell -B $block -I -T "import_ndm_design" -S outputs/floorplan/${block}.ndm -output_log_file logs/fc.import_design.full.log'
#alias ch2_load 'Ifc_shell -I -B $block -F apr_fc -output_log_file logs/fc.interactive.log'
#alias ifc_shell '/p/hdk/cad/fusioncompiler/P-2019.12-SP5/bin/fc_shell'

#alias ch2_custom '/p/hdk/bin/cth_psetup -p ipde -cfg n5_h210m15_r11_pesg.cth -ward $PWD -tool ipde_all -quiet -x "$SETUP_IPDE"'
#alias setup_cb 'echo source /p/cloudbreak/tools/barefoot/etc/barefoot.cshrc; source /nfs/sc/disks/bfn_pd_cb_07/pdonehue/pdonehue.cb-main/project.cshrc'


#alias setup_tfc 'echo source /p/tfc/tools/barefoot/etc/barefoot.cshrc; echo source /nfs/sc/disks/tfc_be_01/pdonehue/tfc/project.cshrc'
alias setup_vmac 'echo ch2_xterm; echo /p/hdk/bin/cth_psetup -p tfc -ward \$PWD -cfg TFC_IPDE.cth -tool ipde_all -quiet;echo \$SETUP_IPDE; echo setenv MGLS_LICENSE_FILE 1717@mentor04p.elic.intel.com:1717@mentor14p.elic.intel.com:1717@mentor03p.elic.intel.com:1717@mentor06p.elic.intel.com:1717@mentor37p.elic.intel.com; echo setenv NETLISTER_SKIP_SRC 1; echo setenv NB_WASH_GROUPS "n5,tfc,user,bxd_cb,bxd_jbay,soc,n7,n7fe"; echo setenv MGC_REALTIME_HOME /p/hdk/cad/calibre/aoi_cal_2021.1_16.10;echo setenv GDMNOTLOADLIB 1'
alias setup_r2g 'echo ch2_xterm; echo /p/hdk/bin/cth_psetup -p tfc -ward \$PWD -cfg tfc_n3.cth -quiet ;echo \$SETUP_IPDE;'

alias setup_vmacold 'echo ch2_xterm; echo /p/hdk/bin/cth_psetup -p tfc -ward \$PWD -tool ipde_all -quiet;echo \$SETUP_IPDE; echo source /p/tfc/tools/barefoot/etc/barefoot.cshrc; echo module load calibre/2021.1_16.10; echo setenv NETLISTER_SKIP_SRC 1; echo setenv NB_WASH_GROUPS "n5,tfc,user,bxd_cb,bxd_jbay,soc,n7,n7fe"; echo setenv MGC_REALTIME_HOME /p/hdk/cad/calibre/aoi_cal_2021.1_16.10'
alias setup_liberate 'echo module load liberate/21.23.02'
alias setup_sis 'echo source /p/tfc/tools/barefoot/etc/barefoot.cshrc; echo module load siliconsmart/S-2021.06-SP1; echo setenv NB_WASH_GROUPS "n5,tfc,user,bxd_cb,bxd_jbay,soc,n7,n7fe"'
alias setup_swiss 'echo https://myhpc.swiss.intel.com'
alias nb_paul 'echo nbstatus jobs --target sc_normal --history 10m "user=~pdonehue"'


setenv SHELL /usr/intel/bin/tcsh
# hack to work around cheetah
setenv BLOCK noblock
