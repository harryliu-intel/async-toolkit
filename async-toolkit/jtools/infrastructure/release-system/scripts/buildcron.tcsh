#!/bin/tcsh
# AAG
# $Id$
# $DateTime$

unsetenv DISPLAY
set common=/home/local/common/fulcrum
set rundir=$common/cron
set buildcron=$common/bin/buildcron
set buildfulcrum=$common/bin/buildfull.pl
set updatefmdb=$common/bin/updatefmdb.pl
set avail=`/bin/df /mnt/fulcrum/scratch1 | awk '/scratch1/ {print int($(NF-2)/1024/1024)}'`
if ( -r /etc/csh.cshrc ) then
    source /etc/csh.cshrc
else
    set path=(/usr/ucb /bin /usr/bin /usr/intel/bin /usr/X11R6/bin)
endif
if (-f /usr/local/grid-6.0/default/common/settings.csh) then
  source /usr/local/grid-6.0/default/common/settings.csh
endif
set path=(/usr/local/bin $common/bin $path)
# check and qrsh from vidar if on vidar
set hostname=`/bin/hostname`
set sshid=`host vidar | awk '/ address / {print $NF}'`
set hostid=`host $hostname | awk '/ address / {print $NF}'`
if ( "$hostid" == "$sshid" ) then
    qrsh -P build-fulcrum -nostdin -now n -N buildcron -l mem=512M $buildcron
    exit 0
endif
mkdir -p $rundir
chdir $rundir
set log=logfile.tmp
touch $log
setenv P4USER system
set change=`p4 changes -s submitted -m 1 | awk '{print $2}'`
date >> $log
hostname >> $log
uname -s >> $log
/bin/ls /mnt/fulcrum/local/ >& /dev/null
df /mnt/fulcrum/local >& /dev/null # attempt to automount local
set logp4=logp4
touch $logp4
/usr/intel/bin/p4 -c build-common-pdk sync @$change >>& $logp4
/usr/intel/bin/p4 -c build-common-tools sync @$change >>& $logp4
cat $logp4 >> $log
echo "Change $change" >> $log
set uptodate=`fgrep -c 'file(s) up-to-date' $logp4`
echo $uptodate
cat $logp4
/bin/rm -f $logp4
if ( "$uptodate" == "2" ) then
    echo "No changes, build unnecessary"
    echo "No changes, build unnecessary" >> $log
    if ( "$log" != "logfile" ) then
        /bin/cat $log >> logfile
        /bin/rm -f $log
    endif
    exit 0
endif
# do pdk first, this way all fmdb updates are correct
if ( -d /mnt/fulcrum/local/common/fulcrum/pdk/fulcrum-tsmc65-pdk/$change ) then
    echo "$change for pdk tsmc65 already built" >> $log
else
    ($buildfulcrum --pdk --change $change --nop4sync --root-project-dir=/mnt/fulcrum/local/checkouts/build/pdk ) >>& $log
endif
if ( -d /mnt/fulcrum/local/common/fulcrum/tools/all/$change ) then
    echo "$change for package all already built" >> $log
else
    ($buildfulcrum --target-arch Linux-i686,Linux-x86_64 --change $change --nop4sync --root-project-dir=/mnt/fulcrum/local/checkouts/build/tools ) >>& $log
endif
# fix when the above does not work correctly
foreach arch ( lx24-x86 lx24-amd64 )
    qrsh -nostdin -now n -l a=$arch,mem=48M -N fix /home/system/build/bin/fixinstall $change
end
/home/system/build/bin/updateall --target-arch=Linux-i686,Linux-x86_64
set proteusfailed=`egrep '^No packages successfully|^Failed|failed to build' $log`
if ( ! -e "/mnt/fulcrum/local/common/fulcrum/tools/all/$change/.installed" || ! -e "/mnt/fulcrum/local/common/fulcrum/pdk/fulcrum-tsmc65-pdk/$change/.installed" ) then
    set proteusfailed="$proteusfailed : install missing"
endif
# leaving 40 g free for others. The actual usage is about 10G
if ( $avail < 50 ) then
    if ( "$proteusfailed" == "" ) then
        set proteusfailed="Not enough space on scratch1 (=$avail G)"
    else
        set proteusfailed="$proteusfailed and Not enough space on scratch1 (=$avail G)"
    endif
endif
if ( "$proteusfailed" != "" ) then
    set msg=`egrep '^Failed|failed to build|^No packages' $log`;
    /bin/mail -s 'Cron Failed on one or more targets' aubrey@fulcrummicro.com <<EOF
There were errors on this morning's cron build, check log files
  $msg

Start in /home/local/common/fulcrum/cron/logfile then find the detailed
logs in /home/local/common/fulcrum/cron/
This was change number $change

Failed message: $proteusfailed
EOF

endif
# merge temp logs to full log file
if ( "$log" != "logfile") then
    /bin/cat $log >> logfile
    /bin/rm -f $log
endif
# purge back one week
find /home/local/common/fulcrum/cron -type f -name 'log.*' -mtime +7 -exec /bin/rm -f {} \;
if ("$proteusfailed" == "" && ! -f "$rundir/runregression" ) then
    /usr/sbin/sendmail -fbuild@fulcrummicro.com pabeerel@fulcrummicro.com <<EOF4
Subject: Proteus did no regression run on $change

No regression run on change $change because regressions disabled until further notice

EOF4
exit 0
endif
# do proteus regression if build was ok
set numnanoproteus=`qstat -r -f |& grep -c nanoproteus=`;
set numnano=`qstat -r -f |& grep -c nano=`;
set numnptickets=`qconf -se global | grep nanoproteus= | sed -e 's/.*nanoproteus=//' -e 's/,.*//' -e 's/ //g' | tail -1`
set numnanotickets=`qconf -se global | grep nano= | sed -e 's/.*nano=//' -e 's/,.*//' -e 's/ //g' | tail -1`
set availtickets=`expr $numnptickets + $numnanotickets - $numnanoproteus - $numnano`
set availtickets=10 # always run, since we do it so seldom
if ("$proteusfailed" == "" && $availtickets >= 3 ) then
    chdir ~build/proteus
    qsub -now n -N pregress -o /dev/null -e /dev/null -cwd -l a=lx24-amd64,mem=16G >/dev/null <<EQS
#!/bin/tcsh
/p/rrc/tools/bin/fulcrum --pdk tsmc65 --latest proteus_regress --client-spec=build-proteus-reg --jobs=5 --verbose=1

/bin/rm -f "$rundir/runregression"

(echo "Subject: Proteus regression run on $change"; \
echo "" ;\
echo "Please see /mnt/fulcrum/scratch1/build/proteus for results."; \
echo "Please see /mnt/fulcrum/scratch1/build/proteus/proteus_regress.$change for terminal log."; \
echo ""; \
cat regressions/latest/qdi.regress.rpt ) \
|& /usr/sbin/sendmail -fbuild@fulcrummicro.com pabeerel@fulcrummicro.com

EQS

else
    if ("$proteusfailed" != "" ) then
    /usr/sbin/sendmail -fbuild@fulcrummicro.com pabeerel@fulcrummicro.com <<EOF2
Subject: Proteus did no regression run on $change

No regression run on change $change because no build was done on this change.
This can occur because of an earlier trigger build or because the build failed.

EOF2

    else
    /usr/sbin/sendmail -fbuild@fulcrummicro.com pabeerel@fulcrummicro.com <<EOF3
Subject: Proteus did no regression run on $change

No regression run on change $change because not enough encounter slots ($availtickets)

EOF3

    endif

endif
