<?php
$authuser=preg_replace("/@.*/", "", $_SERVER["PHP_AUTH_USER"]);
$diskroot="/nfs/site/disks";
$dirfile="lvedirsSC.txt";
$lveroot=$authuser;
$cwdpath="/srv/www/cgi-bin/cwd";
$sqllitepath="/scratch/quick.sqlite";
$lverootregexp="/nfs/site/disks/";
$skipmysql=1;
?>
