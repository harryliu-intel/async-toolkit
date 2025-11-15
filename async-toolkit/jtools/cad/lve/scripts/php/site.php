<?php
$authuser=preg_replace("/@.*/", "", $_SERVER["PHP_AUTH_USER"]);
$diskroot="/p/work";
$dirfile="lvedirs.txt";
$lveroot=$authuser;
$cwdpath="/srv/www/cgi-bin/cwd";
$sqllitepath="/scratch/quick.sqlite";
$lverootregexp="/nfs/site/disks/wdisk.\d+/";
$skipmysql=0;
?>
