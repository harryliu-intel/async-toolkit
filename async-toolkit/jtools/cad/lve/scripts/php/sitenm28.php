<?php
session_save_path("/p/rrc/tools/apache2/session");
$authuser=preg_replace("/@.*/", "", $_SERVER["PHP_AUTH_USER"]);
$authuser="";
$diskroot="/p/work";
$dirfile="lvedirs.txt";
$lveroot="";
$cwdpath="/p/rrc/tools/apache2/cgi-bin/cwd";
$sqllitepath="/p/rrc/tools/apache2/scratch/quick.sqlite";
$lverootregexp="/nfs/site/disks/wdisk.\d+/";
$skipmysql=0;
?>
