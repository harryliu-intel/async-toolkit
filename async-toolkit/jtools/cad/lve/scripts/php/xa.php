<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
    "http://www.w3.org/TR/REC-html40/loose.dtd">
<?php
include "common.php";
$scripttask=script_name();
$uscripttask=strtoupper($scripttask);
$celname="";
if (isset($_REQUEST['FQCN']))
    $cellname=$_REQUEST['FQCN'];
elseif (isset($_SESSION['FQCN']))
    $cellname=$_SESSION['FQCN'];
else
    $cellname="lib.buffer.half.MBUF_1of4.1000";
if (isset($_REQUEST['path'])) {
    $path=$_REQUEST['path'];
}
else {
    print "Path not specified, illegal call to $scripttask.php<br>\n";
    footer();
}
$q=dbquery("select result from $rawtable where fqcn='$cellname' and path='$path' and task='$scripttask'");
$buf=dbfetchrow($q);
$f_status=$buf["result"];
globalheader("<h2 style=\"background-color: $color[$f_status];\">$f_status $uscripttask $cellname</h2>");
$scriptraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($scriptraw))
    $taskstat=stat("$scriptraw");
else
    $taskstat=stat("$scriptraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime<br>\n";
?>
<h3>Files:</h3>
<UL>
<?php
$diag=getfiles();
?>
</UL>
<?php
foreach ($diag as $value) {
diagnostics("$diskroot/$lveroot/$path/$value");
}
footer();
?>
