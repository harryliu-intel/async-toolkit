<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
    "http://www.w3.org/TR/REC-html40/loose.dtd">
<?php
include "common.php";
$scripttask="alint";
$celname="";
restore_quick();
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
    print "Path not specified, illegal call to bump.php<br>\n";
    footer();
}
if (isset($_GET["start"])) $_SESSION["bumpsstart"]=$_GET["start"];
else $_SESSION["bumpsstart"]=0;
if (isset($_REQUEST["filter"])) $_SESSION["bumpsfilter"]=$_REQUEST["filter"];
else $_SESSION["bumpsfilter"]=$_SESSION["lastbumpsfilter"]="";
$bumps=getbumps($lveroot,$path,"alint");
$stat=array();
foreach ($bumps as $line) {
    $s=preg_split("/ /", $line);
    $s=array_shift($s);
    if (!isset($stat[$s])) $stat[$s]=0;
    $stat[$s]++;
}
$f_status=summarizeStatus($stat);
globalheader("<h2 style=\"background-color: $color[$f_status];\">$f_status ALINT Bumps $cellname</h2>");
echo "<center>Path: $path</center>";
$alintraw="$diskroot/".$lveroot."/".$path."/alint.raw";
if (file_exists($alintraw))
    $taskstat=stat("$alintraw");
else
    $taskstat=stat("$alintraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime ";
quicklink();
$filter="";
if (isset($_SESSION["bumpsfilter"])) $filter=$_SESSION["bumpsfilter"];
?>
<form method=post name=filter action="bumps.php?lveroot=$lveroot&path=<?php echo $path ?>">
Filter : <input type=text name=filter value="<?php echo $filter;?>" size=32> Valid filters: 
<?php
$a=array_keys($bumpsitems);
sort($a);
foreach ($a as $value) {
    print "$value(".substr($bumpsitems[$value],0,1).") ";
}
?>
</form>
</center>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
showbumps($bumps, $lveroot, $path, $scripttask);
?>
</table>
<?php
footer();
?>
