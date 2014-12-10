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
if (isset($_REQUEST['addresp'])) {
    $addresp=$_REQUEST['addresp'];
}
if (isset($_GET["start"])) $_SESSION["newbumpsstart"]=$_GET["start"];
else $_SESSION["newbumpsstart"]=0;
if (isset($_REQUEST["filter"])) $_SESSION["newbumpsfilter"]=$_REQUEST["filter"];
else $_SESSION["newbumpsfilter"]=$_SESSION["lastnewbumpsfilter"]="";
$bumps=getnewbumps($lveroot,$path,"alint",$addresp);
$stat=array();
foreach ($bumps as $line) {
    $s=preg_split("/ /", $line);
    $s=array_shift($s);
    if (!isset($stat[$s])) $stat[$s]=0;
    $stat[$s]++;
}
$f_status=summarizeStatus($stat);
$addresp_str="";
if($addresp==1) $addresp_str="Additive response";
globalheader("<h2 style=\"background-color: $color[$f_status];\">$f_status ALINT Bumps $addresp_str $cellname</h2>");
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
if (isset($_SESSION["newbumpsfilter"])) $filter=$_SESSION["newbumpsfilter"];
?>
<form method=post name=filter action="newbumps.php?path=<?php echo $path ?>">
Filter : <input type=text name=filter value="<?php echo $filter;?>" size=32> Valid filters: 
<?php
$a=array_keys($newbumpsitems);
sort($a);
foreach ($a as $value) {
    print "$value(".substr($newbumpsitems[$value],0,1).") ";
}
?>
</form>
</center>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
shownewbumps($bumps, $lveroot, $path, $scripttask, $addresp);
?>
</table>
<?php
footer();
?>
