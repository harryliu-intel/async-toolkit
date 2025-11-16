<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
    "http://www.w3.org/TR/REC-html40/loose.dtd">
<?php
include "common.php";
$scripttask="lib";
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
    print "Path not specified, illegal call to delays.php<br>\n";
    footer();
}
if (isset($_GET["start"])) $_SESSION["libdelaysstart"]=$_GET["start"];
else $_SESSION["libdelaysstart"]=0;
if (isset($_REQUEST["filter"])) $_SESSION["libdelaysfilter"]=$_REQUEST["filter"];
else $_SESSION["libdelaysfilter"]=$_SESSION["lastlibdelaysfilter"]="";
$delays=getlibdelays($lveroot,$path,$scripttask);
$stat=array();
foreach ($delays as $line) {
    $s=preg_split("/ /", $line);
    $s=array_shift($s);
    if (!isset($stat[$s])) $stat[$s]=0;
    $stat[$s]++;
}
$f_status=summarizeStatus($stat);
globalheader("<h2 style=\"background-color: $color[$f_status];\">$f_status LIB Delays $cellname</h2>");
echo "<center>Path: $path</center>";
$libdelaysraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($libdelaysraw))
    $taskstat=stat("$libdelaysraw");
else
    $taskstat=stat("$libdelaysraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime ";
quicklink();
$filter="";
if (isset($_SESSION["libdelaysfilter"])) $filter=$_SESSION["libdelaysfilter"];
?>
<form method=post name=filter action="libdelays.php?lveroot=<?php echo $lveroot ?>&path=<?php echo $path ?>">
Filter : <input type=text name=filter value="<?php echo $filter;?>" size=32> Valid filters: 
<?php
$a=array_keys($libdelaysitems);
sort($a);
foreach ($a as $value) {
    print "$value(".substr($libdelaysitems[$value],0,1).") ";
}
?>
</form>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
showlibdelays($delays, $lveroot, $path, $scripttask);
?>
</table>
<?php
footer();
?>
