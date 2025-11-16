<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
    "http://www.w3.org/TR/REC-html40/loose.dtd">
<?php
include "common.php";
$scripttask=script_name();
$uscripttask=strtoupper($scripttask);
$celname="";
foreach($_SESSION as $key => $value) {
    if (preg_match("/(filter|start)/", $key)) unset($_SESSION[$key]);
}
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
    print "Path not specified, illegal call to $scripttask.php<br>\n";
    footer();
}
$q=dbquery("select result from $rawtable where fqcn='$cellname' and path='$path' and task='$scripttask'");
$buf=dbfetchrow($q);
$f_status=$buf["result"];
globalheader("<h2 style=\"background-color: $color[$f_status];\">$f_status $uscripttask $cellname</h2>");
echo "<center>Path: $path</center>";
$libraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($libraw))
    $taskstat=stat("$libraw");
else
    $taskstat=stat("$libraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime ";
quicklink();
$pathsplit=preg_split(":/:", $path);
$pathcount=count($pathsplit);
$status=$f_status;
$view=$pathsplit[$pathcount-6];
$mode=$pathsplit[$pathcount-5];
$corner=$pathsplit[$pathcount-3];
$voltage=$pathsplit[$pathcount-2];
$temp=$pathsplit[$pathcount-1];
?>
<TABLE BORDER="1" WIDTH="100%" CELLPADDING="1" CELLSPACING="1">
<TR BGCOLOR="#CCCCFF">
<TD COLSPAN=2><FONT SIZE="+2"><B>Parameters</B></FONT></TD></TR>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Corner </CODE></TD>
<TD><CODE><B> <?php echo $corner;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Voltage </CODE></TD>
<TD><CODE><B> <?php echo $voltage;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Temperature </CODE></TD>
<TD><CODE><B> <?php echo $temp;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> View </CODE></TD>
<TD><CODE><B> <?php echo $view;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Mode </CODE></TD>
<TD><CODE><B> <?php echo $mode;?> </B></CODE>
</table>
<h3>Warnings/Notices:</h3>
<h3>Signoffs or failed measurements:</h3>
<?php
$libraw="$diskroot/$lveroot/$path/$scripttask.raw";
if (file_exists("$libraw.gz"))
    $fp=popen("/bin/gunzip -c '$libraw.gz'","r");
elseif(file_exists("$libraw"))
    $fp=fopen ("$libraw","r");
else
    $fp=false;
$delays=array();
while ($fp !== false and ! feof($fp)) {
    $line=fgets($fp);
    if (substr($line,0,4) == "PASS") continue;
    elseif (preg_match("/delay/", $line))
        $delays[]=$line;
}
if (file_exists("$libraw.gz"))
    pclose($fp);
elseif(file_exists("$libraw"))
    fclose($fp);
?>
<h3><a href="libdelays.php?lveroot=<?php echo $lveroot ?>&path=<?php echo $path;?>&task=<?php echo $scripttask;?>">Delays:</a></h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
showlibdelays($delays, $lveroot, $path, $scripttask);
?>
</table>
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
