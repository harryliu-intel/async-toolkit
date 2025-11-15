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
echo "<center>Path: $path</center>";
$totemraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($totemraw))
    $taskstat=stat("$totemraw");
else
    $taskstat=stat("$totemraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime<br>\n";
$pathsplit=preg_split(":/:", $path);
$pathcount=count($pathsplit);
$status=$f_status;
$view=$pathsplit[$pathcount-8];
$environment=$pathsplit[$pathcount-5];
$corner=$pathsplit[$pathcount-4];
$voltage=$pathsplit[$pathcount-3];
$temp=$pathsplit[$pathcount-2];
$duration=$pathsplit[$pathcount-1];
?>
<TABLE BORDER="1" WIDTH="100%" CELLPADDING="1" CELLSPACING="1">
<TR BGCOLOR="#CCCCFF">
<TD COLSPAN=2><FONT SIZE="+2"><B>Parameters</B></FONT></TD></TR>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Environment </CODE></TD>
<TD><CODE><B> <?php echo $environment;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Corner </CODE></TD>
<TD><CODE><B> <?php echo $corner;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Voltage </CODE></TD>
<TD><CODE><B> <?php echo $voltage;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Temperature </CODE></TD>
<TD><CODE><B> <?php echo $temp;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Simulation<br>Duration </CODE></TD>
<TD><CODE><B> <?php echo $duration;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> View </CODE></TD>
<TD><CODE><B> <?php echo $view;?> </B></CODE>
</table>
<h3>Warnings/Notices:</h3>
<?php
$totemraw="$diskroot/$lveroot/$path/$scripttask.raw";
if (file_exists("$totemraw.gz"))
    $fp=popen("/bin/gunzip -c '$totemraw.gz'","r");
elseif(file_exists("$totemraw"))
    $fp=fopen ("$totemraw","r");
else
    $fp=false;
$nodes=array();
while ($fp !== false and ! feof($fp)) {
    $line=fgets($fp);
    $split=preg_split("/ /", $line);
    $status=array_shift($split);
    array_shift($split);
    array_shift($split);
    array_shift($split);
    # accommodate old raw files
    if (! isset($split[0])) continue;
    if (preg_match("/\//", $split[0])) array_shift($split);
    $rest=join(" ", $split);
    print "[<span style='background:$color[$status];'>$status</span>] $rest<br>";
}
if (file_exists("$totemraw.gz"))
    pclose($fp);
elseif(file_exists("$totemraw"))
    fclose($fp);
?>
<h3>Files:</h3>
<UL>
<?php
$diag=getfiles();
?>
</UL>
<?php
if (is_file("$diskroot/$lveroot/$path/$scripttask.out")) {
    $fp=fopen("$diskroot/$lveroot/$path/$scripttask.out", "r");
    $stats=array();
    while ($fp !== false and ! feof($fp)) {
        $line=fgets($fp);
        if (substr($line, 0, 19) == "Circuit statistics:") break;
    }
    while ($fp !== false and ! feof($fp)) {
        $line=fgets($fp);
        if (substr($line, 0, 6) == "Total ") break;
        if (substr($line, 0, 6) == "block ") break;
        if (preg_match("/average cost/", $line)) break;
        list($count,$name)=preg_split("/ /", $line, 2);
        $name=str_replace(" ", "&nbsp;", $name);
        $stats[$name]=$count;
    }
    fclose($fp);
    if (count($stats)) {
?>
<TABLE BORDER="1" WIDTH="100%" CELLPADDING="1" CELLSPACING="1">
<TD COLSPAN=2><FONT SIZE="+2"><B>Circuit Statistics</B></FONT></TD></TR>
<?php
        foreach ($stats as $name => $value) {
            echo "<TR BGCOLOR=\"white\" CLASS=\"TableRowColor\"><TD ALIGN=\"right\" VALIGN=\"top\" WIDTH=\"1%\"><CODE> $name </CODE></TD><TD><CODE><B> $value </B></CODE>\n";
        }
        echo "</table>\n";
    }
}
foreach ($diag as $value) {
diagnostics("$diskroot/$lveroot/$path/$value");
}
footer();
?>
