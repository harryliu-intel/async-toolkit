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
$aspiceraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($aspiceraw))
    $taskstat=stat("$aspiceraw");
else
    $taskstat=stat("$aspiceraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime<br>\n";
$pathsplit=preg_split(":/:", $path);
$pathcount=count($pathsplit);
$status=$f_status;
$view=$pathsplit[$pathcount-9];
$mode=$pathsplit[$pathcount-8];
$environment=$pathsplit[$pathcount-6];
$corner=$pathsplit[$pathcount-5];
$voltage=$pathsplit[$pathcount-4];
$temp=$pathsplit[$pathcount-3];
$duration=$pathsplit[$pathcount-2];
$range=$pathsplit[$pathcount-1];
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
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Time Delay<br>Range </CODE></TD>
<TD><CODE><B> <?php echo $range;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> View </CODE></TD>
<TD><CODE><B> <?php echo $view;?> </B></CODE>
<TR BGCOLOR="white" CLASS="TableRowColor"><TD ALIGN="right" VALIGN="top" WIDTH="1%"><CODE> Mode </CODE></TD>
<TD><CODE><B> <?php echo $mode;?> </B></CODE>
</table>
<h3>Warnings/Notices:</h3>
<?php
$fp=fopen ("$diskroot/$lveroot/$path/$scripttask.raw","r");
$nodes=array();
while ($fp !== false and ! feof($fp)) {
    $line=fgets($fp);
    $ret=parse_raw_line($line);
    $split=preg_split("/ /", $line);
    if (! isset($ret["status"]) or $ret["status"] == "") continue;
    $status=$ret["status"];
    if (isset($ret["node"])) {
        $nodes[]=$line;
    }
    elseif (isset($ret["power"])) {
        $power=$ret["power"];
        print "[<span style='background:$color[$status];'>$status</span>] Power: $power<br>";
        if (isset($ret["peak_power"])) {
            $peak=$ret["peak_power"];
            print "[<span style='background:$color[$status];'>$status</span>] Peak power: $peak<br>";
        }
    }
    else {
        $kind=preg_replace("/=.*/", "", $split[4]);
        print "[<span style='background:$color[$status];'>$status</span>] $kind<br>";
    }
}
fclose($fp);
?>
<h3>Nodes of Interest</h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<tr BGCOLOR="#CCCCFF">
<th>Node</th>
<th>Critical Path</th>
<th>History</th>
<th>Frequency</th>
<th>Measured Cycles</th>
<th>NTPC</th>
</tr>
<?php
foreach ($nodes as $line) {
    $ret=parse_raw_line($line);
    $status=$ret["status"];
    $node=$ret["node"];
    $freq=$ret["frequency"];
    $cycles=$ret["cycles"];
    if (isset($ret["ntpc"]))
        $ntpc=$ret["ntpc"];
    else
        $ntpc="not set";
    $critical="<a href='/work/$lveroot/$path/aspice:$node.critical'>critical</a>";
    $history="<a href='/work/$lveroot/$path/aspice:$node.history'>history</a>";
    $node="<a href='/work/$lveroot/$path/aspice:$node.png'>$node</a>";
    print "<tr style='background:$color[$status];'><td>$node<td>$critical<td>$history<td>$freq<td>$cycles<td>$ntpc\n";
}
?>
</table>
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
