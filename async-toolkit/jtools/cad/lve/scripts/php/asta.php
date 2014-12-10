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
    globalheader("ASTA");
    print "Path not specified, illegal call to $scripttask.php<br>\n";
    footer();
}
$q=dbquery("select result from $rawtable where fqcn='$cellname' and path='$path' and task='$scripttask'");
$buf=dbfetchrow($q);
$f_status=$buf["result"];
globalheader("<h2 style=\"background-color: $color[$f_status];\">$f_status $uscripttask $cellname</h2>");
$astaraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($astaraw))
    $taskstat=stat("$astaraw");
else
    $taskstat=stat("$astaraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime<br>\n";
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
<?php
$astaraw="$diskroot/$lveroot/$path/asta.raw";
if (file_exists("$astaraw.gz"))
    $fp=popen("/bin/gunzip -c '$astaraw.gz'","r");
elseif(file_exists("$astaraw"))
    $fp=fopen ("$astaraw","r");
else
    $fp=false;
$asta=array();
while ($fp !== false and ! feof($fp)) {
    $asta[]=fgets($fp);
}
if (file_exists("$astaraw.gz"))
    pclose($fp);
elseif(file_exists("$astaraw"))
    fclose($fp);
?>
<h3>ASTA Measurements</a></h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<tr BGCOLOR="#CCCCFF">
<th>Tau (ps)</th>
<th>Input slew (ps)</th>
<th>Output cap (fF)</th>
<th>Worst Tau (ps)</th>
<th>Flat Violations</th>
<th>Hierarchical Violations</th>
<th>Flat Timing Arcs</th>
<th>Hierarchical Timing Arcs</th>
<th>stdout</th>
<th>stderr</th>
</tr>
<?php
foreach ($asta as $line) {
    $ret=parse_raw_line($line);
    $split=preg_split("/ /", $line);
    if (! isset($ret["tau"])) continue;
    $status=$ret["status"];
    $tau=$ret["tau"];
    $worst_tau=$ret["worst_tau"];
    $input_slew=$ret["input_slew"];
    $output_cap=$ret["output_cap"];
    $bound=$ret["bound"];
    $out="out";
    $err="err";
    $bin=0;
    if (is_file("$diskroot/$lveroot/$path/asta/$tau/out")) {
        $out="<a href=\"/work/$lveroot/$path/asta/$tau/out\">out</a>";
        $err="<a href=\"/work/$lveroot/$path/asta/$tau/err\">err</a>";
    }
    if (is_file("$diskroot/$lveroot/$path/asta/$tau/flat.violations"))
        $flatv="<a href=\"/work/$lveroot/$path/asta/$tau/flat.violations\">flat.violations</a>";
    else 
        $flatv="missing";
    if (is_file("$diskroot/$lveroot/$path/asta/$tau/hier.violations"))
        $hierv="<a href=\"/work/$lveroot/$path/asta/$tau/hier.violations\">hier.violations</a>";
    else 
        $hierv="missing";
    if (is_file("$diskroot/$lveroot/$path/asta/$tau/flat.timing"))
        $flatt="<a href=\"/work/$lveroot/$path/asta/$tau/flat.timing\">flat.timing</a>";
    else 
        $flatt="missing";
    if (is_file("$diskroot/$lveroot/$path/asta/$tau/hier.timing"))
        $hiert="<a href=\"/work/$lveroot/$path/asta/$tau/hier.timing\">hier.timing</a>";
    else 
        $hiert="missing";
    print "<tr style='background: $color[$status];'><td>$tau<td>$input_slew<td>$output_cap<td>$worst_tau<td>$flatv<td>$hierv<td>$flatt<td>$hiert<td>$out<td>$err</tr>";
}
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
