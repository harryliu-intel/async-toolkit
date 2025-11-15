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
$rteraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($rteraw))
    $taskstat=stat("$rteraw");
else
    $taskstat=stat("$rteraw.gz");
$taskmtime=strftime("%a %b %d %X %Z %Y", $taskstat["mtime"]);
echo "Last Modified: $taskmtime<br>\n";
$pathsplit=preg_split(":/:", $path);
$pathcount=count($pathsplit);
$status=$f_status;
$view=$pathsplit[$pathcount-7];
$mode=$pathsplit[$pathcount-6];
$corner=$pathsplit[$pathcount-4];
$voltage=$pathsplit[$pathcount-3];
$temp=$pathsplit[$pathcount-2];
$env =$pathsplit[$pathcount-1];
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

<h3>RTE Coverage:</h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<tr BGCOLOR="#CCCCFF">
<th>Cosim Pass/Fail</th>
<th>Environment </th>
<th>cspCoverage</th>
<th>ruleCoverage</th>
<th>nodeCoverage</th>
<th>Measured Ntpc</th>
</tr>
<?php
$cspCoverageColor ="white";
$ruleCoverageColor ="white";
$nodeCoveragecolor ="white";

$rteraw="$diskroot/$lveroot/$path/rte.raw";
if (file_exists("$rteraw.gz"))
    $fp=popen("/bin/gunzip -c '$rteraw.gz'","r");
elseif(file_exists("$rteraw"))
    $fp=fopen ("$rteraw","r");
else
    $fp=false;
while ($fp !== false and ! feof($fp)) {
    $line=fgets($fp);
    $ret=parse_raw_line($line);
    if (isset($ret["cspCoverage"])){
        $cspCoverage=$ret["cspCoverage"];
        if(intval($cspCoverage)>85) $cspCoverageColor = "green";
        elseif(intval($cspCoverage) >50) $cspCoverageColor ="yellow";
        else $cspCoverageColor = "red";

    }
    if (isset($ret["ruleCoverage"])){
        $ruleCoverage=$ret["ruleCoverage"];
        if(intval($ruleCoverage)>85) $ruleCoverageColor = "green";
        elseif(intval($ruleCoverage) >50) $ruleCoverageColor ="yellow";
        else $ruleCoverageColor = "red";

    }

    if (isset($ret["nodeCoverage"])){
        $nodeCoverage=$ret["nodeCoverage"];
        if(intval($nodeCoverage)>85) $nodeCoverageColor = "green";
        elseif(intval($nodeCoverage) >50) $nodeCoverageColor ="yellow";
        else $nodeCoverageColor = "red";

    }
    if (isset($ret["ntpc"])){
        $ntpc=$ret["ntpc"];
    }



    if(!feof($fp)){
        print "<tr><td>$f_status<td>$env<td style ='background: $cspCoverageColor;'>$cspCoverage<td style ='background: $ruleCoverageColor;'>$ruleCoverage<td style= 'background :$nodeCoverageColor;'>
      $nodeCoverage<td>$ntpc</tr>";
    }
}
if (file_exists("$rteraw.gz"))
    pclose($fp);
elseif(file_exists("$rteraw"))
    fclose($fp);
?>
</table>
<h3>Files: <h3>
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
