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
foreach($_SESSION as $key => $value) {
    if (preg_match("/(filter|start)/", $key)) unset($_SESSION[$key]);
}
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
$alintraw="$diskroot/".$lveroot."/".$path."/$scripttask.raw";
if (file_exists($alintraw))
    $taskstat=stat("$alintraw");
else
    $taskstat=stat("$alintraw.gz");
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
$alintraw="$diskroot/$lveroot/$path/alint.raw";
if (file_exists("$alintraw.gz"))
    $fp=popen("/bin/gunzip -c '$alintraw.gz'","r");
elseif(file_exists("$alintraw"))
    $fp=fopen ("$alintraw","r");
else
    $fp=false;
$newbumps_addresp=array();
$newbumps=array();
$oldbumps=array();
$delays=array();
$leakage=array();
$em=array();
$errors=array();
$lcnt=0;
$addresp=0;
while ($fp !== false and ! feof($fp)) {
    $line=fgets($fp);
    $lcnt++;
    if (strstr($line, " additive_resp=") !== false) $addresp=1;
    if (substr($line,0,4) == "PASS") continue;
    if (strstr($line, "error=") !== false)
        $errors[]=$line;
    elseif (strstr($line, "inv_bump") !== false)
        $oldbumps[]=$line;
    elseif (strstr($line, " fail_type=") !== false){
        if (strstr($line, " additive_resp=") !== false){
          $newbumps_addresp[]=$line;
        }else{
          $newbumps[]=$line;
        }
    }
    elseif (strstr($line, " resp_up=") !== false)
        $newbumps[]=$line;
    elseif (strstr($line, " resp_dn=") !== false)
        $newbumps[]=$line;
    elseif (strstr($line, " multi_noise_resp_up=") !== false)
        $newbumps[]=$line;
    elseif (strstr($line, " multi_noise_resp_dn=") !== false)
        $newbumps[]=$line;
    elseif (strstr($line, " additive_resp_up=") !== false)
        $newbumps_addresp[]=$line;
    elseif (strstr($line, " additive_resp_dn=") !== false)
        $newbumps_addresp[]=$line;
    elseif (strstr($line, " resp=") !== false)
        $newbumps[]=$line;
    elseif (strstr($line, " additive_resp=") !== false)
        $newbumps_addresp[]=$line;
    elseif (strstr($line, "delay_dn=") !== false)
        $delays[]=$line;
    elseif (strstr($line, "leak_up=") !== false)
        $leakage[]=$line;
    elseif (strstr($line, "em_avg=") !== false)
        $em[]=$line;
}
if (file_exists("$alintraw.gz"))
    pclose($fp);
elseif(file_exists("$alintraw"))
    fclose($fp);
?>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<tr BGCOLOR="#CCCCFF">
<th>Node</th>
<th>stdout</th>
<th>stderr</th>
<th>Delay Dn (ps)<br>(spec)</th>
<th>Delay Up (ps)<br>(spec)</th>
<th>Error Message</th>
</tr>
<?php
showerrors($errors,$lveroot,$path,$scripttask);
?>
</table>
<?php
if ($addresp==1) {
   print "<h3><a href=\"newbumps.php?lveroot=$lveroot&addresp=1&path=$path\">Bumps (Additive Response):</a></h3>";
   print "<table cellpadding=\"2\" cellspacing=\"2\" border=\"1\" width=\"100%\" align=\"center\">";
   $_SESSION["newbumpsstart"]=0;
   shownewbumps($newbumps_addresp, $lveroot, $path, $scripttask,1);
   $_SESSION["newbumpsstart"]=0;
   print "</table>";
   print "</table>";
}
?>
<h3><a href="newbumps.php?lveroot=<?php echo $lveroot ?>&addresp=0&path=<?php echo $path;?>">Bumps:</a></h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
$_SESSION["newbumpsstart"]=0;
shownewbumps($newbumps, $lveroot, $path, $scripttask);
$_SESSION["newbumpsstart"]=0;
?>
</table>
</table>
<h3><a href="bumps.php?lveroot=<?php echo $lveroot ?>&path=<?php echo $path;?>">Old Bumps:</a></h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
$_SESSION["bumpsstart"]=0;
showbumps($oldbumps, $lveroot, $path, $scripttask);
$_SESSION["bumpsstart"]=0;
?>
</table>
<h3><a href="delays.php?lveroot=<?php echo $lveroot ?>&path=<?php echo $path;?>">Delays:</a></h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
$_SESSION["delaysstart"]=0;
showdelays($delays, $lveroot, $path, $scripttask);
$_SESSION["delaysstart"]=0;
?>
</table>
<h3><a href="leakage.php?lveroot=<?php echo $lveroot ?>&path=<?php echo $path;?>">Leakage:</a></h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
$_SESSION["leakagestart"]=0;
showleakage($leakage, $lveroot, $path, $scripttask);
$_SESSION["leakagestart"]=0;
?>
</table>
<h3><a href="em.php?lveroot=<?php echo $lveroot ?>&path=<?php echo $path;?>">Electromigration:</a></h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<?php
$_SESSION["emstart"]=0;
showem ($em, $lveroot, $path, $scripttask);
$_SESSION["emstart"]=0;
?>
</table>
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
