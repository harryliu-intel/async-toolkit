<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
    "http://www.w3.org/TR/REC-html40/loose.dtd">
<?php
include "common.php";
$cellname="";
if (isset($_REQUEST['FQCN']))
    $cellname=$_REQUEST['FQCN'];
elseif (isset($_SESSION['FQCN']))
    $cellname=$_SESSION['FQCN'];
else
    $cellname="lib.buffer.half.MBUF_1of4.1000";

globalheader("<h2>RTE Summary  for $cellname </h2>");

#Select all the rows for this cellname to find the total coverage files
$q = dbquery("select * from $rawtable where fqcn='$cellname' and task='rte'");
$patharray = array();
$combarray = array();
$i =0;
#This loop sorts these files such that rows with the same p-v-t parameters will go in the 
#same array element
#The final combarray is indexed by $cnt and each element is $cnt => all paths with the same pvt (but diff envs)

while($buf = dbfetchrow($q)){
    $path = $buf["path"];
    $f_status = $buf["result"];
    $patharray[$path] = $f_status;

    $pathsplit=preg_split(":/:", $path);
    $pathcount=count($pathsplit);
    $view_i=$pathsplit[$pathcount-7];
    $mode_i=$pathsplit[$pathcount-6];
    $corner_i=$pathsplit[$pathcount-4];
    $voltage_i=$pathsplit[$pathcount-3];
    $temp_i=$pathsplit[$pathcount-2];
    $env_i =$pathsplit[$pathcount-1];
    $flag = 0;
    foreach ($combarray as $j=>$sameruns){
        $pathsplit=preg_split(":/:", $sameruns[0]);
        $pathcount=count($pathsplit);
        $view_j=$pathsplit[$pathcount-7];
        $mode_j=$pathsplit[$pathcount-6];
        $corner_j=$pathsplit[$pathcount-4];
        $voltage_j=$pathsplit[$pathcount-3];
        $temp_j=$pathsplit[$pathcount-2];
        $env_j =$pathsplit[$pathcount-1];
        if($view_i == $view_j && $mode_i == $mode_j && $corner_i == $corner_j && $voltage_i == $voltage_j && $temp_i == $temp_j){
            $combarray[$j][]= $path;
            $flag =1;
            break;
        }
    }
    if(!$flag){
        $cnt = count($combarray);
        $combarray[$cnt][] = $path;
    }
    
    $i++;
}
#Display Individual Parameters (This does not display the total rule and csp coverage
function displayIndividualParams($path,$f_status){
    global $lveroot;
    global $diskroot;
    $pathsplit=preg_split(":/:", $path);
    $pathcount=count($pathsplit);
    $view=$pathsplit[$pathcount-7];
    $mode=$pathsplit[$pathcount-6];
    $corner=$pathsplit[$pathcount-4];
    $voltage=$pathsplit[$pathcount-3];
    $temp=$pathsplit[$pathcount-2];
    $env =$pathsplit[$pathcount-1];

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
    if($fp == false) echo "<br> The file  $diskroot/$lveroot/$path/rte.raw cant be opened<br> ";
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
            $ntpc = $ret["ntpc"];
        }

        if(!feof($fp)){
            print "<tr><td>$f_status<td>$view<td>$mode<td>$corner<td>$voltage<td>$temp<td>$env<td style ='background: $cspCoverageColor;'>$cspCoverage<td style ='background: $ruleCoverageColor;'>$ruleCoverage<td style= 'background :$nodeCoverageColor;'>
            $nodeCoverage<td>$ntpc</tr>";
        }
    }
    if (file_exists("$rteraw.gz"))
        pclose($fp);
    elseif(file_exists("$rteraw"))
        fclose($fp);
    return 1;

}

?>
<h3>RTE Coverage:</h3>
<table cellpadding="2" cellspacing="2" border="1" width="100%" align="center">
<tr BGCOLOR="#CCCCFF">
<th>Cosim Pass/Fail</th>
<th>View</th>
<th>Mode</th>
<th>Corner</th>
<th>Voltage</th>
<th>Temperature</th>
<th>Environment </th>
<th>cspCoverage</th>
<th>ruleCoverage</th>
<th>nodeCoverage</th>
<th>Measured ntpc</th>
</tr>

<?php
foreach ($patharray as $path => $f_status){
    $ret = displayIndividualParams($path,$f_status);
}
?>
</table>
<br>
<?php
foreach ($combarray as $key=>$selpaths){
    # Table for displaying total CSP and total Rule coverage
    echo "<h3> Total Coverage </h3>";
    echo "<table cellpadding='2' cellspacing='2' border='1' width='100%' align='center'>";
    echo "<tr BGCOLOR='#CCCCFF'>";
    echo "<th>Cosim Pass/Fail</th>";
    echo "<th>View</th>";
    echo "<th>Mode</th>";
    echo "<th>Corner</th>";
    echo "<th>Voltage</th>";
    echo "<th>Temperature</th>";
    echo "<th>Environment </th>";
    echo "<th>Total CSP Coverage</th>";
    echo "<th>Total Rule Coverage</th>";
    echo "</tr>";
    computeTotalCoverage($combarray[$key]);

    echo "</table>";

    echo "<h4> Coverage files</h4>";
    echo "<UL>";
    foreach ($combarray[$key] as $index =>$path){
        getCoverageFiles($path);
    }
    echo "</UL>";
}

?>
<?php
footer();
?>




<?php
#Displaying Coverage files(Assuming them to be .gz) 
#getfiles() from common.php is not used here because the direct path to the .gz files is not known.The .gz files are
#put into fqcp/modules by jdsim script. The raw file does not have this path. Hence a find command is used here with
#a parantheses escaped filepath

function getCoverageFiles($path){
    global $lveroot;
    global $diskroot;
    $pathsplit=preg_split(":/:", $path);
    $pathcount=count($pathsplit);
    $env =$pathsplit[$pathcount-1];
    echo "<br>$env<br>";

    $coverageFiles = array();
    $escapedpath = preg_replace("/\(/","\\\(",$path);
    $escapedpath = preg_replace("/\)/","\\\)",$escapedpath);
    if (is_dir("$diskroot/$lveroot/$path")) {
          exec("find $diskroot/$lveroot/$escapedpath -type f -print |grep '.gz'",$coverageFiles);
          foreach ($coverageFiles as $i=>$file){
              $filepath = preg_replace("/p\/work/","work",$file);
              preg_match('/[\w|\.|:|_]+.gz$/',$file,$filename);
              echo "<LI><a href=\"$filepath\">$filename[0]</a><br>";
          }
    }

}
function computeTotalCoverage($pltcomb){
    global $lveroot;
    global $patharray; 
    global $diskroot;

    $cnt = 0;
    $cspCoverageColor ="white";
    $ruleCoverageColor ="white";
    $nodeCoveragecolor ="white";
    $totalruleCoverage =0;
    $totalcspCoverage =0;

    $pathsplit=preg_split(":/:", $pltcomb[0]);
    $pathcount=count($pathsplit);
    $env = $pathsplit[$pathcount -1];
    
    $summaryPath = $pltcomb[0];
    $summaryPath =preg_replace("/$env/","",$summaryPath,1) ;
    $ruleFilePath = "/work/".$lveroot."/" .$summaryPath."/total_rule";
    $cspFilePath ="/work/".$lveroot."/" .$summaryPath."/total_csp";
    $summaryPath = "$diskroot/".$lveroot."/" .$summaryPath;
      
    #Parse the summary file for this pvt and get the total coverage files
    $fp=fopen ("$summaryPath"."rte.raw.summary","r");
    while ($fp !== false and ! feof($fp)) {
        $line=fgets($fp);
        $ret=parse_raw_line($line);
        if (isset($ret["cspCoverage"])){
            $totalcspCoverage=$ret["cspCoverage"];
            if(intval($totalcspCoverage)>85) $cspCoverageColor = "green";
            elseif(intval($totalcspCoverage) >50) $cspCoverageColor ="yellow";
            else $cspCoverageColor = "red";
        }
        if (isset($ret["ruleCoverage"])){
            $totalruleCoverage=$ret["ruleCoverage"];
            if(intval($totalruleCoverage)>85) $ruleCoverageColor = "green";
            elseif(intval($totalruleCoverage) >50) $ruleCoverageColor ="yellow";
            else $ruleCoverageColor = "red";

        }
    }
    fclose($fp);

    foreach ($pltcomb as $index =>$path){
        $pathsplit=preg_split(":/:", $path);
        $pathcount=count($pathsplit);
        $view=$pathsplit[$pathcount-7];
        $mode=$pathsplit[$pathcount-6];
        $corner=$pathsplit[$pathcount-4];
        $voltage=$pathsplit[$pathcount-3];
        $temp=$pathsplit[$pathcount-2];
        $env =$pathsplit[$pathcount-1];
        if($cnt == 0)
            print "<tr><td>$patharray[$path]<td>$view<td>$mode<td>$corner<td>$voltage<td>$temp<td>$env<td
                rowspan=2 style ='background: $cspCoverageColor;'><a
                href='$cspFilePath/rte.total_csp_covered'>$totalcspCoverage</a><td rowspan=2 style ='background:
                $ruleCoverageColor;'><a href='$ruleFilePath/rte.total_covered_rules'>$totalruleCoverage</a></tr>";
        else
            print "<tr><td>$patharray[$path]<td>$view<td>$mode<td>$corner<td>$voltage<td>$temp<td>$env</tr>";
   
        $cnt++;
    }


}
?>
