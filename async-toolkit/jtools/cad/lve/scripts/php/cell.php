<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd>
<?php
include "common.php";
$celname="";
if (isset($_REQUEST['FQCN']))
    $cellname=$_REQUEST['FQCN'];
elseif (isset($_SESSION['FQCN']))
    $cellname=$_SESSION['FQCN'];
else
    $cellname="lib.buffer.half.MBUF_1of4.1000";
$q = dbquery("select * from $cellstable where fqcn='$cellname'");
$buf=dbfetchrow($q);
$stat=array();
$_SESSION["FQCN"]=$cellname;
foreach ($tasklist as $key => $value) {
    if (isset($buf[$key]) and $buf[$key] != "NOT_TESTED") {
        if (!isset($stat[$buf[$key]])) $stat[$buf[$key]]=0;
        $stat[$buf[$key]]++;
    }
}
$f_status=summarizeStatus($stat);
globalheader("<h2 style='background-color: $color[$f_status];'>$f_status  $cellname</h2>");
# get all raw data for this cell
$patharray=array();
$statusarray=array();
$q = dbquery("select * from $rawtable where fqcn='$cellname'");
$cnt=0;
while ($buf=dbfetchrow($q)) {
    $path=$buf["path"];
    $task=$buf["task"];
    $status=$buf["result"];
    $patharray[$task][]=$path;
    $statusarray[$task][]=$status;
    $cnt++;
}
foreach ($tasklist as $task => $type) {
    $utask=strtoupper($task);
    if (isset($patharray[$task])) {
        $path=$patharray[$task][0];
        $status=$statusarray[$task][0];
        if ($type == 0) {
            foreach ($patharray[$task] as $ndx => $path) {
                $status=$statusarray[$task][$ndx];
                print "<a href='$task.php?lveroot=$lveroot&path=$path&FQCN=$cellname'>$utask: $status</a><br>\n";
            }
        }
        else {
            $cnt=0;
            if($type == 3){
                echo "<TABLE BORDER=\"1\" WIDTH=\"100%\" CELLPADDING=\"1\" CELLSPACING=\"1\">\n";
                echo "<TR BGCOLOR=\"#CCCCFF\" >\n";
                echo "</TR>\n";
                echo "<TD><a href='rteSummary.php?FQCN=$cellname'>SUMMARY OF RTE RESULTS</a></TD>";
                echo "</TABLE>\n";
                echo "<br>\n";
            }
            foreach ($patharray[$task] as $ndx => $path) {
                $pathsplit=preg_split(":/:", $path);
                $pathcount=count($pathsplit);
                $status=$statusarray[$task][$ndx];
                 
                if ($type == 1) {
                    $view=$pathsplit[$pathcount-6];
                    $mode=$pathsplit[$pathcount-5];
                    $corner=$pathsplit[$pathcount-3];
                    $voltage=$pathsplit[$pathcount-2];
                    $temp=$pathsplit[$pathcount-1];
                }
                elseif ($type == 2) {
                    $view=$pathsplit[$pathcount-9];
                    $mode=$pathsplit[$pathcount-8];
                    $environment=$pathsplit[$pathcount-6];
                    $corner=$pathsplit[$pathcount-5];
                    $voltage=$pathsplit[$pathcount-4];
                    $temp=$pathsplit[$pathcount-3];
                    $duration=$pathsplit[$pathcount-2];
                    $timeddelay=$pathsplit[$pathcount-1];
                }
                elseif ($type == 3) {
                    $view=$pathsplit[$pathcount-7];
                    $mode=$pathsplit[$pathcount-6];
                    $corner=$pathsplit[$pathcount-4];
                    $voltage=$pathsplit[$pathcount-3];
                    $temp=$pathsplit[$pathcount-2];
                    $environment=$pathsplit[$pathcount-1];
                }
                if ($cnt==0) {
                    echo "<TABLE BORDER=\"1\" WIDTH=\"100%\" CELLPADDING=\"1\" CELLSPACING=\"1\">\n";
                    echo "<TR BGCOLOR=\"#CCCCFF\" >\n";
                    if ($type==1) {
                        echo "<TD>$utask</TD><TD>View Name</TD><TD>Mode</TD><TD>Process Corner</TD><TD>Voltage</TD><TD>Temperature</TD>\n";
                    }
                    elseif ($type==2) {
                        echo "<TD>$utask</TD><TD>View Name</TD><TD>Mode</TD><TD>Environment</TD><TD>Process Corner</TD><TD>Voltage</TD><TD>Temperature</TD><TD>Duration</TD><TD>Timed Delay</TD>\n";
                    }
                    elseif ($type==3) {
                        echo "<TD>$utask</TD><TD>View Name</TD><TD>Mode</TD><TD>Process Corner</TD><TD>Voltage</TD><TD>Temperature</TD><TD>Environment</TD>\n";
                    }
                }
                $cnt++;
                echo "<TR BGCOLOR=\"$color[$status]\">\n";
                if ($type == 1) {
                    echo "<TD><a href='$task.php?lveroot=$lveroot&path=$path&FQCN=$cellname'>$status</a></TD>";
                    echo "<TD>$view</TD>";
                    echo "<TD>$mode</TD>";
                    echo "<TD>$corner</TD>";
                    echo "<TD>$voltage</TD>";
                    echo "<TD>$temp</TD>";
                    echo "</TR>\n";
                }
                elseif ($type==2) {
                    echo "<TD><a href='$task.php?lveroot=$lveroot&path=$path&FQCN=$cellname'>$status</a></TD>";
                    echo "<TD>$view</TD>";
                    echo "<TD>$mode</TD>";
                    echo "<TD>$environment</TD>";
                    echo "<TD>$corner</TD>";
                    echo "<TD>$voltage</TD>";
                    echo "<TD>$temp</TD>";
                    echo "<TD>$duration</TD>";
                    echo "<TD>$timeddelay</TD>\n";
                    echo "</TR>\n";
                }
                elseif ($type==3) {
                    echo "<TD><a href='$task.php?lveroot=$lveroot&path=$path&FQCN=$cellname'>$status</a></TD>";
                    echo "<TD>$view</TD>";
                    echo "<TD>$mode</TD>";
                    echo "<TD>$corner</TD>";
                    echo "<TD>$voltage</TD>";
                    echo "<TD>$temp</TD>";
                    echo "<TD>$environment</TD>";
                    echo "</TR>\n";
                }
            }
        }
        echo "</TABLE>\n";
        echo "<br>\n";
    }
    else {
        if ($type == 0) {
            echo "$utask: NOT_TESTED<br><br>\n";
        }
    }
}
footer();
?>
