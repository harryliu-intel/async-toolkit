<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
    "http://www.w3.org/TR/REC-html40/loose.dtd">
<?php
include "common.php";
globalheader("Analog Verification Statistics");
if (!isset($dbmstat)) {
    $dbmstat=array("mtime" => 0);
    if (is_file("$diskroot/$lveroot/lvedb.db"))
        $dbmstat=stat("$diskroot/$lveroot/lvedb.db");
}
$dbmtime=strftime("%a %b %d %X %Z %Y", $dbmstat["mtime"]);
echo "Last Modified: $dbmtime<br>\n";
$q=dbquery("select * from $cellstable");
$statistics=array();
$cellcnt=0;
while ($d=dbfetchrow($q)) {
    $cellcnt++;
    foreach ($tasklist as $task => $key) {
        if (isset($d[$task])) {
            if (! isset($statistics[$task][$d[$task]]))
                $statistics[$task][$d[$task]]=0;
            $statistics[$task][$d[$task]]++;
        }
    }
}
foreach ($tasklist as $task => $key) {
    $utask=strtoupper($task);
    echo "<p>";
    echo "<center><h3>Results Statistics for $utask</h3></center>\n";
    echo "<TABLE BORDER=\"0\" WIDTH=\"100%\" CELLPADDING=\"2\" CELLSPACING=\"3\" class=\"yellow\" >\n";
    echo "<TBODY>\n";
    echo "<tr BGCOLOR=\"#CCCCFF\" ><td  class=\"yellow\" > <b>Result Category</b> </td><td  class=\"yellow\" > <b>#cells</b> </td><td  class=\"yellow\" > <b>%cells</b> </td></tr>\n";
    foreach ($statistics[$task] as $status => $v) {
        $cnt=$statistics[$task][$status];
        $percent=sprintf("%.2f", 100*$cnt/$cellcnt);
        echo "<tr><td class=\"yellow\" > $status </td><td class=\"yellow\" > $cnt </td><td class=\"yellow\" > $percent </td></tr>\n";
    }
    echo "</TABLE>\n";
}
footer();
?>
