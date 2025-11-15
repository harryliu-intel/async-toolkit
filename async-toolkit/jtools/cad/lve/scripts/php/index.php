<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN" "http://www.w3.org/TR/REC-html40/loose.dtd>
<?php
require 'common.php';
$statusstyle = array ( "PASS" => "background: $color[PASS];",
			 "NA" => "background: $color[NA];",
                         "FAIL" => "background: $color[FAIL];",
                         "FAIL_NEWBUMP" => "background: $color[FAIL_NEWBUMP];",
                         "FAIL_MNOISE" => "background: $color[FAIL_MNOISE];",
                         "WARNING" => "background: $color[WARNING]; font: 90%;",
                         "NOT_TESTED" => "background: $color[NOT_TESTED]; font: 65% bold;",
			 "INCOMPLETE" => "background: $color[INCOMPLETE]; font: 65% bold;",
                         "SIGNOFF" => "background: $color[SIGNOFF]; font: 90%;",
                         "OTHER" => "background: $color[OTHER];"
                         );

$list=array();
$ulist=array();
if (file_exists("$dirfile")) {
    $handle=fopen("$dirfile", "r");
    while($handle !== false and !feof($handle)) {
        $item = str_replace("\n", "", fgets($handle));
        $file = "$diskroot/$item";
        if ($item != "" && is_readable ($file)) {
            $stat = stat($file);
            $uid = $stat[4];
            $list[] = "$item";
            $ulist[] = $uid;
        }
    }
    fclose($handle);
}
globalheader("Analog Verification");


?>
<?php
//hard-coded include files for invoking cast_query; in the future this
//should be filled in by some project config file or something of the sort!
$cast_dir="/p/rrc/checkout/alpine/cast";
$spec_dir="/p/rrc/checkout/alpine/spec";
$cast_query="/p/rrc/tools/bin/fulcrum cast_query --max-heap-size=768M";
$elements = array();

//provide search capability for tests and status

$filter="";
$showlib="";
if (isset($_REQUEST['filter'])) $filter=$_REQUEST['filter'];
elseif (isset($_SESSION['filter'])) $filter=$_SESSION['filter'];
//read and set the option variables here
if(isset($_POST["JLVS"]) || isset($_POST["EXTRACT"]) ||
   isset($_POST["HDRC"]) || isset($_POST["FRC"]) ||
   isset($_POST["ALINT"]) || isset($_POST["ASPICE"]) ||
   isset($_POST["HLVS"]) || isset($_POST["LIB"]) ||
   isset($_POST["HSIM"]) || isset($_POST["HSPICE"]) ||
   isset($_POST["WARNING"]) || isset($_POST["RTE"]) ||
   isset($_POST["ASTA"]) || isset($_POST["TOTEM"]) || isset($_POST["XA"])) {

    if (isset($_POST["JLVS"])) $jlvs = $_POST["JLVS"]; else $jlvs="";
    if (isset($_POST["HLVS"])) $hlvs = $_POST["HLVS"]; else $hlvs="";
    if (isset($_POST["HSPICE"])) $hspice = $_POST["HSPICE"]; else $hspice="";
    if (isset($_POST["HSIM"])) $hsim = $_POST["HSIM"]; else $hsim="";
    if (isset($_POST["XA"])) $xa = $_POST["XA"]; else $xa="";
    if (isset($_POST["RTE"])) $rte = $_POST["RTE"]; else $rte="";
    if (isset($_POST["EXTRACT"])) $extract = $_POST["EXTRACT"]; else $extract="";
    if (isset($_POST["HDRC"])) $hdrc = $_POST["HDRC"]; else $hdrc="";
    if (isset($_POST["FRC"])) $frc = $_POST["FRC"]; else $frc="";
    if (isset($_POST["ALINT"])) $alint = $_POST["ALINT"]; else $alint="";
    if (isset($_POST["ASPICE"])) $aspice = $_POST["ASPICE"]; else $aspice="";
    if (isset($_POST["LIB"])) $lib = $_POST["LIB"]; else $lib="";
    if (isset($_POST["ASTA"])) $asta = $_POST["ASTA"]; else $asta="";
    if (isset($_POST["TOTEM"])) $totem = $_POST["TOTEM"]; else $totem="";
    if (isset($_POST["PASS"])) $pass = $_POST["PASS"]; else $pass="";
    if (isset($_POST["WARNING"])) $warning = $_POST["WARNING"]; else $warning="";
    if (isset($_POST["NA"])) $na = $_POST["NA"]; else $na="";
    if (isset($_POST["FAIL"])) $fail = $_POST["FAIL"]; else $fail="";
    if (isset($_POST["FAIL_NEWBUMP"])) $fail_newnump = $_POST["FAIL_NEWBUMP"]; else $fail_newnump="";
    if (isset($_POST["FAIL_MNOISE"])) $fail_mnoise = $_POST["FAIL_MNOISE"]; else $fail_mnoise="";
    if (isset($_POST["NOT_TESTED"])) $not_tested = $_POST["NOT_TESTED"]; else $not_tested="";
    if (isset($_POST["SIGNOFF"])) $signoff = $_POST["SIGNOFF"]; else $signoff="";
    if (isset($_POST["OTHER"])) $other = $_POST["OTHER"]; else $other="";
    if (isset($_POST["HIER"])) $hier = $_POST["HIER"]; else $hier="";
}
elseif(isset($_SESSION["JLVS"]) || isset($_SESSION["EXTRACT"]) ||
   isset($_SESSION["HDRC"]) || isset($_SESSION["FRC"]) ||
   isset($_SESSION["ALINT"]) || isset($_SESSION["ASPICE"]) ||
   isset($_SESSION["HLVS"]) || isset($_SESSION["LIB"]) ||
   isset($_SESSION["HSPICE"]) || isset($_SESSION["HSIM"]) ||
   isset($_SESSION["WARNING"]) || isset($_SESSION["RTE"]) ||
   isset($_SESSION["ASTA"]) || isset($_SESSION["TOTEM"]) || isset($_SESSION["XA"])) {

    if (isset($_SESSION["JLVS"])) $jlvs = $_SESSION["JLVS"]; else $jlvs="";
    if (isset($_SESSION["HLVS"])) $hlvs = $_SESSION["HLVS"]; else $hlvs="";
    if (isset($_SESSION["HSPICE"])) $hspice = $_SESSION["HSPICE"]; else $hspice="";
    if (isset($_SESSION["HSIM"])) $hsim = $_SESSION["HSIM"]; else $hsim="";
    if (isset($_SESSION["XA"])) $xa = $_SESSION["XA"]; else $xa="";
    if (isset($_SESSION["RTE"])) $rte = $_SESSION["RTE"]; else $rte="";
    if (isset($_SESSION["EXTRACT"])) $extract = $_SESSION["EXTRACT"]; else $extract="";
    if (isset($_SESSION["HDRC"])) $hdrc = $_SESSION["HDRC"]; else $hdrc="";
    if (isset($_SESSION["FRC"])) $frc = $_SESSION["FRC"]; else $frc="";
    if (isset($_SESSION["ALINT"])) $alint = $_SESSION["ALINT"]; else $alint="";
    if (isset($_SESSION["ASPICE"])) $aspice = $_SESSION["ASPICE"]; else $aspice="";
    if (isset($_SESSION["LIB"])) $lib = $_SESSION["LIB"]; else $lib="";
    if (isset($_SESSION["ASTA"])) $asta = $_SESSION["ASTA"]; else $asta="";
    if (isset($_SESSION["TOTEM"])) $totem = $_SESSION["TOTEM"]; else $totem="";
    if (isset($_SESSION["PASS"])) $pass = $_SESSION["PASS"]; else $pass="";
    if (isset($_SESSION["WARNING"])) $warning = $_SESSION["WARNING"]; else $warning="";
    if (isset($_SESSION["NA"])) $na = $_SESSION["NA"]; else $na="";
    if (isset($_SESSION["FAIL"])) $fail = $_SESSION["FAIL"]; else $fail="";
    if (isset($_SESSION["FAIL_NEWBUMP"])) $fail_newnump = $_SESSION["FAIL_NEWBUMP"]; else $fail_newnump="";
    if (isset($_SESSION["FAIL_MNOISE"])) $fail_mnoise = $_SESSION["FAIL_MNOISE"]; else $fail_mnoise="";
    if (isset($_SESSION["NOT_TESTED"])) $not_tested = $_SESSION["NOT_TESTED"]; else $not_tested="";
    if (isset($_SESSION["SIGNOFF"])) $signoff = $_SESSION["SIGNOFF"]; else $signoff="";
    if (isset($_SESSION["OTHER"])) $other = $_SESSION["OTHER"]; else $other="";
    if (isset($_SESSION["HIER"])) $hier = $_SESSION["HIER"]; else $hier="";
}
else {
    $jlvs = "JLVS";
    $hlvs = "HLVS";
    $hsim = "HSIM";
    $xa = "XA";
    $hspice = "HSPICE";
    $rte = "RTE";
    $extract = "EXTRACT";
    $hdrc = "HDRC";
    $frc = "FRC";
    $alint = "ALINT";
    $aspice = "ASPICE";
    $lib = "LIB";
    $asta = "ASTA";
    $totem = "TOTEM";
    $pass = "PASS";
    $warning="WARNING";
    $na = "NA";
    $fail = "FAIL";
    $fail_newbump = "FAIL_NEWBUMP";
    $fail_mnoise = "FAIL_MNOISE";
    $not_tested = "NOT_TESTED";
    $signoff = "SIGNOFF";
    $other = "OTHER";
    $hier = "";
}
$_SESSION["JLVS"] = $jlvs;
$_SESSION["HLVS"] = $hlvs;
$_SESSION["HSPICE"] = $hspice;
$_SESSION["HSIM"] = $hsim;
$_SESSION["XA"] = $xa;
$_SESSION["RTE"] = $rte;
$_SESSION["EXTRACT"] = $extract;
$_SESSION["HDRC"] = $hdrc;
$_SESSION["FRC"] = $frc;
$_SESSION["ALINT"] = $alint;
$_SESSION["ASPICE"] = $aspice;
$_SESSION["LIB"] = $lib;
$_SESSION["ASTA"] = $asta;
$_SESSION["TOTEM"] = $totem;
$_SESSION["PASS"] = $pass;
$_SESSION["WARNING"] = $warning;
$_SESSION["NA"] = $na;
$_SESSION["FAIL"] = $fail;
$_SESSION["FAIL_NEWBUMP"] = $fail_newbump;
$_SESSION["FAIL_MNOISE"] = $fail_mnoise;
$_SESSION["NOT_TESTED"] = $not_tested;
$_SESSION["SIGNOFF"] = $signoff;
$_SESSION["OTHER"] = $other;
$_SESSION["HIER"] = $hier;

if ($filter !== "") $_SESSION["filter"]=$filter;
else unset($_SESSION["filter"]);
if (isset($_REQUEST['showlib'])) $showlib=$_REQUEST['showlib'];

//this section is meant to be used by static pages to leverage
//all the search features of this page
$LinkArgs=0;
if($LinkArgs > 0){
  if($Jlvs > 0)$jlvs = "JLVS"; else $jlvs = "";
  if($Hlvs > 0)$hlvs = "HLVS"; else $hlvs = "";
  if($Hspice > 0)$hspice = "HSPICE"; else $hspice = "";
  if($Hsim > 0)$hsim = "HSIM"; else $hsim = "";
  if($Xa > 0)$xa = "XA"; else $xa = "";
  if($Rte > 0)$rte = "RTE"; else $rte = "";
  if($Extract >0)$extract="EXTRACT"; else $extract = "";
  if($Hdrc > 0)$hdrc="HDRC"; else $hdrc = "";
  if($Frc > 0)$frc="FRC"; else $frc = "";
  if($Alint > 0)$alint="ALINT"; else $alint = "";
  if($Aspice > 0)$aspice="ASPICE"; else $aspice = "";
  if($Lib > 0)$lib="LIB"; else $lib = "";
  if($Asta > 0)$asta="ASTA"; else $asta = "";
  if($Totem > 0)$totem="TOTEM"; else $totem = "";
  if($Pass > 0)$pass="PASS"; else $pass = "";
  if($Warning > 0)$warning="WARNING"; else $warning = "";
  if($Na > 0)$na="NA"; else $na = "";
  if($Fail > 0)$fail="FAIL"; else $fail = "";
  if($Nottested > 0)$not_tested="NOT_TESTED"; else $not_tested = "";
  if($Signoff > 0)$signoff="SIGN_OFF"; else $signoff = "";
  $other="";
}

//set check box variable for different test categories
if($jlvs == "JLVS"){$jlvs_checked = "CHECKED"; } else {$jlvs_checked = "";}
if($hlvs == "HLVS"){$hlvs_checked = "CHECKED"; } else {$hlvs_checked = "";}
if($hsim == "HSIM"){$hsim_checked = "CHECKED"; } else {$hsim_checked = "";}
if($xa == "XA"){$xa_checked = "CHECKED"; } else {$xa_checked = "";}
if($hspice == "HSPICE"){$hspice_checked = "CHECKED"; } else {$hspice_checked = "";}
if($rte == "RTE"){$rte_checked = "CHECKED"; } else {$rte_checked = "";}
if($extract == "EXTRACT"){$extract_checked = "CHECKED"; } else {$extract_checked = "";}
if($hdrc == "HDRC"){$hdrc_checked = "CHECKED"; } else {$hdrc_checked = "";}
if($frc == "FRC"){$frc_checked = "CHECKED"; } else {$frc_checked = "";}
if($alint == "ALINT"){$alint_checked = "CHECKED"; } else {$alint_checked = "";}
if($aspice == "ASPICE"){$aspice_checked = "CHECKED"; } else {$aspice_checked = "";}
if($lib == "LIB"){$lib_checked = "CHECKED"; } else {$lib_checked = "";}
if($asta == "ASTA"){$asta_checked = "CHECKED"; } else {$asta_checked = "";}
if($totem == "TOTEM"){$totem_checked = "CHECKED"; } else {$totem_checked = "";}

//set check box varibles for different status categories
if($pass == "PASS"){$pass_checked = "CHECKED"; } else {$pass_checked = "";}
if($warning == "WARNING"){$warning_checked = "CHECKED"; }
    else {$warning_checked = "";}
if($na == "NA"){$na_checked = "CHECKED"; } else {$na_checked = "";}
if($fail == "FAIL"){$fail_checked = "CHECKED"; } else {$fail_checked = "";}
if($not_tested == "NOT_TESTED"){$not_tested_checked = "CHECKED"; }
    else {$not_tested_checked = "";}
if($signoff == "SIGNOFF"){$signoff_checked = "CHECKED"; }
    else {$signoff_checked = "";}
if($other == "OTHER"){$other_checked = "CHECKED"; } else {$other_checked = "";}
if($hier == "HIER"){$hier_checked = "CHECKED"; } else {$hier_checked = "";}

// =========== START SEARCH/CONFIG ===========
print "<FORM ACTION=index.php METHOD=POST >\n";
print "<TABLE BORDER=\"0\"  WIDTH=\"100%\" CELLPADDING=\"2\" CELLSPACING=\"3\" >\n";
print "<TR><TD colspan=2>";
print "<B>LVE ROOT :</B> <select name='lveroot' OnChange='submit()'>";
$match=0;
$rootisuser=($lveroot === $authuser) ? 1 : 0;
$unmatched=array();
foreach ($list as $value) {
    if ( $value === $lveroot ) {
        print "<option selected>$value";
        $match=1;
    }
    elseif ($rootisuser and substr($value,0,strlen($authuser)+1) == "$authuser/") {
        $match=1;
        print "<option selected>$value";
        $lveroot=$value;
        $rootisuser=0;
        $_SESSION["lveroot"]=$lveroot;
    }
    elseif (substr($value,0,strlen($authuser)+1) == "$authuser/") {
        print "<option>$value";
    }
    elseif (substr($value,0,strlen("lve")+1) == "lve/") {
        print "<option>$value";
    }
    elseif ($value != "") {
        $unmatched[]=$value;
    }
}
if (! $match) {
    print "<option selected>$lveroot";
}
sort($unmatched);
foreach ($unmatched as $value) {
    print "<option>$value";
}
if ($showlib != "")
    $escaped_filtersubmit = str_replace("(", "\\(", $showlib);
else
    $escaped_filter = str_replace("(", "\\(", $filter);
$escaped_filter = str_replace(")", "\\)", $escaped_filter);
$escaped_filter = str_replace("{", "\\{", $escaped_filter);
$escaped_filter = str_replace("}", "\\}", $escaped_filter);
print "</select>";
print "&nbsp; or Type Root: <input type=text value=\"\" name=\"tlveroot\" size=32>";
print "</TD></TR>";
print "<TR><td>\n";
print "<B>FQCN:</B> \n";
print "<INPUT TYPE=TEXT  NAME=\"filter\" VALUE=\"$filter\" SIZE=64 MAXLENGTH=1024 >\n";
print "<P></td><td>\n";
print "<INPUT TYPE=CHECKBOX $hier_checked NAME=HIER VALUE=\"HIER\" > <b>Hierarchical Descent </b><BR>\n";
print "</TD>\n";
print "</TABLE><P>\n";
print "<TABLE BORDER=\"0\" WIDTH=\"1000\" >\n";
print "<tr><td colspan=13>\n";
print "<B><font size=\"-1\">Select Result Category:</font></B></td>\n";
print "<tr>\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $jlvs_checked NAME=JLVS VALUE=\"JLVS\" >-JLVS\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $hlvs_checked NAME=HLVS VALUE=\"HLVS\" >-HLVS\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $extract_checked NAME=EXTRACT VALUE=\"EXTRACT\" >-EXTRACT\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $hdrc_checked NAME=HDRC VALUE=\"HDRC\" >-HDRC\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $frc_checked NAME=FRC VALUE=\"FRC\" >-FRC\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $alint_checked NAME=ALINT VALUE=\"ALINT\" >-ALINT\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $aspice_checked NAME=ASPICE VALUE=\"ASPICE\" >-ASPICE\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $lib_checked NAME=LIB VALUE=\"LIB\" >-LIB\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $asta_checked NAME=ASTA VALUE=\"ASTA\" >-ASTA\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $totem_checked NAME=TOTEM VALUE=\"TOTEM\" >-TOTEM\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $hsim_checked NAME=HSIM VALUE=\"HSIM\" >-HSIM\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $xa_checked NAME=XA VALUE=\"XA\" >-XA\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $hspice_checked NAME=HSPICE VALUE=\"HSPICE\" >-HSPICE\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $rte_checked NAME=RTE VALUE=\"RTE\" >-RTE\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $rte_checked NAME=RTE VALUE=\"RTE\" >-RTE\n";
print "</TABLE>\n";
print "\n";
print "<TABLE BORDER=\"0\" CELLPADDING=\"2\" CELLSPACING=\"3\" >\n";
print "<tr><td colspan=7>\n";
print "<B><font size=\"-1\">Select Result Status:</font></B></td>\n";
print "<tr>";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $pass_checked NAME=PASS VALUE=\"PASS\" >-PASS\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $na_checked NAME=NA VALUE=\"NA\" >-NA\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $fail_checked NAME=FAIL VALUE=\"FAIL\" >-FAIL\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $not_tested_checked NAME=NOT_TESTED VALUE=\"NOT_TESTED\" >-NOT_TESTED/INCOMPLETE\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $signoff_checked NAME=SIGNOFF VALUE=\"SIGNOFF\" >-SIGNOFF\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $warning_checked NAME=WARNING VALUE=\"WARNING\" >-WARNING\n";
print "<td style=\"font-size: 10pt;\"><INPUT TYPE=CHECKBOX $other_checked NAME=OTHER VALUE=\"OTHER\" >-OTHER\n";
print "</TABLE>\n";
print "<center><INPUT TYPE=SUBMIT VALUE=\"I'm Feeling Lucky\" > </center>\n";
print "</FORM>";
print "<HR>\n";

//if hierarchical decent is specified run cast_query
if($hier == "HIER"){
  $output_file=tempnam("/tmp","lve");
  $cast_query_cmd="$cast_query --cast-path=$cast_dir:$spec_dir";
  $cast_query_cmd="$cast_query_cmd --output=$output_file --max-heap-size=1500M";
  $cast_query_cmd="$cast_query_cmd --task=subcells --routed --cell=\"$filter\"";
  $handle = popen("$cast_query_cmd 2>/dev/null", 'r');
  $output="";
  while($handle !== false and !feof($handle)) {
    $output .= fgets($handle);
    $output .= "<br>";
  }
  pclose($handle);
  if(filesize($output_file) == 0){
    print "<b>cast_query failed: your request did not make sense!</b><p>\n";
    print "Make sure your cell $filter exits in $cast_dir:$spec_dir<br>";
    print "Hopefully the cast tree isn't broken!<p>";
    print "$cast_query_cmd";
    print "$output";
  }
  else {
    $query_data = fopen("$output_file", "r");
    //populate table for pickings
    while($query_data !== false and !feof($query_data)){
      $element = fgets($query_data);
      $element = chop($element);
      if($element != ""){
	$elements[$element] = 1;
      }
    }
    fclose($query_data);
  }
  if (is_file($output_file)) unlink($output_file);
}

function meets_criteria($arg, $pass, $na, $fail, $fail_newbump, $fail_mnoise, $not_tested, $signoff, $other, $warning){
    if($arg == $pass)return true;
    if($arg == $na)return true;
    if($arg == $fail)return true;
    if($arg == $fail_newbump)return true;
    if($arg == $fail_mnoise)return true;
    if($arg == $not_tested)return true;
    if($arg == $signoff)return true;
    if($arg == $other)return true;
    if($arg == $warning)return true;
   return false;
}

$hdrorder = array("FQCN", "JLVS", "HLVS", "EXTRACT", "HDRC", "FRC", "ALINT", "ASPICE", "LIB", "ASTA", "TOTEM", "HSIM", "XA", "HSPICE", "RTE");

// we have to break up the single string because
// something fails over 6000000 characters.
$html_entry_ndx=0;
$html_entry[$html_entry_ndx] = "";

//open the data file read contents and construct the desired table
$html_entry[$html_entry_ndx] .= "<TABLE BORDER=1 CELLPADDING=2 CELLSPACING=3 class=\"yellow\" >\n";
$html_entry[$html_entry_ndx] .= "<TBODY>\n";

//generate column headings
function write_header ($jlvs,$hlvs,$extract,$hdrc,$frc,$alint,$aspice,$lib,$asta,$totem,$hsim,$xa,$hspice,$rte,$liborcell){
  global $hdrorder;
  $html_entry = "<tr BGCOLOR=\"#CCCCFF\" class=\"yellow\">\n";
  if (isset($liborcell) and $liborcell == true)
      $html_entry .= "<td class=\"yellow\" <b>LIB (count) or CELL</b> </td>";
  else
      $html_entry .= "<td class=\"yellow\" <b>CELL (yellow=running)</b> </td>";
  // get order from actual index file
  for ($i = 1; $i < count($hdrorder); $i++) {
      $lc = strtolower($hdrorder[$i]);
      $do = 0;
      eval ("if ( \$$lc == \"".$hdrorder[$i]."\" ) \$do = 1;");
      if ($do)
          $html_entry .= "<td class=\"yellow\" <b>".$hdrorder[$i]."</b> </td>";
  }
  $html_entry .= "</tr>";
  return $html_entry;
}
$html_entry[$html_entry_ndx] .= write_header($jlvs,$hlvs,$extract,$hdrc,$frc,$alint,$aspice,$lib,$asta,$totem,$hsim,$xa,$hspice,$rte,false);
$cells_matched = 0;
$libs_matched=0;
//open data file for read and start writing table
$data = NULL;
if ($db) {
    if ($filter != "" and $hier != "HIER") {
        $data = dbquery("select * from $cellstable where fqcn like '%$filter%' order by fqcn");
    }
    else {
        $data = dbquery("select * from $cellstable order by fqcn");
    }
}
else {
    print "No database found.<br>";
}
if ( ! $db or $data === NULL) {
    if ($db) print "No cells in database<br>";
}
else {
    $s_jlvs = array();
    $s_hlvs = array();
    $s_hsim = array();
    $s_xa = array();
    $s_hspice = array();
    $s_rte = array();
    $s_extract = array();
    $s_hdrc = array();
    $s_frc = array();
    $s_alint = array();
    $s_aspice = array();
    $s_lib = array();
    $s_asta = array();
    $s_totem = array();
    $libs=array();
    $libline=array();
    while ( $buffer = dbfetchrow($data)) {

      $search_match = false;
      $tmp_html_entry = "";
      $is_tr=0;

        if ( ! isset($buffer["rte"])) $buffer["rte"]="NOT_TESTED";
        //set all the variables read here
        $f_cell = $buffer["fqcn"];
        $f_jlvs = $buffer["jlvs"];
        $f_hlvs = $buffer["hlvs"];
        $f_hsim = $buffer["hsim"];
        $f_xa = $buffer["xa"];
        $f_hspice = $buffer["hspice"];
        $f_rte = $buffer["rte"];
        $f_extract = $buffer["extract"];
        $f_hdrc = $buffer["hdrc"];
        $f_frc = $buffer["frc"];
        $f_alint = $buffer["alint"];
        $f_aspice = $buffer["aspice"];
        $f_lib = $buffer["lib"];
        $f_asta = $buffer["asta"];
        $f_totem = $buffer["totem"];

        if(((preg_match("/$escaped_filter/",$f_cell) > 0) && $hier !="HIER" ) ||
           ($hier == "HIER" && isset($elements[$f_cell]))){

          if($hier == "HIER"){
            $elements[$f_cell] = 0;
          }

          $fa_cell = $f_cell;
          $thislib=preg_replace("/\.[^\.]+\.[^\.]+$/", "", $f_cell);
          $thislockfile="$diskroot/$lveroot/".preg_replace("/\./", "/", $f_cell)."/cell.lock";
          $isunlocked=true;
          if ( is_file($thislockfile)) {
              $thislockhandle=fopen($thislockfile, "r");
              $isunlocked=1;
              if($thislockhandle) {
                  $isunlocked=flock($thislockhandle, LOCK_EX + LOCK_NB);
                  fclose($thislockhandle);
              }
          }
          $f_link = "cell.php?lveroot=$lveroot&FQCN=$fa_cell";
          $isunlocked = $isunlocked ? "while" : "yellow";
          if ($showlib !== "" and $thislib != $showlib) continue;
          if (! isset($s_jlvs[$thislib])) $s_jlvs[$thislib] = array();
          if (! isset($s_hlvs[$thislib])) $s_hlvs[$thislib] = array();
          if (! isset($s_rte[$thislib])) $s_rte[$thislib] = array();
          if (! isset($s_extract[$thislib])) $s_extract[$thislib] = array();
          if (! isset($s_hdrc[$thislib])) $s_hdrc[$thislib] = array();
          if (! isset($s_frc[$thislib])) $s_frc[$thislib] = array();
          if (! isset($s_alint[$thislib])) $s_alint[$thislib] = array();
          if (! isset($s_aspice[$thislib])) $s_aspice[$thislib] = array();
          if (! isset($s_lib[$thislib])) $s_lib[$thislib] = array();
          if (! isset($s_asta[$thislib])) $s_asta[$thislib] = array();
          if (! isset($s_hsim[$thislib])) $s_hsim[$thislib] = array();
          if (! isset($s_xa[$thislib])) $s_xa[$thislib] = array();
          if (! isset($s_hspice[$thislib])) $s_hspice[$thislib] = array();
          if (! isset($s_totem[$thislib])) $s_totem[$thislib] = array();
          $tmp_html_entry .= "<tr><td class=\"yellow\" style=\"background: $isunlocked;\"> <a href=\"$f_link\">$fa_cell</a></td>";

          if($jlvs == "JLVS"){
            $search_match |= meets_criteria($f_jlvs,$pass, $na, $fail, $fail_newbump, $fail_mnoise, $not_tested, $signoff, $other, $warning);
            $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_jlvs]'>$f_jlvs</td>";
            if (isset($s_jlvs[$thislib][$f_jlvs])) $s_jlvs[$thislib][$f_jlvs]++; else $s_jlvs[$thislib][$f_jlvs]=1;
          }

          if($hlvs == "HLVS"){
            $search_match |= meets_criteria($f_hlvs, $pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
            $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_hlvs]'>$f_hlvs</td>";
            if (isset($s_hlvs[$thislib][$f_hlvs])) $s_hlvs[$thislib][$f_hlvs]++; else $s_hlvs[$thislib][$f_hlvs]=1;
          }

          if($extract == "EXTRACT"){
            $search_match |= meets_criteria($f_extract, $pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
            $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_extract]'>$f_extract</td>";
            if (isset($s_extract[$thislib][$f_extract])) $s_extract[$thislib][$f_extract]++; else $s_extract[$thislib][$f_extract]=1;
          }

          if($hdrc == "HDRC"){
             $search_match |= meets_criteria($f_hdrc,$pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
             $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_hdrc]'>$f_hdrc</td>";
            if (isset($s_hdrc[$thislib][$f_hdrc])) $s_hdrc[$thislib][$f_hdrc]++; else $s_hdrc[$thislib][$f_hdrc]=1;
          }

          if($frc == "FRC"){
             $search_match |= meets_criteria($f_frc,$pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
             $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_frc]'>$f_frc</td>";
            if (isset($s_frc[$thislib][$f_frc])) $s_frc[$thislib][$f_frc]++; else $s_frc[$thislib][$f_frc]=1;
          }
          if($alint == "ALINT"){
             $search_match |= meets_criteria($f_alint,$pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
             $tmp_html_entry .="<td class=\"yellow\" style='$statusstyle[$f_alint]'>$f_alint</td>";
            if (isset($s_alint[$thislib][$f_alint])) $s_alint[$thislib][$f_alint]++; else $s_alint[$thislib][$f_alint]=1;
          }
          if($aspice == "ASPICE"){
             $search_match |= meets_criteria($f_aspice,$pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
             $tmp_html_entry.="<td class=\"yellow\" style='$statusstyle[$f_aspice]'>$f_aspice</td>";
            if (isset($s_aspice[$thislib][$f_aspice])) $s_aspice[$thislib][$f_aspice]++; else $s_aspice[$thislib][$f_aspice]=1;
          }
          if($lib == "LIB"){
             $search_match |= meets_criteria($f_lib,$pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
             $tmp_html_entry.="<td class=\"yellow\" style='$statusstyle[$f_lib]'>$f_lib</td>";
            if (isset($s_lib[$thislib][$f_lib])) $s_lib[$thislib][$f_lib]++; else $s_lib[$thislib][$f_lib]=1;
          }
          if($asta == "ASTA"){
             $search_match |= meets_criteria($f_asta,$pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
             $tmp_html_entry.="<td class=\"yellow\" style='$statusstyle[$f_asta]'>$f_asta</td>";
            if (isset($s_asta[$thislib][$f_asta])) $s_asta[$thislib][$f_asta]++; else $s_asta[$thislib][$f_asta]=1;
          }
          if($totem == "TOTEM"){
             $search_match |= meets_criteria($f_totem,$pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
             $tmp_html_entry.="<td class=\"yellow\" style='$statusstyle[$f_totem]'>$f_totem</td>";
            if (isset($s_totem[$thislib][$f_totem])) $s_totem[$thislib][$f_totem]++; else $s_totem[$thislib][$f_totem]=1;
          }
          if($hsim == "HSIM"){
            $search_match |= meets_criteria($f_hsim, $pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
            $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_hsim]'>$f_hsim</td>";
            if (isset($s_hsim[$thislib][$f_hsim])) $s_hsim[$thislib][$f_hsim]++; else $s_hsim[$thislib][$f_hsim]=1;
          }
          if($xa == "XA"){
            $search_match |= meets_criteria($f_xa, $pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
            $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_xa]'>$f_xa</td>";
            if (isset($s_xa[$thislib][$f_xa])) $s_xa[$thislib][$f_xa]++; else $s_xa[$thislib][$f_xa]=1;
          }
          if($hspice == "HSPICE"){
            $search_match |= meets_criteria($f_hspice, $pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
            $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_hspice]'>$f_hspice</td>";
            if (isset($s_hspice[$thislib][$f_hspice])) $s_hspice[$thislib][$f_hspice]++; else $s_hspice[$thislib][$f_hspice]=1;
          }
          if($rte == "RTE"){
            $search_match |= meets_criteria($f_rte, $pass,$na,$fail,$fail_newbump,$fail_mnoise,$not_tested, $signoff, $other, $warning);
            $tmp_html_entry .= "<td class=\"yellow\" style='$statusstyle[$f_rte]'>$f_rte</td>";
            if (isset($s_rte[$thislib][$f_rte])) $s_rte[$thislib][$f_rte]++; else $s_rte[$thislib][$f_rte]=1;
          }
        }

      $tmp_html_entry .= "</tr>\n";
      if($search_match){
        $html_entry[$html_entry_ndx] .= $tmp_html_entry; $cells_matched++;
        if (isset($libs[$thislib])) $libs[$thislib]++; else {$libs[$thislib]=1; $libs_matched++;}
        $libline[$thislib]=$tmp_html_entry;
        if($cells_matched%40 == 0 && $cells_matched/40 >= 1){
          $html_entry_ndx++;
          $html_entry[$html_entry_ndx] = write_header($jlvs,$hlvs,$extract,$hdrc,$frc,$alint,$aspice,$lib,$asta,$totem,$hsim,$xa,$hspice,$rte,false);
        }
      }
    }
}
//end the table
$html_entry[$html_entry_ndx] .= "</TABLE><P>";
if (($filter != "") or ($showlib != "") or ($cells_matched < 60)) $show_cells=true; else $show_cells=false;

if($cells_matched == 0)print "<b>Search Results ... zarro cells found <p></b>\n";
elseif($show_cells and $cells_matched == 1)print "<b>Search Results ... $cells_matched cell found <p></b>\n";
elseif($show_cells) print "<b>Search Results ... $cells_matched cells found <p></b>\n";
elseif(! $show_cells and $libs_matched == 1)print "<b>Search Results ... $libs_matched lib found <p></b>\n";
else print "<b>Search Results ... $libs_matched libs found <p></b>\n";

//argh, I'd rather use a better data structure of this; array() struct sucks!
//ideally we'd unpopulate the array as we determine that they been tested; then
//all we have to do is
$left_overs = "";
$left_overs_num = 0;
if($hier == "HIER"){
  foreach($elements as $key => $value) {
    if($value == 1){
      $left_overs .= "$key<br>";
      $left_overs_num++;
    }
  }
  if($left_overs_num > 0){
    print "<b>No attempt has been made to run these cells ($left_overs_num):</b><p>\n";
    print "$left_overs\n";
    print "<HR>\n";
  }
}

if (($showlib != "") or ($cells_matched < 60)) {
for( $ndx = 0; $ndx <= $html_entry_ndx; $ndx++) {
    print "$html_entry[$ndx]";
    $html_entry[$ndx] = "";
}
}
else {
echo "<table>";
echo write_header($jlvs,$hlvs,$extract,$hdrc,$frc,$alint,$aspice,$lib,$asta,$totem,$hsim,$xa,$hspice,$rte, true);
foreach ($libs as $thislib => $value) {
    $f_link="index.php?lveroot=$lveroot&showlib=$thislib";
    if ($value == 1) {
      echo $libline[$thislib];
    }
    else {
      echo "<tr><td class=\"yellow\"> <a href=\"$f_link\">$thislib&nbsp;($value)</a></td>";
      for ($i = 1; $i < count($hdrorder); $i++) {
        $lc = strtolower($hdrorder[$i]);
        $do = 0;
        eval ("\$status=summarizeStatus(\$s_$lc"."[\$thislib]);");
        eval ("if ( \$$lc == \"".$hdrorder[$i]."\" ) \$do = 1;");
        if ($do === 1)
          echo "<td class=\"yellow\" style=\"$statusstyle[$status]\">$status</td>";
      }
      echo "</tr>\n";
    }
}
echo "</table>\n";
}

//done reading and displaying data; close the file

//end php section
footer();
?>
