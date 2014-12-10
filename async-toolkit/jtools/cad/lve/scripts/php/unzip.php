<?php
header('Content-type: text/plain');
if (isset($_REQUEST['file'])) {
    $file=$_REQUEST['file'];
    system("/bin/gunzip -c '$file'");
}
?>
