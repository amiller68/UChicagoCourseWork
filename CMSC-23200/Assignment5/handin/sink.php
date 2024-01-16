<html>
<head><title>CS 23200/33250 sink</title></head>
<body>
<p>
<?php
$servername = "localhost";
$username = "amiller68";
$password = "wWaterski$68$";
$dbname = "tracking_db";
$cookie_name = "sess_id";


//IF UA is new -> return the new ID
//IF UA is old -> return old ID 
function checkUA($conn, $ua_string)
{
        printf("UA_String: %s\n<br>", $ua_string);
        $ua_hash = md5($ua_string);
        printf("US_Hash: %s\n<br>", $ua_hash);
        $result = $conn->query(
                "SELECT * FROM ua_string WHERE ua_hash='$ua_hash' LIMIT 1");
        $row = $result->fetch_array(MYSQLI_NUM);
        if( $row  !== NULL) {
                echo "UA found <br> \n";
                printf ("%d | %s | %s\n<br>", $row[0], $row[1], $row[2]);
                return $row[0];
        }
        else {
                $ua_sql = "INSERT INTO ua_string (ua_hash, ua_text)
                VALUES ('$ua_hash', '$ua_string')";

                if ($conn->query($ua_sql) === TRUE) {
                        echo "New UA record created successfully <br>";
                } else {
                        echo "Error: " . $sql . "<br>" . $conn->error;
                }
                //Retruns ID of newly inserted UA
                return $conn->insert_id;
        }
}

//Get REMOTE IP
$IP = getenv("REMOTE_ADDR");
echo "IP Addr: " . $IP . "<br>";

//Get User agent string
$ua_string = $_SERVER['HTTP_USER_AGENT'];

//Check if Cookie is Set
if(!isset($_COOKIE[$cookie_name])) {
        //If NO, set one
        echo "Cookie not set; Setting one now <br>";
        $cookie_val = md5($IP . $ua_string . strval(time()));
        setcookie($cookie_name, $cookie_val, time() + 86400, "/");
        $cookie = $cookie_val;
} else{
        echo "Cookie set<br>";
        $cookie = $_COOKIE[$cookie_name];
}

echo "Cookie Value: " . $cookie . "<br>";

$cook_en = (!empty($_POST['cook_en'])) ? $_POST['cook_en'] : "test";
echo "Cookies Enabled: " . $cook_en . "<br>";

if( $cook_en === "false" ) { $cookie = ""; }

$page = (!empty($_POST['page'])) ? $_POST['page'] : "test";
$w_size = (!empty($_POST['w_size'])) ? $_POST['w_size'] : "test";
$dnt_en = (!empty($_POST['dnt_en'])) ? $_POST['dnt_en'] : "test";
$pop_en = (!empty($_POST['pop_en'])) ? $_POST['pop_en'] : "test";
$fonts = (!empty($_POST['fonts'])) ? $_POST['fonts'] : "ASIL";

// Create connection to DB
$conn = new mysqli($servername, $username, $password, $dbname);
// Check connection
if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}

$ua_no = checkUA($conn, $ua_string);

$sql = "INSERT INTO visit (page, cookie, IP, w_size, ua_id, cook_en, dnt_en, pop_en, fonts)
VALUES ('$page','$cookie', '$IP', '$w_size', '$ua_no', '$cook_en', '$dnt_en', '$pop_en', ('$fonts'))";

if ($conn->query($sql) === TRUE) {
    echo "New record created successfully<br>";
} else {
    echo "Error: " . $sql . "<br>" . $conn->error;
}

$conn->close();
?>

Ok.
</p>
</body>
</html>