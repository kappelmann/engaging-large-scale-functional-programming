<?php
# TODO: Change this here and in the uploading script
$SECRET="Aalksdfjlksajdflksarrrf9045wm3JKL901L1201lwdfwdm9rf8";
$STUDENT_ID_PARAM="student_id";
$TOKEN_PARAM="token";
# TODO: change accordingly
$FILE_NAME = "Exercise08";
$RESULTS_FILE_NAME = "results";
$COMMIT_PARAM="commit";
$COMMIT_FILE_NAME = "commit";
// 50KB
$MAX_FILE_SIZE = 50000;
$SUBMIT_UPLOADS_TO_TOURNAMENT_SERVER = true;
# TODO: change accordingly
$TOURNAMENT_SERVER_SUBMISSION_URL = "http://127.0.0.1:1234/api/upload";
# TODO: change accordingly
$LOCAL_TOURNAMENT_DIR = "./";

# TODO: set deadline if your submission server does not support blocking late submissions
# if (time() > 1610406420 /*2021-01-12T00:07:00+01:00*/) {
#   die("Tournament ended");
# }

// Check for secret token
if (!isset($_POST[$TOKEN_PARAM])) {
  die("Invalid request.");
}

$token=$_POST[$TOKEN_PARAM];

// Compare secret token
if ($token !== $SECRET) {
  die("Invalid request.");
}

// Check for commit
if (!isset($_POST[$COMMIT_PARAM])) {
  die("commit missing.");
}

$commit=$_POST[$COMMIT_PARAM];

// Check for student id
if (!isset($_POST[$STUDENT_ID_PARAM])) {
  die("Student ID missing.");
}

$student_id=$_POST[$STUDENT_ID_PARAM];

if (!preg_match("/^[a-z0-9]+$/", $student_id)) {
  die("Invalid student ID.");
}

// Check if file is a text file
$check = $_FILES[$FILE_NAME]["type"] == "text/plain";
if(!$check) {
  die("Invalid file upload.");
}

// Check file size
if ($_FILES[$FILE_NAME][$file_size] > $MAX_FILE_SIZE) {
  die("Sorry, your file is too large.");
}

// Check if results file is a text file
$check = $_FILES[$RESULTS_FILE_NAME]["type"] == "text/plain";
if(!$check) {
  die("Invalid results file upload.");
}

// Check results file size
if ($_FILES[$RESULTS_FILE_NAME][$file_size] > $MAX_FILE_SIZE) {
  die("Sorry, your results file is too large.");
}

$target_dir = "./uploads/" . $student_id . "/";

// if everything is ok, try to upload file
// create the directory
if (!is_dir($target_dir)) {
  if (!mkdir($target_dir, 0770, true)) {
    die("Could not create directory ". $target_dir . ".");
  }
}

// create hs file
$target_file = $target_dir . $FILE_NAME . ".hs";
if (!move_uploaded_file($_FILES[$FILE_NAME]["tmp_name"], $target_file)) {
  die("Sorry, there was an error uploading your file.");
}

// create results file
$target_results_file = $target_dir . $RESULTS_FILE_NAME . ".xml";
if (!move_uploaded_file($_FILES[$RESULTS_FILE_NAME]["tmp_name"], $target_results_file)) {
  // remove the student submission
  unlink($target_file);
  die("Sorry, there was an error uploading your results file.");
}

// create commit file
$target_commit_file = $target_dir . $COMMIT_FILE_NAME;
file_put_contents($target_commit_file, $commit);

system("chmod -R 775 " . $target_dir);

echo "Submission by " . $student_id . " has been stored.\n Uploading to tournament server...";

// Submit upload to tournament web server
$result_str = file_get_contents($target_results_file);
$src_str = file_get_contents($target_file);
$data_str = http_build_query(array("student_id" => $student_id, "commit" => $commit, "results" => $result_str, "src" => $src_str));

$options = array("http" => array("method" => "POST", "header" => "Content-Type: application/x-www-form-urlencoded", "content" => $data_str));
$context = stream_context_create($options);
$server_response = file_get_contents($TOURNAMENT_SERVER_SUBMISSION_URL, false, $context);
if($server_response === false)
  echo "Could not upload to tournament server.";
else
  echo "Tournament server response: " . $server_response;

?>
