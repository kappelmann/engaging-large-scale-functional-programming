<?php
# TODO: Change this here and in the uploading script
$SECRET="Aalksdfjlksajdflksarrrf9045wm3JKL901L1201lwdfwdm9rf8";
$STUDENT_ID_PARAM="student_id";
$TOKEN_PARAM="token";
$TIMESTAMP_PARAM="timestamp";
$TIMESTAMP_FILE_NAME = "timestamp";
$RESULTS_FILE_NAME = "results";
$COMMIT_PARAM="commit";
$COMMIT_FILE_NAME = "commit";
$EXERCISE_ID_PARAM = "exercise_id";
$FILE_NAME_PREFIX = "Exercise";
// 50KB
$MAX_FILE_SIZE = 50000;


# TODO: set deadline if your submission server does not support blocking late submissions
# if (time() > 1610406420 /*2021-01-12T00:07:00+01:00*/) {
#   die("Competition ended");
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

// Check for exercise id
if (!isset($_POST[$EXERCISE_ID_PARAM])) {
  die("exercise ID missing.");
}

$exercise_id=$_POST[$EXERCISE_ID_PARAM];
$file_name=$FILE_NAME_PREFIX . $exercise_id;

// Check for commit
if (!isset($_POST[$COMMIT_PARAM])) {
  die("commit missing.");
}

$commit=$_POST[$COMMIT_PARAM];

// Check for timestamp
if (!isset($_POST[$TIMESTAMP_PARAM])) {
  die("timestamp missing.");
}

$timestamp=$_POST[$TIMESTAMP_PARAM];

// Check for student id
if (!isset($_POST[$STUDENT_ID_PARAM])) {
  die("Student ID missing.");
}

$student_id=$_POST[$STUDENT_ID_PARAM];

if (!preg_match("/^[a-z0-9]+$/", $student_id)) {
  die("Invalid student ID.");
}

// Check if file is a text file
$check = $_FILES[$file_name]["type"] == "text/plain";
if(!$check) {
  die("Invalid file upload.");
}

// Check file size
if ($_FILES[$file_name][$file_size] > $MAX_FILE_SIZE) {
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

$target_dir = $exercise_id . "/uploads/" . $student_id . "/";

// if everything is ok, try to upload file
// create the directory
if (!is_dir($target_dir)) {
  if (!mkdir($target_dir, 0770, true)) {
    die("Could not create directory ". $target_dir . ".");
  }
}

// create hs file
$target_file = $target_dir . $file_name . ".hs";
if (!move_uploaded_file($_FILES[$file_name]["tmp_name"], $target_file)) {
  die("Sorry, there was an error uploading your file.");
}

// create results file
$target_results_file = $target_dir . $RESULTS_FILE_NAME . ".xml";
if (!move_uploaded_file($_FILES[$RESULTS_FILE_NAME]["tmp_name"], $target_results_file)) {
  // remove the student submission
  unlink($target_file);
  die("Sorry, there was an error uploading your results file.");
}

// create timestamp file
$target_timestamp_file = $target_dir . $TIMESTAMP_FILE_NAME;
file_put_contents($target_timestamp_file, $timestamp);

// create commit file
$target_commit_file = $target_dir . $COMMIT_FILE_NAME;
file_put_contents($target_commit_file, $commit);

system("chmod -R 775 " . $target_dir);

echo "Submission by " . $student_id . " for Exercise " . $file_name . " has been uploaded.";

?>
