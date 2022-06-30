#!/bin/bash
# Arguments:
# -s: run tests safely; in particular, remove solution and test repositories before executing the tests

SERVER_URL=https://somewhere.edu/upload.php
# TODO: Change this here and in the accepting server
SECRET=Aalksdfjlksajdflksarrrf9045wm3JKL901L1201lwdfwdm9rf8
RESULTS_FILE_NAME=results
TARGET_FILE_RESULTS="test-reports/${RESULTS_FILE_NAME}.xml"
EXERCISE_ID="08"
FILE_NAME="Exercise${EXERCISE_ID}"
TARGET_FILE="assignment/src/${FILE_NAME}.hs"
# The tests that need to be passed to qualify for the tournament
TEST1="strategy/starting against grandpa"
TEST2="strategy/grandpa starting"

safe=false

#### Security measurments when testing Haskell submissions on Artemis ####

# check passed flags
while getopts s opt; do
  case $opt in
    s) safe=true;;
  esac
done
shift $((OPTIND-1))

# check for symlinks as they might be abused to link to the sample solution
$safe && find assignment/ -type l | grep -q . && echo "Cannot build with symlinks in submission." && exit 1

# check for unsafe OPTIONS and OPTIONS_GHC pragma as they allow to overwrite command line arguments
$safe && \
while IFS= read file; do
  cat $file | tr -d '\n' | grep -qim 1 "{-#[[:space:]]*options" && \
    echo "Cannot build with \"{-# OPTIONS..\" pragma in source." && exit 1
done < <(find assignment/src -type f)

#### Test execution ####

# build the libraries - do not forget to set the right compilation flag (Prod)
stack build --allow-different-user --flag test:Prod && \
  # delete the solution and tests (so that students cannot access it) when in safe mode
  (
    (! $safe && exit 0) || \
    ($safe && (rm -rf solution && rm -rf test)) \
  )

exitStatus=$?
if [ $exitStatus -ne 0 ]; then
  echo "Compile error"
  exit $exitStatus
fi

# run the test executable
stack exec test --allow-different-user

#### Check student opt-in ####

# students have to opt in to the tournament by including their code in 
# {-WETT ... -TTEW-} tags
file_content=$(cat $TARGET_FILE)
if [[ ! $file_content =~ (\{\-WETT\-\}) ]] || [[ ! $file_content =~ (\{\-TTEW\-\}) ]]; then
  echo "No {-WETT-}...{-TTEW-} tags found. Skipping upload"
  exit 0
fi

#### Retrieve student id ####

# TODO: this retrieves the student id from the repository URL in Artemis;
# you will have to replace to fetch team names according to your setup
remote=$(cd ./assignment/ && git config --get remote.origin.url)
studentId=$(echo $remote | cut -d"-" -f2 | cut -d"." -f1)
if [ "$studentId" = "exercise" ] || [ "$studentId" = "solution" ]; then
  echo "Testing template or solution. Skipping upload."
  exit 0
fi

#### Check if student qualified ####

# Note: this is quite hacky

# get the test entry
if [[ ! $(cat $TARGET_FILE_RESULTS) =~ (testcase name=\"${TEST1}\" [^>]*) ]]; then
  echo "Could not find test \"${TEST1}\" in XML result."
  exit 0
fi
match=${BASH_REMATCH[1]}
# check if the entry has no value (i.e. the test passed) by checking if it ends with an "/>"
if [[ ! "${match: -1}" = "/" ]]; then
  echo "Did not pass tournament test \"${TEST1}\"; no upload."
  exit 0
fi
# get the test entry
if [[ ! $(cat $TARGET_FILE_RESULTS) =~ (testcase name=\"${TEST2}\" [^>]*) ]]; then
  echo "Could not find test \"${TEST2}\" in XML result."
  exit 0
fi
match=${BASH_REMATCH[1]}
# check if the entry has no value (i.e. the test passed) by checking if it ends with an "/>"
if [[ ! "${match: -1}" = "/" ]]; then
  echo "Did not pass tournament test \"${TEST2}\"; no upload."
  exit 0
fi


#### Upload ####

commit=$(cd ./assignment/ && git rev-parse HEAD)
# upload the file
echo $(curl -s -X POST \
  -F "token=${SECRET}" \
  -F "student_id=${studentId}" \
  -F "commit=${commit}" \
  -F "${FILE_NAME}=@${TARGET_FILE};type=text/plain" \
  -F "${RESULTS_FILE_NAME}=@${TARGET_FILE_RESULTS};type=text/plain" \
  ${SERVER_URL})
exitStatus=$?
if [ $exitStatus -ne 0 ]; then
  echo "Could not upload submission file to our tournament server."
fi

# return 0
# Note: as a convention, a failed haskell tasty test suite returns 1, but this stops the JUnit Parser from running.
exit 0

