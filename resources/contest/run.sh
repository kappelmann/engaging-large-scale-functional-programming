#!/bin/bash
# Arguments:
# -s: run tests safely; in particular, remove solution and test repositories before executing the tests

SERVER_URL=https://vmnipkow16.in.tum.de/contest2021/upload.php
SECRET=Aalksdfjlksajdflksarrrf9045wm3JKL901L1201lwdfwdm9rf8
RESULTS_FILE_NAME=results
TARGET_FILE_RESULTS="test-reports/${RESULTS_FILE_NAME}.xml"
EXERCISE_ID="01"
FILE_NAME="Exercise${EXERCISE_ID}"
TARGET_FILE="assignment/src/${FILE_NAME}.hs"
safe=false

timestamp=$(date --iso-8601=seconds -d "+1 hour")

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

remote=$(cd ./assignment/ && git config --get remote.origin.url)
studentId=$(echo $remote | cut -d"-" -f2 | cut -d"." -f1)
if [ "$studentId" = "exercise" ] || [ "$studentId" = "solution" ]; then
  echo "Testing template or solution. Skipping upload."
  exit 0
fi

commit=$(cd ./assignment/ && git rev-parse HEAD)
# upload the file
echo $(curl -s -X POST \
  -F "token=${SECRET}" \
  -F "student_id=${studentId}" \
  -F "commit=${commit}" \
  -F "timestamp=${timestamp}" \
  -F "exercise_id=${EXERCISE_ID}" \
  -F "${FILE_NAME}=@${TARGET_FILE};type=text/plain" \
  -F "${RESULTS_FILE_NAME}=@${TARGET_FILE_RESULTS};type=text/plain" \
  ${SERVER_URL})
exitStatus=$?
if [ $exitStatus -ne 0 ]; then
  echo "Could not upload Wettbewerb file to our server."
fi

# return 0
# Note: as a convention, a failed haskell tasty test suite returns 1, but this stops the JUnit Parser from running.
exit 0
