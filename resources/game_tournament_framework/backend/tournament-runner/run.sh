#!/usr/bin/env sh

set -e

touch src/Competition/SubmissionList.generated
touch src/Competition/SubmissionList.Imports.generated

stack run prepare-competition
stack run competition
