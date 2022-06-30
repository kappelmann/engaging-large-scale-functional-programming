#!/usr/bin/env sh

rm src/Competition/SubmissionList.generated
rm src/Competition/SubmissionList.Imports.generated

# Create empty submission lists to avoid errors during first build
touch src/Competition/SubmissionList.generated
touch src/Competition/SubmissionList.Imports.generated

rm -r uploads
rm submissions/.batch
rm submissions/.submissionId
rm -r ../www/data
rm -r submissions/S*
rm -r .stack-work
rm -r game-cache

git checkout -- competition-runner.cabal
git checkout -- stack.yaml
