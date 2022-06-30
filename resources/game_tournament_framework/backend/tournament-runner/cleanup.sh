#!/usr/bin/env sh

rm src/Competition/SubmissionList.generated
rm src/Competition/SubmissionList.Imports.generated

# Create empty submission lists to avoid errors during first build
touch src/Competition/SubmissionList.generated
touch src/Competition/SubmissionList.Imports.generated

rm -r uploads
rm -r submissions/S*
rm -r .stack-work
rm -r game-cache

# TODO: make sure to reset those files in another way if you do not use git
git checkout -- competition-runner.cabal
git checkout -- stack.yaml
