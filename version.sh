#!/bin/bash

# Create a detailed version string for the program.

export TZ=UTC
RAW_TIME=`git log --date=local --pretty=format:%cd --date=raw -1 HEAD | awk -F" " '{print $1}'`
DATE=`date -u --date="@${RAW_TIME}" '+%Y.%m.%d-%TUTC'`
TAG=`git describe --tags $(git rev-list --tags --max-count=1)`

TAG_DIFF=`git log ${TAG}..HEAD --oneline | wc -l`
TAG_DIFF=`printf "%04d." ${TAG_DIFF}`

if [[ "$TAG_DIFF" == "0000." ]] ; then
    # A percisely tagged version
    V=" ${DATE}"
    if [[ "$1" == "simple" ]] ; then
        echo 0
        exit
    fi
else
    # A pre-release

    GITVER=g`git log --pretty=format:%h -1 HEAD`
    V=".${TAG_DIFF}${GITVER}"

    if [[ "$1" == "simple" ]] ; then
        echo ${TAG_DIFF}${GITVER}
        exit
    fi
fi

echo """
{-# LANGUAGE OverloadedStrings         #-}

module Gitomail.Version where
import Data.Text

version :: Text
version = \"${V}\"
"""
