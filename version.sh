#!/bin/bash

# Create a detailed version string for the program.

export TZ=UTC
RAW_TIME=`git log --date=local --pretty=format:%cd --date=raw -1 HEAD | awk -F" " '{print $1}'`
DATE=`date -u --date="@${RAW_TIME}" '+%Y.%m.%d-%TUTC'`
V=`git log --pretty=format:%h -1 HEAD`-`git describe --dirty --all`-${DATE}

echo """
{-# LANGUAGE OverloadedStrings         #-}

module Gitomail.Version where
import Data.Text

version :: Text
version = \"${V}\"
"""
