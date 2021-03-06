## Gitomail

[![Build Status](https://travis-ci.org/kernelim/gitomail.svg?branch=master)](https://travis-ci.org/kernelim/gitomail)

Gitomail is a tool for generating pretty inline-HTML emails for [Git](https://git-scm.com/) commits, with a sending capability to proper recipients.

It can be used both for knowledge sharing and code review among collaborators on a code base.

[Documentation](http://gitomail.kernelim.com/)

### In a nutshell

* Emails containing pretty HTML colored commit diffs with source code syntax highlighting
  in combination with the old-time plaintext, readable under text-based email readers.
* Optional JIRA integration - emails can be CC'ed to people being refered from the JIRA issues
  mentioned in the commit messages (on user-specified kinds of issue fields).
  Hyperlinks to JIRA issues from the commit messages are done too.
* Optional maintainership tracking per directory or file, meaning that per-commit `To:` and `Cc:`
  destinations correspond to user-specified maintainership information over the files being affected.
* Summary of Ref changes, with differentiation over rebase and fast-forward, along with base-branch
  detection so that the summary properly describes the content of the branches.
