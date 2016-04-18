## ToDo

* Maintainers in diffstat.
* Support sending a user welcome message.
    * Provide a template for it, pointers to GMail filtering and such.
    * To track who we have already sent a welcome message - need a different DB for that.
* Handle Level DB corruption.
* Add in-reply-to from the commit mails to the summary E-Mails.
* Implement manual E-Mail sending similar to the old git-send-email.
* Fixme: Commits that only remove files are skipped (no maintainers).
* Prepare Github, Bitbucket and Gitolite presets for easier integartion.
    * Gitolite integration will be able to tell Gitomail which user did the push.
* Add lots of documentation.
  * A Github monitoring mirror must use a recent-enough Git client that can keep
    the `refs/gitomail/*` intact after fetching.
  * When mirror *from* a Gitomail enabled repository, it would be nice to ignore
    the `refs/gitomail/*`.
  * Explain why it is not possible to use the information provided by the update-hook
    in order to decipher relationship between branches.
* Use the git executable for rev-parsing for '-v'.
* Formatting of the ASCII E-Mail should be improved.
* Verify that the ASCII version of the E-Mail can still be imported as a patch.
* Verify that merge commits are skipped, for now.
* Cleanups, refactoring.
* Submodule support, or at least test for minimal handling.
* Rethink handling of empty commits (currently no mails are sent).
* Rethink handling of invalid/redundant Maintainers data. Provide a command line
  option for it to be used in project's build time.
* Support hinting about whether to pass '-w' to the Git diff generation, via
  explict hints in commit message. The ASCII version of the E-MAil should not be 
  affected perhaps, so that it could still apply as a valid diff?
* Occasionally GMail's SMTP interaction throws an exception:
  `HandshakeFailed (Error_Packet_unexpected "Alert [(AlertLevel_Fatal,BadRecordMac)]" " expected: change cipher")`

## Nice to have

* Support the Index as if it were a commit, especially when reading Maintainers.
* Consider how to present merge commits, perhaps show a tree.
* Support for forked projects, i.e. ignore commits contained in remotes.
