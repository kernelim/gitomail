## ToDo

* Maintainers in diffstat.
* Support sending a user welcome message.
    * Provide a template for it, pointers to GMail filtering and such.
    * To track who we have already sent a welcome message - need a different DB for that.
* Improve formatting of the plain-text version of the E-Mail
* Support ref names adding people to CC automatically according to
  their maintainer alias.
* Handle Level DB corruption.
* Add in-reply-to from the commit mails to the summary E-Mails.
* Implement manual E-Mail sending similar to the old git-send-email.
* Fixme: Commits that only remove files are skipped (no maintainers).
* Prepare Github, Bitbucket and Gitolite presets for easier integartion.
    * Gitolite integration will be able to tell Gitomail which user did the push.
* Add lots of documetation.
* Use the git executable for rev-parsing for '-v'.
* Formatting of the ASCII E-Mail should be improved.
* Verify that the ASCII version of the E-Mail can still be imported as a patch.
* Verify that merge commits are skipped, for now.
* Check that UTF-8 is handled correctly.
* Cleanups, refactoring.
* Submodule support.

## Nice to have

* Support the Index as if it were a commit, especially when reading Maintainers.
* Consider how to present merge commits, perhaps show a tree.
* Support for forked projects, i.e. ignore commits contained in remotes.
