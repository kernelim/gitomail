## Bugs and issues

* Commits that only remove files are skipped (no maintainers for them).
* Should probably not number merge commits in summaries.
* Occasionally Gmail's SMTP interaction throws an exception:
  `HandshakeFailed (Error_Packet_unexpected "Alert [(AlertLevel_Fatal,BadRecordMac)]" " expected: change cipher")`
* Some SMTP servers (notabily Gmail's) take measures against programs
  that send too many mails in a short time. Specially Gitomail's TCP connection
  is thrown during send:

      gitomail: <socket: 42>: hPutBuf: illegal operation (handle is closed)

  It should be configurable.

  For Gmail, looks like the default for a safe throttle would be one email every 5 seconds
  At one instance it tried to send more than 105 emails, and got thrown away for 5 minutes.
* Having tags/.* first in the root-refs means that every push to say, master, will summerize
  all the commits since the previous tag in the 'previously pushed'. We need some way to
  suppress that so that tags can be first, in a convenient manner.

## Possible features and other ToDo's

* Maintainers in diffstat.
* Facilitate support for easier client-side filtering between summaries and commits.
* Support sending a user welcome messages.
    * Provide a template for it, pointers to Gmail filtering and such.
    * To track who we have already sent a welcome message - need a different DB for that.
* Handle LevelDB corruptions more gracefully.
* Add in-reply-to from the commit mails to the summary emails.
* Prepare Github and Bitbucket presets for easier integartion.
* Gitolite integration should be able to tell which user did the push, and
  have that user in the 'From' email field. However if the hook is running
  asynchronously it may not catch the push prefectly. We can solve it partially
  by matching the ref changes from the push to the detected branch changes.
* Add lots of documentation.
  * A Github monitoring mirror must use a recent-enough Git client that can keep
    the `refs/gitomail/*` intact after fetching.
  * When mirror *from* a Gitomail enabled repository, it would be nice to ignore
    the `refs/gitomail/*`.
  * Explain why it is not possible to use the information provided by the update-hook
    in order to decipher relationship between branches.
* Revise command line usage for non-automailer modes.
* Verify that the Plain Text version of the email can still be imported as a patch.
* Revise submodule support - currently we regarding them as simple files.
* Rethink handling of empty commits (currently no mails are sent).
* Improve Maintainer data parsing, so a command can be exported to be used in
  build time.
* Support hinting about whether to pass '-w' to the Git diff generation, via
  explict hints in commit message. The Plain Text version of the email should not be
  affected perhaps, so that it could still apply as a valid diff?
* Formatting of the Plain Text email should be improved.
* Cleanups and refactoring.

## Nice to have

* Support the Index as if it were a commit, especially when reading Maintainers.
* Consider how to present merge commits, perhaps show a tree.
* Support for forked projects, i.e. ignore commits contained in remotes.
