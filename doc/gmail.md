# Recommended Gmail filters

If a mailing list is [configured as a recipient](maintainers-spec.md) for all code changes, it is recommended to use Gmail's filtering so that emails not directly addressed to you (via `To:` or `Cc`) are skipped from the inbox. This way you can paicipiate in discussions on all code, but these discussions get prioritized if they involve code under your maintainership.

Also, for duplicate commits, the flag `InexactDiffDup` can be used in the filtering so that emails for duplicate diffs also skip the inbox and marked as read.

The next three filter descriptions can be _used as template_ to accomplish both targets. To add each of them, take the string in `Matches:` into Gmail's search, and when instructed to create 'a filter like this', follow with the list under `Do this:`.

```no-highlight
Matches: from:(gitomail@company.com) -{to:your.name@company.com InexactDiffDup}
Do this: Skip Inbox, Apply label "Code Review"

Matches: from:(gitomail@company.com) to:(your.name@company.com) -InexactDiffDup
Do this: Apply label "Code Review"

Matches: from:(gitomail@company.com) InexactDiffDup
Do this: Skip Inbox, Mark as read, Apply label "Code Review"
```
