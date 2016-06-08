# Configuration

Gitomail receives configuration via one or more YAML formatted files, using a scheme described here.

## File lookup

Configuration files are loaded in this order:

* If exists - `~/.gitomailconf.yaml`
* If exists - `$GIT_DIR/gitomailconf.yaml` (where `GIT_DIR` is the current Git's repo meta directory)
* Configuration files passed via the command line option `-c`.

Later files override values that were set by former files.

## Values (general)

### repo-name

If not guessed from working directory or passed from command line, this setting
can provide the repository name to be used when sending emails and formatting links.

For example:

```no-highlight
repo-name: someproject
```

### exclude-refs

List of regular expressions that match refs to exclude from monitoring.

For example:

```no-highlight
exclude-refs:
- .*/_.*
```

This would exclude all refs that include the `_/` substring.

### include-refs

If provided, will only monitor refs that matches at least one of the references
provided, and as long as they do match any of the regexes in `exclude-refs`.

For example:

```no-highlight
include-refs:
- heads/review/.*
exclude-refs:
- .*/_.*
```

This would include only refs that start with `review/` and among them
exclude all refs that include the `_/` substring.

### root-refs

This setting is a list of regexes that determine the relationship between refs.
Earlier ones will be traversed before later ones. It defaults to the following setting:

```no-highlight
root-refs:
- heads/master
- tags/.*
```

This is needed because Gitomail cannot guess which branch diverged from which branch,
and while it's only a convention that branches diverge from `master`, there could be
other branches such as `release-x.y.z` from which people diverge (in that case, it
would be fitting to put `heads/release-.*` after `heads/master`.

## Values (Email generation)

### commit-url

Per commit sent or mentioned, Gitomail can create hyperlinks to a Web-based repository
browser in the email, and this setting is the formating string for such links, where:

* `%r` - name of the repository
* `%H` - full commit hash

For example:

```no-highlight
commit-url: https://github.com/kernelim/%r/commit/%H
```

### blob-in-commit-url

In diffs, this optional field can help to create hyperlinks from the filenames presented
in the meta-data to their full version. This setting is the formating string for such links,
where:

* `%r` - name of the repository
* `%H` - full commit hash
* `%f` - filename in tree

For example:

```no-highlight
blob-in-commit-url: https://github.com/kernelim/%r/blob/%H/%f
```

### from-email

The fully formed email address to put in the `Form:` email field.

For example:

```no-highlight
from-email: Gitomail <osiris@aloni.org>
```

### filtered-email-destinations

Default: empty

These are a list of email addresses (e.g. `a@b.c`) to which never to send emails, even
if they appear as maintainers.

### source-highlight

Default: True

A boolean which specifies whether to preform syntax highlighting.

### commit-subject-line

Default (below):

```
commit-subject-line: [%r %b %h%n] %s
```

Format string for the per-commit subject line.


### summary-subject-line

Default (below):

```
default_subject_line: [%r] %s
```

Format string for the summary email subject line.

## Values (SMTP related)

### smtp-hostname

SMTP hostname for sending mails.

### smtp-port

SMTP port for sending mails, defaults to 587.

### smtp-starttls

Whether to enable TLS - defaults to True.

### smtp-username

SMTP username for sending mails.

### smtp-password

SMTP password for sending mails.

__Example Gmail configuration__

A configuration for Gmail or 'Google for Domains' can be based on the following template:

```no-highlight
smtp-hostname: smtp.gmail.com
smtp-starttls: true
smtp-username: example-user@gmail.com
smtp-password: example-password
from-email: Example User <example-user@gmail.com>
```

## Values (JIRA integration)

The following fields allow to optionally activate support for JIRA integration.

### issue-track-match

This field describes a regex that matches strings that link to issues. The inner
most parenthesis is the part of the match that will receive the hyperlink.

For example:

```no-highlight
issue-track-match: '[[]((PROJECT|OTHER|ISSUE)-[0-9]+)[]]'
```

With this matcher, `[PROJECT-123]` will match, and the substring `PROJECT-123`
will get hyperlinked. Plus, that substring is used for the `jira-cc` field,
and `issue-track-url` fields, later on.

### issue-track-url

This field describes the hyperlink to generate for each issue mention in the
commit message.

For example:

```no-highlight
issue-track-url: https://somefakeproject.com/browse/%s
```

### jira-cc

This field can specify a JIRA server and authentication credentials, from which
Gitomail would automatically fetch data concerning issues mentioned in commits. The
meta-data used from these issues can specify additional people to address when
automatically sending the emails.

For example:

```no-highlight
jira-cc:
  url: https://somecompany.atlassian.net/rest/api/2/issue/%s
  http_creds: 'username:password'
  fields:
  - customfield_10300
```
