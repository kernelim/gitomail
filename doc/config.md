# Configuration

Gitomail receives configuration via one or more YAML formatted files, using a scheme described here.

## File lookup

Configuration files are loaded in this order:

* If exists - `~/.gitomailconf.yaml`
* If exists - `$GIT_DIR/gitomailconf.yaml` (where `GIT_DIR` is the current Git's repo meta directory)
* Configuration files passed via the command line option `-c`.

Later files override values that were set by former files.

## Values

### commit_url

Per commit sent or mentioned, Gitomail can create hyperlinks to a Web-based repository
browser in the email, and this setting is the formating string for such links, where:

* `%r` - name of the repository
* `%H` - full commit hash

For example:

```
commit_url: https://github.com/kernelim/%r/commit/%H
```

### blob_in_commit_url

In diffs, this optional field can help to create hyperlinks from the filenames presented
in the meta-data to their full version. This setting is the formating string for such links,
where:

* `%r` - name of the repository
* `%H` - full commit hash
* `%f` - filename in tree.

For example:

```
blob_in_commit_url: https://github.com/kernelim/%r/blob/%H/%f
```

### exclude_refs

List of regular expressions that match refs to exclude from monitoring.

For example:

```yaml
exclude_refs:
- .*/_.*
```

This would exclude all refs that include the `_/` substring.

### include_refs

If provided, will only monitor refs that matches at least one of the references
provided, and as long as they do match any of the regexes in `exclude_refs`.

For example:

```yaml
include_refs:
- heads/review/.*
exclude_refs:
- .*/_.*
```

This would include only refs that start with `review/` and among them
exclude all refs that include the `_/` substring.

### root_refs

This setting is a list of regexes that determine the relationship between refs.
Earlier ones will be traversed before later ones. It defaults to the following setting:

```yaml
root_refs:
- tags/.*
- heads/master
```

This is needed because Gitomail cannot guess which branch diverged from which branch,
and while it's only a convention that branches diverge from `master`, there could be
other branches such as `release-x.y.z` from which people diverge (in that case, it
would be fitting to put `heads/release-.*` after `heads/master`.

### repo_name

If not guessed from working directory or passed from command line, this setting
can provide the repository name to be used when sending emails and formatting links.

For example:

```
repo_name: someproject
```

### from_email

The fully formed email address to put in the `Form:` email field.

For example:

```
from_email: Gitomail <osiris@aloni.org>
```

### filtered_email_destinations

These are a list of email addresses (e.g. `a@b.c`) to which never to send emails, even
if they appear as maintainers.

### smtp_hostname

SMTP hostname for sending mails.

### smtp_port

SMTP port for sending mails, defaults to 587.

### smtp_starttls

Whether to enable TLS - defaults to True.

### smtp_username
### smtp_password


**TBD**

```
TBD:

  X(_commitSubjectLine  , "commit_subject_line" , a     , defl, "[%r %b %h%n] %s", Text          ) \
  X(_summarySubjectLine , "summary_subject_line", a     , defl, "[%r] %s"        , Text          ) \
  X(_jiraCC             , "jira_cc"             , Maybe , pass,                  , JIRACC        ) \
  X(_sourceHighlight    , "source_highlight"    , a     , defl, True             , Bool          ) \
  X(_aliasRefMatch      , "alias_ref_match"     , a     , defl, defaultAliasMatch, Maybe Text    ) \
  X(_issueTrackMatch    , "issue_track_match"   , Maybe , pass,                  , Text          ) \
  X(_issueTrackURL      , "issue_track_url"     , Maybe , pass,                  , Text          ) \
```
