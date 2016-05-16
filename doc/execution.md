# Execution

Running `gitomail`, without arguments, shows the available command line options. This documentation here covers the highlights among the supported modes.

## The auto-mailer command

The `auto-mailer` command responsible for sending emails, when Gitomail is installed in server settings.

When running `gitomail auto-mailer` for the first time in a repository, Gitomail does nothing, and only saves a reference point for further executions.

```no-highlight
$ gitomail auto-mailer
Initial save of ref map. Will start tracking refs from now (this may take awhile)
Relating commits to refs
```

Later, when executed periodically, the command tracks ref changes in the local repository. According to [configuration](config.md), it will send emails when changes are detected.

```no-highlight
$ gitomail auto-mailer
Relating commits to refs
Ref heads/master: creating summaries
Ref heads/master: creating emails
  Formatting 35b713e88688e8bca6840bab2fa7597a59214458
  Formatting 79fccc22f8594cf1bd21bd52202c0add45f30cc1
  Formatting ee223f192987668121abf16a9cf1681ff47fdbfd
  Formatting c8978f3073c62d3e5972deaac998a21def4d70ee
  Formatting a81bf0acb4ec12178f3045ea8e73eb2f1b3174fd
  Formatting 33ac8a455e29766fe1478e0057b85488fbc49e71
  Formatting 0e8eb5974f47fbd72c637335d3991f454998e3dc
  Formatting b0dca7df31e08f4dede0d0b6f13f2559d99b1e49
  Formatting cef53ef7945dbdac38b4b65ce767b82e07917f94
  Formatting f2245253fe77cff4f2898f46fafb135d36a47854
Sending emails!
  Sending '[gitomail] master branch added 10 commits: d6cd759c8..f2245253f'
  Sending '[gitomail master 35b713e88, #1] Documentation work'
  Sending '[gitomail master 79fccc22f, #2] Documentation: Basic intro'
  Sending '[gitomail master ee223f192, #3] doc: Plain Text E-Mails rather than ASCII'
  Sending '[gitomail master c8978f307, #4] Documentation: install.md'
  Sending '[gitomail master a81bf0acb, #5] Documentation fixes'
  Sending '[gitomail master 33ac8a455, #6] 'Email' is the established shortname for 'Electronic Mail''
  Sending '[gitomail master 0e8eb5974, #7] Email - capitalication only when needed'
  Sending '[gitomail master b0dca7df3, #8] Spelling for Webmail'
  Sending '[gitomail master cef53ef79, #9] Documentation: partially write config.md'
  Sending '[gitomail master f2245253f, #10] Documention: config.md roundup'
```

## Gitolite integration

The `integration` folder, installed with Gitomail, or taken from its source tree, contains an example on how to run Gitomail on a [Gitolite](http://gitolite.com/gitolite/index.html) server.


