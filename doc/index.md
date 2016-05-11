## An introduction

Gitomail is a tool for generating pretty inline-HTML EMails for [Git](https://git-scm.com/) commits, with sending capability to proper recipients.

### First, some history

Many years before [Git](https://git-scm.com/) became popular or existed, people were using mailing lists in order to collaborate on code changes. The unified diff format, now popularized, as used as a diff format in the ASCII-formatted E-Mails. A text-based console EMail program such as [mutt](http://www.mutt.org/) presented the user a convinent way to handle these diffs and import them into their source trees.

Below is a fake example of such an E-Mail, based on a commit in the PostgreSQL project:

<img src="example1.png" width="672" align="center">

With the advant of sites like [Github](https://github.com), EMail became under-used for reviewing changes, and in the Webmail era, E-Mails containing diffs may appear somewhat arcane to developers of today.

### Diffs in the age of WebMail

Nowadays, the ASCII version of the EMail message seems outdated. This is where Gitomail comes into the picture.

For example, the E-Mail from above, when sent by Gitomail, can appear like the following:

![example](example2.png)

Combined with full syntax highlighting, the HTML part of the EMail makes this appearance possible.

### Branch changes summaries

TBD

### Automatic recipients and code maintainership

TBD
