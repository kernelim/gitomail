# Maintainers Specification

Gitomail supports its own `Maintainers` file format, which can optionally specify rules to match people to certain files or directories. It's especially useful for single repositories bearing multiple maintainers of code. These files are then used to automatically set the destination address of emails to the rightful maintainers, based on the files modified in the commit, effectively working very similarly to [get_maintainers.pl](https://github.com/torvalds/linux/blob/master/scripts/get_maintainer.pl).

## Directory structure

Each `Maintainers` file applies to the sub-directory in which it resides, and its own sub-directories. Directories need not have `Maintainers` files, so dispresal of these files across the tree and how they match pathnames are under the developer's full control.

## 'alias' lines

By convention, developers assign people to alias names in the top level `Maintainers` file, similiarly to the following:

```no-highlight
alias dan      Dan Aloni <alonid@gmail.com>
```

It's also possible to use aliases for mailing lists:

```no-highlight
alias all      Everyone <code-review@some-project.org>
```

## Comment lines

Comment lines begin with the character '#'.

## 'maintainer/reviewer/observer' lines

Lines the begin with either of `maintainer`, `reviewer`, or `observer`, match aliases
to filenames, using globbing patterns.

**Syntax**:

```no-highlight
[maintainer/reviewer/observer] [alias] (optional file globbing pattern)
```

File name globbing is provided so that the least amount of declarations will be needed in order to assign people to files. The globbing is similar to the `.gitignore` style of globbing.

The following rules apply:

* For each file under consideration, `Maintainers` files are evaluated top to bottom (e.g. root directory `Maintainers`, then `sub/Maintainers`, and then `sub/sub/Maintainers`).
* Each globbing pattern is relative to the directory that contains the `Maintainers` file that specifies it.
* If a globbing pattern is not specified, then 'match all files under here' is inferred.
* There can be up to one `maintainer` per file, however when matches overlap, the latter one will hold, and it serves as a valid use case. Maintainers are carried to the `To:` field of the email.
* There can be up to any amount of `reviewer`s. Reviewers are carried to the `Cc:` field of the email.
* There can be up to any amount of `observer`s (which are considered passive reviewers). Observers are carried to the `Cc:` field of the email. In effect, observers have no semantic difference from reviewers with regard to mails being sent.

## An example

Let's illustrate how rules in `Maintainers` files assign maintainer-ship to files under a source tree.

Suppose we have a tree with three `Maintainers` files, spread across a root directory and two sub-directories:

#### ./Maintainers:

```no-highlight
maintainer patrick
# Patrick is the default maintainer of all the tree.

maintainer george **/*.csv
```

Here, Patrick is the *default* maintainer of all the files in the tree, and George is the *default* maintainer of all CSV files on **any** sub-directory under the tree, overriding Patrick for those files only.

#### ./subdir/Maintainers:

```no-highlight
maintainer rachel *.txt
```

Here, Rachel is the maintainer of TXT files only under .`/subdir` All the *other* files (except for CSV files) in the directory are maintained by Patrick, because of the rule at the root dir `Maintainers` which still applies for them.

#### ./cat/Maintainers:

```no-highlight
maintainer rachel hello.csv
reviewer george hello.csv
```

Here, Rachel overrides the maintainer-ship of `./cat/hello.csv` for herself but George will still get mails about it because he is now also a reviewer for it. All the *other* files (except for CSV files other than `hello.csv`) in the directory are maintained by Patrick, because of the rule at the root dir `Maintainers` which still applies for them.
