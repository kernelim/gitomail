## Install from precompiled binaries

Binary RPMs for some Linux environments are provided:

 * **Fedora** : Available at [Copr](https://copr.fedorainfracloud.org/coprs/alonid/gitomail/)
 * **CentOS** : Available at [Copr](https://copr.fedorainfracloud.org/coprs/alonid/gitomail/)

## Or, build and install from source

Alternatively, it's possible to build Gitomail from source.

1. install the [Stack tool](http://docs.haskellstack.org).
2. Using `stack`, perform build and install:

```shell
git clone "https://github.com/kernelim/gitomail"
cd gitomail
stack install
```

The binary should be available as `~/.local/bin/gitomail`.
