#!/bin/bash

# Create links to auto-generated Hs files in order to statisfy editor's automatic syntax checking.

ln -s ../../../dist/build/gitomail/gitomail-tmp/Gitomail/Maintainers/Lexer.hs src/Gitomail/Maintainers
ln -s ../../../dist/build/gitomail/gitomail-tmp/Gitomail/Maintainers/Parser.hs src/Gitomail/Maintainers
ln -s ../../../dist/build/gitomail/gitomail-tmp/Lib/Lexer/Lexer.hs src/Lib/Lexer
