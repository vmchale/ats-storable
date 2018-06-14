# ats-storable

Fork of
[generic-storable](hackage.haskell.org/package/generic-storable) altered to
work with ATS' algebraic data types.

For documentation concerning the ATS side of things, consult the
[documentation](http://ats-lang.sourceforge.net/DOCUMENT/INT2PROGINATS/HTML/x2179.html).

## Building for Hacking

Install [atspkg](http://hackage.haskell.org/package/ats-pkg-2.5.0.3#readme),
then:

```
atspkg build
cabal new-test
```
