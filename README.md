## eXtreme Gradient Boosting in Haskell [![Build Status](https://travis-ci.org/robertzk/xgboost.hs.svg?branch=master)](https://travis-ci.org/robertzk/xgboost.hs.svg?branch=master)

A Haskell wrapper for [DMLC](https://github.com/dmlc)'s
[xgboost](https://github.com/dmlc/xgboost) machine learning library.

### Development

To get started, run the standard:

```
cabal sandbox init
cabal configure
cabal build
cabal install
```

You can then [enter GHCI with the sandboxed packages](http://stackoverflow.com/questions/17014270/how-can-i-use-ghci-with-the-new-cabal-1-17-sandboxes) using

```
# Assuming GHC >= 7.6
# Note we have to link the dynamic xgboostwrapper library manually when using ghci: http://stackoverflow.com/questions/6323755/osx-ghci-dylib-what-is-the-correct-way
ghci -no-user-package-db -package-db .cabal-sandbox/*.conf.d xgboost/wrapper/libxgboost.so
```
