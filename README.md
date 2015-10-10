# xgboost.hs

A Haskell wrapper for xgboost

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
ghci -no-user-package-db -package-db .cabal-sandbox/*.conf.d
```
