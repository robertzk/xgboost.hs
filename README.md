## eXtreme Gradient Boosting in Haskell

[![Build Status](https://travis-ci.org/robertzk/xgboost.hs.svg?branch=master)](https://travis-ci.org/robertzk/xgboost.hs.svg?branch=master) [![Gitter chat for developers at https://gitter.im/dmlc/xgboost](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/dmlc/xgboost?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
A Haskell wrapper for [DMLC](https://github.com/dmlc)'s
[xgboost](https://github.com/dmlc/xgboost) machine learning library.

Xgboost is an optimized general purpose gradient boosting library.
The library is parallelized, and also provides an optimized distributed version.

It implements machine learning algorithms under the Gradient Boosting framework,
including Generalized Linear Model (GLM) and Gradient Boosted Decision Trees (GBDT).
XGBoost can also be distributed and scale to Terascale data.

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

### License

This project is licensed under the MIT License:

Copyright (c) 2015-2016 Robert Krzyzanowski

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Authors

This wrapper library was created by Robert Krzyzanowski, technoguyrob@gmail.com.
The underlying library was designed by Tianqi Chen and his team at DMLC 
(Distributed (Deep) Machine Learning Common).

