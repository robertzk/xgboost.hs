When making pull requests to this project, please run the following
before a submission to ensure the test suite passes locally.

```bash
cabal configure && cabal build && cabal install && cabal test
```

This will help us process any suggestions and improvements
quicker. If your change is cosmetic (e.g., documentation or
corrections in non-source files) you can include the string
[`[ci skip]`](http://docs.travis-ci.com/user/customizing-the-build/#Skipping-a-build)
in the commit message.

Finally, if you submit a major improvement that significantly 
alters the library, as an end-to-end test please train a model
locally and ensure its performance has not degraded.

