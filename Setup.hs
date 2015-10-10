import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)

main = defaultMainWithHooks simpleUserHooks
    { preBuild = \a b -> makeLib a b >> preBuild simpleUserHooks a b }

makeLib :: Args -> BuildFlags -> IO ()
makeLib _ flags =
    rawSystemExit (fromFlag $ buildVerbosity flags) "env"
        ["CFLAGS=-D_LIB", "make", "--directory=abc", "libxgboost.a"]
