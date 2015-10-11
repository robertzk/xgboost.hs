import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import System.Environment (lookupEnv, setEnv)

-- Blatantly borrowed from http://blog.ezyang.com/2010/06/setting-up-cabal-the-ffi-and-c2hs/
main = defaultMainWithHooks simpleUserHooks
    { preBuild = \a b -> makeLib a b >> preBuild simpleUserHooks a b }

makeLib :: Args -> BuildFlags -> IO ()
makeLib _ flags = do
  let verbosity = fromFlag $ buildVerbosity flags
  cflags <- lookupEnv "CFLAGS" >>= return . maybe "" id
  setEnv "CFLAGS" $ "-fPIC -D_LIB" ++ (' ' : cflags)
  rawSystemExit verbosity "env"
      ["make", "--directory=xgboost", "wrapper/libxgboostwrapper.so"]
