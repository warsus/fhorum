{-# LANGUAGE CPP #-}
#if PRODUCTION
import Controller (withForum)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withForum $ run 3000
#else
import Controller (withForum)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withForum $ run port . debug
#endif
