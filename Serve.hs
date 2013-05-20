{-----------------------------------------------------------------------------}
{-- Static File Server                                          jaburns.net --}
{-----------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-------------------------------------------------------------------------------
 
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.HTTP.Types (status200)
import qualified Data.Text as T
import           Control.Monad.IO.Class (liftIO)
import           System.Directory (doesFileExist)

-------------------------------------------------------------------------------
 
main = do
    let port = 9000
    putStrLn $ "Listening on port " ++ show port
    run port app

app req = 
    case pathInfo req of
        [] -> serveFile "compiled/index.html"
        path -> serveFile $ "static/" `T.append` (T.intercalate "/" path)

-- Attempts to retrieve and serve a file from the "static" folder, responds
-- with "404.html" on failure.
serveFile path = do
    fileExists <- liftIO . doesFileExist $ T.unpack path
    return $ ResponseFile status200 [] (if fileExists
                                      then T.unpack path
                                      else "compiled/404.html") Nothing
