{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
-- module Main where

import Lucid
import Lucid.Base
import Data.Text

import Web.Scotty
import Network.Wai.Middleware.Static


-- integrity_ :: Text -> Attribute
integriry_ = makeAttribute "integrity"

-- crossorigin_ :: Attribute
-- crossorigin_ = makeAttribute "crossorigin"

indexHTML :: Html ()
indexHTML = do
    doctype_
    html_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
            script_ [src_ "https://code.jquery.com/jquery-3.3.1.min.js",
                     integrity_ "sha384-tsQFqpEReu7ZLhBV2VZlAu7zcOV+rXbYlF2cqB8txI/8aZajjp4Bqd+V6D5IgvKT",
                     crossorigin_ "anonymous"] empty
            script_ [src_ "/js/simpleXML.js", type_ "text/javascript"] empty
            link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/simpleXML.css"]
            title_ [] "XEditor for browser"
        body_ $ do
            div_ [id_ "simpleUseCase"] ""
            script_ [src_ "/js/main.js", type_ "text/javascript"] empty

main :: IO ()
main = scotty 8888 $ do
    middleware $ staticPolicy $ addBase "static" >-> 
        (contains "/js/" <|> contains "/css/" <|> contains "/images/")
    get "/" . html . renderText $ indexHTML
