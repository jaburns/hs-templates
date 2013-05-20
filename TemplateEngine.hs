{-----------------------------------------------------------------------------}
{-- Basic HTML Templating Engine                                jaburns.net --}
{-----------------------------------------------------------------------------}

{-# LANGUAGE OverloadedStrings #-}

module Templates (

    loadView,
    insertContent,
    renderView
    
) where

-------------------------------------------------------------------------------

import           Text.XmlHtml
import qualified System.IO.Strict as SIO
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Blaze.ByteString.Builder (toByteString)
import           Data.Maybe (mapMaybe)

-------------------------------------------------------------------------------

-- Loads and parses an HTML file given a path to it.
loadHtmlFromDisk :: FilePath -> IO Document
loadHtmlFromDisk path = do
    fileContents <- SIO.readFile path
    return $ parseHTMLwithErr $ BS.pack $ fileContents


-- Attempts to parse a ByteString in to an HTML document. If it fails it just
-- returns a valid HTML document containing the error message.
parseHTMLwithErr :: BS.ByteString -> Document
parseHTMLwithErr html =
    case parseHTML "" html of
        Right doc -> doc
        Left  err -> HtmlDocument UTF8 Nothing
                        [ Element "h1" [] [TextNode "Parse Error"],
                          Element "p"  [] [TextNode (T.pack err) ] ]

                          
-- Checks if the provided document starts with a "use-template" element
-- and returns the "name" attribute's value if so.
templateName :: Document -> Maybe T.Text
templateName (HtmlDocument _ _ content) = 
    case head content of 
        Element tag attrs _ -> if tag == "use-template"
                             then lookup "name" attrs
                             else Nothing
        otherwise -> Nothing


-- Returns the content regions which are to be inserted in the template
-- document indexed by their "id" attribute values.
contentBlocks :: Document -> [(T.Text,[Node])]
contentBlocks (HtmlDocument _ _ content) = mapMaybe checkNode content
  where    
    checkNode node = case node of 
        Element tag attrs children ->
              if tag == "define-content"
            then fmap (\id -> (id,children)) (lookup "id" attrs)
            else Nothing
        otherwise -> Nothing
        
        
-- Removes extra whitespace and comment nodes from a document.
stripWhitespace :: Document -> Document
stripWhitespace doc =
    doc { docContent = mapMaybe nodeWithoutWS (docContent doc) }
  where
    nodeWithoutWS node =
        case node of 
            TextNode t -> if T.length (T.strip t) > 0
                        then Just (TextNode (T.strip t)) else Nothing
            Comment _ -> Nothing
            Element a b children ->
                Just $ Element a b $ mapMaybe nodeWithoutWS children
        
        
-- Given a list of named content blocks, this function will insert them
-- in to the supplied template document.
insertAllContents :: [(T.Text,[Node])] -> Document -> Document
insertAllContents ((a,b):rest) doc =
    insertAllContents rest $ insertContent (T.unpack a) b doc
insertAllContents [] doc = doc

        
-- Given the id of a content block and its content nodes, this function
-- will insert the block in to the document if an appropriate <insert-content>
-- tag exists.
insertContent :: String -> [Node] -> Document -> Document
insertContent id contents doc =
    doc { docContent = newChildren (docContent doc) }
  where
    newChildren = concat . map doNode
    
    doNode node =
        case node of 
            Element tag attrs children ->
                if tag == "insert-content" && matchIdAttr attrs then contents
                else [Element tag attrs (newChildren children)]
            otherwise -> [node]
    
    matchIdAttr attrs = case lookup "id" attrs of
        Just x -> if x == T.pack id then True else False
        Nothing -> False


-- This function loads a view file from disk and recursively applies all
-- templates required.
loadView :: FilePath -> IO Document
loadView path = do
    doc <- loadHtmlFromDisk $ "views/" ++ path
    maybeApplyTemplate doc
  where
    maybeApplyTemplate doc = 
        case templateName doc of
            Nothing -> return doc
            Just s -> do
                templateDoc <- loadHtmlFromDisk $ "views/" ++ T.unpack s
                let newDoc = insertAllContents (contentBlocks doc) templateDoc
                maybeApplyTemplate newDoc
    

-- Renders an HTML document to a Haskell string.
renderView = BS.unpack . toByteString . render . stripWhitespace

