{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Arrow 
import Control.Monad 
import Control.Monad.Trans 
import System.Directory
import Data.List
import Data.Either
import MiscFSTools

import Happstack.Server hiding (body)
import qualified Happstack.Server as HS
import Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept, method)
import qualified Text.Blaze.Html5.Attributes as A

main :: IO ()

{-
--main = (getAllSubdirs "a" ) >>= return.show >>= putStrLn 
main = 
  createGroup "blkio/bar" >>
  insertPid "blkio/bar" "1061" >>
  localState "blkio/bar" >>= return . either Prelude.id show >>= putStrLn
--main = globalState >>= return . either Prelude.id show >>= putStrLn
-}
main = simpleHTTP nullConf $ msum 
       [ do method GET >> ( uriRest ( drop 1 >>> onGet) ) 
       , do method PUT >> ( uriRest ( drop 1 >>> onPut))
       , (badRequest.ourTemplate) "Just GET or PUT please"
       ]

onGet,onPut :: String -> ServerPartT IO Response

onGet rel = liftIO ( localState rel >>= either (badpage rel) (goodpage rel)) >>= (ok.ourTemplate)
      
badpage :: String -> String -> IO Html      
goodpage :: String -> Viewpoint -> IO Html
    
  {-
    ( ifM ( doesDirectoryExist abs )
          ( getDirectoryContents abs >>= \ dc -> -- (getDirectoryContents abs) returns IO [String]
            readFile (abs++"/tasks") >>= \tasks ->
            return ( table $ tr $
              ( td $ table ( forM_ (drop 1 dc)   ( \el -> (tr . td . (a ! href (toValue el)).toHtml) el ) ) ) >>
              ( td $ table ( forM_ (words tasks) (tr.td.toHtml)   ) ) 
            )
          )
          ( return (toHtml ("Nothing there"::String)) )
   )
   -}


badpage rel = toHtml >>> h1 >>> return 

makeLink :: String -> String -> Html
--makeLink s = H.a ! href (toValue s) ( toHtml . (if s=="" then "Home" else s) )
makeLink rel = \el -> (tr . td . (a ! href (toValue (rel++"/"++el))).toHtml) el 

goodpage rel ((parent, children),(pidsin, pidsout)) = return $
   h3 (toHtml rel) >>
   table ( tr (
     (td . table) (forM_ (parent:children) (makeLink rel >>> td >>> tr)) >>
     td ("Bar") >>
     td ("Wop") 
   ))

onPut rel = page $ ("You put "++rel,([],[]))

-- Takes (header, (buttons,rows)) and draws a single column table of links...
page :: (String, ([String],[String])) -> ServerPartT IO Response
page (ford,(buttons,pidlist)) = 
  (ok.ourTemplate.toHtml) $ 
    table $ 
    (tr.th.toHtml) ford >> 
    forM_ buttons (tr.td.mybutton) >> 
    ( forM_ pidlist ( \el -> (tr.td.(a ! href (toValue el)).toHtml) el ) )

ourTemplate :: Html -> Response
ourTemplate s =
   toResponse $
    appTemplate "CGroup Manipulator"
                [ meta ! name "keywords"
                       ! content "happstack, blaze, html"
                , mystyle
                , myscript
                ]
                s

mybutton :: String -> Html
mybutton s = H.form ! A.method "PUT" $ 
              ( input ! type_ "text" 
                       ! name "name"
              ) >> 
              ( input ! type_ "submit" 
                      ! value "Create CGroup" 
                      ! onclick (toValue $ "window.href=" ++ s ++ ";")
              )

myscript, mystyle :: Html
myscript = script $ toHtml (""::String)
mystyle = H.style $ toHtml ("\
  \td { vertical-align:top;}"::String)

appTemplate :: String -> [Html] -> Html -> Html
appTemplate tit headers bod =
    html $ do
      H.head $ do
        title (toHtml tit)
        meta ! httpEquiv "Content-Type"
               ! content "text/html;charset=utf-8"
        sequence_ headers
      body $ do
        bod


