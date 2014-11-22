{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Arrow 
import Control.Monad 
import Data.Monoid 
import Control.Monad.Trans 
import System.Directory
import Data.List
import Data.Either
import CGroupTools

import Happstack.Server hiding (body)
import qualified Happstack.Server as HS
import Text.Blaze.Html5 hiding (map, head)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes hiding (title, rows, accept, method)
import qualified Text.Blaze.Html5.Attributes as A
import Data.ByteString.Lazy.Char8 (unpack)

main :: IO ()

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

main = simpleHTTP nullConf $ msum 
       [ do method GET >> ( uriRest onGet ) 
       , do method PUT >> decodeBody myPolicy >> 
              -- I wanted the body to simply be the pid without the pid=
              --  but happstack wouldn't play ball
              (look "pid" >>= ( onPut >>> uriRest )) 
              `mappend` -- catch the very common case that look fails cos there's no pid
              ( uriRest $ onPut "" ) -- So the putter in the other file interprets this
              -- blank to mean createGroup instead of insertPid. That looks goofy in 
              -- haskell but it's already like that in REST so we wouldn't gain
              -- generality by splitting it up.
       , (ourTemplate >>> badRequest) "Just GET or PUT please"
       ]

onGet :: String -> ServerPartT IO Response
onPut :: String -> String -> ServerPartT IO Response
--onPut url = (ourTemplate >>> ok) $ h3 $ toHtml $ "You put "++url
onGet url     = liftIO ( uiBundle url        >>= renderGet url ) >>= (ourTemplate >>> ok) 
onPut bod url = liftIO ( putSomething bod url >> renderPut     ) >>= (ourTemplate >>> ok) 
      
renderPut :: IO Html
renderPut = return $ toHtml ("Foo"::String)

renderGet :: String -> UIBundle -> IO Html
renderGet url (mounts,ei) = return $ do
  h1 (toHtml url) 
  table ! class_ "nav" $ tr ! class_ "nav" $ do
    td $ a ! href "/" $ "Help"
    forM_ (map (fst >>> makeLink True) mounts) td
  ( either (toHtml >>> h1) (renderGroup url) ) ei

renderGroup :: String -> CGroupState -> Html
renderGroup url (maybeparent, children, pidsin, pidsout) = do
  table ! customAttribute "border" "1" ! class_ "big" $ do
    tr $ do
      th $ do
        "Children ... "
        H.a ! onclick (toValue $ "newChild('"++url++"');") $ "New"
      th "Pids in here"  
      th "Pids elsewhere"  
    tr ! class_ "big" $ do
      td  ! class_ "col" $ do 
        forM_ (maybe children (:children) maybeparent) (makeLink2 url >>> H.div) 
      td  ! class_ "col" $ do 
        forM_ pidsin $ \pid -> do 
          H.span " "
          H.span ! class_ "pid" $ toHtml pid
          H.span " "
        
        --(toHtml >>> H.span ! class_ "pid")
      td  ! class_ "col" $ do 
        forM_ pidsout $ \pid -> do 
        --(toHtml >>> H.span ! class_ " pid pidout" ! onclick (toValue ("insertPid('"++url++"',"++pid++")"))) pid)
          H.span " "
          H.span ! class_ "pid pidout"  ! onclick (toValue ("insertPid('"++url++"',"++pid++")")) $ toHtml pid
          H.span " "

makeLink :: Bool -> String -> Html
makeLink abs el = (toHtml >>> a ! href (toValue (if abs then "/"++el else el))) el 

makeLink2 :: String -> String -> Html
makeLink2 rel = \el -> ( (a ! href (toValue (rel++"/"++el))).toHtml) el 

ourTemplate :: Html -> Response
ourTemplate s =
   toResponse $
    appTemplate "CGroup Manipulator"
                [ meta ! name "keywords"
                       ! content "happstack, blaze, html"
                , script "" ! src "http://code.jquery.com/jquery-2.1.1.min.js"
                , myscript
                , mystyle
                ]
                s

myscript, mystyle :: Html

myscript = script $ toHtml ("                      \
  \function newChild(parent) {                   \n\
  \  var child = prompt('Name for new group?');  \n\
  \  if (child!=null) {                          \n\
  \    var abs = parent+'/'+child;               \n\
  \    $.ajax({                                  \n\
  \      type:'PUT',                             \n\
  \      url:abs,                                \n\
  \      data:'',                                \n\
  \      success:function () {                   \n\
  \        window.location.replace(abs);         \n\
  \    }});                                      \n\
  \  }                                           \n\
  \}                                             \n\
  \                                              \n\
  \function insertPid(group, pid) {              \n\
  \  $.ajax({                                    \n\
  \    type:'PUT',                               \n\
  \    url:group,                                \n\
  \    data:{'pid':pid},                         \n\
  \    success:function () {                     \n\
  \      window.location.replace(group);         \n\
  \  }});                                        \n\
  \}                                             \n\
  \                                              \n\
  \                                              \n\
  \"::String)

mystyle = H.style $ toHtml ("\
  \td { vertical-align:top; } \n\
  \a, .pidout { background-color:lightblue; } \n\
  \.pid { padding-right:5px; padding-left:5px; } \n\
  \.big, .nav { width:100%; } \n\
  \.col { width:33%; } \n"::String)

appTemplate :: String -> [Html] -> Html -> Html
appTemplate tit headers bod =
  docType >> (
  html $ do
    H.head $ do
      title (toHtml tit)
      meta ! httpEquiv "Content-Type"
             ! content "text/html;charset=utf-8"
      sequence_ headers
    body $ do
      bod )


