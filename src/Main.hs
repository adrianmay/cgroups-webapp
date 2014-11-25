{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Arrow 
import Control.Monad 
import Data.Monoid 
import Control.Monad.Trans 
import System.Directory
import System.Environment   
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

defaultPort = 8000::Int;

myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

main = getArgs >>= (
         -- read port from command line and check it's an integer
         (\case 
           (p:_) -> intFromStringOr defaultPort p
           [] -> defaultPort) >>> \port ->
         -- start web server   
         simpleHTTP nullConf { port = port } $ msum 
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
         ] )

onGet :: String -> ServerPartT IO Response 
onPut :: String -> String -> ServerPartT IO Response

--uiBundle is everything the UI needs to know from the lower level...
onGet url     = liftIO ( uiBundle url         >>= renderGet url ) >>= (ourTemplate >>> ok) 

onPut bod url = liftIO ( putSomething bod url >>= renderPut     ) >>= (toResponse >>> ok) 
      
renderPut :: (Maybe String) -> IO Html -- nobody reads the response, but it has to arrive before the JS UI redirects
renderPut = maybe ( (return.toHtml) (""::String)) -- to the new version of the cgroup page
                  ( return.toHtml )

renderGet :: String -> UIBundle -> IO Html
renderGet url (mounts,ei) = return $ do
  h1 (toHtml url) 
  table ! class_ "nav" $ tr ! class_ "nav" $ do
    td $ a ! href "/" $ "Help"
    forM_ (map (fst >>> makeLink True) mounts) td
  ( either instructions (renderGroup url) ) ei -- Can write the help here after the error message in the h1

instructions :: String -> Html
instructions title = 
  h1 (toHtml title) >>
  p "The nav bar shows mounted cgroup subsystems. Click on one to see the root cgroup in that subsystem." >>
  p "The first column shows links to the children of the current group. This list may start with a link to the parent of the current group if the latter is not the root cgroup of a subsystem. Clicking on these links makes the addressed group the current one." >>
  p "A new group in the current one can be created with the New button on the column header. This prompts for the name of the new group." >>
  p "The second column shows PIDs in the current group. The third shows all other PIDs in the system. Clicking on those pulls them from whatever group of this subsytem they're in now, and inserts them in the current group. Not all PIDs want to move, but I don't know why." >>
  p "Making changes to the cgroup state, i.e. adding groups or moving PIDs, requires that cgroups-webapp is running as root." 


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
  \      error:function (x, res) {               \n\
  \        alert('ERROR');                       \n\
  \      },                                      \n\
  \      success:function (res) {                \n\
  \        if (res!='') alert(res);              \n\
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
  \    error:function (x, res) {                 \n\
  \      alert('ERROR'+res);                     \n\
  \    },                                        \n\
  \    success:function (res) {                  \n\
  \      if (res!='') alert(res);                \n\
  \      window.location.replace(group);         \n\
  \  }});                                        \n\
  \}                                             \n\
  \                                              \n\
  \                                              \n\
  \ "::String)

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


