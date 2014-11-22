{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module CGroupTools (uiBundle, UIBundle, CGroupState, putGroup, (>^>)) where

-- The UI will address cgroups by subsystem, not mount point

import Control.Arrow
import Control.Applicative
import Control.Monad
import System.IO
import System.Directory
import Data.List
import Data.Monoid
import Data.Either

-- Random crumbs --------------------------------------------

ifM cond tc fc = cond >>= \res -> if res then tc else fc
choose tc fc cond = if cond then tc else fc

infixr 1 >^>
(>^>) :: (Arrow a) => a z (b,c) -> (b -> c -> d) -> a z d
(>^>) a f = a >>> (arr.uncurry) f

parentPath :: FilePath -> FilePath
parentPath = reverse >>> dropWhile (/='/') >>> drop 1 >>> reverse 


-- General directory operations -----------------------------

getDirectSubdirs :: FilePath -> IO [FilePath]
-- not including abs
getDirectSubdirs abs =  
  -- get a list of entries in the dir ...
  getDirectoryContents abs >>= 
  -- take out . and .. ...
  filterM ( ( (/=".") &&& (/="..") >^> (&&) ) >>> return ) >>=
  -- filter on ones that are directories and not files ...
  filterM ( ((abs++"/")++) >>> doesDirectoryExist ) >>= 
  return


getAllSubdirs :: FilePath -> IO [FilePath]
-- including abs
getAllSubdirs abs = 
  -- get a list of entries in the dir ...
  getDirectSubdirs abs >>= 
  -- join filename parts..
  ( map ( (abs++"/") ++) >>> return ) >>=
  -- recurse for each ...
  mapM getAllSubdirs >>= 
  ( concat >>> (abs:)  >>> return )

-- Types ------------------------------------------------------

type Subsys = String
type Mount = (Subsys, FilePath)

type Pid = String
type CGroupState = ( -- of a cgroup
  Maybe FilePath, -- parent
  [FilePath], -- children
  [Pid], -- pids included 
  [Pid]) -- pids elsewhere

type UIBundle = ([Mount], Either String CGroupState)


-- CGroup stuff ------------------------------------------------

-- take absolutely any url extension from the UI and maybe interpret it
-- mounts might be returned empty
uiBundle :: String -> IO UIBundle
uiBundle ('/':url) =
  let 
    subsys = takeWhile (/='/') url
    group = (dropWhile (/='/') >>> drop 1) url
  in   
    getMounts >>= \mounts -> 
      case url of
        [] -> ( Left >>> (mounts,) >>> return ) "Instructions"  
        _ -> case (filter (fst >>> (==subsys)) mounts ) of 
           (m:[]) -> cGroupState m subsys group >>= ( Right >>> (mounts,) >>> return )
           _ -> ( Left >>> (mounts,) >>> return ) "Bad subsystem requested"  
    

-- look in /proc/mounts for cgroup mounts
getMounts :: IO [Mount]
getMounts = 
  (readFile "/proc/mounts" >>=
   ( lines >>> map words >>> 
    filter ( \(w:_) -> w=="cgroup" ) >>>
    map ( \(_:mountpoint:_:options:_) -> (ss options, mountpoint) ) >>>
    return ))-- might return empty list
  where 
    ss = map (\case ',' -> ' '; c -> c) >>> 
      words >>> 
      filter ((||) <$> flip elem allSubsystems <*> (drop 5 >>> flip elem allSubsystems)) >>> 
      map (\case (n:a:m:e:'=':rest) -> rest; s -> s) >>>
      intercalate ","

-- maybe we can ask the system for this...
allSubsystems = ["systemd", "blkio", "cpu", "cpuacct", "cpuset", "devices", 
                 "freezer", "memory", "net_cls", "net_prio", "ns" ]


cGroupState :: Mount -> String -> FilePath -> IO CGroupState
--cGroupState mount subsys [] = return $ (Nothing, ["Foo"], [], [])
cGroupState mount subsys group = 
  let 
    abs = if length group == 0 then snd mount else snd mount ++ "/" ++ group 
  in
    getDirectSubdirs abs >>= \children -> 
    pidsInAndOut (snd mount) abs >>= \(pidsin, pidsout) ->
    return (Just "..", children, pidsin, pidsout)


pidsInAndOut :: FilePath -> FilePath -> IO ([Pid],[Pid])
-- both parameters are absolute paths
pidsInAndOut mountpoint group =
  pidsAt group >>= \pidsin ->
  getAllSubdirs mountpoint >>= 
  ( (filter (/= group)) >>> mapM pidsAt ) >>= \pidsout ->
  return (pidsin, concat pidsout)


pidsAt :: FilePath -> IO [Pid]
pidsAt dir = readFile (dir ++ "/tasks") >>= ( return . words )


-- PUT stuff ----------------------------------------------------
-- TODO: 
--   Catch file op failures 
--   Sanity check pid

putGroup :: String -> String -> IO ()
-- called by ajax and return discarded
-- see result on next get
putGroup bod ('/':url) = 
  let 
    subsys = takeWhile (/='/') url
    group = (dropWhile (/='/') >>> drop 1) url
    parent = (reverse >>> dropWhile (/='/') >>> drop 1 >>> reverse) group
  in   
    getMounts >>= \mounts -> 
    case (filter (fst >>> (==subsys)) mounts ) of 
      ((_,mp):[]) -> 
        case bod of 
          [] -> createGroup (mp ++ "/" ++ parent) (mp ++ "/" ++ group)
          _  -> insertPid (mp ++ "/" ++ group) bod
      _ -> return ()


createGroup :: String -> String -> IO ()
createGroup parentpath grouppath = 
  doesDirectoryExist grouppath >>= choose 
    ( return () )
    ( doesDirectoryExist parentpath >>= choose
      ( createDirectory grouppath )
      --( putStrLn grouppath )
      ( return () )
    )


insertPid :: String -> String -> IO ()
insertPid grouppath pid =
  --sanity check pid here
  writeFile (grouppath++"/tasks") (pid++"\n")



