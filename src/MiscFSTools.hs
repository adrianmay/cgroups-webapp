{-# LANGUAGE LambdaCase #-}
module MiscFSTools (
  ifM, (>^>),
  localState, Viewpoint, 
  getAllSubdirs, getDirectSubdirs, 
  createGroup, insertPid
  ) where 
  
-- THIS EXPECTS THAT ALL CGROUPS ARE MOUNTED
--   UNDER A COMMON ROOT SUCH AS /sys/fs/cgroup
-- OTHERWISE IT BARFS GRACEFULLY

  import Control.Arrow
  import Control.Monad
  import System.IO
  import System.Directory
  import Data.List
  import Data.Monoid
  import Data.Either

-- IN THIS MODULE:
-- show direct subdirs of a directory NOT including self
-- show direct or indirect subdirs of a directory INCLUDING self
-- enumerate mounts by grepping /proc/mounts for ^cgroup\s
-- show all pids in one hierarchy
-- show pids in one cgroup
-- show pids not in one cgroup

  -- random crumbs...

  ifM cond tc fc = cond >>= \res -> if res then tc else fc

  infixr 1 >^>
  (>^>) :: (Arrow a) => a z (b,c) -> (b -> c -> d) -> a z d
  (>^>) a f = a >>> (arr.uncurry) f

  parentPath :: FilePath -> FilePath
  parentPath = reverse >>> dropWhile (/='/') >>> drop 1 >>> reverse 

  -- general directory operations...

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


  -- cgroup stuff ---------------------------------------

  -- look in /proc/mounts for cgroup mounts
  getMounts :: IO [FilePath]
  getMounts = 
    readFile "/proc/mounts" >>=
    ( lines >>> map words >>> 
      filter ( \(w:_) -> w=="cgroup" ) >>>
      map ( \(_:c:_) -> c ) >>>
      return -- might return empty list
    )


  type Config = ( -- where are cgroups mounted and which ones are there...
    FilePath,  -- common root of all mounts eg /sys/fs/cgroup (without trailing /) 
    [FilePath] )  -- mounts relative to above (no /s front or back) 

  type PidLists = (
    [String], -- pids in a given group 
    [String] )  -- all the other pids in that hierarchy, i.e. in the computer
  
  type Viewpoint = ( -- the state of the cgroups from the perspective of one group ... 
    Config, -- by sheer coincidence - in fact it's the parent and children of the currently interesting group
    PidLists ) 



  globalState :: IO ( Either String Config )
  globalState =
    getMounts >>= 
    -- all mounts as absolute paths
    \mounts -> return ( map parentPath mounts ) >>= 
    -- shaved off last part of path - expects results all the same
    \case
      [] -> (return . Left) "No cgroups mounted"
      (b:bs) -> 
        if not ( all (==b) bs ) 
        then (return . Left) "Mounted cgroups in confusing places"
        else (return . Right) ( 
          b, -- global base of mounts without trailing /
          map (drop $ (length b+1 )) mounts -- mounts relative to global base
        )


  pidsAt :: FilePath -> IO [String]
  -- just read the tasks file in the passed directory and make list from contents
  pidsAt dir = readFile (dir ++ "/tasks") >>= ( return . words )


  wholehierarchy :: FilePath -> FilePath -> [FilePath]  -> IO [FilePath]
  wholehierarchy b t =  -- base, target, roots
    --roots is a list of cgroup mounts relative to gloabal base e.g. ["blkio","memory","systemd"]
    filter (flip isPrefixOf t) >>> -- which group is the starting string of target - expect one match
    concat >>> -- the one root mount enclosing the target
    ( (b++"/") ++) >>> -- to absolute path
    getAllSubdirs 


  pidlists :: FilePath -> [FilePath] -> IO ( Either String ([String], [String]) )
  pidlists i o =   
    pidsAt i>>= \pidsin -> -- list of pids
    mapM pidsAt o>>= \pidsout -> -- list of list of pids
    (return . Right) (pidsin, concat pidsout)  -- done


  pidsAtNotAt :: FilePath -> Config -> IO ( Either String PidLists ) 
  -- you already read the global config and it's good, 
  -- now get pids inside and outside of the passed group
  pidsAtNotAt target config@(base, roots) =
    wholehierarchy base target roots >>= ( -- list of absolute paths to all roots in whichever hierarchy contains target
       partition (== (base++"/"++target)) >>> \case -- split into two lists: [target] and everything else
          ([],_) -> (return . Left) "Non existent group requested"
          ((match:[]),others) -> pidlists match others -- done
          ((gr:_),_) -> (return . Left) "More than 1 group matched request. Time to eat those boxer shorts."
    )


  localState :: FilePath -> IO (Either String Viewpoint)  
  -- combines the above to make the one call that GET needs
  localState fp = 
    globalState >>= either (return . Left . id) ( \config@(base, roots) -> 
      getDirectSubdirs (base++"/"++fp) >>= \children ->
      pidsAtNotAt fp config >>= either (return . Left . id) (
        \(pidsin, pidsout) -> (return . Right) (
          ( parentPath fp, children ), 
          ( sortBy stringnumbersorter pidsin, 
            sortBy stringnumbersorter pidsout ) )
      )
    ) where stringnumbersorter a b = compare (read a::Integer) (read b::Integer)
  

  createGroup :: FilePath -> IO Bool
  createGroup rp = 
    globalState >>= either (\_ -> return False) ( \config@(base, roots) -> 
        let 
          fp = base ++ "/"++ rp
          allgroupsio = getAllSubdirs base 
          numfound n g = allgroupsio >>= ( filter (==g) >>> length >>> (==n) >>> return )
          pp = parentPath fp 
        in
          ifM ( liftM2 (&&) (numfound 1 pp) (numfound 0 fp) )
              ( createDirectory fp >> return True )
              ( return False )
      ) 

  insertPid :: FilePath -> String -> IO Bool
  insertPid rp pid = 
    globalState >>= either (\_ -> return False) ( \config@(base, roots) -> 
        let 
          fp = base ++ "/"++ rp
          allgroupsio = getAllSubdirs base 
          numfound n g = allgroupsio >>= ( filter (==g) >>> length >>> (==n) >>> return )
        in
          ifM ( numfound 1 fp )
              ( writeFile (fp++"/tasks") (pid++"\n") >> return True )
              ( return False )
      ) 



