{-# LANGUAGE OverloadedStrings #-}

module Persistence where

import Foreign.Ptr
import Foreign.ForeignPtr

import Database.LMDB.Raw
import Data.Maybe
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.HashMap.Strict as M
import qualified System.EasyFile as FS
import qualified System.IO (FilePath)
import Control.Monad
import Control.Concurrent


-- -----------------------------------------------------------------------------
-- | Lambda Database: a usecase of lmdb
-- -----------------------------------------------------------------------------

-- Remove MDB_WRITEMAP, coz it enlarges db to 4gb on Mac OS
lmdbEnvF :: [MDB_EnvFlag]
lmdbEnvF = [MDB_NOLOCK]

data Lambdadb = Lambdadb {
    db_env    :: !MDB_env,
    db_signal :: !(MVar ()),
    db_commit :: !(MVar Commit)
}

type Commit = M.HashMap ByteString ByteString

writeflags = compileWriteFlags []

-- | Init lmdb env
initEnv fp = do
    FS.createDirectoryIfMissing False fp
    env <- mdb_env_create
    mdb_env_set_mapsize env (65536 * 65536)
    mdb_env_set_maxreaders env 128
    mdb_env_set_maxdbs env 4
    mdb_env_open env fp lmdbEnvF
    -- mdb_env_info env >>= print
    return env

-- | Open or Create db
-- Initiate sample data and seq infos
initDb fp = do
    env <- initEnv fp
    txn <- mdb_txn_begin env Nothing False
    data_dbi <- mdb_dbi_open' txn (Just "@") [MDB_CREATE]
    ref_dbi  <- mdb_dbi_open' txn (Just "#") [MDB_CREATE]
    put txn ref_dbi ("block", "0")
    put txn ref_dbi ("tx#", "0")
    mdb_txn_commit txn

-- | Truncate a Database
clearDb db = do
    env <- initEnv "data.mdb"
    txn <- mdb_txn_begin env Nothing False
    dbi <- mdb_dbi_open txn (Just db) []
    mdb_clear txn dbi
    mdb_txn_commit txn

-- | Start lmdb with lambda writer
start_lmdb :: IO Lambdadb
start_lmdb = do
    env <- initEnv "data.mdb"
    dbSignal <- newMVar ()
    dbCommit <- newMVar mempty
    let db = Lambdadb {
        db_env = env,
        db_signal = dbSignal,
        db_commit = dbCommit
    }
    mdb_env_info env >>= print
    forkIO (lambdaWriter db)
    threadDelay 2000000
    return db

-- | Open lmdb with a transaction on one specified database
open_lmdb :: String -> IO (MDB_txn, MDB_dbi')  
open_lmdb db = do
    env <- initEnv "data.mdb"
    txn <- mdb_txn_begin env Nothing False
    dbi <- mdb_dbi_open' txn (Just db) []
    return (txn, dbi)

-- | Clean then re-init all databases
reset_lmdb = do
    clearDb "@"
    clearDb "#"
    initDb "data.mdb"

-- -----------------------------------------------------------------------------
-- | Basic Get/Put Commands
-- -----------------------------------------------------------------------------

-- | Find value in specified db, no lmdb instance required
find' :: String -> ByteString -> IO ByteString
find' db key = do
    (txn, dbi) <- open_lmdb db
    val        <- withBS_as_val key $ get txn dbi
    mdb_txn_commit txn
    case val of
        Nothing -> return "Nothing in lambda-db"
        Just v  -> return v

-- | Find value in specified db
find :: Lambdadb -> String -> ByteString -> IO (Maybe ByteString)
find db t k = do -- t: db name, # or @
    txn <- mdb_txn_begin (db_env db) Nothing True
    dbi <- mdb_dbi_open' txn (Just t) []
    v <- withBS_as_val k $ get txn dbi
    mdb_txn_commit txn
    return v

-- | Try N times to find a value
try 0 f = return "Nothing"
try n f = do
    threadDelay 32768
    val <- f 
    if (val == Nothing) then try (n - 1) f else return (fromJust val)

-- | Push a single record to db    
push_single :: Lambdadb -> (ByteString, ByteString) -> IO ()
push_single db (k, v) = do
    modifyMVarMasked_ (db_commit db) $ \lst -> return (M.insert k v lst)
    dbSignal db

-- | Push a commit (set of record) to db    
push_commit :: Lambdadb -> Commit -> IO ()
push_commit db cm = do
    modifyMVarMasked_ (db_commit db) $ \lst -> return (M.union cm lst)
    dbSignal db

-- -----------------------------------------------------------------------------
-- | Transaction Commands
-- -----------------------------------------------------------------------------

-- get the last key in sequence   
get_ref txn dbi k = do
    val <- withBS_as_val (key_prefix k) $ get txn dbi
    case val of
        Nothing -> do
            print "LMDB: Can't find the sequence reference"
            return "00"
        Just v  -> return v

-- Increment the sequence and put a 
put_ref txn dbi k = do
    key_seq  <- get_ref txn dbi k
    let current_seq =  read (C.unpack key_seq) :: Int
    let new_seq     = C.pack $ show $ current_seq + 1
    update txn dbi (key_prefix k, new_seq)
    return $ C.append (key_prefix k) $ C.append "#" new_seq

get :: MDB_txn -> MDB_dbi' -> MDB_val -> IO (Maybe ByteString)
get txn dbi k = do
    mdb_val <- mdb_get' txn dbi k
    case mdb_val of
        Nothing -> return Nothing
        Just v -> do
            val <- mdbVal_to_BS v
            return $ Just val

put txn dbi (k, v) = do
    withBS_as_val k $ \key ->
        withBS_as_val v $ \value ->
            mdb_put' writeflags txn dbi key value

update txn dbi (k, v) = do
    withBS_as_val k $ \key ->
        withBS_as_val v $ \value -> do
            mdb_del' txn dbi key Nothing
            mdb_put' writeflags txn dbi key value

commit_txn txn = mdb_txn_commit txn            

-- -----------------------------------------------------------------------------
-- | Helpers: cast between bytestring and lmdb object
-- -----------------------------------------------------------------------------

-- | Using bytestring as lmdb val
withBS_as_val :: ByteString -> (MDB_val -> IO a) -> IO a
withBS_as_val s action = withBS s $ \p len -> 
    action (MDB_val (fromIntegral len) p)

-- | Foreign Pointer: withForeignPtr
-- The type ForeignPtr represents references to objects that are maintained in a foreign language, i.e.,
-- that are not part of the data structures usually managed by the Haskell storage manager. 
withBS :: ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withBS (BS.PS fp off len) action =
    withForeignPtr fp $ \p ->
        action (p `plusPtr` off) len

-- | Val to Bytestring        
mdbVal_to_BS :: MDB_val -> IO BS.ByteString
mdbVal_to_BS (MDB_val mv_size mv_data)
    | (mv_size == 0) = return BS.empty
    | otherwise = BS.create len $ \dst -> BS.memcpy dst mv_data len
        where len = fromIntegral mv_size
    
-- Return the prefix of a key
key_prefix :: ByteString -> ByteString        
key_prefix = C.takeWhile (/='#')

-- -----------------------------------------------------------------------------
-- | LambdaDb Writer
-- -----------------------------------------------------------------------------
dbSignal :: Lambdadb -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return () --A non-blocking version of putMVar

lambdaWriter db = do
    -- |  Get write signal
    {-  About takeMVar:
        If the MVar is currently empty, takeMVar will wait until it is full.
        After a takeMVar, the MVar is left empty.-}
    takeMVar (db_signal db) 

    -- | Take out commits (as write set) and put an empty set to db_commit MVar
    {-  About swapMVar:
        Take a value from an MVar, put a new value into the MVar and return the value taken
        This function is atomic only if there are no other producers for this MVar.-}
    cm <- swapMVar (db_commit db) (M.empty) -- pop commits out
    txn <- mdb_txn_begin (db_env db) Nothing False
    data_dbi <- mdb_dbi_open' txn (Just "@") []
    ref_dbi  <- mdb_dbi_open' txn (Just "#") []
    forM_ (M.toList cm) $ \(k, v) -> do
        look_ <- withBS_as_val k $ get txn data_dbi
        case look_ of
            Nothing -> do  -- case: write new
                ref <- put_ref txn ref_dbi k       -- increment seq
                put txn ref_dbi (ref, k)           -- put ref key
                put txn data_dbi (k, v)            -- put data
            Just val -> do -- case: override
                print "key found, replace old value"
                update txn data_dbi (k, v)
    -- commit
    mdb_txn_commit txn
    -- Thread Delay (optional)
    -- threadDelay 480000
    lambdaWriter db