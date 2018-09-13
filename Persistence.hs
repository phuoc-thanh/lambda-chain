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

lmdbEnvF :: [MDB_EnvFlag]
lmdbEnvF = [MDB_NOLOCK, MDB_WRITEMAP]

data Lambdadb = Lambdadb {
    db_env    :: !MDB_env,
    db_signal :: !(MVar ()),
    db_commit :: !(MVar Commit)
}

type Commit = M.HashMap ByteString ByteString

writeflags = compileWriteFlags []

-- Init lmdb env
initEnv fp = do
    FS.createDirectoryIfMissing False fp
    env <- mdb_env_create
    mdb_env_set_mapsize env (65536 * 65536)
    mdb_env_set_maxreaders env 128
    mdb_env_set_maxdbs env 4
    mdb_env_open env fp lmdbEnvF
    -- mdb_env_info env >>= print
    return env

-- Open or Create db
initDb fp = do
    env <- initEnv fp
    txn <- mdb_txn_begin env Nothing False
    data_dbi <- mdb_dbi_open' txn (Just "@") [MDB_CREATE]
    ref_dbi  <- mdb_dbi_open' txn (Just "#") [MDB_CREATE]
    put txn ref_dbi ("blocks", "1")
    mdb_txn_commit txn

-- Truncate a Database
clearDb db = do
    env <- initEnv "data.mdb"
    txn <- mdb_txn_begin env Nothing False
    dbi <- mdb_dbi_open txn (Just db) []
    mdb_clear txn dbi
    mdb_txn_commit txn

-------------------------------- Basic Commands --------------------------------

-- get the reference value
get_ref txn dbi k = do
    val <- withBS_as_val k $ get txn dbi
    return $ fromJust val
    

find :: Lambdadb -> String -> ByteString -> IO (Maybe ByteString)
find db t k = do -- t: db name, # or @
    txn <- mdb_txn_begin (db_env db) Nothing True
    dbi <- mdb_dbi_open' txn (Just t) []
    v <- withBS_as_val k $ get txn dbi
    mdb_txn_commit txn
    return v

-- try N times
try 0 f = return "Nothing"
try n f = do
    threadDelay 32768
    val <- f 
    if (val == Nothing) then try (n - 1) f else return (fromJust val)

push_single :: Lambdadb -> (ByteString, ByteString) -> IO ()
push_single db (k, v) = do
    modifyMVarMasked_ (db_commit db) $ \lst -> return (M.insert k v lst)
    dbSignal db

push_commit :: Lambdadb -> Commit -> IO ()
push_commit db cm = do
    modifyMVarMasked_ (db_commit db) $ \lst -> return (M.union cm lst)
    dbSignal db

-------------------------------- Transaction Commands --------------------------------

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
                   

-------------------------------- Helpers Section --------------------------------

-- | helpers to cast between bytestring and lmdb object
--

-- using bytestring as lmdb val
withBS_as_val :: ByteString -> (MDB_val -> IO a) -> IO a
withBS_as_val s action = withBS s $ \p len -> 
    action (MDB_val (fromIntegral len) p)

-- - Foreign Pointer: withForeignPtr
-- | The type ForeignPtr represents references to objects that are maintained in a foreign language, i.e.,
-- that are not part of the data structures usually managed by the Haskell storage manager. 

withBS :: ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
withBS (BS.PS fp off len) action =
    withForeignPtr fp $ \p ->
        action (p `plusPtr` off) len

-- Val to Bytestring        
mdbVal_to_BS :: MDB_val -> IO BS.ByteString
mdbVal_to_BS (MDB_val mv_size mv_data)
    | (mv_size == 0) = return BS.empty
    | otherwise = BS.create len $ \dst -> BS.memcpy dst mv_data len
        where len = fromIntegral mv_size
    
        
-------------------------------- Writer Setup --------------------------------

dbSignal :: Lambdadb -> IO ()
dbSignal db = tryPutMVar (db_signal db) () >> return () --A non-blocking version of putMVar

lambdaWriter db = do
    -- Get write signal
    -- About takeMVar:
    -- If the MVar is currently empty, takeMVar will wait until it is full.
    -- After a takeMVar, the MVar is left empty.
    takeMVar (db_signal db) 

    -- Take out commits (as write set) and put an empty set to db_commit MVar
    -- About swapMVar:
    -- Take a value from an MVar, put a new value into the MVar and return the value taken
    -- This function is atomic only if there are no other producers for this MVar.
    cm <- swapMVar (db_commit db) (M.empty) --pop commits out
    txn <- mdb_txn_begin (db_env db) Nothing False
    data_dbi <- mdb_dbi_open' txn (Just "@") []
    ref_dbi  <- mdb_dbi_open' txn (Just "#") []
    forM_ (M.toList cm) $ \(k, v) -> do
        look_ <- withBS_as_val k $ get txn data_dbi
        case look_ of
            Nothing -> do                          -- case: write new
                update txn ref_dbi ("current_block", k) 
                put txn data_dbi (k, v) 
            Just val -> update txn data_dbi (k, v) -- case: override
    -- commit
    mdb_txn_commit txn
    -- Thread Delay - Optional
    -- threadDelay 480000
    lambdaWriter db

-------------------------------- Test Section --------------------------------

openDb = do
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

resetDb = do
    clearDb "@"
    clearDb "#"
    initDb "data.mdb"