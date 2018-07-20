module CReader where

  import System.IO
  import Control.Monad.Reader
  import Control.Applicative
  import System.Environment
  import Control.Monad.State

  data AppConfig = AppConfig {
      logFile :: FilePath
    , version :: String
    , maxMessageLength :: Int
  } deriving (Show, Read)

  initLogFile :: String -> AppConfig -> IO Handle
  initLogFile preamble config = do
    handle <- openFile (logFile config) WriteMode
    hPutStrLn handle (preamble ++ ", version: " ++ version config)
    return handle

  validateMessage :: String -> AppConfig -> Either String ()
  validateMessage msg config =
    if (length msg > maxMessageLength config)
    then Left ("Message too long: " ++ msg)
    else Right ()

  type ConfigReader a = AppConfig -> a

  initLogFileTS :: String -> ConfigReader (IO Handle)
  initLogFileTS = initLogFile

  validateMessageTS :: String -> ConfigReader (Either String ())
  validateMessageTS = validateMessage

  validateAndInitLogTS :: String -> ConfigReader (IO (Maybe Handle))
  validateAndInitLogTS prompt config =
    case validateMessage prompt config of
        Left msg -> putStrLn ("Invalid prompt: " ++ msg) >> return Nothing
        Right () -> Just <$> initLogFile prompt config

  newtype CReader a = CReader {runCR :: AppConfig -> a}

  initLogFileCR :: String -> CReader (IO Handle)
  initLogFileCR preamble = CReader $ \c -> initLogFile preamble c

  validateMessageCR :: String -> CReader (Either String ())
  validateMessageCR p = CReader $ \c -> validateMessage p c

  validateAndInitLogCR :: String -> CReader (IO (Maybe Handle))
  validateAndInitLogCR p = CReader $ \c ->
    case runCR (validateMessageCR p) c of
        Left err -> putStrLn ("Invalid prompt: " ++ err) >> return Nothing
        Right () -> Just <$> runCR (initLogFileCR p) c

  runCRWithConfig :: AppConfig -> IO Handle
  runCRWithConfig config = do
    let result = runCR (validateAndInitLogCR "Hello CR") config
    mh <- result
    case mh of Nothing -> error "Log file init failed"
               Just h  -> return h

  instance Functor CReader where
    fmap f (CReader g) = CReader $ f . g


  askConfig :: CReader AppConfig
  askConfig = CReader id

  validateMessageF :: String -> CReader (Either String ())
  validateMessageF p = fmap (validateMessage p) askConfig
--   We defined a functor so that we don;t have to explicitly wrap and unwrap which is why we needed
--   askConfig, atleast now I understand why we need askConfig - its a way to get config basically a function which
--   returns the input i.e the environment here !
--   validateMessageF p = CReader $ \c -> validateMessage p c

  initLogFileF :: String -> CReader (IO Handle)
  initLogFileF p = fmap (initLogFile p) askConfig

-- Below because we would like to run two functions that return CReader after application of a function
-- we need a monad to sequence initLogFileF after validateMessageF otherwise you would end up with
-- CReader (CReader)
--   validateAndInitLogF :: String -> CReader (IO (Maybe Handle))
--   validateAndInitLogF p = fmap doInit (validateMessageF p)
--         where doInit :: Either String () -> (IO (Maybe Handle))
--               doInit (Left err) = putStrLn ("Invalid prompt" ++ p) >> return Nothing
--               doInit (Right ()) = runCR (initLogFileF p)

  instance Applicative CReader where
    pure a = CReader $ \c -> a

    f <*> g = CReader $ \c -> (runCR f) c (runCR g c)

  instance Monad CReader where
    return a = CReader $ \c -> const a c

    a >>= f  = CReader $ \c -> runCR (f ((runCR a) c)) c

  validateAndInitLogM :: String -> CReader (IO (Maybe Handle))
  validateAndInitLogM p = do
        either <- validateMessageF p
        case either of
            Left err -> return (putStrLn ("Invalid prompt: " ++ err) >> return Nothing)
            Right () -> do
                ioHandle <- initLogFileF p
                return $ Just <$> ioHandle

  validateMsgRdr :: String -> Reader AppConfig (Either String ())
  validateMsgRdr msg = do
    max <- reader maxMessageLength
    if (length msg > max)
    then return $ Left ("Message too long: " ++ msg)
    else return $ Right ()

  initLogFileRdr :: String -> Reader AppConfig (IO (Maybe Handle))
  initLogFileRdr msg = do
    f      <- reader logFile
    ver    <- reader version
    return $
        Just <$> do
            handle <- liftIO $ openFile f WriteMode
            hPutStrLn handle (msg ++ ", version: " ++ ver)
            return handle

  initLogFileRdrT :: String -> ReaderT AppConfig IO Handle
  initLogFileRdrT msg = do
        f <- reader logFile
        v <- reader version
        h <- liftIO $ openFile f WriteMode
        liftIO $ hPutStrLn h (msg ++ ", versionL " ++ v)
        return h



