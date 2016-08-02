{-# LANGUAGE CPP #-}

module NLP.Mystem.IO where

  import qualified Data.Attoparsec.Text as P
  import qualified Data.Text            as T
  import qualified Data.Text.IO         as TIO
  import           Control.Monad
  import           System.Directory
  import           System.IO
  import           System.Process
  import           Text.Printf

  import qualified NLP.Mystem.Parser        as MP
  import           NLP.Mystem.Types

  printMSRes :: MSRes -> IO ()
  printMSRes (MSRes sw rw) = do
    TIO.putStrLn sw
    forM_ rw $ \w -> do
      TIO.putStrLn (rword w)
      print $ pos w
      flip (maybe (return ())) (grams w) $ \gms ->
        print gms
      forM_ (cases w) $ \gr ->
        print gr

  mystemExecutabe :: FilePath
#ifdef Windows
  mystemExecutabe = "mystem.exe"
#else
  mystemExecutabe = "mystem"
#endif

  mystemParams :: [String]
  mystemParams = ["-nig", "--eng-gr"]

  getStems :: [T.Text] -> IO [MSRes]
  getStems stemWords = do
    res <- runMystem stemWords
    case P.parseOnly MP.parserMystems res of
      Left s -> error $ "Error: " ++ s
      Right r -> return r

  runMystem :: [T.Text] -> IO T.Text
  runMystem stemWords = do
    exists <- doesFileExist mystemExecutabe
    if not exists
      then error $ printf "Mystem executable %s doesn`t exists." mystemExecutabe
      else do
        (i, o, _, ph) <- createProcess (proc mystemExecutabe mystemParams) { std_in = CreatePipe, std_out = CreatePipe }
        res <- flip (maybe (return T.empty)) i $ \hIn ->
                  flip (maybe (return T.empty)) o $ \hOut -> do
                    hSetEncoding hIn utf8
                    hSetBuffering hIn NoBuffering
                    hSetEncoding hOut utf8
                    TIO.hPutStrLn hIn $ T.unlines stemWords
                    TIO.hGetContents hOut
        void $ waitForProcess ph
        return res
