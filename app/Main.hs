{-# LANGUAGE OverloadedStrings #-}

module Main where

import NLP.Mystem.IO
import qualified Data.Text as T
import Control.Monad (forM_)

main :: IO ()
main = do
  res <- getStems $ T.words "Съешь ещё этих мягких французских булок"
  forM_ res printMSRes -- sample output
