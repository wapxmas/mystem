{-# LANGUAGE OverloadedStrings #-}

module Main where

import NLP.Mystem.IO
import qualified Data.Text as T
import Control.Monad (forM_)

main :: IO ()
main = do
  res <- getStems $ T.words "автомобилей активный безусловно большую велосипедистов включено вопроса выбирали выбранных высадку голосовали голосование городской гражданин групп далее доложили запускает заявил зеленых идея измениться комфортного маломобильных масштабного места москвичами московских напомним насаждений нескольким обновлены обсуждали общественного опроса остановок осуществить отметил отремонтировать очередь парковки первую первоочередной переоснащение пешеходов план планируется появиться пребывания провести проголосовали программе продолжит проходило процента рамках реализацию служит совершенствование содержательную создать стоит сторону транспорта удобных условий участники целям центре числе этапов благоустройство должны жители какие мэр преображению проекта работу создание город сергей собянин москва улиц"
  forM_ res printMSRes -- sample output
