{-# LANGUAGE OverloadedStrings #-}
module Duckling.Time.RU.Corpus
  (corpus
  ,negativeCorpus
  )where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month, refTime)
import Duckling.TimeGrain.Types hiding (add)

context :: Context
context = testContext  {locale = makeLocale RU Nothing  }
corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "1 преложение"
      , "недавно"
      , "скоро"
      ]

allExamples :: [Example]
allExamples = concat
    [examples(datetime (2013, 2,12,0,0,0) Day)
           [ "сегодня"
           ]
    , examples(datetime(2013, 2,12,4,30,0) Second)
           [ "сейчас"
           ]   
    , examples (datetime(2013, 2,13,0,0,0) Day)
          [ "завтра"   
           ] 
    , examples (datetime(2013, 2,14,0,0,0) Day)
           [ "послезавтра"   
          ] 
    , examples (datetime(2013, 2,11,0,0,0) Day)
           [ "вчера"   
           ] 
    , examples (datetime(2013, 2,10,0,0,0) Day)
           [ "позавчера"   
           ] 
    , examples (datetime(2013, 2, 18, 0, 0, 0) Day)
           [ "понедельник"
           ] 
    , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
           [ "вторник"
           ]
    , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
           [ "четверг"
           ]
    , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
           [ "пятница"
           ]
    , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
           [ "суббота"
           ]
    , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
           [ "воскресенье"
           ]
    , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
           [ "1 марта"
           , "первого марта"
           ]
    , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
           [ "15 февраля"
           , "15.2"
           ]
    , examples (datetime (2014, 10, 0, 0, 0, 0) Month)
           [ "октябрь 2014"
           ]
    , examples (datetime (2014, 11, 0, 0, 0, 0) Month)
           [ "ноябрь 2014"
           ]
    , examples (datetime (1974, 10, 31, 0, 0, 0) Day)
           [ "31.10.1974"
           ]
    , examples (datetime (2015, 4, 14, 0, 0, 0) Day)
           [ "14 апреля 2015"
           ]
    , examples (datetime (2016, 2, 0, 0, 0, 0) Month)
           [ "через 3 года"
           ]
  , examples (datetime (2013, 2, 19, 4, 0, 0) Hour)
           [ "через 7 дней"
           ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
           [ "через 1 неделю"
           ]
  , examples (datetime (2013, 2, 5, 4, 0, 0) Hour)
           [ "7 дней назад"
           ]
  , examples (datetime (2013, 1, 29, 4, 0, 0) Hour)
           [ "14 дней назад"
           ]
  , examples (datetime (2013, 1, 29, 0, 0, 0) Day)
           [ "две недели назад"
           ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
           [ "1 неделю назад"
           ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
           [ "три недели назад"
           ]
  , examples (datetime (2012, 11, 12, 0, 0, 0) Day)
           [ "три месяца назад"
           ]
  , examples (datetime (2011, 2, 0, 0, 0, 0) Month)
           [ "два года назад"
           ]
  , examples (datetime (2013, 2, 12, 4, 0, 0) Hour) ------Время
           [ "в 4 утра"
           ]
  , examples (datetime (2013, 2, 12, 3, 18, 0) Minute)
           [ "3:18 утра"
           ]
  , examples (datetime (2013, 2, 13, 3, 18, 0) Minute)
           [ "в 3:18"
           ]
  , examples (datetime (2013, 2, 12, 3, 0, 0) Hour)
           [ "в 3"
           , "в 3 часов"
           ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
           [ "в 15"
           , "в 15 часов"
           , "в 15ч"
           ]
  , examples (datetimeInterval ((2020, 2, 14, 9, 30, 0), (2013, 2, 14, 11, 1, 0)) Minute)
             [ "в четвер c 9:30 до 11:00"
             , "Четвер 9:30 - 11:00"
             , "четвер c 9:30 до 11:00"
             ]
     ]
