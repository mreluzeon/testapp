{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Control.Monad as C
import Text.Blaze.Internal (MarkupM)
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (pack)
import Web.Scotty

import Lib
import Types

quests = [[(Question "asd" ["a", "c"]), (Question "bsd" ["q", "e", "d"])]
         , [(Question "dsa" ["q", "e"]), (Question "oio" ["f", "z"])]]

question :: Show a => Question -> a -> MarkupM ()
question (Question q answs) n = do
  H.li $ H.toHtml q
  C.forM_ answs $ \ans -> do
    H.input H.! A.type_ "radio" H.! A.value (H.toValue ans) H.! A.name (H.toValue (show n))
    H.toMarkup ans
    H.br

render :: H.ToValue a => [Question] -> a -> H.Html
render list variant = do
  H.html $ do
    H.body $ do
      H.h1 "Вопросы"
      H.form H.! A.method "POST" H.! A.action "/check-answers" $ do
        H.input H.! A.name "variant" H.! A.hidden "true" H.! A.value (H.toValue variant)
        H.ul $
          C.forM_ (zip list [1..20]) $ \(q, n) -> do
          question q n
          H.br
        H.input H.! A.type_ "submit" H.! A.value "Отправить ответы"

main :: IO ()
main = do
  scotty 3000 $ do
    get "/" $ do
      variant <- liftIO $ getVariant 2 2
      let questions = getQuestions quests variant 2
      shuffledQuestions <- liftIO $ shuffleAnswers questions
      html $ pack $ renderHtml $ render shuffledQuestions variant

    post "/check-answers" $ do
      answers <- C.mapM (param . pack . show) [1..2] :: ActionM [String]
      variant <- param "variant" :: ActionM String
      let questions = getQuestions quests variant 2
      let chQuests = checkAnswers questions answers
      liftIO $ putStrLn variant
      liftIO $ C.mapM_ print answers
      html $ pack $ concat ["<h1>"
                           , show $ length $ filter id chQuests
                           , " of "
                           , show $ length chQuests
                           ,"</h1>"]

-- main = undefined
