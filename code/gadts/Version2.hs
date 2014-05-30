{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
-- ^^ Activate language extensions

import Control.Applicative

data QuestionType = CountryType | ResultType

data Question :: QuestionType -> * where
  CountryQuestion :: String -> Question CountryType
  ResultQuestion  :: String -> Question ResultType

data Questions :: [QuestionType] -> * where
  QNil  :: Questions '[]
  QCons :: Question t -> Questions ts -> Questions (t ': ts)

exampleQuestions = 
  QCons (CountryQuestion "Wer wird Fussball-Weltmeister?") (
  QCons (ResultQuestion  "Wie endet das Spiel Deutschland -- Portugal?") (
  QNil))

data Answer :: QuestionType -> * where
  CountryAnswer   :: Country -> Answer CountryType
  ResultAnswer    :: Int -> Int -> Answer ResultType

data Answers :: [QuestionType] -> * where
  ANil  :: Answers '[]
  ACons :: Answer t -> Answers ts -> Answers (t ': ts)

data Country = Germany | Netherlands | Brazil | Spain | Other
  deriving (Eq, Show, Ord, Read)

exampleAnswers =
  ACons (CountryAnswer Germany) (
  ACons (ResultAnswer 3 1) (
  ANil))

displayQAs :: Questions ts -> Answers ts -> IO ()
displayQAs (QCons q qs) (ACons a as) = do
  displayQA q a
  displayQAs qs as
displayQAs QNil         ANil         = return ()

displayQA :: Question t -> Answer t -> IO ()
displayQA (CountryQuestion txt) (CountryAnswer a) = do
  putStr txt
  putStr " "
  print a
displayQA (ResultQuestion txt) (ResultAnswer m n) = do
  putStr txt
  putStr " "
  putStrLn (show m ++ " : " ++ show n)

getAnswers :: Questions ts -> IO (Answers ts)
getAnswers QNil         = return ANil
getAnswers (QCons q qs) = ACons <$> getAnswer q <*> getAnswers qs
  where
    getAnswer :: Question t -> IO (Answer t)
    getAnswer (CountryQuestion txt) = do
      putStrLn txt
      CountryAnswer <$> readLn
    getAnswer (ResultQuestion txt) = do
      putStrLn txt
      ResultAnswer <$> readLn <*> readLn

