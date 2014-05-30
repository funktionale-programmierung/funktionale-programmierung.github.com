import Control.Applicative

type Questions = [Question]
type Answers   = [Answer]

data Question =
    CountryQuestion String
  | ResultQuestion  String

exampleQuestions :: Questions
exampleQuestions = [
    CountryQuestion "Wer wird Fußball-Weltmeister?"
  , ResultQuestion  "Wie endet das Spiel Deutschland -- Portugal?"
  ]

displayQuestion :: Question -> String
displayQuestion (CountryQuestion txt) = txt ++ " Gesucht ist ein Teilnehmerland."
displayQuestion (ResultQuestion txt)  = txt ++ " Gesucht ist ein Spielergebnis."

data Answer =
    CountryAnswer  Country
  | ResultAnswer   Int Int
  deriving Show

exampleAnswers :: Answers
exampleAnswers = [
    CountryAnswer Germany
  , ResultAnswer 3 1
  ]

data Country = Germany | Netherlands | Brazil | Spain | Other
  deriving (Eq, Show, Ord, Read)

type CorrectAnswers = [Maybe Answer]

possiblyCorrectAnswers :: CorrectAnswers
possiblyCorrectAnswers = [
    Nothing
  , Just (ResultAnswer 2 2)
  ]

getAnswers :: Questions -> IO Answers
getAnswers []       = return []
getAnswers (q : qs) = (:) <$> getAnswer q <*> getAnswers qs
  where
    getAnswer :: Question -> IO Answer
    getAnswer (CountryQuestion txt) = do
      putStrLn txt
      CountryAnswer <$> readLn
    getAnswer (ResultQuestion txt) = do
      putStrLn txt
      ResultAnswer <$> readLn <*> readLn

displayQAs :: Questions -> Answers -> IO ()
displayQAs (q : qs) (a : as) = do
  displayQA q a
  displayQAs qs as
displayQAs []       []       = return ()
displayQAs _        _        = fail "incompatible questions and answers"

displayQA :: Question -> Answer -> IO ()
displayQA (CountryQuestion txt) (CountryAnswer a) = do
  putStr txt
  putStr " "
  print a
displayQA (ResultQuestion txt) (ResultAnswer m n) = do
  putStr txt
  putStr " "
  putStrLn (show m ++ " : " ++ show n)
displayQA _ _ = fail "incompatible questions and answers"

computeTotalScore :: Answers -> CorrectAnswers -> Int
computeTotalScore (q : qs) (a : as) = computeScore q a + computeTotalScore qs as
computeTotalScore []       []       = 0
computeTotalScore _        _        = error "incompatible questions and answers"

computeScore :: Answer -> Maybe Answer -> Int
computeScore _                  Nothing                     = 0
computeScore (CountryAnswer c)  (Just (CountryAnswer c'))   = if c == c' then 1 else 0
computeScore (ResultAnswer m n) (Just (ResultAnswer m' n')) = if (m, n) == (m', n') then 1 else 0
computeScore _                  _                           =
  error "incompatible questions and answers"
