{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Static
import Web.Scotty
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, decode, encode)

data DBTable = DBTable {
       dbTable :: String
  }

data Result = Result {
  successR  :: Bool,
  messageR  :: String
  } deriving (Show, Generic)

-- Data
data Unit = Unit {
  unId   :: Int,
  unName :: String
  } deriving (Show, Generic)

data Question = Question {
  quId   :: Int,
  quUnit :: Int,
  quText :: String,
  quExpl :: String
  } deriving (Show, Generic)

data CrQuestion = CrQuestion {
  cqUnit :: Int,
  cqText :: String,
  cqExpl :: String,
  cqAns1 :: String,
  cqCor1 :: Bool,
  cqAns2 :: String,
  cqCor2 :: Bool,
  cqAns3 :: String,
  cqCor3 :: Bool,
  cqAns4 :: String,
  cqCor4 :: Bool,
  cqAns5 :: String,
  cqCor5 :: Bool,
  cqAns6 :: String,
  cqCor6 :: Bool
  } deriving (Show, Generic)

data Code = Code {
  code :: String,
  lang :: String
  } deriving (Show, Generic)

data Answer = Answer {
  anId      :: Int,
  anParent  :: Int,
  anText    :: String,
  anCorrect :: Bool
} deriving(Show, Generic)

data DBInt = DBInt {
  dbInt :: Int
  } deriving (Show)

instance FromRow DBInt where
  fromRow = DBInt <$> field

instance ToJSON Result
instance FromJSON Result

instance FromJSON CrQuestion

instance FromRow DBTable where
  fromRow = DBTable <$> field

instance FromRow Unit where
  fromRow = Unit <$> field <*> field

instance FromRow Question where
  fromRow = Question <$> field <*> field <*> field <*> field
  
instance ToJSON Unit
instance ToJSON Question
  
instance ToRow Unit where
  toRow (Unit id name) = toRow [name]

instance ToRow Question where
  toRow (Question id unit text expl) = toRow (unit, text, expl)

instance ToRow Answer where
  toRow (Answer id parent text correct) = toRow (parent, text, correct)
instance FromJSON Code  

dbFile = "static/data/tawquizz.db"

-- | Get Table definition
createTab :: String -> Connection -> IO ()
createTab t conn
  | t == "unit" = do
      execute_
        conn
        "CREATE TABLE unit (id INTEGER PRIMARY KEY, name TEXT)"
  | t == "question" = do
      execute_
        conn
        "CREATE TABLE question (id INTEGER PRIMARY KEY, parent INT, text TEXT, explanation TEXT)"
  | t == "answer" = do
      execute_
        conn
        "CREATE TABLE answer (id INTEGER PRIMARY KEY, parent NUMERIC, text TEXT, correct BOOL)"

-- | Check single table
checkTable :: String -> ActionM ()
checkTable tname = do
  liftIO $ checkTable' tname

checkTable' :: String -> IO ()
checkTable' tname = do
  conn <- open dbFile
  c <- query
       conn
       "SELECT name FROM sqlite_master WHERE type = ? AND name = ?"
       ["table" :: String,
        tname   :: String ] :: IO [DBTable]

  if null c then do
    createTab tname conn
  else return ()
  close conn    

-- | Check tables in DB
checkTables :: ActionM ()
checkTables = do
  checkTable "unit"
  checkTable "question"
  checkTable "answer"

-- | Checks system
startup :: ActionM Result
startup = do
  res <- checkTables
  return $ (Result True "API v0.3")

createUnit :: String -> ActionM Result
createUnit name = do
  r <- liftIO $ createUnit' name
  return r

createUnit' :: String -> IO Result
createUnit' name = do
  conn <- open dbFile
  execute conn "INSERT INTO unit (name) VALUES (?)" (Unit 0 name)
  close conn
  return $ Result True "Unit created"

getQuestions :: Int -> ActionM [Question]
getQuestions u = do
  qs <- liftIO $ getQuestions' u
  return qs

getQuestions' :: Int -> IO [Question]
getQuestions' u = do
  conn <- open dbFile
  r <- query conn "SELECT * FROM question WHERE parent = ?" [u] :: IO [Question]
  return r
                                                           
  
getUnits :: ActionM [Unit]
getUnits = do
  us <- liftIO getUnits'
  return us

getUnits' :: IO [Unit]
getUnits' = do
  conn <- open dbFile
  r <- query conn "SELECT * FROM unit" () :: IO [Unit]
  return r

saveQuestion :: Int -> String -> String -> ActionM Int
saveQuestion u q e = do
  qid <- liftIO $ saveQuestion' u q e
  return qid

saveQuestion' :: Int -> String -> String -> IO Int
saveQuestion' u q e = do
  conn <- open dbFile
  execute conn "INSERT INTO question (parent, text, explanation) VALUES (?, ?, ?)" (Question 0 u q e)
  close conn
  lq <- getLastQuestion' u
  return lq

getLastQuestion' :: Int -> IO Int
getLastQuestion' u = do
  conn <- open dbFile
  c <- query conn "SELECT MAX (id) FROM question WHERE parent = ?" [u] :: IO [DBInt]
  let qid = (dbInt . head) c
  return qid

saveAnswers :: Int -> [(String, Bool)] -> ActionM ()
saveAnswers q xs= do
  liftIO $ saveAnswers' q xs
  return ()

saveAnswers' :: Int -> [(String, Bool)] -> IO ()
saveAnswers' _ []     = return ()
saveAnswers' q (x:xs) = do
  conn <- open dbFile
  execute conn "INSERT INTO answer (parent, text, correct) VALUES (?, ?, ?)" (Answer 0 q (fst x) (snd x))
  close conn
  saveAnswers' q xs

getAnswers :: [String] -> [Bool] -> [(String, Bool)]
getAnswers []     []     = []
getAnswers (x:xs) (y:ys) 
  | null x    = getAnswers xs ys
  | otherwise = (x, y) : getAnswers xs ys

main :: IO ()
main = scotty 3000 $ do
  middleware $staticPolicy (noDots >-> addBase "static")
  get "/" $ do
    file "static/index.html"
  get "/quizz" $ do
    file "static/quizz.html"
  get "/quizz/:qid" $ do
    file "static/quizz.html"
  get "/units" $ do
    file "static/units.html"
  get "/create-unit" $ do
    file "static/create-unit.html"
  get "/create-unit/:unit" $ do
    unit <- param "unit"
    r <- createUnit unit
    if (successR r) then
      redirect "/units"
    else
      error "Unit could not be created"
  get "/get-units" $ do
    us <- getUnits
    json us
  get "/unit/:unid" $ do
--    unid <- param "unid"
    file "static/unit.html"
  get "/startup" $ do
    res <- startup
    json res
  get "/create-unit" $ do
    file "static/create-unit.html"
  get "/create-unit/:unit" $ do
    unit <- param "unit"
    r <- createUnit unit
    if (successR r) then
      redirect "/units"
    else
      error "Unit could not be created"
  get "/get-units" $ do
    us <- getUnits
    json us
  get "/get-questions/:unit" $ do
    unit <- param "unit"
    qs <- getQuestions unit
    json qs
  get "/get-first-question/:unit" $ do
    unit <- param "unit"
    qs <- getQuestions unit
    if null qs then
      error "No Questions found for this unit"
    else
      json (head qs)
  get "/startup" $ do
    res <- startup
    json res
  get "/create-question/:unid/" $ do
    file "static/create-question.html"
  post "/save-question" $ do
    d <- body
    liftIO $ putStrLn $ show d
    let n = decode d :: Maybe CrQuestion
    liftIO $ putStrLn $ show n
    case n of
      Just q -> do
        qId <- saveQuestion (cqUnit q) (cqText q) (cqExpl q)
        let ans = getAnswers [(cqAns1 q), (cqAns2 q), (cqAns3 q), (cqAns4 q), (cqAns5 q), (cqAns6 q)]
                             [(cqCor1 q), (cqCor2 q), (cqCor3 q), (cqCor4 q), (cqCor5 q), (cqCor6 q)]
        liftIO $ putStrLn $ show ans
        saveAnswers qId ans
        json qId
      otherwise -> error "Question not created"



