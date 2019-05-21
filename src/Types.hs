module Types where


data Question = Question { title :: String
                         , answers :: [String]
                         } deriving (Show, Eq)
