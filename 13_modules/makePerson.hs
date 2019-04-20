type Name = String
type Age = Integer
data Person = Person Name Age deriving Show
data PersonInvalid =
         NameEmpty
        | AgeTooLow
        | PersonInvalidUnknown String
        deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0
    = Right $ Person name age
    | name == ""
    = Left NameEmpty
    | not (age > 0)
    = Left AgeTooLow
    | otherwise
    = Left
        $  PersonInvalidUnknown
        $  "Name was: "
        ++ show name
        ++ " Age was: "
        ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Enter name: "
    name <- getLine
    putStr "Enter age: "
    age <- getLine
    let couldBeAPerson = mkPerson name (read age :: Integer)
    case couldBeAPerson of
        Right (Person name' age') ->
            print
                $  "Yay! Successfully got a person: "
                <> name'
                <> " who is "
                <> (show age')
                <> " years old"
        Left personError -> print $ "Boo! " <> (show personError)
