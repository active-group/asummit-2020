module Effect where

main :: () -> IO String
main () =
  do putStr "What is your name?"
     line <- getLine
     putStr ("Hello, " ++ line)
     return line

-- IO a -> a

data MIO a =
      Put String (()     -> MIO a)
    | Get        (String -> MIO a)
    | Done a

-- [Put "What is your name?", Get, Put ("Hello" ++ ???)]
p :: MIO String
p = Put "What is your name?" (\ () -> 
    Get (\ line ->
    Put ("Hello" ++ line) (\ () -> Done line)))

p' :: MIO String
p' = put "What is your name?" `splice` (\ () ->
     get `splice` (\ line ->
     put ("Hello " ++ line) `splice` (\ () ->
     Done line)))

put :: String -> MIO ()
put text = Put text (\ () -> Done ())

get :: MIO String 
get = Get (\ line -> Done line)

splice :: MIO a -> (a -> MIO b) -> MIO b 
splice (Put text callback) next = 
    Put text (\ () ->
          let mioa = callback ()
          in splice mioa next)
splice (Get callback) next = 
    Get (\ line ->
         let mioa = callback line
         in splice mioa next)
splice (Done result) next = next result

instance Functor MIO where

instance Applicative MIO where


instance Monad MIO where
    (>>=) = splice
    return = Done

main' () =
  do put "What is your name?"
     line <- get
     put ("Hello, " ++ line)
     return (line ++ " ist doof!")

runMIO :: MIO a -> [String] -> a
runMIO (Done result) inputs = result
runMIO (Get callback) (first:rest) =
    runMIO (callback first) rest
runMIO (Put text callback) inputs =
    runMIO (callback ()) inputs