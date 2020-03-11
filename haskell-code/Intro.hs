module Intro where

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Schlangen

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

-- Schlange hat folgende Eigenschaften:
-- - Länge
-- - Dicke
data Liveness = Dead | Alive
  deriving Show

data Animal =
    Dillo Liveness Int 
  | Snake Int Int
  deriving Show

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo liveness weight) = Dillo Dead weight
runOverAnimal (Snake length thickness) = Snake length 0

-- Tier füttern
feedAnimal :: Int -> (Animal -> Animal)
feedAnimal amount (Dillo liveness weight) =
    Dillo liveness (weight + amount)
feedAnimal amount (Snake length thickness) =
    Snake length (thickness + amount)


feedAnimal' :: Int -> Animal -> Animal
feedAnimal' amount =
    \ animal ->
        case animal of
            Dillo liveness weight ->
                Dillo liveness (weight + amount)
            Snake length thickness ->
                Snake length (thickness + amount)

type AnimalTransformer =
    Animal -> Animal 

-- doBoth :: AnimalTransformer -> AnimalTransformer -> AnimalTransformer
doBoth :: (b -> c) -> (a -> b) -> (a -> c)
doBoth at1 at2 =
    \ animal ->
        at1 (at2 animal)
