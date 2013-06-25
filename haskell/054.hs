-- Originally submitted as part of the Tuesdays with Haskell project:
-- https://github.com/emcien/tuesdays-with-haskell/blob/master/project-euler/054.hs
-- 
-- This program analyzes a file representing a collection of poker games and
-- computes statistics about the results of thoe games.

-- Load it up by running `gchi` and entering `:l 054`. You can modify this
-- file and then reload it with `:r`.

import Data.List (group, sort, sortBy, nub, intercalate)
import Data.Monoid (mconcat)
import Data.Ord (comparing)
import qualified Data.Map as Map

-- First, let's specify our ontology and interfaces

-- `data` defines a new data type. In all of these, we're specifying a finite
-- set of values that the type can take on.
-- The `deriving` keyword will enable some standard convenience functions for
-- this data type. For the above case, we have:
--   Show: enables the `show` function (think `to_s`)
--   Ord:  enables comparison using < or >
--   Eq:   enables comparison using ==
--   Enum: enables [Two .. Ace] syntax for specifying the array of values
data Rank = Two  | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
          | Jack | Queen | King | Ace
          deriving (Show, Ord, Eq, Enum)


data Suit = Hearts | Clubs | Diamonds | Spades
          deriving (Show, Eq, Enum)

-- The Card data type is defined using record syntax. It enables the creation
-- syntax that you will see later, as well as defining the functions
-- rank :: Card -> Rank and suit :: Card -> Suit
data Card = Card { rank :: Rank, suit :: Suit }

-- Instead of deriving show (which would print the record syntax), we specify
-- the manner in which Card is an instance of the Show typeclass (think
-- "defining Card#to_s")
instance Show Card where
  show card = show(rank card) ++ " of " ++ show(suit card)

-- A hand is just a list of cards. This type synonym makes that clear.
type Hand = [Card]

data Result = High | OnePair | TwoPairs | ThreeOfAKind | Straight | Flush 
            | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
            deriving (Show, Ord, Eq)


-- Input string -> data mappings
-- Map.fromList is analogous to Ruby's Hash[list]
rank_map :: Map.Map Char Rank
rank_map = Map.fromList $ zip "23456789TJQKA" [Two .. Ace]

suit_map :: Map.Map Char Suit
suit_map = Map.fromList $ zip "HCDS" [Hearts .. Spades]

-- Either of the map lookups could fail, so we work inside the Maybe monad
-- to propagate that failure forward. This will return either a Just Card if
-- the lookup succeeds, or Nothing if it does not.
-- Note that the `do` block always indicates working inside some monad, and
-- which monad is determined by the type signature.
parseCard :: String -> Maybe Card
parseCard string = do
  -- Note that !! is array access
  rank <- Map.lookup (string !! 0) rank_map
  suit <- Map.lookup (string !! 1) suit_map
  return Card { rank=rank, suit=suit }

-- For now, we'll just drop a lookup error on the floor an not worry about
-- propogating Maybes through. This is a terrible thing to do in code that
-- will every be called by anything else.
-- ASSIGNMENT: improve this program to use monadic failures throughout
-- HINT: hoogle [Maybe a] -> Maybe [a]
parseCard' :: String -> Card
parseCard' string = case parseCard string of
  Just card -> card
  Nothing   -> error $ "Could not parse a card from " ++ string

parseHands :: String -> (Hand, Hand)
parseHands line = 
  let 
    cards = words line
    h1 = map parseCard' . take 5 $ cards
    h2 = map parseCard' . drop 5 $ cards
  in
    (h1, h2)

-- This function reads a single line, as will be present in the file, and
-- returns which player wins.
-- ASSIGNMENT: cover the cases whene the comparison fails. How can we make
-- this more robust?
findWinner :: String -> Int
findWinner line = 
  let
    (h1, h2) = parseHands line
    r1 = handResult h1
    r2 = handResult h2
  in case compare r1 r2 of
    GT -> 1
    LT -> 2
    EQ -> error $ "Could not determine a winner:\n" ++
      line ++ "\n" ++
      show r1 ++ " vs. " ++ show r2

-- This helper function sorts a list of lists first on the length of the
-- lists and then on the values - this should reflect the order in which
-- we want to compare rank groups.
--
-- The Ord a => bit at the start of the type signature simply means that this
-- function is only defined for things in the Ord typeclass, that is, things
-- for which < and > make sense.
--
-- This is probably the most mysterious bit of this program, so choose one:
-- ASSIGNMENT: re-write this to be more explicit - OR -
-- ASSIGNMENT: read up on `comparing` and the Ordering monoid until this makes
--             some sense
comparisonSort :: Ord a => [[a]] -> [[a]]
comparisonSort = sortBy . flip  . mconcat $ [comparing length, compare]

-- This is a comparatively simple comparator, but shows a more typical use case
sortByRank :: [Card] -> [Card]
sortByRank = sortBy . comparing $ rank

-- This function counts the size of the various rank groups in a hand and
-- and returns an appropriate Result and list of Ranks for comparing
handResult :: [Card] -> (Result, [Rank])
handResult cards =
  let
    -- group only groups adjacent elements, so we sort first
    groups = comparisonSort . group . sort . map rank $ cards
    result = case map length groups of
      (4:_)   -> FourOfAKind
      (3:2:_) -> FullHouse
      (3:_)   -> ThreeOfAKind
      (2:2:_) -> TwoPairs
      (2:_)   -> OnePair
      (1:_)   -> checkSpecial cards
  in
    (result, map (!!0) groups)

-- If we're here, the ranks in this hand should be unique and we need to check
-- for special hands like straights or flushes
-- This |-delimited definition is an example of guard syntax
checkSpecial :: [Card] -> Result
checkSpecial cs
  | isRoyal cs         = RoyalFlush
  | isStraightFlush cs = StraightFlush
  | isFlush cs         = Flush
  | isStraight cs      = Straight
  | otherwise          = High

-- Where those various helpers are given by
ranks :: [Card] -> [Rank]
ranks cards = sort . map rank $ cards

isStraight :: [Card] -> Bool
isStraight cs = ranks cs == [(head . ranks $ cs) .. (last . ranks $ cs)]

-- This helper is written in so-called "point free" style, since the definition
-- does not include an argument
-- `nub` uniques the list of suits
isFlush :: [Card] -> Bool
isFlush = (1 == ) . length . nub . map suit

isStraightFlush :: Hand -> Bool
isStraightFlush hand = isStraight hand && isFlush hand

isRoyal :: [Card] -> Bool
isRoyal cs = ((Ace ==) . last . ranks $ cs) && (isStraightFlush cs)

-- This runs through the input file, counting the number of lines which
-- either player wins. Note that this is the first time we've actually
-- needed to work in the IO monad.
run :: Int -> IO ()
run n = do
  f <- readFile "054.txt"
  putStrLn . show . length . filter ((n ==) . findWinner) . lines $ f


-- The remainder of this file consist of utility functions that I created
-- to be able to inspect the results. They aren't strictly necessary, but are
-- left for comparison. See also: `trace`.
describe :: [Card] -> String
describe cards = 
  let
    line = intercalate ", " . map show . sortByRank $ cards
    (result, ranks) = handResult cards
    signature = intercalate "," . map show $ ranks
  in line ++ "\n" ++ show result ++ " - " ++ signature ++ "\n"

print_line :: String -> String
print_line l = 
  let 
    (h1, h2) = parseHands l
    winner = findWinner l
  in describe(h1) ++ describe(h2) ++ "Winner: " ++ show winner ++ "\n"

run' :: Int -> IO ()
run' n = do
  f <- readFile "054.txt"
  putStrLn . intercalate "\n" . map print_line . take n . lines $ f
