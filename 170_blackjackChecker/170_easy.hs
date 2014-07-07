import Data.List

-- test games
game1 = "3\nAlice: Ace of Diamonds, Ten of Clubs\nBob: Three of Hearts, Six of Spades, Seven of Spades\nChris: Ten of Hearts, Three of Diamonds, Jack of Clubs"
game2 = "4\nAlice: Ace of Diamonds, Ten of Clubs\nBob: Three of Hearts, Six of Spades, Seven of Spades\nChris: Ten of Hearts, Three of Diamonds, Jack of Clubs\nDavid: Two of Hearts, Three of Clubs, Three of Hearts, Five of Hearts, Six of Hearts"
-- edge cases submitted by reddit user /u/chunes:
chunesAces = "3\nAlice: Ace of Diamonds, Ace of Spades, Ace of Clubs, Ace of Hearts\nBob: Three of Hearts, Six of Spades, Seven of Spades, Ten of Diamonds\nChris: Ten of Hearts, Three of Diamonds"
chunesBust = "3\nAlice: Nine of Diamonds, Five of Clubs, Jack of Spades\nBob: King of Diamonds, Six of Spades, Seven of Spades\nChris: Ten of Hearts, Three of Diamonds, Jack of Clubs"
chunes5ers = "3\nAlice: Two of Clubs, Three of Clubs, Two of Diamonds, Six of Diamonds, Four of Diamonds\nBob: Four of Spades, Three of Hearts, Two of Spades, Six of Spades, Four of Hearts\nChris: King of Spades, Queen of Hearts, Ace of Diamonds"
chunesTies = "3\nAlice: Ten of Hearts, Jack of Spades\nBob: King of Diamonds, Five of Hearts, Five of Clubs\nChris: Queen of Spades, King of Hearts"

nths :: (Ord a) => Int -> [a] -> [a]
nths _ [] = []
nths n (x:xs) = x : nths n (drop (n-1) xs)

parseHand = nths 3

cardVal :: String -> Int
cardVal card = head [val | (name,val) <- cardvals, name == card]
    where cardvals = [("One",1),("Two",2),("Three",3),("Four",4),("Five",5),
                      ("Six",6),("Seven",7),("Eight",8),("Nine",9),("Ten",10),
                      ("Jack",10),("Queen",10),("King",10),("Ace",11)]

processScore :: Int -> [String] -> Int
processScore score hand
    | score <= 21       = score
    | elem "Ace" hand   = processScore (score - 10) (delete "Ace" hand)
    | otherwise         = 0

scoreHand :: [String] -> Int
scoreHand cards
    | has5 && score <= 21 = 999 -- TILT! Winner!
    | otherwise           = processScore score cards
    where score = sum (map cardVal cards)
          has5 = length cards == 5

playerScore :: String -> (String, Int)
playerScore summary = (delete ':' (head parts), scoreHand hand)
    where parts = words summary
          hand = parseHand (tail parts)

bjWinner :: String -> String
bjWinner gameSummary
    | rankedPlayers == [] = "tie"
    | length defaultWinners > 1 = "tie"
    | length defaultWinners == 1 = head defaultWinners ++ " has won with a 5-card trick!"
    | snd (head rankedPlayers) == snd (head (tail rankedPlayers)) = "tie"
    | otherwise = (fst (head rankedPlayers)) ++ " has won!"
    where playerSummaries = tail (lines gameSummary)
          scoreSort = \a b -> (snd a) `compare` (snd b)
          filterBusts = \players -> [(p,s) | (p,s) <- players, s /= 0, s <= 21]
          playerScores = map playerScore $ playerSummaries
          rankedPlayers = reverse (filterBusts (sortBy scoreSort playerScores))
          defaultWinners = [p | (p,s) <- playerScores, s == 999]

