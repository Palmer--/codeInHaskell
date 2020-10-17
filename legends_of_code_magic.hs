import System.IO
import Control.Monad

inHand = 0
onMyBoard = 1
onOpponentBoard = -1

data Action = SUMMON Int | ATTACK Int Int | PICK Int | PASS deriving (Show, Read)
data Ability = Breakthrough | Charge | Guard deriving (Show, Read)
data Player = Player { playerHP :: Int,
                       playerMana :: Int,
                       playerDeck :: Int,
                       playerRune :: Int,
                       playerDraw :: Int
                      } deriving (Show, Read) 
                                           
                      
data Card = Card { cardNumber :: Int,
                   instanceId :: Int,
                   location :: Int,
                   cardType :: Int,
                   cost :: Int,
                   attack :: Int,
                   defense :: Int,
                   abilities :: [Ability],
                   myhealthchange :: Int,
                   opponenthealthchange :: Int,
                   carddraw :: Int
                 } deriving (Show, Read) 
                      
                      
data GameState = GameState { playerStatus :: Player,
                             opponentStatus :: Player,
                             opponentHand :: Int,
                             opponentActions :: [Action],
                             cards :: [Card]
                            }

getGameState = do
  playerStatus <- getPlayerStatus
  opponentStatus <- getPlayerStatus
  (oppHand, oppInfo) <- getOpponent
  cards <- getCards
  return $ GameState playerStatus opponentStatus oppHand oppInfo cards

                            
getPlayerStatus = do
  input_line <- getLine
  let input = words input_line
  let hp = read (input!!0) :: Int
  let mana = read (input!!1) :: Int
  let deck = read (input!!2) :: Int
  let rune = read (input!!3) :: Int
  let draw = read (input!!4) :: Int
  --return $ read test :: Player
  return $ Player hp mana deck rune draw

getCard = do
  cardString <- getLine
  return (read cardString :: Card)
  
getOpponent = do
  input_line <- getLine
  let input = words input_line
  let opponenthand = read (input!!0) :: Int
  let opponentactions = read (input!!1) :: Int
  actions <- replicateM opponentactions $ do
          readAction <- getLine
          let ac = read readAction :: Action
          return ac
  return (opponenthand, actions)

getCards :: IO [Card]  
getCards = do
  input_line <- getLine
  let cardCount = read input_line :: Int
  cards <- replicateM cardCount $ do
      input_line <- getLine
      let input = words input_line
      let cardNumber = read (input!!0) :: Int
      let instanceId = read (input!!1) :: Int
      let location = read (input!!2) :: Int
      let cardType = read (input!!3) :: Int
      let cost = read (input!!4) :: Int
      let attack = read (input!!5) :: Int
      let defense = read (input!!6) :: Int
      let abilities = [read (input!!7) :: Ability]
      let myHealthChange = read (input!!8) :: Int
      let opponentHealthChange = read (input!!9) :: Int
      let cardDraw = read (input!!10) :: Int
      return $ Card cardNumber instanceId location cardType cost attack defense abilities myHealthChange opponentHealthChange cardDraw
  return cards  

draftPhase :: [Card] -> IO [Card]
draftPhase deck
  |length deck == 30 = return deck
  |otherwise = go
    where go = do 
                state <- getGameState
                let options = cards state
                let pick = draftSelectCard deck options
                putStrLn $ show (PICK pick)
                draftPhase $ (options!!pick) : deck
  
  
draftSelectCard :: [Card] -> [Card] -> Int
draftSelectCard deck options
  |(length deck) >= 30 = 999
  |otherwise = 0
  
costCompare :: Card -> Card -> Ordering
costCompare a b
  |cost a < cost b = GT
  |cost a > cost b = LT
  |cost a == cost b = EQ
  
  
--getPlayActions :: GameState -> [Action]
getPlayActions state = 
  let mana = playerMana $ playerStatus state
    in let handCards = filter (\x -> location x == inHand) $ cards state
      in let playableCards = filter (\x -> cost x <= mana) handCards
        in if length playableCards > 0 then [SUMMON (instanceId (playableCards!!0))] else [PASS]
  
getAttackActions :: GameState -> [Action]
getAttackActions state =
    map (\x -> ATTACK (instanceId x) (-1)) myMonsters
    where myMonsters = filter (\x -> location x == onMyBoard) $ cards state

createActionStr :: [Action] -> String    
createActionStr [] = "PASS"

createActionStr (x:xs) = 
  cleanString ++ ";" ++ (createActionStr xs)
    where
      cleanString = filter (\w -> not $ (w == '(') || (w == ')')) $ show x 
  
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    draftPhase []
    forever $ do
        gameState <- getGameState
        let actions = getPlayActions gameState ++ getAttackActions gameState
        putStrLn $ createActionStr actions
        -- hPutStrLn stderr "Debug messages..."