module Mtgmodel where

import Prelude


--Recources
data Mana = Red | Green | White | Blue | Black | Colourless deriving (Eq)

newtype Manapool = Pool [(Int,Mana)]

--costs are aved as negative numbers
newtype Manacost = MCost [(Int, Mana)]

newtype Library = Lib [Card]

newtype Graveyard = Grave [Card]

newtype Battlefield = Btlf [Card]

newtype Hand = Hand [Card]

newtype Exile = Exile [Card]

newtype Stack = Stack [Card]

newtype Life = Life Int

data Zone = ZLibrary | ZGraveyard | ZBattlefield | ZHand | ZExile deriving (Eq)

data Side = Side Player Library Graveyard Battlefield Hand Exile Manapool Life


--Gameprocedure related
data Phase = UntapPhase | Upkeep | Draw | FirstMain | DeclareAttackers | DeclareBlockers | ResolveCombat | SecondMain | Endstep deriving (Enum, Bounded, Eq)

newtype Player = Id Int deriving (Eq)


-- Card related Identifiers
newtype Cardname = Cardname String deriving (Eq)

newtype CardId = CardId (Int, Cardname) deriving (Eq)

data Cardtype = Instant | Sorcery | Creature | Enchantment | Artifact | Planeswalker | Land | Legendary Cardtype | Token deriving (Eq)

data Subtype = Creaturetype | Mountain | Forest | Plains | Island | Swamp | Aura | Equipment deriving (Eq)

newtype Creaturetype = CreatureType String deriving (Eq)

newtype PowerToughness = PT (Maybe (Int, Int))

--all static abilitys that can occur in m2010 and are abilites that cause alternate procedures and can be requested by other cards. Full List here: http://mtgsalvation.gamepedia.com/Evergreen
data Keyword = Deathtouch | Defender | DoubleStrike | FirstStrike | Flash | Flying | Haste | Hexproof | Indestructible | Lifelink | Menace | Reach | Trample | Vigilance | Fear | Shroud | Intimidate

--Possible events that can be asked by triggers. Listed undet Actions at: http://mtgsalvation.gamepedia.com/Evergreen
data Event = Activate | Attach | Cast | Counter | Destroy | Discard | Exchange | ExileCard | Fight | Play | Regenerate | Reveal | Sacrifice | Scry | Search | Shuffle | Tap | Untap deriving (Eq)

data Change = Change CardId (Card -> Card)

--               Id     Name     Type       Subtype   Power/ Toughness  Cost     Additional Cost  Effect Owner   Damage Untapped = True  Active Changes affecting the card  Keywords
data Card = Card CardId Cardname [Cardtype] [Subtype] PowerToughness    Manacost Cost             Effect Player  Int    Bool             [Change]                           [Keyword]  [Trigger]


--Gamestate
--                         All Sides  Player whose Turn it is  Current Phase  Player whose Priority it is  List of all Players
data Gamestate = Gamestate [Side]     Player                   Phase          Player                       [Player]

setPhaseTo :: Phase -> Gamestate -> Gamestate
setPhaseTo phs (Gamestate s p ph pl all) = Gamestate s p phs pl all

advancePhase :: Gamestate -> Gamestate
advancePhase (Gamestate s p ph pl all) = let g = (Gamestate s p ph pl all) in if ph == maxBound then nextTurn g else (Gamestate s p (succ ph) pl all)

nextTurn :: Gamestate -> Gamestate
nextTurn (Gamestate s p ph pl all) = let x = findNext p all in (Gamestate s x minBound x all)

changePriority :: Gamestate -> Gamestate
changePriority (Gamestate s p ph pl all) = let x = findNext pl all in (Gamestate s p minBound x all)

findNext :: Eq a => a -> [a] -> a
findNext x y = case maybefindNext x y of
                    Just b  -> b
                    Nothing -> head y
                    
maybefindNext :: Eq a => a -> [a] -> Maybe a
maybefindNext _ [a] = Nothing
maybefindNext x (y :ys) = if x == y then Just (head ys) else maybefindNext x ys


--currently a stub repeats endlessly
checkWinner :: Gamestate -> Bool
checkWinner _ = False


--Cardeffects
data Scope a = Scope (a -> Bool)

data Resource = RLibrary | RGraveyard | RBattlefield | RHand | RExile | RStack | RManapool | RLife

data Cost = Cost (Side -> Bool) [(Side -> Side)]

data Action = Action Target Event (Maybe Int) (Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure)

--                      Trigger        Consequence OneTime or Permanent Origin Zone in which the Trigger will activate
data Trigger = Trigger  Event          Action      Bool                 CardId Zone

newtype Procedure = Procedure (Gamestate -> IO Gamestate)

data Target = Self | Opponent | ConditionalTarget (Card -> Bool) (Zone -> Bool) (Player -> Bool) | AllPlayer | ConditionalAllCards (Card -> Bool) (Zone -> Bool) (Player -> Bool)

data Source = CardSource CardId | PlayerInitiator Player

--Replaces a procedure with an alternative. Boolean checks if this is done once or as long as the card persists
data AltProcedure = Alternative ((Gamestate -> Gamestate), Int) Bool   CardId

data Effect = Effect [Action] [Trigger] [AltProcedure]


--Functions modelling change in the model


ownerSide :: Player -> Side -> Bool
ownerSide p (Side pl _ _ _ _ _ _ _) = p == pl


checkCard :: CardId -> Card -> Bool
checkCard i (Card id _ _ _ _ _ _ _ _ _ _ _ _ _) = id == i 


checkTrigger :: Event -> Zone -> Trigger -> Bool
checkTrigger e z (Trigger e' _ _ _ z') = e == e' && z == z'


affectedTriggers :: (Trigger -> Bool) -> Card -> [Trigger]
affectedTriggers scp (Card _ _ _ _ _ _ _ _ _ _ _ _ _ tr) = filter scp tr


affectedTriggersZone :: (Trigger -> Bool) -> [Card] -> [Trigger]
affectedTriggersZone scp s = concatMap (affectedTriggers scp) s


dealDamage :: Int -> Card -> Card
dealDamage x (Card id n tp s pt c ac e o d t chng k tr) = Card id n tp s pt c ac e o (d+x) t chng k tr


checkLethal :: Card -> Bool
checkLethal (Card _ _ _ _ (PT Nothing) _ _ _ _ d _ _ _ _)      = False
checkLethal (Card _ _ _ _ (PT (Just (_,b))) _ _ _ _ d _ _ _ _) = b <= d


allAffectedTriggers :: Event -> Side -> [Trigger]
allAffectedTriggers ev (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) _ _) = concatMap (\(i,j) -> affectedTriggersZone (checkTrigger ev i) j) (zip [ZLibrary, ZGraveyard, ZBattlefield, ZHand, ZExile] [lib, g, b, h, e])


manipulateResource :: Player -> (Side -> Side) -> Gamestate -> Gamestate
manipulateResource pl r (Gamestate s t p pr all) = Gamestate (condMap (ownerSide pr) r s) t p pr all


condMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
condMap c p []       = []
condMap c p (x : xs) = if c x then ((p x) : (condMap c p xs)) else (x : (condMap c p xs)) 


-- important function which will be used with the parser
changeLife :: Int -> Side -> Side
changeLife c (Side pl lib g b h e m (Life l)) = Side pl lib g b h e m (Life (l+c))


addToZone :: Zone -> (Card, Side) -> Side
addToZone z (c, (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l)) = 
 case z of
  ZLibrary     -> Side pl (Lib (c:lib)) (Grave g) (Btlf b) (Hand h) (Exile e) m l
  ZGraveyard   -> Side pl (Lib lib) (Grave (c:g)) (Btlf b) (Hand h) (Exile e) m l
  ZBattlefield -> Side pl (Lib lib) (Grave g) (Btlf (c:b)) (Hand h) (Exile e) m l
  ZHand        -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand (c:h)) (Exile e) m l
  ZExile       -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile (c:e)) m l

  
removeFromZone :: CardId -> Zone -> Side -> (Card,Side)
removeFromZone ci z (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) = 
 case z of
  ZLibrary     -> ((head (filter (checkCard ci) lib)), Side pl (Lib (filter (not.checkCard ci) lib)) (Grave g) (Btlf b) (Hand h) (Exile e) m l)
  ZGraveyard   -> ((head (filter (checkCard ci) g)), Side pl (Lib  lib) (Grave (filter (not.checkCard ci) g)) (Btlf b) (Hand h) (Exile e) m l)
  ZBattlefield -> ((head (filter (checkCard ci) b)), Side pl (Lib  lib) (Grave g) (Btlf (filter (not.checkCard ci) b)) (Hand h) (Exile e) m l)
  ZHand        -> ((head (filter (checkCard ci) h)), Side pl (Lib  lib) (Grave g) (Btlf b) (Hand (filter (not.checkCard ci) h)) (Exile e) m l)
  ZExile       -> ((head (filter (checkCard ci) e)), Side pl (Lib  lib) (Grave g) (Btlf b) (Hand h) (Exile (filter (not.checkCard ci) e)) m l)

  
-- important function which will be used with the parser
changeZone :: Zone -> Zone -> CardId -> Side -> Side
changeZone zi zii cid = (addToZone zi).(removeFromZone cid zi)

fromPool :: Manapool -> [(Int,Mana)]
fromPool (Pool m) = m

checkMana :: Manapool -> Mana -> Int
checkMana (Pool [])   m       = 0
checkMana (Pool ((x,y):xs)) m = if m == y then x else checkMana (Pool xs) m


removeMana :: Mana -> Manapool -> Manapool
removeMana _ (Pool []) = Pool []
removeMana m (Pool ((x,y):xs)) = if m == y then Pool xs else (Pool ((x,y) : (fromPool (removeMana m (Pool xs)))))


manipulateMana :: (Int,Mana) -> Manapool -> Manapool
manipulateMana (i,m) p = let j = checkMana p m in Pool ((i+j,m): fromPool (removeMana m p))


checkManaCost :: Manacost -> Manapool -> Bool
checkManaCost (MCost c) p = let (a,b) = unzip c in foldr (\(i,j) r ->r && (-i) <= j) True (zip a (map (checkMana p) b))

--Side Manipulation will check if Enough Mana is present but must be modelled together with Addcost
payManaCost :: Manacost -> Manapool -> Manapool
payManaCost (MCost []) p = p 
payManaCost (MCost (x:xs)) p = payManaCost (MCost xs) (manipulateMana x p)



--Functions for Cardmanipulation
--True = untap false = tap
tapuntap :: Bool -> Card -> Card
tapuntap t (Card id n tp s pt c ac e o d _ chng k tr) = Card id n tp s pt c ac e o d t chng k tr


checkUntap :: Card -> Bool
checkUntap (Card _ _ _ _ _ _ _ _ _ _ t _ _ _) = t


getCardId :: Card -> CardId
getCardId (Card id _ _ _ _ _ _ _ _ _ _ _ _ _) = id


getOwner :: Card -> Player
getOwner (Card _ _ _ _ _ _ _ _ p _ _ _ _ _) = p


checkType :: Cardtype -> Card -> Bool
checkType ct (Card _ _ (x:xs) _ _ _ _ _ _ _ _ _ _ _) = if x == ct then True else findA ct xs


findA :: Eq a => a -> [a] -> Bool
findA x [] = False
findA x (y:ys) = if x == y then True else findA x ys


checkSubType :: Subtype -> Card -> Bool
checkSubType ct (Card _ _ _ (x:xs) _ _ _ _ _ _ _ _ _ _) = if x == ct then True else findA ct xs


copyCard :: Card -> Card
copyCard (Card (CardId (i,nm)) n tp s pt c ac e o d t chng k tr) = Card (CardId (i+1,nm)) n tp s pt c ac e o d t chng k tr


changePT :: (Either () CardId) -> (Int, Int) -> Card -> Card
changePT i (a,b) (Card id n tp s pt c ac e o d t chng k tr) = case pt of
                                                                PT Nothing           -> (Card id n tp s pt c ac e o d t chng k tr)
                                                                PT (Just (pwr, tgh)) -> case i of
                                                                                          Right ci -> let reverse = Change ci (changePT (Left ()) (-a,-b)) in 
                                                                                                        (Card id n tp s (PT (Just (pwr+a,tgh+b))) c ac e o d t (reverse : chng) k tr)
                                                                                          Left ()  -> (Card id n tp s (PT (Just (pwr+a,tgh+b))) c ac e o d t chng k tr)
                                                                


allAccept :: a -> Bool
allAccept _ = True


affectCards :: (Card -> Bool) -> (Card -> Card) -> [Card] -> [Card]
affectCards scp efct c = map efct (filter scp c)

--two functions used for affecting a bunch of cards at once
affectInZone :: Zone -> (Card -> Bool) -> (Card -> Card) -> Side -> Side
affectInZone z scp efct (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) = 
 case z of
  ZLibrary     -> Side pl (Lib (affectCards scp efct lib)) (Grave g) (Btlf b) (Hand h) (Exile e) m l
  ZGraveyard   -> Side pl (Lib lib) (Grave (affectCards scp efct g)) (Btlf b) (Hand h) (Exile e) m l
  ZBattlefield -> Side pl (Lib lib) (Grave g) (Btlf (affectCards scp efct b)) (Hand h) (Exile e) m l
  ZHand        -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand (affectCards scp efct h)) (Exile e) m l
  ZExile       -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile (affectCards scp efct e)) m l


affectZoneChange :: (Card -> Bool) -> Zone -> Zone -> Side -> Side
affectZoneChange scp zi zii (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) = let s = (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) in
 case zi of
  ZLibrary     -> foldr (changeZone zii zi) s (map getCardId (filter scp lib))
  ZGraveyard   -> foldr (changeZone zii zi) s (map getCardId (filter scp g))
  ZBattlefield -> foldr (changeZone zii zi) s (map getCardId (filter scp b))
  ZHand        -> foldr (changeZone zii zi) s (map getCardId (filter scp h))
  ZExile       -> foldr (changeZone zii zi) s (map getCardId (filter scp e))


--checks if there are x cards in zone z which fulfill condition scp
checkAmountZone :: Int -> Zone -> (Card -> Bool) -> Side -> Bool
checkAmountZone x z scp (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) _ _) = 
 case z of
  ZLibrary     -> x <= (length (filter scp lib))
  ZGraveyard   -> x <= (length (filter scp g))
  ZBattlefield -> x <= (length (filter scp b))
  ZHand        -> x <= (length (filter scp h))
  ZExile       -> x <= (length (filter scp e))


--combined with getXpossibleCards allows to create a scope which only allows a certain number of cards to be affected by another scope
xCardScope :: [CardId] -> (Card -> Bool)
xCardScope flt = \c -> findA (getCardId c) flt


getXpossibleCards :: Int -> Zone -> (Card -> Bool) -> Side -> [CardId]
getXpossibleCards x z scp (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) = let s = (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) in
 if checkAmountZone x z scp s then
    case z of
        ZLibrary     -> map getCardId (take x (filter scp lib))
        ZGraveyard   -> map getCardId (take x (filter scp g))
        ZBattlefield -> map getCardId (take x (filter scp b))
        ZHand        -> map getCardId (take x (filter scp h))
        ZExile       -> map getCardId (take x (filter scp e))
 else []


applyAll :: [(a -> a)] -> a -> a
applyAll [] v = v
applyAll (x : xs) v = applyAll xs (x v)
 

ioapplyAll :: [(a -> IO a)] -> a -> IO a
ioapplyAll [] v = return v
ioapplyAll (x : xs) v = do v' <- x v
                           ioapplyAll xs v'
                           
                           
ioFold :: (a -> b -> IO b) -> b -> [a] -> IO b
ioFold _ v []       = return v
ioFold f v (x : xs) = do v' <- f x v
                         ioFold f v' xs
                           

payaddCost :: Cost -> Side -> Side
payaddCost (Cost _ chngs) s = applyAll chngs s


findSide :: Gamestate -> Player -> Side
findSide (Gamestate s _ _ _ _) p' =  head (filter (ownerSide p') s)


allZonesSatisfying :: (Zone -> Bool) -> Side -> [Card]
allZonesSatisfying zn (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) _ _)= concatMap (snd) (filter (\(i,j) -> zn i) (zip [ZLibrary, ZGraveyard, ZBattlefield, ZHand, ZExile] [lib, g, b, h, e]))


allCardsSatisfying :: (Card -> Bool) -> (Zone -> Bool) -> (Player -> Bool) -> [Side] -> [CardId]
allCardsSatisfying c zn o s = map getCardId (filter c (concatMap (allZonesSatisfying zn) (filter (\(Side pl _ _ _ _ _ _ _) -> o pl) s)))

--Main method for paying costs but still need binding functions for casting and activating abilities which will also check the cost and afterwards apply all effects
payCost :: Manacost -> Cost -> Side -> Side
payCost mc c s = let Side pl lib g b h e m l = payaddCost c s in Side pl lib g b h e (payManaCost mc m) l



--Block of IO functions modelling the standard interactions and procedures of the game
findTarget :: Gamestate -> Target -> IO (Either [CardId] [Player])
findTarget (Gamestate s p ph pl all) t = case t of
                                          Self                        -> return (Right [pl])
                                          Opponent                    -> do x <- chooseOpponent pl all
                                                                            return (Right [x])
                                          ConditionalTarget c' zn' o' -> do y <- chooseCard c' zn' o' s
                                                                            return (Left [y])
                                          AllPlayer                   -> return (Right all)
                                          ConditionalAllCards c zn o  -> return (Left (allCardsSatisfying c zn o s))
                                          
                                          
chooseOpponent :: Player -> [Player] -> IO Player
chooseOpponent p all = do putStr "Choose Opponent: " 
                          x <- getLine
                          case filter ((\p' -> (p' == (parsePlayer x)) && (not (p' == p)))) all of
                            []  -> do putStr "Invalid Target"
                                      chooseOpponent p all
                            [z] -> return z
                                          

chooseCard :: (Card -> Bool) -> (Zone -> Bool) -> (Player -> Bool) -> [Side] -> IO CardId
chooseCard c zn o s = do putStr "Choose Card: "
                         x <- getLine
                         let crds = allCardsSatisfying c zn o s in
                             case filter (\c' -> c' == (parseCard x)) crds of
                                  []  -> do putStr "Invalid Target"
                                            chooseCard c zn o s
                                  [z] -> return z
                             

                                          
parsePlayer :: String -> Player
parsePlayer s = undefined


parseCard :: String -> CardId
parseCard c = undefined


applyProcedure :: Procedure -> Gamestate -> IO Gamestate
applyProcedure (Procedure p) g = p g
           

executeAction :: Source -> Action -> Gamestate -> IO Gamestate
executeAction s (Action t e str a) (Gamestate s' p ph pl all) = let g = (Gamestate s' p ph pl all) in
                                                                    do x   <- findTarget g t
                                                                       let tr = concatMap (allAffectedTriggers e) (map (findSide g) all) in
                                                                        do g'  <- applyProcedure (a s x str) g
                                                                           g'' <- ioFold executeTrigger g' tr
                                                                           return g''
                                                                 
                                      


executeTrigger :: Trigger -> Gamestate -> IO Gamestate
executeTrigger t g = undefined
                                      
           
game :: IO ()
game = do g <- initalize
          (while (not.checkWinner) turn) g
          return ()


initalize :: IO Gamestate
initalize = undefined
          
while :: (a -> Bool) -> (a -> IO a) -> (a -> IO a)
while p body = loop
    where loop x = if p x then do x' <- body x
                                  loop x'
                          else return x

turn :: Gamestate -> IO Gamestate
turn (Gamestate s p ph pl all) = let g = (Gamestate s p ph pl all) in
                                  ioapplyAll [untapStep, upkeepStep, drawStep, firstMainStep, combatStep, secondMainStep, endStep] g
                                 
                                 
--TODO: change to use execute action           
untapStep :: Gamestate -> IO Gamestate
untapStep (Gamestate s p ph pl all) =  let g = (Gamestate s p ph pl all) in
                                           return (advancePhase (manipulateResource p (affectInZone ZBattlefield allAccept (tapuntap True)) g))
                                         
upkeepStep :: Gamestate -> IO Gamestate
upkeepStep g = undefined

drawStep :: Gamestate -> IO Gamestate
drawStep g = undefined

firstMainStep :: Gamestate -> IO Gamestate
firstMainStep g = undefined

combatStep :: Gamestate -> IO Gamestate
combatStep g = undefined

secondMainStep :: Gamestate -> IO Gamestate
secondMainStep g = undefined

endStep :: Gamestate -> IO Gamestate
endStep g = undefined



