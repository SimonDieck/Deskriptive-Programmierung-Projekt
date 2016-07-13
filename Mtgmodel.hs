module Mtgmodel where

import Prelude
import SupportFunctions


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

newtype Player = Id Int deriving (Eq, Show)


-- Card related Identifiers
newtype Cardname = Cardname String deriving (Eq)

newtype CardId = CardId (Int, Cardname) deriving (Eq)

data Cardtype = Instant | Sorcery | Creature | Enchantment | Artifact | Planeswalker | Land | Legendary Cardtype | Token deriving (Eq)

data Subtype = Creaturetype | Mountain | Forest | Plains | Island | Swamp | Aura | Equipment deriving (Eq)

newtype Creaturetype = CreatureType String deriving (Eq)

newtype PowerToughness = PT (Maybe (Int, Int))

--all static abilitys that can occur in m2010 and are abilites that cause alternate procedures and can be requested by other cards. Full List here: http://mtgsalvation.gamepedia.com/Evergreen
data Keyword = Keyword String deriving (Eq)

--Possible events that can be asked by triggers. Listed under Actions at: http://mtgsalvation.gamepedia.com/Evergreen
data Event = Event String deriving (Eq)

data Change = Change CardId (Card -> Card)

--               Id     Name     Type       Subtype   Power/ Toughness  Cost     Additional Cost  Effect Owner   Damage Untapped = True  Active Changes affecting the card  Keywords
data Card = Card CardId Cardname [Cardtype] [Subtype] PowerToughness    Manacost Cost             Effect Player  Int    Bool             [Change]                           [Keyword]  [Trigger]


errorCard :: Card
errorCard = Card (CardId ((-1), (Cardname "Error"))) (Cardname "Error") [] [] (PT (Nothing)) (MCost []) (Cost allAccept []) (Effect []) (Id (-1)) 0 False [] [] []


--Gamestate
--                         All Sides  Player whose Turn it is  Current Phase  Player whose Priority it is  List of all Players
data Gamestate = Gamestate [Side]     Player                   Phase          Player                       [Player]

--currently a stub repeats endlessly
checkWinner :: Gamestate -> Bool
checkWinner _ = False


--Cardeffects
data Scope a = Scope (a -> Bool)

data Resource = RLibrary | RGraveyard | RBattlefield | RHand | RExile | RStack | RManapool | RLife

data Cost = Cost (Side -> Bool) [(Side -> Side)]

data Action = Action Target Event (Maybe Int) (Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure) | ChainedAction [Action]

newtype StandardAction = Descriptor String deriving (Eq)

--used to active Triggers of certain events
eventAction :: Event -> Action
eventAction e = Action Self e Nothing emptyChange

--                      Trigger        Consequence OneTime or Permanent Active   Origin Zone in which the Trigger will activate Scope to check if the Source satisfies certain conditions Owner of the Trigger
data Trigger = Trigger  Event          Action      Bool                 Bool     CardId Zone                                    (Source -> Bool)                                          Player

newtype Procedure = Procedure (Gamestate -> IO Gamestate)

data Target = Self | Opponent | ConditionalTarget (Card -> Bool) (Zone -> Bool) (Player -> Bool) | AllPlayer | ConditionalAllCards (Card -> Bool) (Zone -> Bool) (Player -> Bool) | ConditionalOwnTarget (Card -> Bool) (Zone -> Bool)

data Source = CardSource CardId | PlayerInitiator Player

--
data Effect = Effect [(StandardAction, Action)] 

--standardActions that get executed unless specified otherwise on a card

standardAttack :: Action
standardAttack = Action Opponent (Event "Attack") Nothing attack


standardDeclareBlockers :: Action
standardDeclareBlockers = Action (ConditionalOwnTarget (checkUntap) (checkZone ZBattlefield)) (Event "DeclareBlocker") Nothing block


--Functions modelling change in the model
checkZone :: Zone -> Zone -> Bool
checkZone a b = a == b


findAction :: StandardAction -> Card -> Action
findAction desc (Card _ _ _ _ _ _ _ (Effect x) _ _ _ _ _ _) = findInEffect desc x


findInEffect :: StandardAction -> [(StandardAction, Action)] -> Action
findInEffect _ [] = eventAction (Event "ERROR")
findInEffect desc ((a,b) : xs ) = if a == desc then b else findInEffect desc xs


checkTrigger :: Source -> Event -> Zone -> Trigger -> Bool
checkTrigger src e z (Trigger e' _ _ b _ z' scp _) = e == e' && z == z' && scp src && b


affectedTriggers :: (Trigger -> Bool) -> Card -> [Trigger]
affectedTriggers scp (Card _ _ _ _ _ _ _ _ _ _ _ _ _ tr) = filter scp tr


affectedTriggersZone :: (Trigger -> Bool) -> [Card] -> [Trigger]
affectedTriggersZone scp s = concatMap (affectedTriggers scp) s


identifyTrigger :: Event -> CardId -> Zone -> Player -> Trigger -> Bool
identifyTrigger e i z o (Trigger e' _ _ _ i' z' _ o') = e == e' && i == i' && z == z' && o == o'


allAffectedTriggers :: Source -> Event -> Side -> [Trigger]
allAffectedTriggers src ev (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) _ _) = concatMap (\(i,j) -> affectedTriggersZone (checkTrigger src ev i) j) (zip [ZGraveyard, ZBattlefield, ZHand, ZExile] [g, b, h, e])


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


affectCards :: (Card -> Bool) -> (Card -> Card) -> [Card] -> [Card]
affectCards scp efct c = map efct (filter scp c)


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


allZonesSatisfying :: (Zone -> Bool) -> Side -> [Card]
allZonesSatisfying zn (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) _ _)= concatMap (snd) (filter (\(i,j) -> zn i) (zip [ZLibrary, ZGraveyard, ZBattlefield, ZHand, ZExile] [lib, g, b, h, e]))


allCardsSatisfying :: (Card -> Bool) -> (Zone -> Bool) -> (Player -> Bool) -> [Side] -> [CardId]
allCardsSatisfying c zn o s = map getCardId (filter c (concatMap (allZonesSatisfying zn) (filter (\(Side pl _ _ _ _ _ _ _) -> o pl) s)))

{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Getters

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}

findSide :: Gamestate -> Player -> Side
findSide (Gamestate s _ _ _ _) p' =  head (filter (ownerSide p') s)


getCardId :: Card -> CardId
getCardId (Card id _ _ _ _ _ _ _ _ _ _ _ _ _) = id


getOwner :: Card -> Player
getOwner (Card _ _ _ _ _ _ _ _ p _ _ _ _ _) = p


findCard :: CardId -> Zone -> Side -> Card
findCard id z s = case filter (checkCard id) (allZonesSatisfying (\zb -> zb == z) s) of
                        []     -> errorCard
                        (x:xs) -> x
                        
findCardAnywhere :: CardId -> Zone -> Gamestate -> Card
findCardAnywhere id z (Gamestate s _ _ _ _) = head (filter (\c -> getCardId c /= (CardId ((-1), (Cardname "Error")))) (map (findCard id z) s))


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Card -> Bool Functions 

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


--combined with getXpossibleCards allows to create a scope which only allows a certain number of cards to be affected by another scope
xCardScope :: [CardId] -> (Card -> Bool)
xCardScope flt = \c -> findA (getCardId c) flt


checkType :: Cardtype -> Card -> Bool
checkType ct (Card _ _ (x:xs) _ _ _ _ _ _ _ _ _ _ _) = if x == ct then True else findA ct xs


checkSubType :: Subtype -> Card -> Bool
checkSubType ct (Card _ _ _ (x:xs) _ _ _ _ _ _ _ _ _ _) = if x == ct then True else findA ct xs


checkUntap :: Card -> Bool
checkUntap (Card _ _ _ _ _ _ _ _ _ _ t _ _ _) = t


checkLethal :: Card -> Bool
checkLethal (Card _ _ _ _ (PT Nothing) _ _ _ _ d _ _ _ _)      = False
checkLethal (Card _ _ _ _ (PT (Just (_,b))) _ _ _ _ d _ _ _ _) = b <= d


checkCard :: CardId -> Card -> Bool
checkCard i (Card id _ _ _ _ _ _ _ _ _ _ _ _ _) = id == i 


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Side -> Bool Functions 

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


--checks if there are x cards in zone z which fulfill condition scp
checkAmountZone :: Int -> Zone -> (Card -> Bool) -> Side -> Bool
checkAmountZone x z scp (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) _ _) = 
 case z of
  ZLibrary     -> x <= (length (filter scp lib))
  ZGraveyard   -> x <= (length (filter scp g))
  ZBattlefield -> x <= (length (filter scp b))
  ZHand        -> x <= (length (filter scp h))
  ZExile       -> x <= (length (filter scp e))
  
  
ownerSide :: Player -> Side -> Bool
ownerSide p (Side pl _ _ _ _ _ _ _) = p == pl


checkManaPlayer :: Manacost -> Side -> Bool
checkManaPlayer cst (Side _ _ _ _ _ _ m _) = checkManaCost cst m 


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Card -> Card Functions 

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


copyCard :: Card -> Card
copyCard (Card (CardId (i,nm)) n tp s pt c ac e o d t chng k tr) = Card (CardId (i+1,nm)) n tp s pt c ac e o d t chng k tr


changePT :: (Either () CardId) -> (Int, Int) -> Card -> Card
changePT i (a,b) (Card id n tp s pt c ac e o d t chng k tr) = case pt of
                                                                PT Nothing           -> (Card id n tp s pt c ac e o d t chng k tr)
                                                                PT (Just (pwr, tgh)) -> case i of
                                                                                          Right ci -> let reverse = Change ci (changePT (Left ()) (-a,-b)) in 
                                                                                                        (Card id n tp s (PT (Just (pwr+a,tgh+b))) c ac e o d t (reverse : chng) k tr)
                                                                                          Left ()  -> (Card id n tp s (PT (Just (pwr+a,tgh+b))) c ac e o d t chng k tr)
                                                                                          
                                                                                          

--True = untap false = tap
tapuntap :: Bool -> Card -> Card
tapuntap t (Card id n tp s pt c ac e o d _ chng k tr) = Card id n tp s pt c ac e o d t chng k tr

removeTriggerFromCard :: Event -> CardId -> Zone -> Player -> Card -> Card
removeTriggerFromCard ev i z ow (Card id n tp s pt c ac e o d t chng k tr) = Card id n tp s pt c ac e o d t chng k (filter (not.(identifyTrigger ev i z ow)) tr)


dealDamage :: Int -> Card -> Card
dealDamage x (Card id n tp s pt c ac e o d t chng k tr) = Card id n tp s pt c ac e o (d+x) t chng k tr


addKeyword :: Keyword -> Card -> Card
addKeyword kw (Card id n tp s pt c ac e o d t chng k tr) = (Card id n tp s pt c ac e o d t chng (kw : k) tr)


removeKeyword :: Keyword -> Card -> Card
removeKeyword kw (Card id n tp s pt c ac e o d t chng k tr) = (Card id n tp s pt c ac e o d t chng (filter (\k' -> k' /= kw) k) tr)

{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Side -> Side Functions 

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}

manipulateManaPlayer :: (Manapool -> Manapool) -> Side -> Side
manipulateManaPlayer ptp  (Side pl lib g b h e m l) = Side pl lib g b h e (ptp m) l

--Main method for paying costs but still need binding functions for casting and activating abilities which will also check the cost and afterwards apply all effects
payCost :: Manacost -> Cost -> Side -> Side
payCost mc c s = let Side pl lib g b h e m l = payaddCost c s in Side pl lib g b h e (payManaCost mc m) l


payaddCost :: Cost -> Side -> Side
payaddCost (Cost _ chngs) s = applyAll chngs s


affectZoneChange :: (Card -> Bool) -> Zone -> Zone -> Side -> Side
affectZoneChange scp zi zii (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) = let s = (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) in
 case zi of
  ZLibrary     -> foldr (changeZone zii zi) s (map getCardId (filter scp lib))
  ZGraveyard   -> foldr (changeZone zii zi) s (map getCardId (filter scp g))
  ZBattlefield -> foldr (changeZone zii zi) s (map getCardId (filter scp b))
  ZHand        -> foldr (changeZone zii zi) s (map getCardId (filter scp h))
  ZExile       -> foldr (changeZone zii zi) s (map getCardId (filter scp e))


--two functions used for affecting a bunch of cards at once
affectInZone :: Zone -> (Card -> Bool) -> (Card -> Card) -> Side -> Side
affectInZone z scp efct (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) m l) = 
 case z of
  ZLibrary     -> Side pl (Lib (affectCards scp efct lib)) (Grave g) (Btlf b) (Hand h) (Exile e) m l
  ZGraveyard   -> Side pl (Lib lib) (Grave (affectCards scp efct g)) (Btlf b) (Hand h) (Exile e) m l
  ZBattlefield -> Side pl (Lib lib) (Grave g) (Btlf (affectCards scp efct b)) (Hand h) (Exile e) m l
  ZHand        -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand (affectCards scp efct h)) (Exile e) m l
  ZExile       -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile (affectCards scp efct e)) m l

  
-- important function which will be used with the parser
changeZone :: Zone -> Zone -> CardId -> Side -> Side
changeZone zi zii cid = (addToZone zi).(removeFromZone cid zi)


-- important function which will be used with the parser
changeLife :: Int -> Side -> Side
changeLife c (Side pl lib g b h e m (Life l)) = Side pl lib g b h e m (Life (l+c))


removeTrigger :: Trigger -> Side -> Side
removeTrigger (Trigger e _ _ _ i z _ o) s = affectInZone z (\c -> (getCardId c) == i) (removeTriggerFromCard e i z o) s


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Gamestate -> Gamestate Functions 

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


manipulateResource :: Player -> (Side -> Side) -> Gamestate -> Gamestate
manipulateResource pl r (Gamestate s t p pr all) = Gamestate (condMap (ownerSide pr) r s) t p pr all


setPhaseTo :: Phase -> Gamestate -> Gamestate
setPhaseTo phs (Gamestate s p ph pl all) = Gamestate s p phs pl all

advancePhase :: Gamestate -> Gamestate
advancePhase (Gamestate s p ph pl all) = let g = (Gamestate s p ph pl all) in if ph == maxBound then nextTurn g else (Gamestate s p (succ ph) pl all)

nextTurn :: Gamestate -> Gamestate
nextTurn (Gamestate s p ph pl all) = let x = findNext p all in (Gamestate s x minBound x all)

changePriority :: Gamestate -> Gamestate
changePriority (Gamestate s p ph pl all) = let x = findNext pl all in (Gamestate s p minBound x all)

  
  
{-
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||


IO Block



||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
--Block of IO functions modelling the standard interactions and procedures of the game-}

messageIO :: String -> Gamestate -> IO Gamestate
messageIO s g = do putStr s
                   return g
                   
noChange :: Gamestate -> IO Gamestate
noChange = return


findTarget :: Player -> Gamestate -> Target -> IO (Either [CardId] [Player])
findTarget owner (Gamestate s p ph pl all) t = case t of
                                               Self                        -> return (Right [pl])
                                               Opponent                    -> do x <- chooseOpponent owner all
                                                                                 return (Right [x])
                                               ConditionalTarget c' zn' o' -> do y <- chooseCard owner c' zn' o' s
                                                                                 return (Left [y])
                                               AllPlayer                   -> return (Right all)
                                               ConditionalAllCards c zn o  -> return (Left (allCardsSatisfying c zn o s))
                                               ConditionalOwnTarget c'' z''-> do y' <- chooseCard owner c'' z'' (==owner) s
                                                                                 return (Left [y'])
                                          
                                          
chooseOpponent :: Player -> [Player] -> IO Player
chooseOpponent p all = do putStr (show p)
                          putStr "Choose Opponent: " 
                          x <- getLine
                          case filter ((\p' -> (p' == (parsePlayer x)) && (not (p' == p)))) all of
                            []  -> do putStr "Invalid Target"
                                      chooseOpponent p all
                            [z] -> return z
                                          

chooseCard :: Player -> (Card -> Bool) -> (Zone -> Bool) -> (Player -> Bool) -> [Side] -> IO CardId
chooseCard p c zn o s = do putStr (show p)
                           putStr "Choose Card: "
                           x <- getLine
                           let crds = allCardsSatisfying c zn o s in
                             case filter (\c' -> c' == (parseCard x)) crds of
                                  []  -> do putStr "Invalid Target"
                                            chooseCard p c zn o s
                                  [z] -> return z
                             

                                          
parsePlayer :: String -> Player
parsePlayer s = undefined


parseCard :: String -> CardId
parseCard c = undefined


applyProcedure :: Procedure -> Gamestate -> IO Gamestate
applyProcedure (Procedure p) g = p g
           

executeAction :: Player -> Source -> Action -> Gamestate -> IO Gamestate
executeAction owner s (ChainedAction a) g                           = ioFold (executeAction owner s) g a
executeAction owner s (Action t e str a) (Gamestate s' p ph pl all) = let g = (Gamestate s' p ph pl all) in
                                                                          do x   <- findTarget owner g t
                                                                             let tr = concatMap (allAffectedTriggers s e) (map (findSide g) all) in
                                                                                 do g'  <- applyProcedure (a s x str) g
                                                                                    g'' <- ioFold executeTrigger g' tr
                                                                                    return g''
                                                                 
                                      


executeTrigger :: Trigger -> Gamestate -> IO Gamestate
executeTrigger (Trigger e a b act i z scp p) g = let tr = (Trigger e a b act i z scp p) in
                                                     if b && (not act)
                                                      then executeAction p (CardSource i) a (manipulateResource p (removeTrigger tr) g)
                                                      else executeAction p (CardSource i) a g
                                              
                                      


emptyChange :: (Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure)
emptyChange _ _ _ = Procedure noChange
                                      
                                      
cast :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
cast (PlayerInitiator p) (Left (c:cs)) Nothing = Procedure (\g -> let s = findSide g p 
                                                                      (Card id _ _ _ _ mc (Cost chk cst) _ _ _ _ _ _ _) = findCard c ZHand s in
                                                                           if chk s && (checkManaPlayer mc s) then
                                                                              do g' <- (return (manipulateResource p (payCost mc (Cost chk cst)) g))
                                                                                 executeAction p (CardSource c) (eventAction (Event "Resolve")) g'
                                                                              else messageIO "Insufficient Ressources " g)
cast _ _ _ = Procedure (messageIO "Error: Invalid Cast")
                                                          
                                                                          


counter :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
counter = undefined

                  --From    To   
changeZoneAction :: Zone -> Zone -> Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
changeZoneAction zi zii _  (Left c) _ = let ac = affectZoneChange (xCardScope c) zi zii in
                                            Procedure (\(Gamestate s t p pr all) -> ioFold (\pl g -> return (manipulateResource pl ac g)) (Gamestate s t p pr all) all)
changeZoneAction zi zii _ (Right p) (Just am) = let ac = (\pl g -> affectZoneChange (xCardScope (getXpossibleCards am zi allAccept (findSide g pl))) zi zii) in
                                                      Procedure (\g'' -> ioFold (\pl' g' -> return (manipulateResource pl' (ac pl' g') g')) g'' p)
changeZoneAction _ _ _ _ _ = Procedure (messageIO "Error: Invalid Target for Zone Change")


declareAttackers :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
declareAttackers (PlayerInitiator id) (Left c) _ = let ac = affectInZone ZBattlefield (xCardScope c) (addKeyword (Keyword "Attacking")) in
                                                       Procedure (\g -> do g' <- return (manipulateResource id ac g)
                                                                           ioFold (\cid g'' -> executeAction id (CardSource cid) (findAction (Descriptor "Attack") (findCard cid ZBattlefield (findSide g'' id))) g'') g' c
                                                                 )


attack :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
attack (CardSource c) (Right (p:ps)) _ = Procedure (\g -> (executeAction p (CardSource c) (findAction (Descriptor "DeclareBlockers") (findCardAnywhere c ZBattlefield g)) g))
                                                                 
                                                                 
block :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
block (CardSource atk) (Left (blk : xs)) _ =  Procedure (resolveCombat (atk,blk))


resolveCombat :: (CardId, CardId) -> Gamestate -> IO Gamestate
resolveCombat (a,b) g = let (Card id n tp s (PT (Just (pwr,tgh))) c ac e o d t chng k tr) = findCardAnywhere a ZBattlefield g
                            (Card id' n' tp' s' (PT (Just (pwr',tgh'))) c' ac' e' o' d' t' chng' k' tr') = findCardAnywhere b ZBattlefield g
                        in do g'  <- executeAction o (CardSource id') (Action (ConditionalAllCards (checkCard id) (==ZBattlefield) (==o)) (Event "CombatDamage") (Just pwr') affectDamage) g
                              executeAction o' (CardSource id) (Action (ConditionalAllCards (checkCard id') (==ZBattlefield) (==o')) (Event "CombatDamage") (Just pwr) affectDamage) g'


affectDamage :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
affectDamage _ (Left c) (Just d) = let ac = affectInZone ZBattlefield (xCardScope c) (dealDamage (-d)) in
                                       Procedure (\(Gamestate s t p pr all) -> ioFold (\pl g -> return (manipulateResource pl ac g)) (Gamestate s t p pr all) all)
affectDamage _ (Right p) (Just d) = let ac = changeLife d in
                                        Procedure (\g -> ioFold (\pl g' -> return (manipulateResource pl ac g')) g p)
affectDamage _ _ _                = Procedure (messageIO "Error: No value while changing life")


affectTapUntap :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
affectTapUntap = undefined


affectChange :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
affectChange = undefined


reveal :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
reveal = undefined


activateAbility :: Source -> (Either [CardId] [Player]) -> (Maybe Int) -> Procedure
activateAbility = undefined


                                      
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



