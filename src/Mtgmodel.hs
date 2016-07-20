module Mtgmodel where

import Prelude
import SupportFunctions


--Recources
{-- 
Mana is the primary ressource for paying costs in MtG (Magic: The Gathering)
Every card mandatorily (with one exception in ca. 13 thousand unique cards) has a manacost associated with it.
This Mana comes in one of five colours and a cards colour is every colour occuring in its Manacost.
--}
data Mana = Red | Green | White | Blue | Black | Colourless deriving (Eq, Show)


{--
Storage for active unspend Mana a player has
--}
newtype Manapool = Pool [(Int,Mana)] deriving (Show)


{--
Manacost of a card as explained eralier.
Costs are saved as negative numbers
--}
newtype Manacost = MCost [(Int, Mana)] deriving (Show)


{--
The following six Zones are were cards can be. Every player has his own set of these zones.
The Library is the deck of cards from where cards are drawn from.
The Hand is where cards are drawn to most cards can only be cast from your hand.
Battelfield is where all active cards are placed.
The Graveyard is where used cards are placed.
Exile has similar functions to the graveyard
The Stack is were every card is placed when it is played from ones hand. From there it continues on to the Battlefield or the Graveyard. Used for players to interact with cards before they become active.
--}
data Zone = ZLibrary | ZGraveyard | ZBattlefield | ZHand | ZExile | ZStack | ZGlobal deriving (Eq)

newtype Library = Lib [Card] deriving (Show)

newtype Graveyard = Grave [Card] deriving (Show)

newtype Battlefield = Btlf [Card] deriving (Show)

newtype Hand = Hand [Card] deriving (Show)

newtype Exile = Exile [Card] deriving (Show)

newtype Stack = Stack [Card] deriving (Show)


{--
Starts at 20 if it hits 0 a player loses the game. Is also used as ressource to pay fro some spells and effects.
--}
newtype Life = Life Int deriving (Eq, Show)

--A player of the game
newtype Player = Id Int deriving (Eq, Show)

{--
Used for targeting purposes, so a card knows which players it is supposed to effect.
You =  Owner of the card
TOpponent = Not owner of the card
Each = all Players
--}
data PlayerType = You | TOpponent | Each

--Combination of everything before culminates in a side of a player
data Side = Side Player Library Graveyard Battlefield Hand Exile Stack Manapool Life

instance Show Side where
    show (Side pl lib g b h e s m l) = show pl ++ "\n" ++ show lib ++ "\n" ++ show g ++ "\n" ++ show h ++ "\n" ++ show e ++ "\n" ++ show s ++ "\n" ++ show m ++ "\n" ++ show l ++ "\n"


--Phases that are progressed through every turn
data Phase = UntapPhase | Upkeep | Draw | FirstMain | DeclareAttackers | DeclareBlockers | ResolveCombat | SecondMain | Endstep deriving (Enum, Bounded, Eq)


--Card related Identifiers
newtype Cardname = Cardname String deriving (Eq, Show)

newtype CardId = CardId (Int, Cardname) deriving (Eq, Show)

{--
Cardtypes are split into primary types and subtypes.
Primary types have certain rules associated with it and come with implicit effects for the card
Subtypes are used to target only a specific subset of cards or specify the effects implicated by the primary type.
--}
data Cardtype = Instant | Sorcery | Creature | Enchantment | Artifact | Planeswalker | Land | Legendary Cardtype | Token deriving (Eq)

data Subtype = Sub Creaturetype | Mountain | Forest | Plains | Island | Swamp | Aura | Equipment | Subtype String deriving (Eq)

newtype Creaturetype = CreatureType String deriving (Eq)

{--
Creatures have a Power and Toughness value associated with them used for combat. All other cardtypes don't have one, but can be transfromed into creatures. Therefore a Maybevalue was chosen.
--}
newtype PowerToughness = PT (Maybe (Int, Int))

--all static abilitys that can occur in m2010 and are abilites that cause alternate procedures and can be requested by other cards. Full List here: http://mtgsalvation.gamepedia.com/Evergreen Also changes in in the state of a card have been included here.
data Keyword = Keyword String deriving (Eq)

--Possible events that can be asked by triggers. Listed under Actions at: http://mtgsalvation.gamepedia.com/Evergreen
data Event = Event String deriving (Eq, Show)

{--
Changes are functions that can revert a change that was previously conducted by an other cards effect. This is necessary as for example static changes to Power and Toughness disappear when teir source changes zones.
Most obvious and regular use is in changePT.
--}
data Change = Change CardId (Card -> Card)


--               Id     Name     Type       Subtype   Power/ Toughness  Cost     Additional Cost  Effect Owner   Damage Untapped = True  Active Changes affecting the card  Keywords
data Card = Card CardId Cardname [Cardtype] [Subtype] PowerToughness    Manacost Cost             Effect Player  Int    Bool             [Change]                           [Keyword]  [Trigger]

instance Show Card where
    show (Card id _ _ _ _ _ _ _ _ _ _ _ _ _) = show id

{--
used for error handling in Card -> Card functions
--}
errorCard :: Card
errorCard = Card (CardId (-1, Cardname "Error")) (Cardname "Error") [] [] (PT Nothing) (MCost []) (Cost allAccept []) (Effect []) (Id (-1)) 0 False [] [] []


--Gamestate
--                         All Sides  Player whose Turn it is  Current Phase  Player whose Priority it is  List of all Players   GlobalTrigger
data Gamestate = Gamestate [Side]     Player                   Phase          Player                       [Player]              [Trigger]


instance Show Gamestate where
    show (Gamestate s _ _ _ _ _) = show s


--currently a stub repeats endlessly
checkWinner :: Gamestate -> Bool
checkWinner _ = False


--Cardeffects
data Scope a = Scope (a -> Bool) -- conclueded this didn't need an extra type so it was never used.

data Resource = RLibrary | RGraveyard | RBattlefield | RHand | RExile | RStack | RManapool | RLife --same as with Scope its functionality was replaced by Zones


{--
Used to model additional costs of cards or effects. A card can require you to pay additional non Mana ressources to be cast. The (Side -> Bool) function checks if those ressources are present.
Every cost like a change in life or discarding cards can be coded as a Side -> Side function, which are applied consecutively after their existence has been verified.
--}
data Cost = Cost (Side -> Bool) [Side -> Side]

{--
Together with Trigger the most important part of the model.
The last part causes a change in Gamestate. The functions used for this can be found in the IO block.
To know what is affected by the change described in the last function the Target mode is specified in Target.
As executeAction is an (Gamestate -> IO Gamestate) function itself it can be part of the Procedure so actions can cascade into each other using informations from the previous action.
Events are names for the actions and can cause triggers who listen for this to fire allowing for an alternative way of actions cascading itno each other, this way information is separate though.
ChainedAction is a simple way of specifying a sequence of Actions that don't need information from each other without triggers.
ConditionalActions is a sequence of actions where all actions after the first know the target of the first Action.
--}
data Action = Action Target Event (Maybe Int) (Source -> Either [CardId] [Player] -> Maybe Int -> Procedure) | ChainedAction [Action] | CondAction [Action]

{--
Desctiprotrs for implicit actions of cards. That every card of a certain cardtype must have.
--}
newtype StandardAction = Descriptor String deriving (Eq)

--If you use this Action you can call all Triggers listening for a certain event.
eventAction :: Event -> Action
eventAction e = Action Self e Nothing emptyChange


{--
A Trigger listens for a certain event. If an Action with that Event is applied the Trigger also applies it's own Action, if the thing that caused the event satisfies the (Source -> Bool)-function.
Certain Triggers are only active in certain zones. This can be used to prevent actions from happening by changing the zone of the card. Used to model Counterspells.
--}
--                      Trigger        Consequence OneTime or Permanent Active   Origin Zone in which the Trigger will activate Scope to check if the Source satisfies certain conditions Owner of the Trigger
data Trigger = Trigger  Event          Action      Bool                 Bool     CardId Zone                                    (Source -> Bool)                                          Player


instance Show Trigger where
    show (Trigger e _ _ _ _ _ _ _) = show e


newtype Procedure = Procedure (Gamestate -> IO Gamestate)

{--
The Target mode specifies which cards or players are affected by an action.
Again - Used by Conditional Action to use a target found by the first action.
Self - Owner of the action
Opponet - A player other then the owner
Condtional Target - A player chooses a card which satisfies all three conditions
AllPlayer - All players are affected
ConditionalAllCards - All cards satisfying all three conditions are affected
CondtionalOwnTarget - Replaced by the extension of the third condition in conditionalTarget
--}
data Target = Again | Self | Opponent | ConditionalTarget (Card -> Bool) (Zone -> Bool) (Player -> Player -> Bool) | AllPlayer | ConditionalAllCards (Card -> Bool) (Zone -> Bool) (Player -> Player -> Bool) | ConditionalAllPlayer (Player -> Player -> Bool) |ConditionalOwnTarget (Card -> Bool) (Zone -> Bool)

data Source = CardSource CardId | PlayerInitiator Player deriving (Show)

--List of implicit cardtype actions. These can be replaced by more specific conditions. For example a creature with vigilance wouldn't be tapped if it attacked.
data Effect = Effect [(StandardAction, Action)] 

--standardActions that get executed unless specified otherwise on a card

{--
Good example for cascading into other actions. For standard attack the woner of the card gets to choose an opponet. The attack function than executes an action, where the choosen opponent can choose a creaute he controls to block the attacker.
Which in turn calls an action where the attacking creature and the blocking creature deal damage to each other.
--}
standardAttack :: Action
standardAttack = Action Opponent (Event "Attack") Nothing attack


standardDeclareBlockers :: Action
standardDeclareBlockers = Action (ConditionalOwnTarget checkUntap (checkZone ZBattlefield)) (Event "DeclareBlocker") Nothing block


--Functions modelling change in the model
{--
This first set of functions couldn't be sorted in a certain category but are used mostly to find more specific data when given a broader set with conditions.
--}
checkPlayerType :: PlayerType -> Player -> Player -> Bool
checkPlayerType pt p p' = case pt of
                                You       -> p' == p
                                TOpponent -> p' /= p
                                Each      -> allAccept p


identifyColours :: Manacost -> [Keyword]
identifyColours (MCost [])     = []
identifyColours (MCost (x:xs)) = case x of
                                    (_,White)      -> Keyword "White" : identifyColours (MCost xs)
                                    (_,Blue)       -> Keyword "Blue" : identifyColours (MCost xs)
                                    (_,Black)      -> Keyword "Black" : identifyColours (MCost xs)
                                    (_,Red)        -> Keyword "Red" : identifyColours (MCost xs)
                                    (_,Green)      -> Keyword "Green" : identifyColours (MCost xs)
                                    (_,Colourless) -> identifyColours (MCost xs)


nameFromId :: CardId -> Cardname
nameFromId (CardId id) = snd id


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
affectedTriggersZone scp  = concatMap (affectedTriggers scp) 


identifyTrigger :: Event -> CardId -> Zone -> Player -> Trigger -> Bool
identifyTrigger e i z o (Trigger e' _ _ _ i' z' _ o') = e == e' && i == i' && z == z' && o == o'


allAffectedTriggers :: Source -> Event -> Side -> [Trigger]
allAffectedTriggers src ev (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack s) _ _) = concatMap (\(i,j) -> affectedTriggersZone (checkTrigger src ev i) j) (zip [ZGraveyard, ZBattlefield, ZHand, ZExile, ZStack] [g, b, h, e, s])



--secondary helper functions used to facilitate the functionality of later functions.
addToZone :: Zone -> (Card, Side) -> Side
addToZone z (c, Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack s) m l) = 
 case z of
  ZLibrary     -> Side pl (Lib (c:lib)) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack s) m l
  ZGraveyard   -> Side pl (Lib lib) (Grave (c:g)) (Btlf b) (Hand h) (Exile e) (Stack s) m l
  ZBattlefield -> Side pl (Lib lib) (Grave g) (Btlf (c:b)) (Hand h) (Exile e) (Stack s) m l
  ZHand        -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand (c:h)) (Exile e) (Stack s) m l
  ZExile       -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile (c:e)) (Stack s) m l
  ZStack       -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack (c:s)) m l

  
removeFromZone :: CardId -> Zone -> Side -> (Card,Side)
removeFromZone ci z (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack s) m l) = 
 case z of
  ZLibrary     -> (head (filter (checkCard ci) lib), Side pl (Lib (filter (not.checkCard ci) lib)) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack s) m l)
  ZGraveyard   -> (head (filter (checkCard ci) g), Side pl (Lib  lib) (Grave (filter (not.checkCard ci) g)) (Btlf b) (Hand h) (Exile e) (Stack s) m l)
  ZBattlefield -> (head (filter (checkCard ci) b), Side pl (Lib  lib) (Grave g) (Btlf (filter (not.checkCard ci) b)) (Hand h) (Exile e) (Stack s) m l)
  ZHand        -> (head (filter (checkCard ci) h), Side pl (Lib  lib) (Grave g) (Btlf b) (Hand (filter (not.checkCard ci) h)) (Exile e) (Stack s) m l)
  ZExile       -> (head (filter (checkCard ci) e), Side pl (Lib  lib) (Grave g) (Btlf b) (Hand h) (Exile (filter (not.checkCard ci) e)) (Stack s) m l)
  ZStack       -> (head (filter (checkCard ci) s), Side pl (Lib  lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack (filter (not.checkCard ci) s)) m l)


fromPool :: Manapool -> [(Int,Mana)]
fromPool (Pool m) = m

checkMana :: Manapool -> Mana -> Int
checkMana (Pool [])   m       = 0
checkMana (Pool ((x,y):xs)) m = if m == y then x else checkMana (Pool xs) m


removeMana :: Mana -> Manapool -> Manapool
removeMana _ (Pool []) = Pool []
removeMana m (Pool ((x,y):xs)) = if m == y then Pool xs else Pool ((x,y) : fromPool (removeMana m (Pool xs)))


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
getXpossibleCards x z scp (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack stc) m l) = let s = Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack stc) m l in
 if checkAmountZone x z scp s then
    case z of
        ZLibrary     -> map getCardId (take x (filter scp lib))
        ZGraveyard   -> map getCardId (take x (filter scp g))
        ZBattlefield -> map getCardId (take x (filter scp b))
        ZHand        -> map getCardId (take x (filter scp h))
        ZExile       -> map getCardId (take x (filter scp e))
        ZStack       -> map getCardId (take x (filter scp stc))
 else []


allZonesSatisfying :: (Zone -> Bool) -> Side -> [Card]
allZonesSatisfying zn (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack s) _ _)= concatMap snd (filter (\(i,j) -> zn i) (zip [ZLibrary, ZGraveyard, ZBattlefield, ZHand, ZExile, ZStack] [lib, g, b, h, e, s]))


allCardsSatisfying :: (Card -> Bool) -> (Zone -> Bool) -> (Player -> Bool) -> [Side] -> [CardId]
allCardsSatisfying c zn o s = map getCardId (filter c (concatMap (allZonesSatisfying zn) (filter (\(Side pl _ _ _ _ _ _ _ _) -> o pl) s)))


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Getters

|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}

findSide :: Gamestate -> Player -> Side
findSide (Gamestate s _ _ _ all _ ) p' = if findA p' all then head (filter (ownerSide p') s) else head s


getCardId :: Card -> CardId
getCardId (Card id _ _ _ _ _ _ _ _ _ _ _ _ _) = id


getLife :: Side -> Life
getLife (Side _ _ _ _ _ _ _ _ l) = l


getOwner :: Card -> Player
getOwner (Card _ _ _ _ _ _ _ _ p _ _ _ _ _) = p


findCard :: CardId -> Zone -> Side -> Card
findCard id z s = case filter (checkCard id) (allZonesSatisfying (== z) s) of
                        []     -> errorCard
                        (x:xs) -> x
                        
findCardAnywhere :: CardId -> Zone -> Gamestate -> Card
findCardAnywhere id z (Gamestate s _ _ _ _ _ ) = head (filter (\c -> getCardId c /= CardId (-1, Cardname "Error")) (map (findCard id z) s))


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Card -> Bool Functions 


Mainly used to find targets for actions.
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


--combined with getXpossibleCards allows to create a scope which only allows a certain number of cards to be affected by another scope
xCardScope :: [CardId] -> (Card -> Bool)
xCardScope flt c =  findA (getCardId c) flt


checkType :: Cardtype -> Card -> Bool
checkType ct (Card _ _ (x:xs) _ _ _ _ _ _ _ _ _ _ _) =  (x == ct) || findA ct xs


checkSubType :: Subtype -> Card -> Bool
checkSubType ct (Card _ _ _ (x:xs) _ _ _ _ _ _ _ _ _ _) = (x == ct) || findA ct xs


checkKeyword :: Keyword -> Card -> Bool
checkKeyword k (Card _ _ _ _ _ _ _ _ _ _ _ _ x _) = findA k x


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


Mainly used to find targets for actions.
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


--checks if there are x cards in zone z which fulfill condition scp
checkAmountZone :: Int -> Zone -> (Card -> Bool) -> Side -> Bool
checkAmountZone x z scp (Side _ (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) _ _ _) = 
 case z of
  ZLibrary     -> x <= length (filter scp lib)
  ZGraveyard   -> x <= length (filter scp g)
  ZBattlefield -> x <= length (filter scp b)
  ZHand        -> x <= length (filter scp h)
  ZExile       -> x <= length (filter scp e)
  
  
ownerSide :: Player -> Side -> Bool
ownerSide p (Side pl _ _ _ _ _ _ _ _) = p == pl


checkManaPlayer :: Manacost -> Side -> Bool
checkManaPlayer cst (Side _ _ _ _ _ _ _ m _) = checkManaCost cst m 


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Card -> Card Functions 


changing specific values of a card used by actions
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


copyCard :: Card -> Card
copyCard (Card (CardId (i,nm)) n tp s pt c ac e o d t chng k tr) = Card (CardId (i+1,nm)) n tp s pt c ac e o d t chng k tr


changePT :: Either () CardId -> (Int, Int) -> Card -> Card
changePT i (a,b) (Card id n tp s pt c ac e o d t chng k tr) = case pt of
                                                                PT Nothing           -> Card id n tp s pt c ac e o d t chng k tr
                                                                PT (Just (pwr, tgh)) -> case i of
                                                                                          Right ci -> let reverse = Change ci (changePT (Left ()) (-a,-b)) in 
                                                                                                        Card id n tp s (PT (Just (pwr+a,tgh+b))) c ac e o d t (reverse : chng) k tr
                                                                                          Left ()  -> Card id n tp s (PT (Just (pwr+a,tgh+b))) c ac e o d t chng k tr
                                                                                          
                                                                                          

--True = untap false = tap
tapuntap :: Bool -> Card -> Card
tapuntap t (Card id n tp s pt c ac e o d _ chng k tr) = Card id n tp s pt c ac e o d t chng k tr

removeTriggerFromCard :: Event -> CardId -> Zone -> Player -> Card -> Card
removeTriggerFromCard ev i z ow (Card id n tp s pt c ac e o d t chng k tr) = Card id n tp s pt c ac e o d t chng k (filter (not.identifyTrigger ev i z ow) tr)


dealDamage :: Int -> Card -> Card
dealDamage x (Card id n tp s pt c ac e o d t chng k tr) = Card id n tp s pt c ac e o (d+x) t chng k tr


addKeyword :: Keyword -> Card -> Card
addKeyword kw (Card id n tp s pt c ac e o d t chng k tr) = Card id n tp s pt c ac e o d t chng (kw : k) tr


removeKeyword :: Keyword -> Card -> Card
removeKeyword kw (Card id n tp s pt c ac e o d t chng k tr) = Card id n tp s pt c ac e o d t chng (filter ( /= kw) k) tr

{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Side -> Side Functions 

changing either resource values like the Manapool or life. Or changing the Zone of a specific card / set of cards
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}

manipulateManaPlayer :: (Manapool -> Manapool) -> Side -> Side
manipulateManaPlayer ptp  (Side pl lib g b h e s m l) = Side pl lib g b h e s (ptp m) l

--Main method for paying costs but still need binding functions for casting and activating abilities which will also check the cost and afterwards apply all effects
payCost :: Manacost -> Cost -> Side -> Side
payCost mc c s = let Side pl lib g b h e stc m l = payaddCost c s in Side pl lib g b h e stc (payManaCost mc m) l


payaddCost :: Cost -> Side -> Side
payaddCost (Cost _ chngs)  = applyAll chngs 


affectZoneChange :: (Card -> Bool) -> Zone -> Zone -> Side -> Side
affectZoneChange scp zi zii (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack stc) m l) = let s = Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack stc) m l in
 case zi of
  ZLibrary     -> foldr (changeZone zi zii.getCardId) s (filter scp lib)
  ZGraveyard   -> foldr (changeZone zi zii.getCardId) s (filter scp g)
  ZBattlefield -> foldr (changeZone zi zii.getCardId) s (filter scp b)
  ZHand        -> foldr (changeZone zi zii.getCardId) s (filter scp h)
  ZExile       -> foldr (changeZone zi zii.getCardId) s (filter scp e)
  ZStack       -> foldr (changeZone zi zii.getCardId) s (filter scp stc)


--two functions used for affecting a bunch of cards at once
affectInZone :: Zone -> (Card -> Bool) -> (Card -> Card) -> Side -> Side
affectInZone z scp efct (Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack stc) m l) = 
 case z of
  ZLibrary     -> Side pl (Lib (affectCards scp efct lib)) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack stc) m l
  ZGraveyard   -> Side pl (Lib lib) (Grave (affectCards scp efct g)) (Btlf b) (Hand h) (Exile e) (Stack stc) m l
  ZBattlefield -> Side pl (Lib lib) (Grave g) (Btlf (affectCards scp efct b)) (Hand h) (Exile e) (Stack stc) m l
  ZHand        -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand (affectCards scp efct h)) (Exile e) (Stack stc) m l
  ZExile       -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile (affectCards scp efct e)) (Stack stc) m l
  ZStack       -> Side pl (Lib lib) (Grave g) (Btlf b) (Hand h) (Exile e) (Stack (affectCards scp efct stc)) m l

  
-- important function which will be used with the parser
changeZone :: Zone -> Zone -> CardId -> Side -> Side
changeZone zi zii cid = addToZone zii . removeFromZone cid zi


-- important function which will be used with the parser
changeLife :: Int -> Side -> Side
changeLife c (Side pl lib g b h e s m (Life l)) = Side pl lib g b h e s m (Life (l+c))


removeTrigger :: Trigger -> Side -> Side
removeTrigger (Trigger e _ _ _ i z _ o)  = affectInZone z (\c -> getCardId c == i) (removeTriggerFromCard e i z o) 


{-
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

Gamestate -> Gamestate Functions 

manipulating the broader aspects of the game like whose turn it is and who currently is allowed to take action. etc.
|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-}


manipulateResource :: Player -> (Side -> Side) -> Gamestate -> Gamestate
manipulateResource pl r (Gamestate s t p pr all tr) = Gamestate (condMap (ownerSide pl) r s) t p pr all tr


setPhaseTo :: Phase -> Gamestate -> Gamestate
setPhaseTo phs (Gamestate s p ph pl all tr) = Gamestate s p phs pl all tr

advancePhase :: Gamestate -> Gamestate
advancePhase (Gamestate s p ph pl all tr) = let g = Gamestate s p ph pl all tr in if ph == maxBound then nextTurn g else Gamestate s p (succ ph) pl all tr

nextTurn :: Gamestate -> Gamestate
nextTurn (Gamestate s p ph pl all tr) = let x = findNext p all in Gamestate s x minBound x all tr

changePriority :: Gamestate -> Gamestate
changePriority (Gamestate s p ph pl all tr) = let x = findNext pl all in Gamestate s p minBound x all tr

  
  
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
findTarget owner (Gamestate s p ph pl all tr) t = case t of
                                                    Self                        -> return (Right [pl])
                                                    Opponent                    -> do x <- chooseOpponent owner all
                                                                                      return (Right [x])
                                                    ConditionalTarget c' zn' o' -> do y <- chooseCard owner c' zn' (o' owner) s
                                                                                      return (Left [y])
                                                    AllPlayer                   -> return (Right all)
                                                    ConditionalAllCards c zn o  -> return (Left (allCardsSatisfying c zn (o owner) s))
                                                    ConditionalOwnTarget c'' z''-> do y' <- chooseCard owner c'' z'' (==owner) s
                                                                                      return (Left [y'])
                                                    ConditionalAllPlayer o''    -> return (Right (filter (o'' owner) all))
                                          
                                          
chooseOpponent :: Player -> [Player] -> IO Player
chooseOpponent p all = do putStr (show p)
                          putStr "Choose Opponent: " 
                          x <- getLine
                          case filter (\p' -> (p' == parsePlayer x) && (p' /= p)) all of
                            []  -> do putStr "Invalid Target"
                                      chooseOpponent p all
                            [z] -> return z
                                          

chooseCard :: Player -> (Card -> Bool) -> (Zone -> Bool) -> (Player -> Bool) -> [Side] -> IO CardId
chooseCard p c zn o s = do putStr (show p)
                           putStr "Choose Card: "
                           x <- getLine
                           let crds = allCardsSatisfying c zn o s in
                             case filter (\c' -> c' == parseCard x) crds of
                                  []  -> do putStr "Invalid Target"
                                            chooseCard p c zn o s
                                  [z] -> return z
                             

                                          
parsePlayer :: String -> Player
parsePlayer s = undefined


parseCard :: String -> CardId
parseCard c = undefined


applyProcedure :: Procedure -> Gamestate -> IO Gamestate
applyProcedure (Procedure p)  = p 


executeWithTarget :: Either [CardId] [Player] -> Player -> Source -> Action -> Gamestate -> IO Gamestate
executeWithTarget x owner s (CondAction ca)   g                               = messageIO "Parsed faulty dependencies" g
executeWithTarget x owner s (ChainedAction a) g                               = ioFold (executeWithTarget x owner s) g a
executeWithTarget x owner s (Action Again e str a) (Gamestate s' p ph pl all tr') = let g = Gamestate s' p ph pl all tr'
                                                                                        tr = concatMap (allAffectedTriggers s e . findSide g) all ++ filter (checkTrigger s e ZGlobal) tr' in
                                                                                        do g'  <- applyProcedure (a s x str) g
                                                                                           ioFold executeTrigger g' tr
executeWithTarget x owner s (Action t e str a) g = executeAction owner s (Action t e str a) g


executeAction :: Player -> Source -> Action -> Gamestate -> IO Gamestate
executeAction owner s (CondAction ca)  (Gamestate s' p ph pl all tr')   = let Action t e str a = head ca 
                                                                              g = Gamestate s' p ph pl all tr' in
                                                                                   do x <- findTarget owner g t
                                                                                      ioFold (executeWithTarget x owner s) g ca
executeAction owner s (ChainedAction a) g                               = ioFold (executeAction owner s) g a
executeAction owner s (Action t e str a) (Gamestate s' p ph pl all tr') = let g = Gamestate s' p ph pl all tr' in
                                                                           do x   <- findTarget owner g t
                                                                              putStrLn ("Found Target: " ++ show x)
                                                                              let tr = concatMap (allAffectedTriggers s e.findSide g) all ++ filter (checkTrigger s e ZGlobal) tr' in
                                                                                  do g'  <- applyProcedure (a s x str) g
                                                                                     putStrLn ("Applied Procedure, executing Triggers for Event " ++ show e ++ " with Triggers " ++ show tr ++ " from " ++ show s)
                                                                                     g'' <- ioFold executeTrigger g' tr
                                                                                     putStrLn ("All Triggers applied for Event: " ++ show e)
                                                                                     return g''
                                                                 
                                      


executeTrigger :: Trigger -> Gamestate -> IO Gamestate
executeTrigger (Trigger e a b act i z scp p) g = let tr = Trigger e a b act i z scp p in
                                                     if b && not act
                                                      then executeAction p (CardSource i) a (manipulateResource p (removeTrigger tr) g)
                                                      else executeAction p (CardSource i) a g
                                              
                                      


emptyChange :: (Source -> Either [CardId] [Player] -> Maybe Int -> Procedure)
emptyChange _ _ _ = Procedure noChange
                                      

--checks if costs can be payed and then puts the card onto the stack, giving via a global or local trigger waiting for the cast action other players the chance to remove the card from the stack therefore preventing the Resolve trigger of a card from applying
cast :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
cast (PlayerInitiator p) (Left (c:cs)) Nothing = Procedure (\g -> let s = findSide g p 
                                                                      (Card id _ _ _ _ mc (Cost chk cst) _ _ _ _ _ _ _) = findCard c ZHand s in
                                                                           if chk s && checkManaPlayer mc s then
                                                                             let g' =  manipulateResource p (payCost mc (Cost chk cst)) g in
                                                                              do putStrLn ("Start Cast of: " ++ show c)
                                                                                 g'' <- executeAction p (CardSource c) (Action (ConditionalAllCards (checkCard c) (checkZone ZHand) (checkPlayerType You)) (Event "Cast") Nothing (changeZoneAction ZHand ZStack)) g'
                                                                                 putStrLn "Call Resolve Trigger"
                                                                                 executeAction p (CardSource c) (eventAction (Event "Resolve")) g''
                                                                              else messageIO "Insufficient Ressources " g)
cast _ _ _ = Procedure (messageIO "Error: Invalid Cast")
                                                          
                                                                          

--was replaced by changeZoneAction with the resolve trigger only working if the card is on the Stack.
counter :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
counter = undefined


                  --From    To   
changeZoneAction :: Zone -> Zone -> Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
changeZoneAction zi zii _  (Left c) _ = let ac = affectZoneChange (xCardScope c) zi zii in
                                            Procedure (\(Gamestate s t p pr all tr) -> do putStrLn ("Change Zone of: " ++ show c)
                                                                                          gmn <- ioFold (\pl g -> return (manipulateResource pl ac g)) (Gamestate s t p pr all tr) all
                                                                                          putStrLn ("Successfull change of Zones: " ++ show c)
                                                                                          return gmn)
changeZoneAction zi zii _ (Right p) (Just am) = let ac pl g = affectZoneChange (xCardScope (getXpossibleCards am zi allAccept (findSide g pl))) zi zii in
                                                      Procedure (\g'' -> ioFold (\pl' g' -> return (manipulateResource pl' (ac pl' g') g')) g'' p)
changeZoneAction _ _ _ _ _ = Procedure (messageIO "Error: Invalid Target for Zone Change")


declareAttackers :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
declareAttackers (PlayerInitiator id) (Left c) _ = let ac = affectInZone ZBattlefield (xCardScope c) (addKeyword (Keyword "Attacking")) in
                                                       Procedure (\g -> let g' = manipulateResource id ac g in
                                                                            ioFold (\cid g'' -> executeAction id (CardSource cid) (findAction (Descriptor "Attack") (findCard cid ZBattlefield (findSide g'' id))) g'') g' c
                                                                 )


attack :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
attack (CardSource c) (Right (p:ps)) _ = Procedure (\g -> executeAction p (CardSource c) (findAction (Descriptor "DeclareBlockers") (findCardAnywhere c ZBattlefield g)) g)
                                                                 
                                                                 
block :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
block (CardSource atk) (Left (blk : xs)) _ =  Procedure (resolveCombat (atk,blk))


resolveCombat :: (CardId, CardId) -> Gamestate -> IO Gamestate
resolveCombat (a,b) g = let (Card id n tp s (PT (Just (pwr,tgh))) c ac e o d t chng k tr) = findCardAnywhere a ZBattlefield g
                            (Card id' n' tp' s' (PT (Just (pwr',tgh'))) c' ac' e' o' d' t' chng' k' tr') = findCardAnywhere b ZBattlefield g
                        in do g'  <- executeAction o (CardSource id') (Action (ConditionalAllCards (checkCard id) (==ZBattlefield) (==)) (Event "CombatDamage") (Just pwr') affectDamage) g
                              executeAction o' (CardSource id) (Action (ConditionalAllCards (checkCard id') (==ZBattlefield) (==)) (Event "CombatDamage") (Just pwr) affectDamage) g'


affectDamage :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
affectDamage _ (Left c) (Just d) = let ac = affectInZone ZBattlefield (xCardScope c) (dealDamage (-d)) in
                                       Procedure (\(Gamestate s t p pr all tr) -> ioFold (\pl g -> return (manipulateResource pl ac g)) (Gamestate s t p pr all tr) all)
affectDamage _ (Right p) (Just d) = let ac = changeLife d in
                                        Procedure (\g -> do putStr ("Change Life of: " ++ show p)
                                                            ioFold (\pl g' -> return (manipulateResource pl ac g')) g p)
affectDamage _ _ _                = Procedure (messageIO "Error: No value while changing life")


--wasn't really necessary so wasn't implemented.
affectTapUntap :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
affectTapUntap = undefined

--would be used to apply temporary changes.
affectChange :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
affectChange = undefined

--doesn't really make sense without GUI
reveal :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
reveal = undefined

--activated abilities weren't fully implemented was still debating if I could fully code them into triggers.
activateAbility :: Source -> Either [CardId] [Player] -> Maybe Int -> Procedure
activateAbility = undefined


{--
Functions that would specify how a game anturally processes and ask the players if they would want to take actions specific to the phase.
Wasn't planned in the pitch to fully add. If this was fleshed out you could have game in the console.
--}
game :: IO ()
game = do g <- initalize
          while (not.checkWinner) turn g
          return ()


initalize :: IO Gamestate
initalize = undefined
          
while :: (a -> Bool) -> (a -> IO a) -> (a -> IO a)
while p body = loop
    where loop x = if p x then do x' <- body x
                                  loop x'
                          else return x

turn :: Gamestate -> IO Gamestate
turn (Gamestate s p ph pl all tr) = let g = Gamestate s p ph pl all tr in
                                     ioapplyAll [untapStep, upkeepStep, drawStep, firstMainStep, combatStep, secondMainStep, endStep] g
                                 
                                 
--TODO: change to use execute action           
untapStep :: Gamestate -> IO Gamestate
untapStep (Gamestate s p ph pl all tr) =  let g = Gamestate s p ph pl all tr in
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



