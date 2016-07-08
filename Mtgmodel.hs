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

data Zone = ZLibrary | ZGraveyard | ZBattlefield | ZHand | ZExile

data Side = Side Player Library Graveyard Battlefield Hand Exile Manapool Life


--Gameprocedure related
data Phase = Untap | Upkeep | Draw | FirstMain | DeclareAttackers | DeclareBlockers | ResolveCombat | SecondMain | Endstep deriving (Enum, Bounded)

newtype Player = Id Int deriving (Eq)


-- Card related Identifiers
newtype Cardname = Cardname String deriving (Eq)

newtype CardId = CardId (Int, Cardname) deriving (Eq)

data Cardtype = Instant | Sorcery | Creature | Enchantment | Artifact | Planeswalker | Land | Legendary Cardtype | Token deriving (Eq)

data Subtype = Creaturetype | Mountain | Forest | Plains | Island | Swamp | Aura | Equipment deriving (Eq)

newtype Creaturetype = CreatureType String deriving (Eq)

newtype PowerToughness = PT (Maybe (Int, Int))

--               Id     Name     Type       Subtype   Power/ Toughness  Cost     Additional Cost  Effect Owner   Damage Untapped = True
data Card = Card CardId Cardname [Cardtype] [Subtype] PowerToughness    Manacost Cost             Effect Player  Int    Bool


--Gamestate
--                         All Sides  Player whose Turn it is  Current Phase  Player whose Priority it is
data Gamestate = Gamestate [Side]     Player                   Phase          Player


--Cardeffects
data Scope a = Scope (a -> Bool)

data Resource = RLibrary | RGraveyard | RBattlefield | RHand | RExile | RStack | RManapool | RLife

data Cost = Cost [(Scope Resource, Resource)]

--Actions have IDs so Triggers can be called
data Action = Action (Resource -> Resource) Player Int | Procedure

--                      Trigger        Consequence OneTime or Permanent Origin
data Trigger = Trigger (Scope Action)  Action      Bool                 CardId

type Procedure = (Gamestate -> Gamestate)

--Replaces a procedure with an alternative. Boolean checks if this is done once or as long as the card persists
data AltProcedure = Alternative ((Gamestate -> Gamestate), Int) Bool   CardId

data Effect = Effect [Action] [Trigger] [AltProcedure]


--Functions modelling change in the model


ownerSide :: Player -> Side -> Bool
ownerSide p (Side pl _ _ _ _ _ _ _) = p == pl


checkCard :: CardId -> Card -> Bool
checkCard i (Card id _ _ _ _ _ _ _ _ _ _) = id == i 


manipulateResource :: Player -> (Side -> Side) -> Gamestate -> Gamestate
manipulateResource pl r (Gamestate s t p pr) = Gamestate (map r (filter (ownerSide pl) s)) t p pr

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
tapuntap t (Card id n tp s pt c ac e o d _) = Card id n tp s pt c ac e o d t


checkUntap :: Card -> Bool
checkUntap (Card _ _ _ _ _ _ _ _ _ _ t) = t


getCardId :: Card -> CardId
getCardId (Card id _ _ _ _ _ _ _ _ _ _) = id


checkType :: Cardtype -> Card -> Bool
checkType ct (Card _ _ (x:xs) _ _ _ _ _ _ _ _) = if x == ct then True else findA ct xs


findA :: Eq a => a -> [a] -> Bool
findA x [] = False
findA x (y:ys) = if x == y then True else findA x ys


checkSubType :: Subtype -> Card -> Bool
checkSubType ct (Card _ _ _ (x:xs) _ _ _ _ _ _ _ ) = if x == ct then True else findA ct xs


copyCard :: Card -> Card
copyCard (Card (CardId (i,nm)) n tp s pt c ac e o d t) = Card (CardId (i+1,nm)) n tp s pt c ac e o d t


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




