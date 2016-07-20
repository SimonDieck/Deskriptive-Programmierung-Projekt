module TestFile where

import Test.HUnit

import Prelude
import Mtgmodel
import SupportFunctions

player1 :: Player
player1 = Id 1


player2 = Id 2

noAddCost :: Cost
noAddCost = Cost allAccept []


selfAccept :: CardId -> Source -> Bool
selfAccept i (CardSource i') = i == i'
selfAccept i (PlayerInitiator _) = False 


selfAcceptPlayer :: Player -> Source -> Bool
selfAcceptPlayer p (CardSource _) = False
selfAcceptPlayer p (PlayerInitiator p') = p == p'


someCards :: Int -> Player -> [Card]
someCards i p = map (angelsMercy p) (map (card1) [ j | j <- [i..(i+9)]])


--http://magiccards.info/m13/en/3.html
card1 :: Int -> CardId
card1 i = CardId (i , Cardname "Angel's Mercy")

angelsMercy :: Player -> CardId -> Card
angelsMercy p c = Card c (Cardname "Angel's Mercy") [Instant] [] (PT Nothing) (MCost [((-2),Colourless),((-2),White)]) noAddCost (Effect []) p 0 True [] [Keyword "White"] [angelsMercyResolve c p]

angelsMercyResolve :: CardId -> Player -> Trigger
angelsMercyResolve i p = Trigger (Event "Resolve") (ChainedAction [angelsMercyEffect, angelsMercyFinishResolve i]) False True i ZStack (selfAccept i) p

angelsMercyEffect :: Action
angelsMercyEffect = Action Self (Event "Gain Life") (Just 7) affectDamage

angelsMercyFinishResolve :: CardId -> Action                                                                      --"Enter the Graveyard" analog ETB : "Enter the Battlefield"
angelsMercyFinishResolve i = Action (ConditionalAllCards (checkCard i) (checkZone ZStack) (checkPlayerType You)) (Event "ETG") Nothing (changeZoneAction ZStack ZGraveyard)


--http://magiccards.info/ogw/en/153.html
card2 :: Int -> CardId
card2 i = CardId (i,Cardname "Cliffhaven Vampire")

cliffHaven :: Player -> CardId -> Card
cliffHaven p c = Card c (Cardname "Cliffhaven Vampire") [Creature] [Sub (CreatureType "Vampire"), Sub (CreatureType "Warrior"), Sub (CreatureType "Ally")] (PT (Just (2,4))) (MCost [((-2),Colourless),((-1),White),((-1),Black)]) noAddCost cliffHavenEffect p 0 True [] [Keyword "Black", Keyword "White", Keyword "Flying"] [cliffHavenResolve c p, cliffHavenTrigger c p]

--The Special Declare Blocker Action is caused by the Flying effect of Cliffhaven Vampire which states in its Ruletext "This creature can only be blocked by creatures with Flying or Reach"
cliffHavenEffect :: Effect
cliffHavenEffect = Effect [(Descriptor "Attack",standardAttack),(Descriptor "DeclareBlockers", (Action (ConditionalOwnTarget (combineConditions (&&) checkUntap (combineConditions (||) (checkKeyword (Keyword "Flying")) (checkKeyword (Keyword "Reach")))) (checkZone ZBattlefield)) (Event "DeclareBlocker") Nothing block))]

cliffHavenResolve :: CardId -> Player -> Trigger
cliffHavenResolve c p = Trigger (Event "Resolve") (Action (ConditionalAllCards (checkCard c) (checkZone ZStack) (checkPlayerType You)) (Event "ETB") Nothing (changeZoneAction ZStack ZBattlefield)) False True c ZStack (selfAccept c) p


cliffHavenTrigger :: CardId -> Player -> Trigger
cliffHavenTrigger c p = Trigger (Event "Gain Life") (Action (ConditionalAllPlayer (checkPlayerType TOpponent)) (Event "Lose Life") (Just (-1)) affectDamage) False True c ZBattlefield (allAccept) p


simpleGamestate :: Gamestate
simpleGamestate = Gamestate [side1,side2] player1 FirstMain player1 [player1,player2] []

side1 = Side player1 (Lib (someCards 0 player1)) (Grave []) (Btlf []) (Hand ((someCards 10 player1) ++ [cliffHaven player1 (card2 1)])) (Exile []) (Stack [angelsMercy player1 (card1 401)]) (Pool [(2,Black),(0,Blue),(5,White),(0,Green),(0,Red),(8,Colourless)]) (Life 20)

side2 = Side player2 (Lib (someCards 20 player2)) (Grave []) (Btlf []) (Hand (someCards 30 player2)) (Exile []) (Stack []) (Pool [(0,Black),(0,Blue),(4,White),(0,Green),(0,Red),(4,Colourless)]) (Life 20)




test1 = TestCase (do g <- executeAction player1 (PlayerInitiator player1) (Action (ConditionalAllCards (checkCard (card2 1)) (checkZone ZHand) (checkPlayerType You)) (Event "Declare Cast") Nothing cast) simpleGamestate
                     g' <- executeAction player1 (PlayerInitiator player1) (Action (ConditionalAllCards (checkCard (card1 12)) (checkZone ZHand) (checkPlayerType You)) (Event "Declare Cast") Nothing cast) g
                     assertEqual "LifeChange after cast of Angel's Mercy, " (Life 27) (getLife (findSide g' player1))
                     assertEqual "LifeChange after Trigger of Cliffhaven Vampire, " (Life 19) (getLife (findSide g' player2)))
                     
                     
                     
tests = TestList [TestLabel "ResolveTest" test1]

