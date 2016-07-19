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


someCards :: Int -> Player -> [Card]
someCards i p = map (angelsMercy p) (map (card1) [ j | j <- [i..(i+9)]])


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


simpleGamestate :: Gamestate
simpleGamestate = Gamestate [side1,side2] player1 FirstMain player1 [player1,player2] []

side1 = Side player1 (Lib (someCards 0 player1)) (Grave []) (Btlf []) (Hand (someCards 10 player1)) (Exile []) (Stack [angelsMercy player1 (card1 401)]) (Pool [(0,Black),(0,Blue),(4,White),(0,Green),(0,Red),(4,Colourless)]) (Life 20)

side2 = Side player2 (Lib (someCards 20 player2)) (Grave []) (Btlf []) (Hand (someCards 30 player2)) (Exile []) (Stack []) (Pool [(0,Black),(0,Blue),(4,White),(0,Green),(0,Red),(4,Colourless)]) (Life 20)




test1 = TestCase (do g <- executeAction player1 (PlayerInitiator player1)  (Action (ConditionalAllCards (checkCard (card1 12)) (checkZone ZHand) (checkPlayerType You)) (Event "On Stack") Nothing (changeZoneAction ZHand ZStack)) simpleGamestate
                     assertBool ("Error Check") True
                     g' <- executeAction player1 (PlayerInitiator player1) (Action (ConditionalAllCards (checkCard (card1 12)) (checkZone ZHand) (checkPlayerType You)) (Event "Declare Cast") Nothing cast) simpleGamestate
                     assertEqual "LifeChange after cast of Angel's Mercy, " (Life 27) (getLife (findSide g' player1)))
                     
                     
                     
tests = TestList [TestLabel "ResolveTest" test1]

