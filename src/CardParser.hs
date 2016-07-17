module CardParser where

import Prelude
import Data.Char
import Text.Parsec
import Mtgmodel
import SupportFunctions


defaultOwner :: Player
defaultOwner = Id 401


standardResolveSpell :: CardId -> Action
standardResolveSpell c = (Action (ConditionalOwnTarget (checkCard c) (checkZone ZHand)) (Event "ETG") Nothing (changeZoneAction ZStack ZGraveyard))


data EffectSet = Set Cost [Trigger] Effect [Keyword]

extractCost :: EffectSet -> Cost
extractCost (Set c _ _ _) = c

extractTriggers :: EffectSet -> [Trigger]
extractTriggers (Set _ t _ _) = t

extractEffect :: EffectSet -> Effect
extractEffect (Set _ _ e _) = e

extractKeywords :: EffectSet -> [Keyword]
extractKeywords (Set _ _ _ k) = k


player :: Parsec String () Player
player = do p <- trueDigit
            return (Id p)
         
         
trueDigit :: Parsec String () Int
trueDigit = do i <- digit
               return (digitToInt i)
               
trueInt :: Parsec String () Int
trueInt = consecutiveInt 0

consecutiveInt :: Int -> Parsec String () Int
consecutiveInt i = (try (do i' <- trueDigit
                            consecutiveInt (i*10+i') )) <|> return i 
 
 
ignoreField :: Parsec String () ()
ignoreField = between (char '[') (char ']') (skipMany (field '{' '}' (skipMany1 alphaNum)))


dataSetName :: String -> Parsec String () ()
dataSetName n = do field '"' '"' (string n)
                   char ':'
                   return ()
                   

irrelevantDataName :: Parsec String () ()
irrelevantDataName = do field '"' '"' (skipMany1 alphaNum)
                        char ':'
                        return ()

                   
irrelevantData :: Parsec String () ()
irrelevantData = field '"' '"' (skipMany1 alphaNum)


irrelevantDataPair :: Parsec String () ()
irrelevantDataPair = do irrelevantDataName
                        irrelevantData


learn :: String -> Parsec String () String
learn s = do x <- anyChar
             return s

 
learn' :: String -> Parsec String () String
learn' s = do x <- ignoreField
              learn s 


textChars :: Parsec String () Char
textChars = alphaNum <|> headOf (string "'") <|> char ',' <|> char '.'


headOf :: Parsec String () String -> Parsec String () Char
headOf s = do x <- s
              return (head x)
              

earlyIdentify :: Parsec String () a -> Parsec String () a
earlyIdentify p = lookAhead (do skipDataTo p
                                p)
              
              
field :: Char -> Char -> Parsec String () a -> Parsec String () a
field c c' = between (char c) (char c')


commaSep :: Parsec String () ()
commaSep = do char ','
              return ()

              
skipDataTo :: Parsec String () a -> Parsec String () ()
skipDataTo s = do manyTill anyChar (try (lookAhead s))
                  return ()

                  
skipDataToResult :: Parsec String () a -> Parsec String () a
skipDataToResult s = do manyTill anyChar (try (lookAhead s))
                        s
                  

dataSep :: Parsec String () a  -> Parsec String () a
dataSep p = field '"' '"' p


m10Set :: Parsec String () [Card]
m10Set = do skipDataTo (dataSetName "cards")
            dataSetName "cards"
            cards
            

cards :: Parsec String () [Card]
cards = sepBy1 (field '{' '}' card) commaSep


card :: Parsec String () Card
card = do t <- earlyIdentify cardTypeField
          if findA Creature t
             then creature
             else if (findA Instant t) || (findA Sorcery t)
                     then spell
                     else permanent
             

             
creature :: Parsec String () Card
creature = undefined


spell :: Parsec String () Card
spell = do mc <- skipDataToResult manaCostField
           id <- skipDataToResult cardidFields
           ef <- skipDataToResult (ruleTextFieldSpell id)
           tp <- skipDataToResult cardTypeField
           let tr = extractTriggers ef
               stdac = extractEffect ef
               ac = extractCost ef
               k = extractKeywords ef
               in return (Card id (nameFromId id) tp [] (PT Nothing) mc ac stdac defaultOwner 0 True [] (k ++ (identifyColours mc)) tr)


permanent :: Parsec String () Card
permanent = undefined
             

cardTypeField :: Parsec String () [Cardtype]
cardTypeField = do dataSetName "types"
                   field '[' ']' (sepBy1 (dataSep cardType) commaSep)
                   
                   
cardType :: Parsec String () Cardtype
cardType = do t <- many1 letter
              case t of
                "Creature"     -> return Creature
                "Instant"      -> return Instant
                "Sorcery"      -> return Sorcery
                "Enchantment"  -> return Enchantment
                "Planeswalker" -> return Planeswalker
                "Artifact"     -> return Artifact
                "Land"         -> return Land

cardidFields :: Parsec String () CardId
cardidFields = do dataSetName "multiverseid"
                  x <- trueInt
                  char ','
                  dataSetName "name"
                  n <- dataSep (many1 textChars)
                  return (CardId (x,(Cardname n)))
                  
                  
manaCostField :: Parsec String () Manacost
manaCostField = do dataSetName "manaCost"
                   dataSep manacost
                   
                   
manacost :: Parsec String () Manacost
manacost = do x <- many1 mana
              return (MCost (condenseCost x))
              

condenseCost :: [(Int,Mana)] -> [(Int,Mana)]
condenseCost [] = []
condenseCost [x] = [x]
condenseCost ((a,m) : (b,m') : xs) = if m == m' then condenseCost ((a+b,m) : xs) else (a,m) : (condenseCost ((b,m') : xs)) 
             
              
mana :: Parsec String () (Int,Mana)
mana = try (do m <- field '{' '}' letter
               case m of
                 'W' -> return (1,White)
                 'U' -> return (1,Blue)
                 'B' -> return (1,Black)
                 'G' -> return (1,Green)
                 'R' -> return (1,Red)
            ) <|> do m' <- field '{' '}' trueDigit
                     return (m',Colourless)
                     
                     
continueTextWith :: Parsec String () a -> Parsec String () a
continueTextWith p = (do string ", "
                         p) <|> (do string ", and "
                                    p) <|> (do string "and "
                                               p) <|> (do string "then "
                                                          p)


ruleTextFieldSpell :: CardId -> Parsec String () EffectSet
ruleTextFieldSpell id = do dataSetName "text"
                           dataSep (ruleTextSpell id)
                        

ruleTextSpell :: CardId -> Parsec String () EffectSet
ruleTextSpell id = let (CardId (x,(Cardname n))) = id in
                     do cs <- addCost
                        ac <- spellAction (Cardname n)
                        let rslvTr = (Trigger (Event "Resolve") (ChainedAction [ac, standardResolveSpell id]) False True id ZStack allAccept defaultOwner) in
                            return (Set cs [rslvTr] (Effect []) [])



addCost :: Parsec String () Cost
addCost = undefined



whosePrs :: Parsec String () (Player -> Player -> Bool)
whosePrs = (do (string "You ") <|> (string "you ") <|> (string "your ")
               return (checkPlayerType You)) <|> (do string "Each opponent "
                                                     return (checkPlayerType TOpponent)) <|> (do (string "Each player ") <|> (string "all ")  <|>(string "a ") 
                                                                                                 return (checkPlayerType Each)) <|> return allAccept


wherePrs :: Parsec String () (Zone -> Bool)
wherePrs = (do (string "Graveyard ") <|> (string "graveyard ") <|> (string "graveyards ")
               return (checkZone ZGraveyard)) <|> (do (string "Library ") <|> (string "lybrary ")
                                                      return (checkZone ZLibrary)) <|> (do (string "control ")
                                                                                           return (checkZone ZBattlefield)) <|> return (not.allAccept)


whatPrs :: Parsec String () (Card -> Bool)
whatPrs = typechecker <|> keywordchecker <|> subtypechecker


typechecker :: Parsec String () (Card -> Bool)
typechecker = undefined


keywordchecker :: Parsec String () (Card -> Bool)
keywrodchecker = undefined


subtypechecker :: Parsec String () (Card -> Bool)
subtypechecker = undefined


spellAction :: Cardname -> Parsec String () Action
spellAction n = commandPattern False <|> namePattern n <|> targetPattern <|> allSatisfyingPattern <|> (do ac  <- spellAction n
                                                                                                          ac' <- spellAction n
                                                                                                          return (ChainedAction [ac,ac']))
               --True if the change is conditional to other effects.         
commandPattern :: Bool -> Parsec String () Action
commandPattern t = (try (completeCommand t)) <|> (try (simpleCommandTarget t)) <|> (try (commandTargetSpecification t))


completeCommand :: Bool -> Parsec String () Action
completeCommand False   = do cmnd <- (manyTill alphaNum (char ' '))
                             case cmnd of
                                "Prevent"  -> do skipDataTo (char '.')
                                                 char '.'
                                                 return (eventAction (Event "Prevent"))
                                "Search "  -> do cnd  <- whosePrs
                                                 cnd' <- wherePrs
                                                 (string "for a ") <|> (string "for an ")
                                                 cnd'' <- whatPrs
                                                 let t = ConditionalTarget cnd'' cnd' cnd
                                                     a = (Action t (Event "Search") Nothing emptyChange) in
                                                     try (do a' <- continueTextWith (commandPattern True)
                                                             return (CondAction [a,a'])) <|> (do char '.'
                                                                                                 return a)
                                                                                               
                                                                                               
simpleCommandTarget :: Bool -> Parsec String () Action
simpleCommandTarget = undefined


commandTargetSpecification :: Bool -> Parsec String () Action
commandTargetSpecification = undefined


namePattern :: Cardname -> Parsec String () Action
namePattern = undefined


targetPattern :: Parsec String () Action
targetPattern = undefined


allSatisfyingPattern :: Parsec String () Action
allSatisfyingPattern = undefined


