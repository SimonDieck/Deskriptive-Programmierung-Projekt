module CardParser where

import Prelude
import Data.Char
import Text.Parsec
import Mtgmodel
import SupportFunctions


defaultOwner :: Player
defaultOwner = Id 401


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
               in return (Card id (nameFromId id) tp [] (PT Nothing) mc ac stdac defaultOwner 0 True [] k tr)


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
manaCostField = undefined


ruleTextFieldSpell :: CardId -> Parsec String () EffectSet
ruleTextFieldSpell = undefined



                  
