module CardParser where

import Prelude
import Data.Char
import Text.Parsec
import Mtgmodel


player :: Parsec String () Player
player = do p <- trueDigit
            return (Id p)
         
         
trueDigit :: Parsec String () Int
trueDigit = do i <- digit
               return (digitToInt i)
 
 
ignoreField :: Parsec String () ()
ignoreField = between (char '[') (char ']') (skipMany (field '{' '}' (skipMany1 alphaNum)))


dataSetName :: String -> Parsec String () ()
dataSetName n = do field '"' '"' n
                   char ':'
                   return ()
                   

irrelevantDataName :: Parsec String () ()
irrelevantDataName = do field '"' '"' (skipMany1 alphaNum)
                        char ':'
                        return ()

                   
irrelevantData :: Parsec String () ()
irrelevantData = field '"' '"' (skipMany1 alphaNum)


irrelevantDataPair :: ParsecString () ()
irrelevantDataPair = do irrelevantDataName
                        irrelevantData


learn :: String -> Parsec String () String
learn s = do x <- anyChar
             return s

 
learn' :: String -> Parsec String () String
learn' s = do x <- ignoreField
              learn s 



earlyIdentify :: Parsec String () a -> Parsec String () a
earlyIdentify p = do skipDataTo p
                     p
              
              
field :: Char -> Char -> Parsec String () a -> Parsec String () a
field c c' = between (char c) (char c')


commaSep :: Parsec String () ()
commaSep = do char ','
              return ()

              
skipDataTo :: Parsec String () () -> Parsec String () ()
skipDataTo s = manyTill anyChar (try (lookAhead s))


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
spell = undefined


permanent :: Parsec String () Card
permanent = undefined
             

cardTypeField :: Parsec String () [CardType]
cardTypeField = do dataSetName "types"
                   field '[' ']' (sepBy1 (dataSep cardType) commaSep)
                   
                   
cardType :: Parsec String () CardType
cardType = do t <- many1 letter
              case t of
                "Creature"     -> return Creature
                "Instant"      -> return Instant
                "Sorcery"      -> return Sorcery
                "Enchantment"  -> return Enchantment
                "Planeswalker" -> return Planeswalker
                "Artifact"     -> return Artifact
                "Land"         -> return Land


