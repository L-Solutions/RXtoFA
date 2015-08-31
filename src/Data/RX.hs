{- LANGUAGE GADTs -}

module Data.RX where

import           Control.Applicative         (Applicative, pure, (<$>), (<*),
                                              (<*>))
import           Control.Monad
import           Data.Foldable
import           Data.Monoid
import           Data.Traversable

import           Data.Char                   (digitToInt, chr)
import           Data.Maybe                  (maybeToList)
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import           Data.Tree

import           Text.Parsec.ByteString.Lazy (Parser, parseFromFile)
import           Text.Parsec.Char            (char, digit, lower, noneOf, oneOf,
                                              spaces, string, upper)
import           Text.Parsec.Combinator      (between, chainl, chainl1, lookAhead,
                                              many1, option, optionMaybe)
import           Text.Parsec.Prim


data RX a = RXEmpty
          | RXToken a
          | RXKleene (RX a)
          | RXChoice [RX a]
          | RXSeq [RX a]
  deriving Eq

instance Show a => Show (RX a) where
    show RXEmpty = "()"
    show (RXToken x) = show x
    show (RXKleene rx) = show rx ++ "*"
    show (RXSeq rxs) = showRXOp "." rxs
    show (RXChoice rxs) = showRXOp "|" rxs

{- l'affichage ne produit pas de préseance d'operateur -}
showRXOp :: Show a => String -> [RX a] -> String
showRXOp sep rx = toString rx
  where toString [] = ""
        toString [x] = show x
        toString (x:xs) = show x ++ sep ++ toString xs

instance Monoid (RX a) where
    mempty = RXEmpty
    RXEmpty `mappend` rx = rx
    rx `mappend` RXEmpty = rx
    RXSeq rxa `mappend` RXSeq rxb = RXSeq $ rxa ++ rxb
    RXSeq rxs `mappend` rx = RXSeq $ rxs ++ [rx]
    rx `mappend` RXSeq rxs = RXSeq $ rx : rxs
    rxl `mappend` rxr = RXSeq [rxl, rxr]

instance Functor RX where
    fmap _ RXEmpty = RXEmpty
    fmap f (RXToken x) = RXToken $ f x
    fmap f (RXKleene rx) = RXKleene $ fmap f rx
    fmap f (RXSeq rxs) = RXSeq $ fmap (fmap f) rxs
    fmap f (RXChoice rxs) = RXChoice $ fmap (fmap f) rxs

instance Applicative RX where
    pure = RXToken
    RXEmpty <*> _ = RXEmpty
    _ <*> RXEmpty = RXEmpty
    (RXToken f) <*> rx = fmap f rx
    (RXKleene rxf) <*> rx = RXKleene $ rxf <*> rx
    (RXChoice rxfs) <*> rxs = RXChoice $ fmap (<*> rxs) rxfs
    (RXSeq rxfs) <*> rxs = RXSeq $ fmap (<*> rxs) rxfs

instance Foldable RX where
    foldMap _ RXEmpty = mempty
    foldMap f (RXToken x) = f x
    foldMap f (RXKleene rx) = foldMap f rx
    foldMap f (RXChoice rxs) = mconcat $ fmap (foldMap f) rxs
    foldMap f (RXSeq rxs) = mconcat $ fmap (foldMap f) rxs

instance Traversable RX where
    traverse _ RXEmpty = pure RXEmpty
    traverse f (RXToken x) = RXToken <$> f x
    traverse f (RXKleene rx) = RXKleene <$> traverse f rx
    traverse f (RXChoice rxs) = RXChoice <$> traverse (traverse f) rxs
    traverse f (RXSeq rxs) = RXChoice <$> traverse (traverse f) rxs

instance Monad RX where
    return = pure
    RXEmpty >>= _ = RXEmpty
    RXToken x >>= f = f x
    RXKleene rx >>= f = RXKleene $ rx >>= f
    RXChoice rxs >>= f = RXChoice [rx >>= f | rx <- rxs]
    RXSeq rxs >>= f = RXSeq [rx >>= f | rx <- rxs]

type RXParser = Parser (RX Char)

{-
      ######
     #     #    ##     ####   #   ####
     #     #   #  #   #       #  #    #
     ######   #    #   ####   #  #
     #     #  ######       #  #  #
     #     #  #    #  #    #  #  #    #
     ######   #    #   ####   #   ####
-}
digitInt :: Parser Int
digitInt = digit >>= (return . digitToInt)

int :: Parser Int
int =  digitInt `chainl1` return (\m n -> 10*m + n)

{- Range -}
rxNum :: Parser (Int,Int)
rxNum = rxBetweenCurlyBracket rxRange
 where
    rxBetweenCurlyBracket :: Parser (Int,Int) -> Parser (Int,Int)
    rxBetweenCurlyBracket = between (string "\\{") (string "\\}")

    rxRange :: Parser (Int,Int)
    rxRange = rxRangeUp <|> rxRangeComplete

    rxRangeStart :: Parser Int
    rxRangeStart = int

    rxRangeEnd :: Parser Int
    rxRangeEnd = char ',' >> int

    rxRangeUp :: Parser (Int,Int)
    rxRangeUp = do end <- rxRangeEnd
                   return (0,end)

    rxRangeComplete :: Parser (Int,Int)
    rxRangeComplete = do start <- rxRangeStart
                         end <- ( rxRangeEnd <|> return 256 )
                         return (start, end)


{- Char and the dot -}
rxSpecialChars :: String
rxSpecialChars = ['|','\\','*','+','?','(',')','[',']','^','-','.',' ']

rxPoint :: RXParser
rxPoint = do char '.'
             return $ RXChoice $ fmap (RXToken . chr) [0..127]

rxLegal :: RXParser
rxLegal = noneOf rxSpecialChars >>= return . RXToken

rxSpecial :: RXParser
rxSpecial = char '\\' >> oneOf rxSpecialChars >>= return . RXToken


{- set of chars -}

rxSet :: RXParser
rxSet = do char '['
           --  negation <- optionMaybe $ char '^' -- TODO prendre en compte la negation
           content <- rxSetContent
           char ']'
           return content
 where
    rxCreateSet :: Parser Char -> RXParser
    rxCreateSet p = do start <- p
                       char '-'
                       end <- p
                       return $ RXChoice $ fmap RXToken [start..end]

    rxSetContent :: RXParser
    rxSetContent = try rxDigitSet
                   <|> try rxUpperSet
                   <|> try rxLowerSet
                   <|>  rxCharsSet

    rxUpperSet :: RXParser
    rxUpperSet = withClosingBracket $ rxCreateSet upper

    rxLowerSet :: RXParser
    rxLowerSet = withClosingBracket $ rxCreateSet lower

    rxDigitSet :: RXParser
    rxDigitSet = withClosingBracket $ rxCreateSet digit

    rxCharsSet :: RXParser
    rxCharsSet = do head <- liftM maybeToList $ optionMaybe (char ']')
                    set <- many1 noClosingBracket
                    return $ RXChoice [ RXToken x | x <- head ++ set ]

    noClosingBracket = noneOf "]"
    closingBracket = char ']'

    withClosingBracket p = p `followedBy` closingBracket

    followedBy p o = do content <- p
                        lookAhead o
                        return content


{-
     #######
     #     #  #####
     #     #  #    #
     #     #  #    #
     #     #  #####   ###
     #     #  #       ###
     #######  #       ###
-}
rxChoiceOp :: RXParser
rxChoiceOp = spaces >> char '|' >>= return . RXToken

rxKleeneOp :: RXParser
rxKleeneOp = spaces >> char '*' >>= return . RXToken

rxPositiveOp :: RXParser
rxPositiveOp = spaces >> char '+' >>= return . RXToken

rxOptionOp :: RXParser
rxOptionOp = spaces >> char '?' >>= return . RXToken

{- ---- -}
rxChoice :: RXParser
rxChoice = try $ do spaces
                    rxl <- rxSeq
                    spaces
                    rxr <- chainl (rxChoiceOp >> rxSeq) (return (+++)) rxl 
                    if (rxl == rxr)
                    then return rxl
                    else return $ rxl +++ rxr
   where
     RXChoice rxa +++ RXChoice rxb = RXChoice $ rxa ++ rxb
     RXChoice rxa +++ rx = RXChoice $ rx : rxa
     rx +++ RXChoice rxb = RXChoice $ rx : rxb
     rxl +++ rxr = RXChoice $ [rxl,rxr]

rxSeq :: RXParser
rxSeq = try $ spaces >> rxAtom `chainl1` return mappend

rxAtom :: RXParser
rxAtom = try $ spaces >> (between (char '(') (char ')') rx <|> rxSet <|> rxLegal <|> rxPoint <|> rxSpecial)

rx :: RXParser
rx = rxChoice