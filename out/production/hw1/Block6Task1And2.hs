{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}

module Block6Task1And2 where

import GHC.Base
import Data.Char (isDigit, digitToInt)

newtype Parser s a =
  Parser
    { runParser :: [s] -> Maybe (a, [s])
    }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser exec) = Parser $ \xs -> fmap (first f) (exec xs)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser pf <*> Parser pa =
    Parser $ \s ->
      case pf s of
        Nothing -> Nothing
        Just (f, t) ->
          case pa t of
            Nothing     -> Nothing
            Just (a, r) -> Just (f a, r)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser exec >>= f =
    Parser $ \input ->
      case exec input of
        Nothing      -> Nothing
        Just (x, l') -> runParser (f x) l'


instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser exec1 <|> Parser exec2 = Parser $ \input -> case exec1 input of
                                            Nothing -> exec2 input
                                            result  -> result


-- always succeeds without consuming any input
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)


-- succeeds only at the end of input stream
eof :: Parser s ()
eof =
  Parser $ \s -> case s of
    [] -> Just ((), s)
    _ -> Nothing

-- consumes only single character and returns it if predicate is true
satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \case
    [] -> Nothing
    (x:xs) ->
      if p x
        then Just (x, xs)
        else Nothing


element :: (Eq s) => s -> Parser s s
element e = satisfy (== e)

stream :: (Eq s) => [s] -> Parser s [s]
stream = traverse element



parseBrackets :: Parser Char ()
parseBrackets = brackets *> eof
  where
    brackets = brackets *> (element '(' *> brackets *> element ')' *> brackets) <|> ok


data Sign
  = Plus
  | Minus

getSign :: Parser Char Sign
getSign = Minus <$ element '-' <|> Plus <$ element '+'

parseNumbers :: Parser Char Int
parseNumbers = symb <*> (foldl (\f a -> f * 10 + a) 0 <$> some toNumber)
  where
    symb = id <$ element '+' <|> negate <$ element '-' <|> id <$ ok
toNumber :: Parser Char Int
toNumber = fmap digitToInt (satisfy isDigit)
