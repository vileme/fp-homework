{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Block6Task1And2
  ( Parser(..)
  , element
  , eof
  , ok
  , parseBrackets
  , satisfy
  , stream
  ) where

import GHC.Base

-- | Data class which illustrates parser
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
            Nothing -> Nothing
            Just (a, r) -> Just (f a, r)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser exec >>= f =
    Parser $ \input ->
      case exec input of
        Nothing -> Nothing
        Just (x, l') -> runParser (f x) l'

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser exec1 <|> Parser exec2 =
    Parser $ \input ->
      case exec1 input of
        Nothing -> exec2 input
        result -> result

-- | Always succeeds without consuming any input
ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

-- | Succeeds only at the end of input stream
eof :: Parser s ()
eof =
  Parser $ \s ->
    case s of
      [] -> Just ((), s)
      _ -> Nothing

-- | Consumes only single character and returns it if predicate is true
satisfy :: (s -> Bool) -> Parser s s
satisfy p =
  Parser $ \case
    [] -> Nothing
    (x:xs) ->
      if p x
        then Just (x, xs)
        else Nothing

-- | Parses one char and consumes it
element :: (Eq s) => s -> Parser s s
element e = satisfy (== e)
-- | Parses the string and consumes it
stream :: (Eq s) => [s] -> Parser s [s]
stream = traverse element

-- | Parses the correct brackets sequence
parseBrackets :: Parser Char ()
parseBrackets = brackets *> eof
  where
    brackets = (element '(' *> brackets *> element ')' *> brackets) <|> ok