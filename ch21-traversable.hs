module Ch21Traversable where

import Control.Monad
import Data.Maybe

stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse chatToMorse

type Morse = String

chatToMorse :: Char -> Maybe Morse
chatToMorse c =
  case c of
    'a' -> Just ".-"
    'e' -> Just "."
    'i' -> Just ".."
    'o' -> Just "._."
    'u' -> Just "..-"
    _ -> Nothing

morseToChar :: Morse -> Maybe Char
morseToChar m =
  case m of
    ".-" -> Just 'a'
    "." -> Just 'e'
    ".." -> Just 'i'
    "._." -> Just 'o'
    "..-" -> Just 'u'
    _ -> Nothing

morse s = fromMaybe [] (stringToMorse s)

--

data Query = Query

data SomeObj = SomeObj

data IoOnlyObj = IoOnlyObj

data Err = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj ::
  [SomeObj] ->
  IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn ::
  Query ->
  IO (Either Err [(SomeObj, IoOnlyObj)])
-- pointfree with kleisli fish monadic composition:
pipelineFn = (traverse makeIoOnlyObj . traverse decodeFn) <=< fetchFn

-- is the same as (without the fishing):
--pipelineFn = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

-- Just to see if I could implement traverse for List by myself: check!
trav :: Applicative f => (a -> f b) -> [a] -> f [b]
trav _ [] = pure []
trav f (x : xs) = (:) <$> f x <*> trav f xs
