module Test.Main where

import Prelude(class Eq, class Show, Unit, discard, pure, show, unit, ($), (/=), (<>))
import Effect (Effect)
import Data.Default
import Effect.Exception.Unsafe(unsafeThrow)
import Data.Either(Either(..))
import Data.Tuple(Tuple(..))
import Data.Maybe(Maybe(..))
import Data.Generic.Rep
import Data.Eq.Generic(genericEq)
import Data.Show.Generic(genericShow)

data Color = RED | YELLOW | GREEN
instance defaultColorInstance :: DefaultValue Color where
  defaultValue = GREEN

derive instance generiColor :: Generic Color _ 
instance showColor :: Show Color where show = genericShow
instance eqColor :: Eq Color where eq = genericEq
main :: Effect Unit
main = do
  let defaultPerson = defaultValue :: {name :: String, age :: Int, height :: Number}
      expectedPerson = {name : "", age: 0, height: 0.0}

  check defaultPerson expectedPerson
  
  let eitherDefaultValue = defaultValue :: Either String Int 
      expectedEither = Left "" ::  Either String Int 

  check eitherDefaultValue expectedEither

  let tupleDefaultValue = defaultValue :: Tuple (Maybe Int) (Array Number) 
      expectedTuple = Tuple Nothing []
  
  check tupleDefaultValue expectedTuple


  let defaultColor = defaultValue :: Color 
      expectedColor = GREEN
  
  check defaultColor expectedColor
  where check :: forall a.Eq a => Show a => a -> a -> Effect Unit 
        check actual expected = 
          if (actual /= expected) 
                then unsafeThrow $ "Expected: " <> (show expected) <> "but actual: " <> (show actual) 
                else pure unit