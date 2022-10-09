module Data.Default
 ( class DefaultValue
 , defaultValue
 , class DefaultValueRecord
 , defaultRecord
 )
 where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record.Unsafe (unsafeSet)
import Type.Proxy (Proxy(..))

class DefaultValue a where
   defaultValue :: a 

instance defaultUnit :: DefaultValue Unit where
   defaultValue = unit 
   
instance defaultInt :: DefaultValue Int where
   defaultValue = 0 

instance defaultString :: DefaultValue String where
   defaultValue = ""

instance defaultNumber :: DefaultValue Number where
   defaultValue = 0.0

instance defaultBoolean :: DefaultValue Boolean where
   defaultValue = false

instance defaultMaybe :: DefaultValue (Maybe a) where
   defaultValue = Nothing
   
instance defaultEitherLeft :: (DefaultValue e) => DefaultValue (Either e a) where
   defaultValue = Left defaultValue

instance defaultArray :: DefaultValue (Array a) where 
   defaultValue = []

instance defaultTuple :: (DefaultValue a, DefaultValue b) => DefaultValue (Tuple a b) where 
   defaultValue = Tuple defaultValue defaultValue
class DefaultValueRecord :: RL.RowList Type -> Row Type -> Constraint
class DefaultValueRecord rowList row| rowList -> row where 
    defaultRecord :: Proxy rowList -> Record row 

instance  defaultValueRecordNil :: DefaultValueRecord RL.Nil () where 
    defaultRecord _  = {}
    
instance  defaultValueRecordCons :: 
    (IsSymbol key
    , DefaultValue focus
    , Row.Cons key focus rowTail row
    , DefaultValueRecord rowListTail rowTail
    ) => DefaultValueRecord (RL.Cons key focus rowListTail) row where 
    defaultRecord _ = insert defaultValue tail 
          where
            key = reflectSymbol (Proxy :: Proxy key)
            insert = unsafeSet key :: focus -> Record rowTail -> Record row
            tail = defaultRecord (Proxy :: Proxy rowListTail)
    
instance defaultValueForRecord :: (RL.RowToList row list, DefaultValueRecord list row) => DefaultValue (Record row) where
      defaultValue = defaultRecord (Proxy :: Proxy list)
        
