{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module OT.Operations where 

import Data.Functor
import Control.Applicative
import Data.Aeson
import Data.Text

type Property = Text
type Position = Int
data PathSegment = PropSeg Property | PosSeg Position
                 deriving Show
type Path = [PathSegment]

data JSONNumberOperation = Add Path Int
                         deriving Show
data JSONStringOperations = StringInsert Path Position Text
                          | StringDelete Path Position Text
                          deriving Show
data JSONArrayOperation = ArrayInsert  Path Position Value
                        | ArrayDelete  Path Position Value
                        | ArrayReplace Path Position Value Value
                        | ArrayMove    Path Position Int
                        deriving Show
data JSONObjectOperation = Insert  Path Property Value
                         | Delete  Path Property 
                         | Replace Path Property Value Value
                         deriving Show

instance FromJSON PathSegment where
  parseJSON (String a) = PropSeg <$> parseJSON (String a)
  parseJSON (Number a) = PosSeg  <$> parseJSON (Number a)
  parseJSON _ = fail "Invalid Pathsegment"

instance FromJSON JSONNumberOperation where
  parseJSON (Object v) = Add <$> (v .: "p") <*> (v .: "ni")
  parseJSON _          = fail "Not an Object"

{-opMap (Add path a) = Data.Map.fromList [("p", path), ("na", a)]-}
{-opMap (StringInsert path pos str) = Data.Map.fromList [("si", )]-}


{-renderOperation (Add path num) = "{ p:"++(show path)++", na:"++(show num)++"}"-}
