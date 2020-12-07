{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Baba.Game (
  move,
  Board,
  X,
  Y,
  Tile,
  findYou,
  play,
) where


import Baba.Enum (GenericPred, GenericSucc, enumeration, predMay, succMay)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set, partition)
import GHC.Generics (Generic(Rep))
import qualified Data.Map as Map
import qualified Data.Set as Set


type Board = (X, Y) -> Set Tile
instance Show Board where
  show = printBoard

data X = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9
  deriving stock (Eq, Ord, Generic, Show)

data Y = Y0 | Y1 | Y2 | Y3 | Y4 | Y5 | Y6 | Y7 | Y8 | Y9
  deriving stock (Eq, Ord, Generic, Show)


move :: (X, Y) -> (X, Y) -> Board -> Board
move from to b
    | Set.null wantToMove =
        {-
          There is nothing we want to move, so the "move" is a
          no-op. Return the original board.
        -}
        b
    | Set.null inTheWay =
        \coord ->
          if coord == to then
            wantToMove <> nonBlocking
          else if coord == from then
            notMoving
          else
            consequences coord
    | otherwise =
        {-
          We are still blocked, so we can't make the move. Return
          the original board.
        -}
        b
  where
    wantToMove :: Set Tile
    notMoving :: Set Tile
    inTheWay :: Set Tile
    nonBlocking :: Set Tile
    (wantToMove, notMoving) = partition isBlock (b from)
    (inTheWay, nonBlocking) = partition isBlock (consequences to)

    consequences :: Board
    consequences =
      if Set.null wantToMove then b
      else
        case next of
          Nothing -> b
          Just coord -> move to coord b

    next :: Maybe (X, Y)
    next =
      let ((fx, fy), (tx, ty)) = (from, to)
      in
        do
          x <- n fx tx
          y <- n fy ty
          pure (x, y)

      where
        n ::
             ( Ord a
             , Generic a
             , GenericPred (Rep a)
             , GenericSucc (Rep a)
             )
          => a
          -> a
          -> Maybe a
        n f t =
          case compare f t of
            GT -> predMay t
            EQ -> Just t
            LT -> succMay t

    isBlock :: Tile -> Bool
    isBlock = const True

data Tile
  = Baba
  | Block
  deriving (Eq, Ord, Show)


findYou :: Board -> Maybe (X, Y)
findYou b =
    case
      take 1 [
        coord
        | coord <- enumeration
        , any isYou (b coord)
      ]
    of
      [] -> Nothing
      a:_ -> Just a
  where
    isYou :: Tile -> Bool
    isYou = \case
      Baba -> True
      Block -> False

data Go
  = GoUp
  | GoDown
  | GoLeft
  | GoRight

go :: Go ->  Board -> Board
go dir board =
  let
    theMove :: Maybe ((X, Y), (X, Y))
    theMove = do
        you@(x, y) <- findYou board
        next <-
          case dir of
            GoUp    -> (x,) <$> succMay y
            GoDown  -> (x,) <$> predMay y
            GoLeft  -> (,y) <$> predMay x
            GoRight -> (,y) <$> succMay x
        pure (you, next)
  in
    case theMove of
      Nothing -> board
      Just (from, to) -> move from to board


printBoard :: Board -> String
printBoard board =
    fold [
      showRow y
      | y <- reverse enumeration
    ]
  where
    showRow :: Y -> String
    showRow y =
      (
        fold [
          showTile (x, y)
          | x <- enumeration
        ]
      )
      <> "\n"

    showTile :: (X, Y) -> String
    showTile coord = 
      let
        onTile :: Tile -> Bool
        onTile tile = tile `Set.member` board coord
      in
        if onTile Block then
          " █ "
        else if onTile Baba then
          " Φ "
        else
          " ∙ "


play :: Board -> IO Board
play board = do
  putStr $ "\n" <> printBoard board
  theMove <-
    getChar <&> \case
      'l' -> Just GoRight
      'h' -> Just GoLeft
      'j' -> Just GoDown
      'k' -> Just GoUp
      _ -> Nothing
  play $
    case theMove of
      Nothing -> board
      Just dir -> normalize (go dir board)


toGrid :: Board -> Map (X, Y) (Set Tile)
toGrid b =
  Map.fromList [
    (coord, b coord)
    | coord <- enumeration
  ]

fromGrid :: Map (X, Y) (Set Tile) -> Board
fromGrid m coord = fromMaybe mempty (Map.lookup coord m)
  
normalize :: Board -> Board
normalize = fromGrid . toGrid
  
