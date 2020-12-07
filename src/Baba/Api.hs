{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Baba.Api (
  Api(..),
  GameId,
  Move(..),
) where


import Data.Text
import Baba.Game (Board)
import Servant.API ((:>), Capture, Get, Post, ReqBody)
import Servant.API.Generic (GenericMode((:-)))


{- | The HTTP api spec. -}
data Api route = Api {
    getGameState :: route
      :- Capture "game-id" GameId
      :> Get '[Board] Board,
    move :: route
      :- Capture "game-id" GameId
      :> ReqBody '[Move] Move
      :> Post '[Board] Board
  }


newtype GameId = GameId {
    unGameId :: Text
  }
  deriving newtype (Eq, Ord)


data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight


