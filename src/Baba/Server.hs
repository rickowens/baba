{-# LANGUAGE FlexibleContexts #-}

module Baba.Server (
  server,
) where

import Baba.Api
import Baba.Enum
import Baba.Game hiding (move)
import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Map
import Data.Maybe
import Data.Text
import Servant.Server
import Servant.Server.Generic
import qualified Baba.Game as Game
import qualified Data.Map as Map


server :: TVar Games -> Api AsServer
server games =
    Api {
      getGameState = getGameStateImpl,
      move = moveImpl
    }
  where
    getGameStateImpl ::
         ( MonadIO m
         , MonadError ServerError m
         )
      => GameId
      -> m Board
    getGameStateImpl gid =
      withGame gid games id

    moveImpl ::
         ( MonadIO m
         , MonadError ServerError m
         )
      => GameId
      -> Move
      -> m Board
    moveImpl gid move =
        withGame gid games (\board ->
          case findYou board of
            Nothing -> board
            Just you ->
              let
                to :: (X, Y)
                to =
                  fromMaybe
                    you
                    (
                      case move of
                        MoveUp    -> m Just succMay you
                        MoveDown  -> m Just predMay you
                        MoveLeft  -> m predMay Just you
                        MoveRight -> m succMay Just you
                    )
              in
                Game.move you to board
        )
      where
        m :: (X -> Maybe X) -> (Y -> Maybe Y) -> (X, Y) -> Maybe (X, Y)
        m nextX nextY (x, y) =
          (,)
            <$> nextX x
            <*> nextY y

newtype Games = Games {
    unGames :: Map GameId Board
  }


withGame :: (MonadIO m) => GameId -> TVar Games -> (Board -> Board) -> m Board
withGame gameId games f = liftIO . atomically $ do
  gameMap <- unGames <$> readTVar games
  let
    board :: Board
    board =
      f $
        case Map.lookup gameId gameMap of
          Nothing -> pure undefined
          Just board -> board
  writeTVar games (Games (Map.insert gameId board gameMap))
  pure board


