module Main where

import Prelude
import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, defaultSpawnOptions, onError, onExit, spawn)
import Node.FS (FS)
import Node.FS.Stats (isDirectory, isFile)
import Node.FS.Sync (readdir, rmdir, stat, unlink)
import Node.Path (FilePath, concat)
import Node.Process (PROCESS, argv)

data Repository = Repository String String

instance showRepository :: Show Repository where
  show (Repository user repo) = "https://github.com/" <> user <> "/" <> repo <> ".git"

data Command = Command
  { repo :: Repository
  , to :: String
  }

command :: Repository -> String -> Command
command repo to = Command { repo, to }

clone :: forall eff. Repository -> String -> Eff (cp :: CHILD_PROCESS | eff) ChildProcess
clone repo to =
  spawn "git" ["clone", show repo, to] defaultSpawnOptions

rmdir' :: forall eff. FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
rmdir' path = do
  xs <- readdir path
  _ <- foreachE ((map \x -> concat [path, x]) xs) remove
  rmdir path

  where
    remove :: FilePath -> Eff (fs :: FS, err :: EXCEPTION | eff) Unit
    remove p = do
      s <- stat p
      case s of
        _
          | isDirectory s -> rmdir' p
          | isFile s -> unlink p
          | otherwise -> pure unit

parseRepo :: String -> Maybe Repository
parseRepo input =
  case tokens of
    [user, repo] -> Just (Repository user repo)
    _ -> Nothing

  where
    tokens :: Array String
    tokens = split (Pattern "/") input

parseCommand :: Array String -> Maybe Command
parseCommand [_, _, repoStr, to] = command <$> parseRepo repoStr <*> Just to
parseCommand _ = Nothing

main :: Eff (process :: PROCESS, fs :: FS, err :: EXCEPTION, cp :: CHILD_PROCESS, console :: CONSOLE) Unit
main = do
  args <- argv
  case parseCommand args of
    Just (Command c) -> do
      cp <- clone c.repo c.to
      waitAndLog cp c.repo c.to
    Nothing -> log "Invalid command"

  where
    waitAndLog :: forall eff
      . ChildProcess
      -> Repository
      -> String
      -> Eff (console :: CONSOLE, fs :: FS, err :: EXCEPTION | eff) Unit
    waitAndLog cp repo to = do
      _ <- onExit cp \e -> do
        _ <- rmdir' $ concat [to, ".git"]
        log $ "Cloned " <> show repo <> " into " <> to
      _ <- onError cp \err -> log $ "Failed to clone " <> show repo
      pure unit
