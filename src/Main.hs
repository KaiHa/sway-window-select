{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (join, unless, when)
import Control.Monad.Extra (unlessM)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Fmt
import Options.Applicative
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as P
import Text.Pretty.Simple


main :: IO ()
main = do
  cfg <- execParser opts
  ws <- getWindows cfg
  case direction cfg of
    Nothing -> selectWindow cfg ws >>= raiseWindow
    Just dir -> moveFocus dir $ leaves $ Map.elems ws
 where
  opts =
    info
      (config <**> helper)
      ( fullDesc
          <> progDesc "Change focus to a window"
          <> header "sway-window-select - a window selector for swaywm"
      )


getWindows :: Cfg -> IO (Map ConId Con)
getWindows cfg = do
  tree <- P.readProcessStdout_ (P.proc "swaymsg" ["-t", "get_tree"])
  when (verbose cfg) $ putStrLn "//// TREE IN RAW JSON: ////" >> C.putStrLn tree
  case eitherDecode tree :: Either String Root of
    Left e -> error e
    Right root -> do
      when (verbose cfg) $ putStrLn "//// PARSED TREE: ////" >> pPrint root
      pure $ Map.fromList $ root2cons root
 where
  root2cons :: Root -> [(ConId, Con)]
  root2cons =
    map (\a@Con{con_id = cid} -> (cid, a)) . concatMap cons . concatMap workspaces . outputs


moveFocus :: Direction -> [Con'] -> IO ()
moveFocus dir cs = do
  handled <- case find (focused . unCon') cs of
    Just (Con' w)
      | app w == "Emacs" ->
        (== ExitSuccess) <$> P.runProcess (P.proc "emacsclient" ["-e", "(windmove-" <> dir' <> ")"])
    _ -> pure False
  unless handled $ P.runProcess_ $ P.proc "swaymsg" ["focus", dir']
 where
  dir' = case dir of
    North -> "up"
    South -> "down"
    East -> "right"
    West -> "left"


selectWindow :: Cfg -> Map ConId Con -> IO ConId
selectWindow cfg ws =
  when (verbose cfg) (putStrLn "//// BEMENU ARGS: ////" >> print (bemenuArgs cfg))
    >> read'
      <$> P.readProcessStdout_
        ( P.setStdin (P.byteStringInput $ C.unlines $ map show' $ leaves $ Map.elems ws) $
            P.proc "bemenu" (bemenuArgs cfg)
        )
 where
  show' :: Con' -> C.ByteString
  show' (Con' (Con (ConId c) n _ a p _ _)) =
    padLeftF 4 ' ' c |+ ": " +| padLeftF 20 ' ' a |+ " -- " +| n |+ " (" +|| p ||+ ")"
  read' = ConId . read . C.unpack . C.takeWhile (/= ':')


leaves :: [Con] -> [Con']
leaves = join . map leaves'
 where
  leaves' c = case con_cons c of
    [] -> [Con' c]
    cs -> leaves cs


raiseWindow :: ConId -> IO ()
raiseWindow (ConId cid) =
  P.runProcess_ (P.proc "swaymsg" ["[con_id=" ++ show cid ++ "] focus"])


config :: Parser Cfg
config =
  Cfg
    <$> option
      auto
      ( long "bemenu-args"
          <> short 'b'
          <> metavar "ARGS"
          <> help "Arguments for bemenu as a List of Strings (Haskell notation)"
          <> value
            [ "--ignorecase"
            , "--prompt=Raise:"
            , "--list=12"
            , "--prefix=   -> "
            , "--scrollbar=autohide"
            , "--fn=FiraCode 9"
            , "--tf=#cccccc"
            , "--ff=#ffffff"
            , "--hf=#ffffff"
            , "--hb=#44aa77"
            , "--scf=#cccccc"
            ]
          <> showDefault
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )
    <*> optional
      ( option
          auto
          ( long "direction"
              <> short 'd'
              <> help "Move focus in the given direction (North, South, ...)"
          )
      )


data Cfg = Cfg
  { bemenuArgs :: [String]
  , verbose :: Bool
  , direction :: Maybe Direction
  }


data Direction = North | South | East | West deriving (Eq, Ord, Show, Read)


data Protocol = X | Wayland deriving (Eq, Ord, Show, Read)


newtype Root = Root {outputs :: [Output]} deriving (Eq, Show)


data Output = Output
  { output_name :: String
  , workspaces :: [Workspace]
  }
  deriving (Eq, Show)


data Workspace = Workspace
  { workspace_name :: String
  , cons :: [Con]
  }
  deriving (Eq, Show)


data Con = Con
  { con_id :: ConId
  , con_name :: Maybe String
  , con_type :: String
  , app :: String
  , protocol' :: Protocol
  , focused :: Bool
  , con_cons :: [Con]
  }
  deriving (Eq, Show)


-- Like Con but without nested Cons (used by the leaves function)
newtype Con' = Con' {unCon' :: Con}


newtype WClass = WClass {class_name :: String} deriving (Eq, Show)


newtype ConId = ConId Int deriving (Eq, Ord, Read, Show)


instance FromJSON Root where
  parseJSON = withObject "Root" $ \obj ->
    unlessM ((("root" :: String) ==) <$> obj .: "type") (fail "Object is not of type 'root'")
      >> Root <$> obj .: "nodes"


instance FromJSON Output where
  parseJSON = withObject "Output" $ \obj ->
    unlessM ((("output" :: String) ==) <$> obj .: "type") (fail "Object is not of type 'output'")
      >> Output
        <$> obj .: "name"
        <*> obj .: "nodes"


instance FromJSON Workspace where
  parseJSON = withObject "Workspace" $ \obj -> do
    unlessM ((("workspace" :: String) ==) <$> obj .: "type") (fail "Object is not of type 'workspace'")
    n <- obj .:? "nodes"
    fn <- obj .:? "floating_nodes"
    Workspace
      <$> obj .: "name"
      <*> pure (concat $ maybeToList n ++ maybeToList fn)


instance FromJSON Con where
  parseJSON = withObject "Con" $ \obj -> do
    unlessM (p <$> obj .: "type") (fail "Object is not of type 'con' or 'floating_con'")
    app_id <- join <$> obj .:? "app_id"
    wclass <- obj .:? "window_properties"
    Con
      <$> (ConId <$> obj .: "id")
      <*> obj .: "name"
      <*> obj .: "type"
      <*> pure (fromMaybe (maybe "N/A" class_name wclass) app_id)
      <*> pure (if isJust app_id then Wayland else X)
      <*> obj .: "focused"
      <*> obj .: "nodes"
   where
    p :: String -> Bool
    p a = a == "con" || a == "floating_con"


instance FromJSON WClass where
  parseJSON = withObject "WClass" $ \obj ->
    WClass <$> obj .: "class"
