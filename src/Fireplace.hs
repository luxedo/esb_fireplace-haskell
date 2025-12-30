{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fireplace (
  v1Run,
)
where

import           Data.Time.Clock
import           Data.Typeable
import           Options.Applicative

data Part = One | Two
  deriving (Eq, Ord, Show, Read, Bounded)

data Arguments = Arguments
  { part :: Part
  , args :: [String]
  }

parsePart :: ReadM Part
parsePart = eitherReader $ \case
  "1" -> Right One
  "2" -> Right Two
  _ -> Left "Part must be 1 or 2"

parser :: Parser Arguments
parser =
  Arguments
    <$> option
      parsePart
      ( long "part"
          <> short 'p'
          <> help "Run solution part 1 or part 2"
      )
    <*> ( flag'
            ()
            ( long "args"
                <> short 'a'
                <> help "Additional arguments for running the solutions"
            )
            *> many
              (strArgument (metavar "ARGS"))
              <|> pure []
        )

opts :: ParserInfo Arguments
opts =
  info
    (parser <**> helper)
    ( fullDesc
        <> progDesc "Elf Script Brigade Haskell solution runner"
    )

v1Run ::
  (Show r1, Typeable r1, Show r2, Typeable r2) =>
  (String -> [String] -> r1) ->
  (String -> [String] -> r2) ->
  IO ()
v1Run solvePt1 solvePt2 = do
  Arguments{part, args} <- execParser opts
  input <- getContents
  start <- getCurrentTime
  putStrLn $ case part of
    One -> renderAny $ solvePt1 input args
    Two -> renderAny $ solvePt2 input args
  end <- getCurrentTime
  let duration = diffUTCTime end start
      ns = round (realToFrac duration * 1e9 :: Double) :: Int
  putStrLn $ "RT " ++ show ns ++ " ns"
 where
  renderAny :: (Typeable r, Show r) => r -> String
  renderAny val = case cast val :: Maybe String of
    Just s  -> s
    Nothing -> show val
