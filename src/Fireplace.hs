{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fireplace
  ( v1Run,
  )
where

import           Data.Typeable
import           Options.Applicative

data Part = One | Two
  deriving (Eq, Ord, Show, Read, Bounded)

parsePart :: ReadM Part
parsePart = eitherReader $ \case
  "1" -> Right One
  "2" -> Right Two
  _ -> Left "Part must be 1 or 2"

data Arguments = Arguments
  { part :: Part,
    args :: [String]
  }

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
              ( strArgument (metavar "ARGS")
              )
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
  (Show ret, Typeable ret) =>
  (String -> [String] -> IO ret) ->
  (String -> [String] -> IO ret) ->
  IO ()
v1Run solvePt1 solvePt2 = do
  Arguments {part, args} <- execParser opts
  input <- getContents
  let solver = case part of
        One -> solvePt1
        Two -> solvePt2
  result <- solver input args
  maybe (print result) putStrLn (cast result)
