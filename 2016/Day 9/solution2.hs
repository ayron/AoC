import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec

main = do
  input <- readFile "input.txt"
  case parse_str input of
    Left err -> print err
    Right n  -> print (n-1)

parse_str :: String -> Either ParseError Int
parse_str s = parse message "(unkwown)" s

message :: Parser Int
message = many segment >>= return . sum

segment :: Parser Int
segment = try marker <|> text

text :: Parser Int
text = many1 (noneOf "(") >>= return . length

marker :: Parser Int
marker = do
  char '('
  cs <- uint
  char 'x'
  n <- uint
  char ')'
  ss <- count cs anyChar
  case parse_str ss of
    Left err -> fail $ show err
    Right m  -> return (n*m)

uint :: Parser Int
uint = read <$> many1 digit
