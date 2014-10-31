module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (natural)
--import Text.ParserCombinators.Parsec.Token hiding (natural)

import AST


--main = do
--    let input = "A"

--parseKey :: String -> Either ParseError Key


musicLiteralParser :: Parser MusicLiteral
musicLiteralParser =
        try noteLiteralParser
    <|> try restParser
    <|> try chordParser

    where
        noteLiteralParser :: Parser MusicLiteral
        noteLiteralParser = do
            n <- noteParser
            return (NoteLiteral n)

        restParser :: Parser MusicLiteral
        restParser = do
            char '('
            spaces
            char 'R'
            spaces
            char ','
            spaces
            d <- natural
            spaces
            char ')'
            return (Rest d)

        chordParser :: Parser MusicLiteral
        chordParser = do
            char '['
            notes <- sepBy1 noteParser (char ',')
            char ']'
            return (Chord notes)

noteParser :: Parser Note
noteParser = do
    char '('
    spaces
    t <- toneParser
    spaces
    char ','
    spaces
    d <- natural
    spaces
    char ')'
    return (Note t d)

toneParser :: Parser Tone
toneParser = do
    key <- keyParser
    octave <- natural
    return (Tone {octave=octave, key=key})

keyParser :: Parser Key
keyParser =
        try parseCompound
    <|> baseKeyParser
    where
        parseCompound = do
            k <- baseKeyParser
            mods <- many1 modifierParser
            return (Compound k mods)

        baseKeyParser =
                (char 'A' >> return A)
            <|> (char 'B' >> return B)
            <|> (char 'C' >> return C)
            <|> (char 'D' >> return D)
            <|> (char 'E' >> return E)
            <|> (char 'F' >> return F)
            <|> (char 'G' >> return G)

modifierParser :: Parser Modifier
modifierParser =
        (char '#' >> return Sharp)
    <|> (char 'b' >> return Flat)

natural :: Parser Integer
natural = do
    ds <- many1 digit
    return (read ds)
