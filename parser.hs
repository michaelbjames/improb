module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (natural)
--import Text.ParserCombinators.Parsec.Token hiding (natural)

import AST


--main = do
--    let input = "A"

--parseKey :: String -> Either ParseError Key

musicPatternParser :: Parser MusicPattern
musicPatternParser = chainr1 singleLiteral continuation
-- thanks http://stuckinaninfiniteloop.blogspot.com/2011/10/left-recursion-in-parsec.html
    where
        singleLiteral :: Parser MusicPattern
        singleLiteral = do
            ml <- musicLiteralParser
            return (Single ml)

        continuation :: Parser (MusicPattern -> MusicPattern -> MusicPattern)
        continuation = do
            spaces
            string "->"
            spaces
            return Continuation

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
