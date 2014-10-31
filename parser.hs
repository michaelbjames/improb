module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (natural, identifier)
--import Text.ParserCombinators.Parsec.Token hiding (natural)

import AST

programParser :: Parser Program
programParser = do
    aliases <- many1 (try (withSpacing aliasParser))
    many newline
    tempo <- withSpacing tempoParser
    many newline
    voices <- many1 (withSpacing voiceParser)
    return (Program aliases tempo voices)
    where
        withSpacing p = do
            parsed <- p
            many newline
            return parsed

        tempoParser = do
            string "tempo"
            spaces
            char ':'
            spaces
            natural

aliasParser :: Parser Alias
aliasParser = do
    name <- many1 alphaNum
    spaces
    string ":="
    spaces
    pattern <- musicPatternParser
    return (Alias {identifier = name, pattern = pattern})

voiceParser :: Parser Voice
voiceParser = do
    instrument <- between (char ':') (char ':') (many1 alphaNum)
    transitions <- sepBy1 transitionParser newline
    return (Voice instrument transitions)

transitionParser :: Parser Transition
transitionParser =
        try intro
    <|> try transition
    where
        intro :: Parser Transition
        intro = do
            string "=>"
            spaces
            mp <- musicPatternParser
            return (Intro mp)

        transition :: Parser Transition
        transition = do
            start <- musicPatternParser
            spaces
            string "=>"
            spaces
            end <- musicPatternParser
            return (Transition start end)

musicPatternParser :: Parser MusicPattern
musicPatternParser = chainr1 singleLiteral (try continuation)
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
    return (Tone {octave = octave, key = key})

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
