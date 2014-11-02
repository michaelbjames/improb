module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token hiding (natural, identifier)

import AST

parseProgram input = Text.Parsec.parse programParser "" input

programParser :: Parser Program
programParser = do
    tempo <- withSpacing tempoParser
    many newline
    aliases <- many (try (withSpacing aliasParser))   
    many newline
    voices <- many1 (withSpacing voiceParser)
    return (Program tempo aliases voices)
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
    many newline
    transitions <- mySepBy1 transitionParser newline
    return (Voice instrument transitions)
    where
        mySepBy1 p sep = many1 $ do
            x <- p
            many sep
            return x

transitionParser :: Parser Transition
transitionParser =
        try intro
    <|> transition
    where
        transitionError = "Transition Introduction (=>)"
        intro :: Parser Transition
        intro = do
            string "=>" <?> transitionError
            spaces
            mp <- musicPatternParser
            return (Intro mp)

        transition :: Parser Transition
        transition = do
            start <- musicPatternParser
            spaces
            string "=>" <?> transitionError
            spaces
            end <- musicPatternParser
            many newline
            return (Transition start end)

musicPatternParser :: Parser MusicPattern
musicPatternParser = chainr1 endMusicNode (try continuation)
-- thanks http://stuckinaninfiniteloop.blogspot.com/2011/10/left-recursion-in-parsec.html
    where
        endMusicNode :: Parser MusicPattern
        endMusicNode = try single <|> try alias

        single :: Parser MusicPattern
        single = do
            ml <- musicLiteralParser
            return (Single ml)

        alias :: Parser MusicPattern
        alias = do
            name <- many1 alphaNum
            return (Lookup name)

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
    char ',' <?> "Missing octave, modifier, or note."
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
