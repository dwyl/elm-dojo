module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Char
import Random
import Kitten


tests : Test
tests =
    describe "Kitten"
        [ test "stating something" <|
            \() ->
                Expect.equal "Meow"
                    (Kitten.hey "Tom-ay-to, tom-aaaah-to.")
        , test "shouting" <|
            \() ->
                Expect.equal
                    "Leave me alone"
                    (Kitten.hey "WATCH OUT!")
        , test "shouting gibberish" <|
            \() ->
                Expect.equal
                    "Leave me alone"
                    (Kitten.hey (uppercaseGibberish 10))
        , test "asking a question" <|
            \() ->
                Expect.equal
                    "Yes please"
                    (Kitten.hey "Does this cryogenic chamber make me look fat?")
        , test "asking a numeric question" <|
            \() ->
                Expect.equal
                    "Yes please"
                    (Kitten.hey "You are, what, like 15?")
        , test "asking gibberish" <|
            \() ->
                Expect.equal
                    "Yes please"
                    (Kitten.hey (gibberishQuestion 20))
        , test "talking forcefully" <|
            \() ->
                Expect.equal
                    "Meow"
                    (Kitten.hey "Let's go make out behind the gym!")
        , test "using acronyms in regular speech" <|
            \() ->
                Expect.equal
                    "Meow"
                    (Kitten.hey "It's OK if you don't want to go to the DMV.")
        , test "forceful questions" <|
            \() ->
                Expect.equal
                    "Leave me alone"
                    (Kitten.hey "WHAT THE HELL WERE YOU THINKING?")
        , test "shouting numbers" <|
            \() ->
                Expect.equal
                    "Leave me alone"
                    (Kitten.hey "1, 2, 3 GO!")
        , test "only numbers" <|
            \() ->
                Expect.equal
                    "Meow"
                    (Kitten.hey "1, 2, 3")
        , test "question with only numbers" <|
            \() ->
                Expect.equal
                    "Yes please"
                    (Kitten.hey "4?")
        , test "shouting with special characters" <|
            \() ->
                Expect.equal
                    "Leave me alone"
                    (Kitten.hey "ZOMG THE %^*@#$(*^ ZOMBIES ARE COMING!!11!!1!")
        , test "shouting with no exclamation mark" <|
            \() ->
                Expect.equal
                    "Leave me alone"
                    (Kitten.hey "I HATE YOU")
        , test "statement containing a question mark" <|
            \() ->
                Expect.equal
                    "Meow"
                    (Kitten.hey "Ending with ? means a question.")
        , test "prattling on" <|
            \() ->
                Expect.equal
                    "Yes please"
                    (Kitten.hey "Wait! Hang on. Are you going to be OK?")
        , test "silence" <|
            \() ->
                Expect.equal
                    "Cat got your tongue"
                    (Kitten.hey "")
        , test "prolonged silence" <|
            \() ->
                Expect.equal
                    "Cat got your tongue"
                    (Kitten.hey "       ")
        , test "alternate silences" <|
            \() ->
                Expect.equal
                    "Cat got your tongue"
                    (Kitten.hey "\t  \n  \t   ")
        , test "on multiple line questions" <|
            \() ->
                Expect.equal
                    "Meow"
                    (Kitten.hey "\nDoes this cryogenic chamber make me look fat?\nno")
        ]


character : Int -> Int -> Random.Generator Char
character start end =
    Random.map Char.fromCode (Random.int start end)


anyCharacter : Random.Generator Char
anyCharacter =
    character 32 126


uppercaseCharacter : Random.Generator Char
uppercaseCharacter =
    character 65 90


listOfCharacters : Int -> Random.Generator Char -> Random.Generator (List Char)
listOfCharacters length characterList =
    Random.list length characterList


gibberish : Int -> Random.Generator Char -> String
gibberish length characterList =
    Tuple.first (Random.step (Random.map String.fromList (listOfCharacters length characterList)) (Random.initialSeed 424242))


uppercaseGibberish : Int -> String
uppercaseGibberish length =
    gibberish length uppercaseCharacter


gibberishQuestion : Int -> String
gibberishQuestion length =
    (gibberish length anyCharacter) ++ "?"
