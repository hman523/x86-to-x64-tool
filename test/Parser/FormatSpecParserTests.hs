module Parser.FormatSpecParserTests where

import Test.Hspec
import Parser.FormatSpecParser (parseLenMod)

formatSpecParserSpec :: Spec
formatSpecParserSpec = describe "FormatSpecParser" $ do

    describe "parseLenMod" $ do

        it "parses empty modifier (bare conversion)" $
            parseLenMod "d rest" `shouldBe` ("", "d rest")

        it "parses 'h' modifier" $
            parseLenMod "hd rest" `shouldBe` ("h", "d rest")

        it "parses 'hh' modifier (greedy)" $
            parseLenMod "hhd rest" `shouldBe` ("hh", "d rest")

        it "parses 'l' modifier" $
            parseLenMod "ld rest" `shouldBe` ("l", "d rest")

        it "parses 'll' modifier (greedy)" $
            parseLenMod "lld rest" `shouldBe` ("ll", "d rest")

        it "parses 'j' modifier" $
            parseLenMod "jd rest" `shouldBe` ("j", "d rest")

        it "parses 'z' modifier" $
            parseLenMod "zu rest" `shouldBe` ("z", "u rest")

        it "parses 't' modifier" $
            parseLenMod "td rest" `shouldBe` ("t", "d rest")

        it "parses 'L' modifier (long double)" $
            parseLenMod "Lf rest" `shouldBe` ("L", "f rest")

        it "parses 'q' modifier (BSD quad)" $
            parseLenMod "qd rest" `shouldBe` ("q", "d rest")

        it "returns empty modifier for unknown char" $
            parseLenMod "xd rest" `shouldBe` ("", "xd rest")

        it "handles empty input" $
            parseLenMod "" `shouldBe` ("", "")

        it "does not consume past the modifier" $
            parseLenMod "llu" `shouldBe` ("ll", "u")

        it "hh does not consume more than two h's" $
            let (m, r) = parseLenMod "hhhn"
            in (m, r) `shouldBe` ("hh", "hn")

    describe "parseLenMod edge cases" $ do

        it "single 'l' before a complex format tail is parsed correctly" $
            parseLenMod "l10d rest" `shouldBe` ("l", "10d rest")

        it "'z' modifier does not get doubled or altered" $
            parseLenMod "zu" `shouldBe` ("z", "u")

        it "'t' modifier parses correctly before 'd'" $
            parseLenMod "td" `shouldBe` ("t", "d")

        it "bare 'L' before 'g' is parsed as one character" $
            parseLenMod "Lg" `shouldBe` ("L", "g")

        it "returns empty modifier when input starts with digit (width, not modifier)" $
            parseLenMod "10d rest" `shouldBe` ("", "10d rest")
