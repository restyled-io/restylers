{-# LANGUAGE OverloadedStrings #-}

module WhitespaceSpec
    ( spec
    )
where

import Test.Hspec
import Whitespace

spec :: Spec
spec = do
    let
        opts = FormatOptions
            { foSpaces = True
            , foNewlines = True
            , foStrict = True -- Unused
            , foPaths = [] -- Unused
            }

    it "strips trailing whitespace from the given content" $ do
        let content = mconcat
                [ "line one  "
                , "\nline two "
                , "\n "
                , "\nline three \\" -- preserved
                , "\n"
                ]
            expected = mconcat
                ["line one", "\nline two", "\n", "\nline three \\", "\n"]

        format opts content `shouldBe` expected
        format opts { foSpaces = False } content `shouldBe` content

    it "strips extra newlines from the end of the content" $ do
        let content =
                mconcat
                    [ "line one"
                    , "\nline two"
                    , "\n"
                    , "\n"
                    , "\nline three"
                    , "\n"
                    , "\n"
                    , "\n"
                    ]
            expected =
                mconcat ["line one", "\nline two", "\n\n", "\nline three", "\n"]

        format opts content `shouldBe` expected
        format opts { foNewlines = False } content `shouldBe` content

    it "does not affect completely empty files" $ do
        format opts "" `shouldBe` ""
