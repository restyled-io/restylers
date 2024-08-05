module Main
  ( main
  )
where

import Prelude

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "-XBangPatterns"
    , "-XDataKinds"
    , "-XDeriveAnyClass"
    , "-XDeriveFoldable"
    , "-XDeriveFunctor"
    , "-XDeriveGeneric"
    , "-XDeriveLift"
    , "-XDeriveTraversable"
    , "-XDerivingStrategies"
    , "-XFlexibleContexts"
    , "-XFlexibleInstances"
    , "-XGADTs"
    , "-XGeneralizedNewtypeDeriving"
    , "-XLambdaCase"
    , "-XMultiParamTypeClasses"
    , "-XNoImplicitPrelude"
    , "-XNoMonomorphismRestriction"
    , "-XOverloadedStrings"
    , "-XRankNTypes"
    , "-XRecordWildCards"
    , "-XScopedTypeVariables"
    , "-XStandaloneDeriving"
    , "-XTypeApplications"
    , "-XTypeFamilies"
    , "src"
    ]
