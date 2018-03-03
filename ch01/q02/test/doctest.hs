import ClassyPrelude
import Test.DocTest

main :: IO ()
main = doctest $ ["src"] <> extensions

extensions =
  [ "-XAutoDeriveTypeable"
  , "-XBangPatterns"
  , "-XBinaryLiterals"
  , "-XConstraintKinds"
  , "-XDataKinds"
  , "-XDefaultSignatures"
  , "-XDeriveAnyClass"
  , "-XDeriveDataTypeable"
  , "-XDeriveFoldable"
  , "-XDeriveFunctor"
  , "-XDeriveGeneric"
  , "-XDeriveTraversable"
  , "-XDoAndIfThenElse"
  , "-XEmptyDataDecls"
  , "-XExistentialQuantification"
  , "-XFlexibleContexts"
  , "-XFlexibleInstances"
  , "-XFunctionalDependencies"
  , "-XGADTs"
  , "-XGeneralizedNewtypeDeriving"
  , "-XInstanceSigs"
  , "-XKindSignatures"
  , "-XLambdaCase"
  , "-XMultiParamTypeClasses"
  , "-XMultiWayIf"
  , "-XNamedFieldPuns"
  , "-XNoImplicitPrelude"
  , "-XOverloadedLists"
  , "-XOverloadedStrings"
  , "-XPartialTypeSignatures"
  , "-XPatternGuards"
  , "-XPolyKinds"
  , "-XRankNTypes"
  , "-XRecordWildCards"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTupleSections"
  , "-XTypeApplications"
  , "-XTypeFamilies"
  , "-XTypeSynonymInstances"
  , "-XViewPatterns"
  ]

