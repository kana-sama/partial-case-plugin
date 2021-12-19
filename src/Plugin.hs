{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Plugin (plugin) where

import Control.Lens
import Data.Generics.Labels ()
import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.String (fromString)
import GHC.Generics (Generic)
import GHC.Hs
import GhcPlugins

deriving stock instance Generic HsParsedModule

deriving stock instance Generic (HsModule pass)

deriving stock instance Generic (MatchGroup pass e)

deriving stock instance Generic (Match pass e)

deriving stock instance Generic (GRHSs pass e)

deriving stock instance Generic (GRHS pass e)

rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

loc :: Traversal' (Located a) a
loc = traverse

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = \_ _ mod -> pure (mod & #hpm_module . loc %~ transformModule)}

pattern Partial :: LHsExpr GhcPs
pattern Partial <- L _ (HsVar _ (L _ (rdrNameString -> "partial")))

mkVar :: String -> LHsExpr GhcPs
mkVar var = noLoc (HsVar NoExtField (noLoc (mkVarUnqual (fromString var))))

fallback :: Match GhcPs (LHsExpr GhcPs)
fallback = Match NoExtField CaseAlt [noLoc (WildPat NoExtField)] (GRHSs NoExtField [noLoc (GRHS NoExtField [] (mkVar "empty"))] (noLoc (EmptyLocalBinds NoExtField)))

transformModule :: HsModule GhcPs -> HsModule GhcPs
transformModule = Uniplate.transformBi \case
  HsApp _ Partial (L l (HsCase _ expr mgroup)) ->
    HsCase NoExtField expr (addFallback (wrapPure mgroup))
  HsApp _ Partial (L l (HsLamCase _ mgroup)) ->
    HsLamCase NoExtField (addFallback (wrapPure mgroup))
  other -> other

wrapPure :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
wrapPure =
  #_MG . simple . _2 . loc . each . loc . #_Match . simple . _4 . #_GRHSs . simple . _2 . each . loc . #_GRHS . simple . _3 . loc
    %~ HsApp NoExtField (mkVar "pure") . noLoc

addFallback :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
addFallback = #_MG . simple . _2 . loc <>~ [noLoc fallback]
