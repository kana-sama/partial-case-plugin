{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Plugin (plugin) where

import Data.Generics.Uniplate.Data qualified as Uniplate
import Data.String (fromString)
import GHC.Hs
import GhcPlugins

rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction}
  where
    parsedResultAction _ _ mod@HsParsedModule {hpm_module = L l x} =
      pure mod {hpm_module = L l (transformModule x)}

pattern Partial :: LHsExpr GhcPs
pattern Partial <- L _ (HsVar _ (L _ (rdrNameString -> "partial")))

mkVar :: String -> LHsExpr GhcPs
mkVar var = noLoc (HsVar NoExtField (noLoc (mkVarUnqual (fromString var))))

fallback :: Match GhcPs (LHsExpr GhcPs)
fallback = Match NoExtField CaseAlt [noLoc (WildPat NoExtField)] (GRHSs NoExtField [noLoc (GRHS NoExtField [] (mkVar "empty"))] (noLoc (EmptyLocalBinds NoExtField)))

transformModule :: HsModule GhcPs -> HsModule GhcPs
transformModule = Uniplate.transformBi \case
  HsApp _ Partial (L _ (HsCase _ expr mgroup)) ->
    HsCase NoExtField expr (addFallback (wrapPure mgroup))
  HsApp _ Partial (L _ (HsLamCase _ mgroup)) ->
    HsLamCase NoExtField (addFallback (wrapPure mgroup))
  other -> other

wrapPure :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
wrapPure = Uniplate.descendBi (HsApp NoExtField (mkVar "pure") . noLoc)

addFallback :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
addFallback = Uniplate.descendBi (++ [noLoc fallback :: LMatch _ _])
