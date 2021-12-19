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
import GHC.LanguageExtensions.Type (Extension (BlockArguments))
import GhcPlugins

rdrNameString :: RdrName -> String
rdrNameString = occNameString . rdrNameOcc

plugin :: Plugin
plugin = defaultPlugin {dynflagsPlugin, parsedResultAction}
  where
    dynflagsPlugin opts flags = pure (xopt_set flags BlockArguments)
    parsedResultAction _ _ mod@HsParsedModule {hpm_module = L l x} =
      pure mod {hpm_module = L l (addImport (transformPartial x))}

pattern Partial :: LHsExpr GhcPs
pattern Partial <- L _ (HsVar _ (L _ (rdrNameString -> "partial")))

controlApplicativeVar :: String -> LHsExpr GhcPs
controlApplicativeVar var = noLoc (HsVar NoExtField (noLoc (mkRdrQual controlApplicativeAlias (mkVarOcc var))))

controlApplicativeAlias :: ModuleName
controlApplicativeAlias = mkModuleName "PartialCasePlugin.Control.Applicative"

controlApplicativeImport :: LImportDecl GhcPs
controlApplicativeImport =
  noLoc
    ImportDecl
      { ideclExt = NoExtField,
        ideclSourceSrc = NoSourceText,
        ideclName = noLoc (mkModuleName "Control.Applicative"),
        ideclPkgQual = Nothing,
        ideclSource = False,
        ideclSafe = False,
        ideclQualified = QualifiedPre,
        ideclImplicit = False,
        ideclAs = Just (noLoc controlApplicativeAlias),
        ideclHiding = Nothing
      }

fallback :: LMatch GhcPs (LHsExpr GhcPs)
fallback = noLoc (Match NoExtField CaseAlt [noLoc (WildPat NoExtField)] (GRHSs NoExtField [noLoc (GRHS NoExtField [] (controlApplicativeVar "empty"))] (noLoc (EmptyLocalBinds NoExtField))))

addImport :: HsModule GhcPs -> HsModule GhcPs
addImport = Uniplate.descendBi (++ [controlApplicativeImport])

transformPartial :: HsModule GhcPs -> HsModule GhcPs
transformPartial = Uniplate.transformBi \case
  HsApp _ Partial (L _ (HsCase _ expr mgroup)) ->
    HsCase NoExtField expr (addFallback (wrapPure mgroup))
  HsApp _ Partial (L _ (HsLamCase _ mgroup)) ->
    HsLamCase NoExtField (addFallback (wrapPure mgroup))
  other -> other

wrapPure :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
wrapPure = Uniplate.descendBi (HsApp NoExtField (controlApplicativeVar "pure") . noLoc)

addFallback :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
addFallback = Uniplate.descendBi (++ [fallback])
