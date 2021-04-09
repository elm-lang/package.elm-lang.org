{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import qualified Data.Set as Set
import System.Console.CmdArgs
import qualified System.Directory as Dir

import qualified Crawl
import qualified Elm.Package as Pkg
import qualified GetDates as Dates
import qualified Task
import qualified MoveDocs as Docs
import qualified MoveElmJson as ElmJson
import qualified MoveReadme as Readme




-- FLAGS


data Flags =
  Flags
    { batch :: Int
    , github :: String
    }
    deriving (Data,Typeable,Show,Eq)


flags :: Flags
flags =
  Flags
    { batch = 40
        &= help "Maximum number of packages to process"
    , github = ""
        &= help "OAuth token for talking to GitHub"
    }



-- MAIN


main :: IO ()
main =
  do  cargs <- cmdArgs flags
      Task.run (github cargs) $
        do  liftIO $ putStrLn "---- CRAWLING DIRECTORIES ----"
            newPackages <- skipProblems <$> Crawl.newPackages

            liftIO $ putStrLn "---- MOVING ASSETS ----"
            mapM_ moveAssets newPackages

            liftIO $ putStrLn "---- CHECKING RELEASE DATES ----"
            Dates.get =<< Crawl.upgradingPackages

            liftIO $ putStrLn "DONE"


moveAssets :: Crawl.Package -> Task.Task ()
moveAssets (Crawl.Package pkg vsns) =
  forM_ vsns $ \vsn ->
    Task.attempt pkg vsn $
      do  liftIO $ putStrLn $ Pkg.toString pkg ++ " " ++ Pkg.versionToString vsn
          liftIO $ Dir.createDirectoryIfMissing True (Crawl.newDir pkg vsn)
          Readme.move pkg vsn
          ElmJson.move pkg vsn
          Docs.move pkg vsn



-- PROBLEM PACKAGES


skipProblems :: [Crawl.Package] -> [Crawl.Package]
skipProblems packages =
  filter (\(Crawl.Package pkg _) -> not (Set.member pkg problemPackages)) packages


problemPackages :: Set.Set Pkg.Name
problemPackages =
  Set.fromList
    [ Pkg.Name "joneshf" "elm-proof"                -- crazy module name in docs
    , Pkg.Name "adam-r-kowalski" "elm-css-legacy"   -- deleted
    , Pkg.Name "danstn" "elm-postgrest"             -- deleted
    , Pkg.Name "elm-community" "elm-function-extra" -- deleted
    , Pkg.Name "elm-community" "elm-undo-redo"      -- deleted
    , Pkg.Name "folkertdev" "elm-ordereddict"       -- deleted
    , Pkg.Name "frenchdonuts" "elm-autocomplete"    -- too weird
    , Pkg.Name "geekyme" "elm-charts"               -- deleted
    , Pkg.Name "humio" "elm-plot"                   -- missing tags / why fork?
    , Pkg.Name "izdi" "junk"                        -- deleted
    , Pkg.Name "JasonGFitzpatrick" "elm-router"     -- deleted
    , Pkg.Name "jasonmahr" "html-escaped-unicode"   -- deleted
    , Pkg.Name "JOEANDAVERDE" "flex-html"           -- deleted
    , Pkg.Name "joonazan" "elm-ast"                 -- missing tags / why fork?
    , Pkg.Name "jvoigtlaender" "elm-drag-and-drop"  -- deleted
    , Pkg.Name "lagunoff" "elm-mdl"                 -- deleted
    , Pkg.Name "lukewestby-fake-elm-lang-1" "redirect-test-1" -- deleted
    , Pkg.Name "nicklawls" "elm-html-animation"     -- deleted
    , Pkg.Name "nvaldes" "elm-bootstrap"            -- deprecated / https://github.com/elm-lang/package.elm-lang.org/issues/227
    , Pkg.Name "omarroth" "elm-dom"                 -- deleted
    , Pkg.Name "omarroth" "elm-parts"               -- deleted
    , Pkg.Name "rluiten" "lunrelm"                  -- deprecated
    , Pkg.Name "stasdavydov" "elm-cart"             -- unlicensed
    , Pkg.Name "RomanErnst" "updated-list"          -- deleted
    , Pkg.Name "Spottt" "elm-dialog"                -- missing tags / why fork?
    , Pkg.Name "terezka" "elm-view-utils"           -- renamed
    , Pkg.Name "williamwhitacre" "gigan"            -- renamed / unlicensed
    , Pkg.Name "williamwhitacre" "scaffold"         -- signals / unlicensed but BSD3
    , Pkg.Name "wittjosiah" "elm-css"               -- deleted
    , Pkg.Name "z5h" "time-app"                     -- deprecated
    , Pkg.Name "special-elektronik" "elm-autocomplete" -- renamed https://github.com/elm/package.elm-lang.org/issues/339
    ]
