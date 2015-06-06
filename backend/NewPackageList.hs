{-# LANGUAGE OverloadedStrings #-}
module NewPackageList (newPackages, addIfNew) where

import qualified Data.Aeson as Json
import qualified Data.Aeson.Encode.Pretty as Json
import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified System.Directory as Dir
import System.IO

import qualified Elm.Package.Constraint as C
import qualified Elm.Package.Description as Desc
import qualified Elm.Package.Version as V


newPackages :: String
newPackages =
    "new-packages.json"


addIfNew :: Desc.Description -> IO ()
addIfNew desc =
  case C.isSatisfied (Desc.elmVersion desc) V.elm of
    False ->
        return ()
    True ->
        do  let name = Desc.name desc
            exists <- Dir.doesFileExist newPackages
            case exists of
              False ->
                  LBS.writeFile newPackages (Json.encodePretty [name])

              True ->
                withBinaryFile newPackages ReadMode $ \handle ->
                    do  json <- LBS.hGetContents handle
                        case Json.decode json of
                          Nothing ->
                              error "new-package.json is corrupted! do not modify them manually."

                          Just names ->
                              LBS.writeFile newPackages (Json.encodePretty (List.insert name names))
