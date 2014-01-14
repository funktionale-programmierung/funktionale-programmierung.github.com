import Development.Shake
import System.FilePath
import System.Environment
import Data.Maybe
import qualified Data.List as List

cIncludes :: FilePath -> Action [FilePath]
cIncludes x =
    do s <- readFile' x
       return $ mapMaybe parseInclude (lines s)
    where
      parseInclude line =
          do rest <- List.stripPrefix "#include \"" line
             return $ takeWhile (/= '"') rest

rules :: Rules ()
rules =
    do "*.o" *> \out ->
           do let c = replaceExtension out "c"
              headers <- cIncludes c
              need headers
              system' "gcc" ["-o", out, "-c", c]

       "Main" *> \out -> buildBinary out ["Hello.c", "Main.c"]

       "Codegen" *> \out -> buildBinary out ["Codegen.c"]

       "Auto.h" *> \out ->
           do need ["Codegen"]
              system' "./Codegen" [out]

buildBinary :: FilePath -> [FilePath] -> Action ()
buildBinary out cs =
    do let os = map (\c -> replaceExtension c "o") cs
       need os
       system' "gcc" (["-o", out] ++ os)

main :: IO ()
main =
    do args <- getArgs
       shake shakeOptions $
          do let targets = if null args then ["Main"] else targets
             rules
             want targets
