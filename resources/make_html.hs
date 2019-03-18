#!/usr/bin/env runhaskell
import System.Environment
import System.IO

outputFilename = "angery.html"

main :: IO ()
main = do
  args <- getArgs
  inHandle <- case args of
    [] -> return stdin
    ["-"] -> return stdin
    [filename] -> openFile filename ReadMode
    _ -> error "Invalid argument"
  content <- hGetContents inHandle
  outHandle <- openFile outputFilename WriteMode
  let processData = makeHtml . unlines . makeBlocks . map makeImg . lines in
    hPutStr outHandle (processData content)

makeImg :: String -> String
makeImg url = concat ["<div style=\"width:280px\"><img src=\"", url, "\" /><br>", id, "</div>"]
  where id = takeWhile (/= '.') $ reverse $ takeWhile (/= '/') $ reverse url

makeBlocks :: [String] -> [String]
makeBlocks [] = []
makeBlocks lns =  "<div id=\"block\">" : left ++ "</div>" : makeBlocks right
  where (left, right) = splitAt 5 lns

makeHtml :: String -> String
makeHtml body = concat [
  "<style>#block { display: flex; justify-content: center; }</style>",
  body,
  "<script type=\"text/javascript\">setTimeout(() => {",
    "let is = document.getElementsByTagName('img');",
    "let with_heights = [];",
    "for (let i = 0; i < is.length; i++) {",
      "with_heights.push({ h: is[i].height, s: is[i].src });",
    "};",
    "with_heights.sort((a, b) => a.h - b.h);",
    "for (let i = 0; i < is.length; i++) {",
      "s = with_heights[i].s;",
      "is[i].parentElement.innerHTML = `<img src=\"${s}\" /><br>${s.substr(s.length - 36, 32)}`;",
    "}",
  "}, 2000);</script>"]
