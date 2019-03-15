main :: IO ()
main = interact $ make_html . unlines . make_blocks . map make_img . lines

make_img :: String -> String
make_img url = concat ["<div style=\"width:280px\"><img src=\"", url, "\" /><br>", id, "</div>"]
  where id = fst $ break (== '.') $ reverse $ fst $ break (== '/') $ reverse url

make_blocks :: [String] -> [String]
make_blocks [] = []
make_blocks lns =  "<div id=\"block\">" : left ++ "</div>" : make_blocks right
  where (left, right) = splitAt 5 lns

make_html :: String -> String
make_html body = concat [
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
