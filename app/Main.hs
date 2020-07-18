{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import Text.Pandoc.JSON
import Text.Pandoc.Walk
import qualified Data.Text as T

-- https://hackage.haskell.org/package/pandoc-types-1.21/docs/Text-Pandoc-Definition.html#t:Attr

emptyAttrs :: (T.Text, [T.Text], [(T.Text, T.Text)])
emptyAttrs = ("", [], [])

main :: IO ()
main = toJSONFilter runAll

runAll :: Block -> Block
runAll =  makeTitle . flattenBlock . examples . methods . variants . header2 . cleanBlock

makeTitle :: Block -> Block
makeTitle (Div a ((Header 1 _ inlines) : bs)) = Div a (Header 1 emptyAttrs (title inlines) : bs)
  where
    title :: [Inline] -> [Inline]
    title ((Span (_, [inband], _) titlestrs):is)
      | inband == "in-band" = foldr (\ x acc -> case x of
                                        (Str t1) -> Str t1 : acc
                                        (Link _ [(Str t1)] t) -> Link emptyAttrs [Str t1] t : acc
                                        _ -> acc) [] titlestrs
    title (_:is) = title is
    title [] = []
makeTitle x = x

-- cleanBlock removes things like the sidebar
cleanBlock :: Block -> Block
cleanBlock (Div (_, [classname], _) _)
  | classname == "shortcuts"
    || classname == "sidebar-elems"
    || classname == "theme-picker"
    || classname == "infos"
    || classname == "search-container"
    || classname == "sidebar-menu"
    || classname == "logo-container"
    || classname == "toggle-wrapper"
  = Null

cleanBlock (Div (_, classname : _, _) _)
  | classname == "toggle-wrapper"
  = Null

cleanBlock (Plain ((Link (_, [name], _) _ _) : _))
  | name == "sidebar-title"
    || name == "test-arrow"
  = Null

cleanBlock (Div (tag, _, _) bs)
  | tag == "main"
    || tag == "search"
    || tag == "imlementation-list"
    || tag == "synthetic-implementation-list"
    || tag == "blanket-implementation-list"
  = Div emptyAttrs $ map cleanBlock bs

cleanBlock (Plain inlines) = Plain $ cleanInlines $ filter (\case
                                               (Link (_, [classname], _) _ _)
                                                 | classname == "srclink"
                                                   || classname == "collapse" -> False

                                               _ -> True) inlines

cleanBlock (Para ins) = Para $ cleanInlines ins
cleanBlock (Header 4 attr [Code _ _]) = Null
cleanBlock (Header a attr ins) = Header a attr (cleanInlines ins)
cleanBlock x = x

-- Remove some causes of unwanted linebreaks
cleanInlines :: [Inline] -> [Inline]
cleanInlines x = foldr (\ x acc -> case x of
    Space -> Str " " : acc
    LineBreak -> Str " " : acc
    (Link _ (_ : (Span (_, [inner], _) _) : _) _) | inner == "inner" -> acc
    (Link _ is target)  -> Link emptyAttrs (cleanInlines is) target : acc
    (Span attr is)  -> Span attr (cleanInlines is) : acc
    SoftBreak -> acc
    _ -> x : acc) [] x

variants :: Block -> Block
variants (Div (_,[_, section], _) xs)
  | section == "small-section-header" = Div emptyAttrs xs
variants (Plain [Link _ _ (url, _), Code (id, _, _) code]) = Header 3 emptyAttrs [Code emptyAttrs code]
variants x = x

methods :: Block -> Block
methods b = case b of
  (Header 3 (_, [classname], _) (code : (Link _ _ (url, _)) : _)) | classname == "impl" -> Header 3 emptyAttrs [Link emptyAttrs [code] (url, "")]

  (Header 4 _ ins) -> Header 4 emptyAttrs $ filter (\case
                                                       (Link _ [Str src] _) | src == "[src]" -> False
                                                       (Link _ (Str bracket : _) _) | bracket == "[" -> False
                                                       (Span (_, [since], _) _) | since == "since" -> False
                                                       x -> True) ins

  (Plain (hidden : methods)) -> Div emptyAttrs [Header 4 emptyAttrs $ cleanInlines methods ,  Plain $ fixMustUse hidden]

  (Div (_, [docblock], _) docs) | docblock == "docblock" -> Div emptyAttrs docs

  x -> x

--Some functions have a "must_use" description that comes out quite ugly unless we do something about it.
fixMustUse :: Inline -> [Inline]
fixMustUse inline = case inline of
  (Span _ ins) -> [Span emptyAttrs (foldr (\ x acc -> case x of
                                              (Str deleteMe)
                                                | deleteMe == "#[must_use" -> acc
                                                | deleteMe == "=" -> acc
                                              SoftBreak -> acc
                                              (Str quote) | head (T.unpack quote) == '\"' -> Str (T.tail quote) : acc
                                              (Str endbracket) | ']' `elem` T.unpack endbracket -> Str (T.dropEnd 2 endbracket) : acc
                                              x -> x : acc) [] (drop 3 ins))]
  _ -> [inline]

examples :: Block -> Block
examples (Header 1 (_, [sectionHeader], _) ins)
  | sectionHeader == "section-header" = Plain ins
examples x = x

flattenBlock :: Block -> Block
flattenBlock b = case b of
  (Div _ [Header l a ins]) -> Header l a ins
  (Div (_, names, _) blocks)
      | "unstable" `elem` names -> Div emptyAttrs $ walk (\x -> map (\case
                                          (Header 4 _ ins) -> Para ins
                                          x -> x) x) blocks
  x -> x


--Link Attr [Inline] Target
header2 :: Block -> Block
header2  (Header 2 _ ((Str name) : Link _ _ (url, _) : inlines)) = Header 2 emptyAttrs [Link emptyAttrs [Str name] (url, name)]
header2  a = a
