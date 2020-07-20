{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk
import qualified Data.Text                     as T

main :: IO ()
main = toJSONFilter runAll

runAll :: [ Block ] -> [ Block ]
runAll =  map (fixBulletList . makeTitle  . sections  . cleanBlock)

emptyAttrs :: (T.Text, [T.Text], [(T.Text, T.Text)])
emptyAttrs = ("", [], [])

makeTitle :: Block -> Block
makeTitle block = case block of
  (Div _ ((Header 1 _ headerData) : bs)) -> Div
    emptyAttrs
    (Header 1 emptyAttrs
     (cleanInlines (filter (\case
                                 (Span (_, [inband], _) _) | inband == "in-band" -> True -- The title information we wish to save and show is in this Span.
                                 _ -> False) headerData))
      : bs)

  _ -> block

-- cleanBlock removes things like the sidebar, and calls cleanInlines to remove whitespace and more
cleanBlock :: Block -> Block
cleanBlock block = case block of
  (Div (_, (classname : _), _) _)
    | classname
      == "shortcuts"
      || classname
      == "sidebar-elems"
      || classname
      == "theme-picker"
      || classname
      == "infos"
      || classname
      == "search-container"
      || classname
      == "sidebar-menu"
      || classname
      == "logo-container"
      || classname
      == "toggle-wrapper"
    -> Null

  (Div (_, [_, section], _) xs) | section == "small-section-header" -> Null

  (Para [Link _ [] _]) -> Null

  (Plain ((Link (_, [name], _) _ _) : _))
    | name == "sidebar-title" || name == "test-arrow" -> Null

  (Div (tag, _, _) bs)
    | tag
      == "main"
      || tag
      == "search"
      || tag
      == "imlementation-list"
      || tag
      == "synthetic-implementations-list"
      || tag
      == "blanket-implementations-list"
    -> Div emptyAttrs $ map cleanBlock bs

  (Plain [Span _ [Str panics] ]) | panics == "Panics" -> Null
  (Plain inlines) -> Plain $ cleanInlines $ filter
    (\case
      (Link (_, [classname], _) _ _)
        | classname == "srclink" || classname == "collapse" -> False

      _ -> True
    )
    inlines

  (Header 1 (panics, _, _) ins) | T.isPrefixOf "panics" panics ->
    Plain $ cleanInlines ins

  (CodeBlock a t)  -> Para [Str "#+BEGIN_SRC rust \n",  Str t, Str "\n#+END_SRC"] -- This lets us use syntax highlighting
  (Header 1 _ [Str panics]) | panics == "Panics" -> Null
  (Para ins) -> Para $ cleanInlines ins
  (Header 4 attr [Code _ _]) -> Null
  (Header _ _ []) -> Null
  (Div _ [Header 4 _ [], Plain [Span _ []]]) -> Null
  (Header 1 _ [Link _ [(Str examples)] target]) | examples == "Examples" -> Null
  (Header a attr ins) -> Header a attr (cleanInlines ins)

  (Div attr blocks) -> Div attr (map cleanBlock blocks)
  (BlockQuote blocks) -> BlockQuote (map cleanBlock blocks)
  (BulletList blocklists) -> BulletList $ map (map cleanBlock) blocklists

  _ -> block

-- Amongst other things, removes some causes of unwanted linebreaks, for example space leads to linebreaks sometimes so it's replaced with Str " "
cleanInlines :: [Inline] -> [Inline]
cleanInlines x = foldr
  (\x acc -> case x of
    Space              -> Str " " : acc
    LineBreak          -> Str " " : acc
    SoftBreak -> Str " " : acc
    (Link _ _ (url, _)) | url == "#required-sections" -> acc
    (Link _ (_ : (Span (_, [inner], _) _) : _) _) | inner == "inner" -> acc
    (Link _ is target) -> Span emptyAttrs (cleanInlines is) : acc
    (Span _    []    ) -> acc
    (Span (_, [classname], _) _) | classname  == "since" || classname == "emoji" -> acc
    (Span attr is    ) -> Span attr (cleanInlines is) : acc
    (Strong    ins)    -> Strong (cleanInlines ins) : acc
    (Emph      ins)    -> Emph (cleanInlines ins) : acc
    (Strikeout ins)    -> Strikeout (cleanInlines ins) : acc
    _                  -> x : acc
  )
  []
  x

-- This makes [inline] of This is a nightly-only experimental API, so that it doesn't become a header.
mkNotice :: [Block] -> [Inline]
mkNotice blocks = foldr
  (\x acc -> case x of
    (Plain ins     ) -> ins ++ acc
    (Para  ins     ) -> ins ++ acc
    (Div _ bs      ) -> mkNotice bs ++ acc
    (Header _ _ ins) -> ins ++ acc
    _                -> acc
  )
  []
  blocks


sections :: Block -> Block
sections b = case b of
  (Plain [Link _ _ (url, _), Code (id, _, _) code]) ->
    Header 3 emptyAttrs [Code emptyAttrs code]
  (Div (_, classnames, _) blocks) | "stability" `elem` classnames ->
    Plain $ mkNotice blocks

  (Header 3 (_, [classname], _) (code : (Link _ _ (url, _)) : _))
    | classname == "impl" -> Header 2
                                    emptyAttrs
                                    [Link emptyAttrs [code] (url, "")]
  (Header 1 _ ins) -> Header 1 emptyAttrs $ foldInlines ins

  (Header l _ ins) -> Header (l - 1) emptyAttrs $ foldInlines ins
  (Plain (hidden : sections)) ->
    Div emptyAttrs [Header 3 emptyAttrs sections, Plain [hidden]]

  (Div (_, [docblock], _) docs) | docblock == "docblock" -> Div emptyAttrs docs
  _ -> b

 where
  foldInlines ins = foldr
    (\x acc -> case x of
      (Span _ [Str src]) | src == "[src]"         -> acc
      (Code _ desc) | T.isPrefixOf "#[must" desc ->
        let descNoMustUse = T.drop 1 (T.dropWhile (/= ']') desc)
            mustUse       = T.takeWhile (/= ']') (T.drop 1 (T.dropWhile (/= '=') desc))
        in  Code emptyAttrs descNoMustUse : Str " " : if T.length mustUse > 3
              then Note [Plain [Str mustUse]] : acc
              else acc
      (Span attr ins) -> Span attr ins : acc
      x               -> x : acc
    )
    []
    ins

--Bulletlists become super strange when imported. For example, the first word becomes last in the list so we have to take it and put it in the front.
fixBulletList :: Block -> Block
fixBulletList (BulletList bullets) = BulletList $ foldr
  (\x acc -> case x of
    ((Div _ ((Header _ _ bullet) : bs)) : divs) -> (head bs : Plain bullet : tail bs) : acc -- TODO notice that this is ignoring divs, may lead to missing elements in the future?
    _ -> x : acc
  )
  []
  bullets
fixBulletList x = x
