function map(tbl, f)
  local t = {}
  for k,v in pairs(tbl) do
    t[k] = f(v)
  end
  return t
end

filters = {
Space = pandoc.Str(" "),
LineBreak = pandoc.Str(" "),
SoftBreak = pandoc.Str(" "),

Str = function(el)
  if el.text == "[src]" then
    return pandoc.Str(" ")
  end
end,

Link = function(el)
  return pandoc.Span(el.content)
end,


Div = function(el)
  if el.classes:includes("shortcuts") or el.classes:includes("sidebar-elems") or el.classes:includes("theme-picker") or el.classes:includes("infos") or el.classes:includes("search-container")  or el.classes:includes("sidebar-menu") or el.classes:includes("logo-container") or el.classes:includes("toggle-wrapper") then
    return pandoc.Null
  end
end,

CodeBlock = function(el)
  if el.classes:includes("line-numbers") then
    return pandoc.Null
  else
    return pandoc.Para(pandoc.Str("#+BEGIN_SRC rust \n" .. el.text .. "\n#+END_SRC"))
  end
end,

}

function Pandoc(el)
  return pandoc.Pandoc(pandoc.walk_block(pandoc.Div(el.blocks), filters), pandoc.Meta({}))
end
