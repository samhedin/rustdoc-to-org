local function cleaninlines(content)
  for i,v in ipairs(content) do
    content[i] = pandoc.walk_inline(content[i], cleaninline)
  end
  return content
end

cleaninline = {
  Space = function(el)
    return pandoc.Str(" ")
  end,
  LineBreak = function(el)
    return pandoc.Str(" ")
  end,
  SoftBreak = function(el)
    return pandoc.Str(" ")
    end,
}

cleanblocks = {
  Str = function(el)
    if el.text == "[src]" then
      return pandoc.Str(" ")
    end
  end,
Link = function(el)
  return pandoc.Span(el.content)
end,

Header = function(el)
  if el.classes:includes("section-header") then
    return pandoc.Null
  end
end,

Div = function(el)
  if el.classes:includes("shortcuts") or el.classes:includes("sidebar-elems") or el.classes:includes("theme-picker") or el.classes:includes("infos") or el.classes:includes("search-container")  or el.classes:includes("sidebar-menu") or el.classes:includes("logo-container") or el.classes:includes("toggle-wrapper") then
    return pandoc.Null
  end
end,

Plain = function(el)
  return pandoc.Plain(cleaninlines(el.content))
end,


Para = function(el)
  return pandoc.Para(cleaninlines(el.content))
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
  return pandoc.Pandoc(pandoc.walk_block(pandoc.Div(el.blocks), cleanblocks), pandoc.Meta({}))
end
