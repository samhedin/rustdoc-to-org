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
  if el.classes:includes("small-section-header") then
    return pandoc.Header(1, pandoc.List:new{el.content[1]})
  end
  if el.classes:includes("impl") then
    return pandoc.Header(2, pandoc.List:new{el.content[1]})
  end

  if el.classes:includes("method") then
    local code = el.content[1]
    local methodname = ""
    local must_use_text = ""
    local contains_must_use = false
    local in_methodname = true
    local in_must_use_text = false

    if string.match(code.text, "must_use") then
      in_methodname = false
      contains_must_use = true
    end

    for i = 1, #code.text do
      local c = code.text:sub(i, i);

      if in_methodname then
        methodname = methodname .. c
      elseif in_must_use_text then
        must_use_text = must_use_text .. c
      end

      if c == "]" then
        in_methodname = true
      elseif c == "\"" then
        in_must_use_text = true
      end
    end

    if contains_must_use then
      return pandoc.List:new({pandoc.Header(3, methodname), pandoc.Plain(must_use_text:sub(1, -3))})
    end
    return pandoc.Header(3, methodname)
  end
end,

Div = function(el)
  if el.classes:includes("shortcuts") or el.classes:includes("sidebar-elems") or el.classes:includes("theme-picker") or el.classes:includes("infos") or el.classes:includes("search-container")  or el.classes:includes("sidebar-menu") or el.classes:includes("logo-container") or el.classes:includes("toggle-wrapper") then
    return pandoc.Null
  elseif el.classes:includes("variant") and el.classes:includes("small-section-header") then
    return pandoc.List:new({pandoc.Header(2, el.content[1].content[2])})
  end
end,

Plain = function(el)
  for i,v in ipairs(el.content) do
    if v.t == "Span" and v.content[1] and v.content[1].t == "Str" and v.content[1].text == "Run" then
      return pandoc.Null
    end

    if v.t == "Span" and v.classes:includes("emoji") then
      table.remove(el.content, 1)
      return pandoc.Plain(el.content)
      end
  end
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
