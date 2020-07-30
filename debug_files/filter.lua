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

  if el.classes:includes("method") then
    local code = el.content[1]
    local methodname = ""
    local must_use_text = ""
    local contains_must_use = false
    local methodname_started = true
    local in_must_use_text = false
    if string.match(code.text, "must_use") then
      methodname_started = false
      contains_must_use = true
    end

    for i = 1, #code.text do
      local c = code.text:sub(i, i);

      if methodname_started then
        methodname = methodname .. c
      elseif in_must_use_text then
        must_use_text = must_use_text .. c
      end

      if c == "]" then
        methodname_started = true
      elseif c == "\"" and in_must_use_text then
        in_must_use_text = false
      elseif c == "\"" then
        in_must_use_text = true
      end
    end

    if contains_must_use then
      must_use_text = must_use_text:sub(1, -2)
      return pandoc.List:new({pandoc.Header(2, methodname), pandoc.Plain(must_use_text)})
    end
    return pandoc.Header(2, methodname)
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
