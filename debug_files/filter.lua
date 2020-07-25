local function elem(tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return true
    end
  end
  return false
end

function map(tbl, f)
  local t = {}
  for k,v in pairs(tbl) do
    t[k] = f(v)
  end
  return t
end

Div = function(el)
  if elem(el.classes, "shortcuts") or elem(el.classes, "sidebar-elems") or elem(el.classes, "theme-picker") or elem(el.classes, "infos") or elem(el.classes, "search-container")  or elem(el.classes,"sidebar-menu") or elem(el.classes, "logo-container") or elem(el.classes, "toggle-wrapper") then
    return pandoc.Null
      else
    return el
  end
end

CodeBlock = function(el)
  if elem(el.classes, "line-numbers") then
    return pandoc.Null
  else
    return pandoc.Para(pandoc.Str("#+BEGIN_SRC rust \n" .. el.text .. "\n#+END_SRC"))
  end
end


Header = function(el)
  return pandoc.Header(5, el.content)
end
