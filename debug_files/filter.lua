local function elem(tab, val)
  for index, value in ipairs(tab) do
    if value == val then
      return true
    end
  end
  return false
end

Div = function(el)
  if elem(el.classes, "shortcuts") or elem(el.classes, "sidebar-elems") or elem(el.classes, "theme-picker") or elem(el.classes, "infos") or elem(el.classes, "search-container")  or elem(el.classes,"sidebar-menu")   or elem(el.classes, "logo-container")  or elem(el.classes, "toggle-wrapper") then
    return pandoc.Null
  else
    return el
  end
end


Header = function(el)
  return pandoc.Header(5, el.content)
end
