-- True size for charts (Nov 2026): every chart image <name>.png gets its
-- "true" width in screen pixels, from the PNG's own width and resolution
-- (ggsave writes the dpi into the file), in a --true-w style property.
-- theme/csis-redesign.scss never lets a chart grow beyond that width, so a
-- chart's text appears at its real point size (a 13-point title at about
-- 17 pixels) however wide the card is, instead of being blown up on wide
-- screens and full-width cards. Charts still shrink to fit smaller cards.

local function png_true_width(path)
  local f = io.open(path, "rb")
  if not f then return nil end
  local head = f:read(256)
  f:close()
  if not head or head:sub(2, 4) ~= "PNG" then return nil end
  local function u32(s, i)
    local a, b, c, d = s:byte(i, i + 3)
    return ((a * 256 + b) * 256 + c) * 256 + d
  end
  local width = u32(head, 17)
  local p = head:find("pHYs", 1, true)
  if not p then return nil end
  local ppm = u32(head, p + 4)          -- pixels per metre
  if head:byte(p + 12) ~= 1 or ppm == 0 then return nil end
  local dpi = ppm * 0.0254
  return math.floor(width * 96 / dpi + 0.5)
end

function Image(img)
  local src = img.src:gsub("%%20", " ")
  if not src:match("%.png$") then return nil end
  local w = png_true_width(src)
  if not w then return nil end
  local style = img.attributes["style"] or ""
  img.attributes["style"] = style .. "--true-w:" .. w .. "px;"
  img.attributes["data-truew"] = tostring(w)
  return img
end
