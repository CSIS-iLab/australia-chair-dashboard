-- Hover tooltips: for every chart image <name>.png with a <name>_hover.json
-- beside it (written by write_hover() in '0. Chart data.R'), store the file's
-- contents in the image's data-hover attribute, and the US$ twin's
-- (<name>_usd_hover.json) in data-hover-usd. The script in
-- theme/chart-footer.html shows the text when the pointer is over a hover area.

local function read_all(path)
  local f = io.open(path, "rb")
  if not f then return nil end
  local s = f:read("a")
  f:close()
  return s
end

function Image(img)
  local src = img.src:gsub("%%20", " ")
  if not src:match("%.png$") or src:match("_usd%.png$") then return nil end
  local stem = src:gsub("%.png$", "")
  local aud = read_all(stem .. "_hover.json")
  if not aud then return nil end
  img.attributes["data-hover"] = aud
  local usd = read_all(stem .. "_usd_hover.json")
  if usd then img.attributes["data-hover-usd"] = usd end
  return img
end
