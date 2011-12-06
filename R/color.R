rgbToKMLColor =
  # RGBA to abgr
function(x)
{
  if(!is.character(x))
     x = rgb(x[1], x[2], x[3], maxColorValue = if(all(x < 1)) 1 else 255)
  
  if(substring(x, 1, 1) != "#") {
     tmp = col2rgb(x)[,1]
     x = rgb(tmp[1], tmp[2], tmp[3], maxColorValue = 255)
  } 
  
  if(nchar(x) < 8)
    x = sprintf("%sFF", x)
  
  els = strsplit(x, "")
  
  idx = c(8:9, 6:7, 4:5, 2:3)
  structure(sapply(els, function(v) paste(v[idx], collapse = "")), names = x)
}
