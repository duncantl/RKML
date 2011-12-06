makeBallIcon =
function(col = "red", file = "/tmp/ball.png", width = 40, height = width, radius = .43)
{
  makeIcon( function()
               symbols(1, 1, circle = radius, bg = col, inches = FALSE, add = TRUE),
            file, width, height)
}

makeRectIcon =
function(col = "red", file = "/tmp/rect.png", width = 40, height = width, radius = 1)
{  
  makeIcon(function()
              symbols(1, 1, square = radius, bg = col, inches = FALSE, add = TRUE),
           file, width, height)
}

makePchIcon =
function(pch = "red", col = "red", file = sprintf("pch_%s.png", as.character(pch)), cex = 4, width = 40, height = width, radius = 1)
{  
  makeIcon(function()
              points(1, 1, pch = pch, col = col, cex = cex),
            file, width, height)
}

makeIcon =
function(fun, file = "/tmp/ball.png", width = 40, height = width)
{  
  png(file, width, height, bg = "transparent")
  par(oma = c(0, 0, 0, 0), mar = rep(0, 4))
  plot(1, 1, axes = FALSE, type = "n")
  fun()

  dev.off()
  file
}


