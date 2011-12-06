kmlFormula =
  #
  #   ~ long + lat | g
  #   ~ long + lat
  #   mean(x) ~ long + lat | g
  #   ~ long + lat @ time | groups
  #   ~ f(long) + g(lat) @ time | groups  
  #   a + b ~ long + lat @ time | groups
  #   log(a) + sqrt(b) ~ long + lat @ time | groups

  #
  #  kmlFormula( ~ - long + log(lat) @ time)
  #  kmlFormula( ~ - long + (2*log(lat)) @ time)

  # kmlFormula( ~ - long + (2 * log(lat)) @ I( sqrt(time)))
  # kmlFormula( ~ - long + (2 * log(lat)) @ I( sqrt(time)) | groups)
  # kmlFormula( ~ (- long) + (2 * log(lat)) @ I( sqrt(time)) | groups)  

  # These cause problems. Don't separate the time
  #  kmlFormula( ~ - long + 2*log(lat) @ time)
  #     But this is okay kmlFormula( ~ - long + (2*log(lat)) @ time)
  #
  # kmlFormula( plot(bsqft, price, type = "l") ~ long + lat)
  #
  
function(f, data = NULL)
{
  if(!inherits(f, "formula") || as.character(f[[1]]) != "~")
    stop("Not a recognized formula")

  ans = list()
  
  if(length(f) == 2) {
     if(as.character(f[[2]][[1]]) == '|') {
       ans$LongLat = getLongLat(f[[2]][[2]])
       ans$groups = f[[2]][[3]]
     } else
       ans$LongLat = getLongLat(f[[2]])
   } else if(length(f) == 3 && as.character(f[[2]][[1]]) != '|') { # what is the second test doing?
     # ans$RHS = f[[2]]
     ans$RHS = f[[3]]
     ans$LHS = f[[2]]

      tmp = if(as.character(f[[3]][[1]]) == "|")
              f[[3]][[2]]
            else
              f[[3]]
      ans$LongLat = getLongLat(tmp)   # was f[[3]][[2]]
         # If there is a |, then we have a group
      if(as.character(f[[3]][[1]]) == '|') 
        ans$groups = f[[3]][[3]]
   }

  if("time" %in% names(ans$LongLat)) {
    tt = ans$LongLat$time
    ans$LongLat = ans$LongLat[ names(ans$LongLat) != "time"]
    if(is.call(tt) && tt[[1]] == "I") {
       if(is.call(tt[[2]]) && tt[[2]][[1]] == "-")
          tt  = TimeSpanSpec(tt[[2]][[2]], tt[[2]][[3]], tt[[2]])
       else
         tt = new("TimeSpec", orig = tt[[2]])
    } else
      tt = new("TimeSpec", orig = tt)
    ans$time = tt
  }

  ans$formula = f
  structure(ans, class = "KMLFormula")
}

setClass("TimeSpec", representation(orig = "language"))
setClass("TimeSpanSpec", representation(end = "language", start = "language"), contains = "TimeSpec")

setClass("TimeSpan", contains = "data.frame")

tmp = function(x, i, j, ...) {
              new("TimeSpan", lapply(x, function(a) a[i]))
           }
setMethod("[", c("TimeSpan", "logical", "missing"), tmp)
setMethod("[", c("TimeSpan", "numeric", "missing"), tmp)
setMethod("[", c("TimeSpan", "integer", "missing"), tmp)

  # Target class for converting time info to KML format.
setClass("KMLTime", contains = "character")

setAs("TimeSpan", "KMLTime",
       function(from) {
          sprintf("<TimeSpan><begin>%s</begin><end>%s</end></TimeSpan>", from[[1]], from[[2]])
       })

tmp = function(from) sprintf("<TimeStamp><when>%s</when></TimeStamp>", from)
setAs("POSIXt", "KMLTime", tmp)
setAs("character", "KMLTime", tmp)





TimeSpanSpec =
function(end, start, expr)
{
  new("TimeSpanSpec", end = end, start = start, orig = expr)
}



getLongLat =
  #
  # For parsing the formula. Internal function.
  #
function(term)
{
#     term = term[[2]]
     if(length(term) == 3) {
       ans = list(long = term[[2]], lat = term[[3]])
       for(i in seq(along = ans)) {
         el = ans[[i]]
         if(!inherits(el, "name") &&  as.character(el[[1]])[1] == '@') {   # should this be as.character()[1]

           if(is.call(el[[1]])) {
                  # this is for the case of long + lat @ I( expr )
             fun = el[[1]][[3]]
                # or o = new("call"); o[[1]] = fun; o[[2]] = el[[2]]
             o = call("xx", el[[2]])
             o[[1]] = fun
             ans$time = o
             ans[[i]] = el[[1]][[2]]
           } else {
              ans$time = ans[[i]][[3]]
              ans[[i]] = ans[[i]][[2]]
           }
         }
       }
       ans
     } else 
       stop("what sort of formula is that?")
}
