\name{kmlFormula}
\alias{kmlFormula}

\alias{[,TimeSpan,integer,missing-method}
\alias{[,TimeSpan,logical,missing-method}
\alias{[,TimeSpan,numeric,missing-method}

\title{Formula language for KML displays}
\description{
  The \code{\link{kml}} function allows the caller
  to use a formula to describe the different aspects of the
  KML display to be created.
  The idea is the formula allows us to identify the variables
  in the data frame or even expressions that generate the
  variables that we will use for longitude and latitude and
  also for grouping the placemarks into separate folders
  and also for specifying the time stamp.

  We use the standard formula language in R and then add a simple
  extension to it that adds time.
  We use the '|'  symbol at the end of the formula to break
  the data into groups, e.g. magnitudes of earthquakes.
  We use the @ symbol to provide an expression that specifies
  time for each observation.
  
% This does actually work!  
% One cannot (easily) access S4  slots within a KML formula
% as it may get confused for the time specification. One can
% enclose these within calls such as\code{I(x@slot)}.

  The \code{kmlFormula} function parses the specified formula
  and returns an object containing the separate elements
  including the longitude and latitude, and the groups and
  time expressions.
}
\usage{
kmlFormula(f, data = NULL)
}
\arguments{
  \item{f}{a formula object}
  \item{data}{the optional data frame in which the terms will eventually
         be evaluated, but not in this function}
}
\details{
}
\value{
  A list with an element named LongLat giving the
  expressions or symbols identifying the longitude and latitude.
  If the formula has a time or groups component, the result will
  have corresponding elements.
}
\author{
Duncan Temple Lang
}

\seealso{
 \code{\link{kml}}
}
\examples{

kmlFormula(  ~ long + lat | g)
kmlFormula(  ~ long + lat)
kmlFormula(  mean(x) ~ long + lat | g)
kmlFormula(  ~ long + lat @ time | groups)
kmlFormula(  ~ f(long) + g(lat) @ time | groups  )
kmlFormula(  a + b ~ long + lat @ time | groups)
kmlFormula(  log(a) + sqrt(b) ~ long + lat @ time | groups)


kmlFormula( ~ - long + log(lat) @ time)
kmlFormula( ~ - long + (2*log(lat)) @ time)

kmlFormula( ~ - long + (2 * log(lat)) @ I( sqrt(time)))
kmlFormula( ~ - long + (2 * log(lat)) @ I( sqrt(time)) | groups)
kmlFormula( ~ (- long) + (2 * log(lat)) @ I( sqrt(time)) | groups)

 # Using an S4 slot works, but looks funny with two @'s being
 # interpreted differently !.  Don't rely on this.
kmlFormula( ~ (- long) +  data@lat @ I( sqrt(time)) | groups)  

 # Note that this one gives the "wrong" answer, not identifying the time
 #  So we need the latitude term to be in (), i.e. (2 * log(lat)) @ I(sqrt(time))
kmlFormula( ~ (- long) + 2 * log(lat) @ I( sqrt(time)) | groups)  

 # This version specifies a range of time. This would use a time span
 # for each point based on the expression.
f = kmlFormula(~ 2*long + log(lat) @ I( end - start) | geogroups)
g = kmlFormula(~ 2*long + log(lat) @ I(2*time) | geogroups)

  # This one shows how we can have an expression on the right hand side
  # This would draw lattice plots  - each made up of different panels
  # base on the levels of 'groups' within each data.frame/subgroup identified by
  # geogorups -
  # 
kmlFormula(xyplot(ht ~ wt | groups) ~ 2*long + log(lat) @ I( end - start) | geogroups)


    ##################
    # Illustrates evaluating the terms, not just parsing them.
  data(USCities)
  caCities = subset(USCities, name \%in\% c("Sacramento", "SanFrancisco", "LosAngeles"))[c(1, 2, 4),]

  doc = kml(~ - longitude + latitude, caCities, addLines = TRUE)
  saveXML(doc, "lines.kml")



 #
  kmlFormula(xyplot( y ~ x) ~ long + lat @ time | condition)
  kmlFormula(boxplot( ~ x) ~ long + lat @ time | condition)
}
\keyword{plot}

