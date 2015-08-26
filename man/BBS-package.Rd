\name{BBS_shapefile-package}
\alias{BBS_shapefile-package}
\alias{BBS_shapefile}
\docType{package}
\title{
BBS
}
\description{
Reads in Relative Abundance data as either a raster or shapefile
}
\details{
\tabular{ll}{
Package: \tab BBS_shapefile\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-07-30\cr
License: \tab What license is it under?\cr
}
BBSmap - downloads data from USGS and reads in as raster file (default)
}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}

\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
Ovenbird<-BBSmap(SpeciesCode="OVEN",MakeRaster=TRUE) # creates a raster of OVEN relative abundance #

Ovenbird_shp<-BBSmap(SpeciesCode="OVEN",MakeRaster=FALSE) # returns spatialpolygondataframe (see raster package)
}
