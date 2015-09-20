#'Mean Annual H2 in precipitation - raster
#'
#'@param \code{NorthAmerica} logical defaults to \code{TRUE},
#'@param \code{Globe} logical defaults to \code{FALSE}
#'@return a raster containing the mean annual H2 in precipitation for North America, the Globe or a list containing both. 
#'Both maps have a North America Albers Equal Area Conic projection.
#'@usage \code{getH2map(NorthAmerica=TRUE,Globe=FALSE)}
#'@export
getH2map<-function(NorthAmerica=TRUE,Globe=FALSE){
  if(NorthAmerica==TRUE & Globe==FALSE){
    northam<-raster::raster(system.file("extdata","mad_northam.txt",package="BBSshapefile"))
    northam<-raster::readAll(northam)
    return(northam)
  }
if(NorthAmerica==FALSE & Globe==TRUE){
  world<-raster::raster(system.file("extdata","mad_globe.txt",package="BBSshapefile"))
  #world<-suppressWarnings(raster::projectRaster(world,crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  return(world)
}
  if(NorthAmerica==TRUE & Globe==TRUE){
    shapes<-vector('list',2)
    shapes[[1]]<-raster::raster(system.file("extdata","mad_northam.txt",package="BBSshapefile"))
    shapes[[2]]<-raster::raster(system.file("extdata","mad_globe.txt",package="BBSshapefile"))
    #shapes[[2]]<-suppressWarnings(raster::projectRaster(shapes[[i]],crs="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    names(shapes)<-c("NorthAmerica","Globe")
    return(Shapes)
  }
}