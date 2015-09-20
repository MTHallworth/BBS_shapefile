#'Shapefiles for Country and State Borders
#'
#'@param \code{Countries} logical defaults to \code{TRUE},
#'@param \code{States} logical defaults to \code{FALSE}
#'@return a shapefile of either world countries, United States with state boundaries or a list containing both. 
#'Both maps have a North America Albers Equal Area Conic projection.
#'@usage \code{readBorders(Countries=TRUE,States=FALSE)}
#'@export

readBorders<-function(NorthAmerica=TRUE,Countries=FALSE,States=FALSE){
  if(NorthAmerica==TRUE & Countries==FALSE & States==FALSE){
    northam<-raster::shapefile(system.file("extdata","northamerica.shp",package="BBSshapefile"))
    return(northam)
  }
  if(NorthAmerica==FALSE & Countries==TRUE & States==FALSE){
    world<-raster::shapefile(system.file("extdata","world.shp",package="BBSshapefile"))
    return(world)
  }
  if(NorthAmerica==FALSE & Countries==FALSE & States==TRUE){
    states<-raster::shapefile(system.file("extdata","states.shp",package="BBSshapefile"))
    return(states)
  }
  if(NorthAmerica==FALSE & Countries==FALSE & States==FALSE){
    print("Error: no input value - please specify which shapefile you would like North America, Countries or States")
  }
  if(NorthAmerica==TRUE & Countries==TRUE & States==TRUE){
    shapes<-vector('list',3)
    shapes[[1]]<-raster::shapefile(system.file("extdata","northamerica.shp",package="BBSshapefile"))
    shapes[[2]]<-raster::shapefile(system.file("extdata","world.shp",package="BBSshapefile"))
    shapes[[3]]<-raster::shapefile(system.file("extdata","states.shp",package="BBSshapefile"))
    names(shapes)<-c("NorthAmerica","Countries","States")
    return(shapes)
  }
  if(NorthAmerica==TRUE & Countries==TRUE & States==FALSE){
    shapes<-vector('list',2)
    shapes[[1]]<-raster::shapefile(system.file("extdata","northamerica.shp",package="BBSshapefile"))
    shapes[[2]]<-raster::shapefile(system.file("extdata","world.shp",package="BBSshapefile"))
    #shapes[[3]]<-raster::shapefile(system.file("extdata","states.shp",package="BBSshapefile"))
    names(shapes)<-c("NorthAmerica","Countries")
    return(shapes)
  }
  if(NorthAmerica==TRUE & Countries==FALSE & States==TRUE){
    shapes<-vector('list',2)
    shapes[[1]]<-raster::shapefile(system.file("extdata","northamerica.shp",package="BBSshapefile"))
    #shapes[[2]]<-raster::shapefile(system.file("extdata","world.shp",package="BBSshapefile"))
    shapes[[2]]<-raster::shapefile(system.file("extdata","states.shp",package="BBSshapefile"))
    names(shapes)<-c("NorthAmerica","States")
    return(shapes)
  }
  if(NorthAmerica==FALSE & Countries==TRUE & States==TRUE){
    shapes<-vector('list',2)
    #shapes[[1]]<-raster::shapefile(system.file("extdata","northamerica.shp",package="BBSshapefile"))
    shapes[[1]]<-raster::shapefile(system.file("extdata","world.shp",package="BBSshapefile"))
    shapes[[2]]<-raster::shapefile(system.file("extdata","states.shp",package="BBSshapefile"))
    names(shapes)<-c("Countries","States")
    return(shapes)
  }
}
