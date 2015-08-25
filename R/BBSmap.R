#'Read in BBS Relative Abundance Shapefile
#'
#'@param SpeciesCode = x - vector of species alpha codes
#'@return returns a shapefile with relative abundance information; 
#'if more than one alpha code is supplied returns a list of shapfiles named
#'with the alpha code
#'@usage data(Birds)
#'@export
BBSmap<-function(SpeciesCode,raster=FALSE){
  Birds<-data(NAbirdsList)
  x<-SpeciesCode
  y<-destfile<-exdir<-rep(NA,length(x))  
  shapes<-exentShape<-rastShape<-rastEmpty<-vector('list',length(x))
  for(i in 1:length(x)){
    if(x[i] %in% Birds[,4]==FALSE) print("Alpha Code not in list")
    y[i]<-sprintf("%05d",Birds[grep(Birds[,4],pattern=x[i]),2])   
    # Download the shapefile to working directory with name CODE.zip#
    destfile[i]<-paste(getwd(),"/",x[i],".zip",sep="")
    download.file(paste("http://www.mbr-pwrc.usgs.gov/bbs/ra12/ra",y[i],".zip",sep=""),destfile=destfile[i],mode="wb")
    exdir[i]<-paste(getwd(),"/",x[i],"_unzip",sep="")
    unzip(paste(x[i],".zip",sep=""),exdir=exdir[i])
    shapes[[i]]<-raster::shapefile(paste(exdir[i],"/","ra",y[i],".shp",sep=""))
    extentShape[[i]]<-raster::extent(Shapes[[i]])
    rastEmpty[[i]]<-raster::raster(xmn=extentShape[[i]][1],
                                   xmx=extentShape[[i]][2],
                                   ymn=extentShape[[i]][3],
                                   ymx=extentShape[[i]][4], 
                                   crs=("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"),
                                   resolution=c(10000,10000)) #10km res
    print(paste("Creating Raster for",x[i],sep=" "))
    rastShape[[i]]<-raster::rasterize(shapes[[i]],rasterEmpty[[i]],shapes[[i]]$RASTAT)                                 
  }
  names(shapes)<-c(x)
  if(length(rastShape)==1) return(rastShape[[1]]) else return(rastShape)
  
}
