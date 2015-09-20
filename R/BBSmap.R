#'Read in BBS Relative Abundance Shapefile
#'
#'@param \code{SpeciesCode} = x - vector of species alpha codes,
#'@param \code{MakeRaster} defaults = \code{TRUE}
#'@param \code{RelAbun} default = \code{FALSE}
#'@param \code{SnapToH2} default = \code{FALSE}
#'@return a shapefile or raster with relative abundance information; 
#'if more than one alpha code is supplied returns a list of shapfiles named
#'with the alpha code
#'@usage \code{BBSmap(SpeciesCode="Species Alpha Code", MakeRaster=TRUE,RelAbun=FALSE,res=10000,SnapToH2=FALSE)}
#'@export
BBSmap<-function(SpeciesCode,MakeRaster=TRUE,RelAbun=FALSE,SnapToH2=FALSE){
  data(Birds)
  x<-SpeciesCode
  y<-destfile<-exdir<-rep(NA,length(x))
  WGS84<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  Lambert<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  if(SnapToH2==TRUE){
          print("Reading in Mean Annual Deuterium Raster")
          MAD<-raster::raster(system.file("extdata","mad_northam.txt",package="BBSshapefile"))
          #MAD<-suppressWarnings(raster::projectRaster(MAD,crs=Lambert))
          }
  shapes<-extentShape<-rastShape<-rastEmpty<-RelAbundance<-snapRast<-vector('list',length(x))
  for(i in 1:length(x)){
    if(x[i] %in% Birds[,4]==FALSE) print("Alpha Code not in list")
    y[i]<-sprintf("%05d",Birds[grep(Birds[,4],pattern=x[i]),2])
    # Download the shapefile to working directory with name CODE.zip#
    destfile[i]<-paste(getwd(),"/",x[i],".zip",sep="")
    download.file(paste("http://www.mbr-pwrc.usgs.gov/bbs/ra12/ra",y[i],".zip",sep=""),destfile=destfile[i],mode="wb")
    exdir[i]<-paste(getwd(),"/",x[i],"_unzip",sep="")
    unzip(paste(x[i],".zip",sep=""),exdir=exdir[i])
    shapes[[i]]<-raster::shapefile(paste(exdir[i],"/","ra",y[i],".shp",sep=""))
    shapes[[i]]<-sp::spTransform(shapes[[i]],CRS(WGS84))
    names(shapes)<-c(x)
    if(MakeRaster==FALSE){break} else {
    extentShape[[i]]<-raster::extent(shapes[[i]])
    rastEmpty[[i]]<-raster::raster(xmn=extentShape[[i]][1],
                                   xmx=extentShape[[i]][2],
                                   ymn=extentShape[[i]][3],
                                   ymx=extentShape[[i]][4], 
                                   crs=(WGS84),
                                   resolution=c(0.333333,0.333333)) #10km res
    print(paste("Creating Raster for",x[i],sep=" "))
    rastShape[[i]]<-raster::rasterize(shapes[[i]],rastEmpty[[i]],shapes[[i]]$RASTAT)
    } 
    names(rastShape)<-c(x)
    if(RelAbun==FALSE){break} else {
      print(paste("Creating Probability surface for",x[i],sep=" "))
      RelAbundance[[i]]<-raster::calc(rastShape[[i]],fun=function(x){x/raster::cellStats(rastShape[[i]],sum)})
    names(RelAbundance)<-c(x)
      }
    if(SnapToH2==FALSE){break} else{
      print(paste("Snapping probability surface of",x[i],"to deuterium map",sep=" "))
      #snapRast[[i]]<-raster::mask(MAD,rastShape[[i]])
      snapRast[[i]]<-raster::resample(RelAbundance[[i]],MAD,method="bilinear",res=c(0.333333,0.333333))
      snapRast[[i]]<-extend(snapRast[[i]],MAD)
      snapRast[[i]]<-mask(snapRast[[i]],MAD)
      names(snapRast)<-c(x)
      }
      }
  if(MakeRaster==FALSE){
         if(length(shapes)==1){ return(shapes[[1]])}
            else {return(shapes)}
     }
  if(MakeRaster==TRUE & RelAbun==FALSE & SnapToH2==FALSE){
    if(length(rastShape)==1){ return(rastShape[[1]])}
    else {return(rastShape)}
  }
  if(MakeRaster==TRUE & RelAbun==TRUE & SnapToH2==FALSE){
    if(length(RelAbundance)==1){ return(RelAbundance[[1]])}
    else {return(RelAbundance)}
  }
  if(MakeRaster==TRUE & RelAbun==TRUE & SnapToH2==TRUE){
    if(length(snapRast)==1){ return(snapRast[[1]])}
    else {return(snapRast)}
  }
}


