#'Read in BBS Relative Abundance Shapefile
#'
#'@param SpeciesCode = x - vector of species alpha codes
#'@return returns a shapefile with relative abundance information; 
#'if more than one alpha code is supplied returns a list of shapfiles named
#'with the alpha code
#'@usage data(Birds)
#'@export
BBSmap<-function(SpeciesCode,MakeRaster=TRUE,RelAbun=FALSE,res=10000){
  data(Birds)
  x<-SpeciesCode
  y<-destfile<-exdir<-rep(NA,length(x))  
  shapes<-extentShape<-rastShape<-rastEmpty<-RelAbundance<-vector('list',length(x))
  for(i in 1:length(x)){
    if(x[i] %in% Birds[,4]==FALSE) print("Alpha Code not in list")
    y[i]<-sprintf("%05d",Birds[grep(Birds[,4],pattern=x[i]),2])
    # Download the shapefile to working directory with name CODE.zip#
    destfile[i]<-paste(getwd(),"/",x[i],".zip",sep="")
    download.file(paste("http://www.mbr-pwrc.usgs.gov/bbs/ra12/ra",y[i],".zip",sep=""),destfile=destfile[i],mode="wb")
    exdir[i]<-paste(getwd(),"/",x[i],"_unzip",sep="")
    unzip(paste(x[i],".zip",sep=""),exdir=exdir[i])
    shapes[[i]]<-raster::shapefile(paste(exdir[i],"/","ra",y[i],".shp",sep=""))
    names(shapes)<-c(x)
    if(MakeRaster==FALSE){break} else {
    extentShape[[i]]<-raster::extent(shapes[[i]])
    rastEmpty[[i]]<-raster::raster(xmn=extentShape[[i]][1],
                                   xmx=extentShape[[i]][2],
                                   ymn=extentShape[[i]][3],
                                   ymx=extentShape[[i]][4], 
                                   crs=("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"),
                                   resolution=c(res,res)) #10km res
    print(paste("Creating Raster for",x[i],"with",res/1000,"km cell size",sep=" "))
    rastShape[[i]]<-raster::rasterize(shapes[[i]],rastEmpty[[i]],shapes[[i]]$RASTAT)                                 
    } 
    names(rastShape)<-c(x)
  if(RelAbun==FALSE){break} else {
    print(paste("Creating Probability surface for",x[i],sep=" "))
    RelAbundance[[i]]<-raster::calc(rastShape[[i]],fun=function(x){x/raster::cellStats(rastShape[[i]],sum)})
  }
  }
  if(MakeRaster==FALSE){
                        if(length(shapes)==1){ return(shapes[[1]])}
                        else {return(shapes)}
  }
  if(RelAbun==FALSE){
                        if(length(rastShape)==1){ return(rastShape[[1]])}
                        else {return(rastShape)}
  }
  else{ if(length(RelAbundance)==1){return(RelAbundance[[1]])} else {return(RelAbundance)}
  } 
  }

