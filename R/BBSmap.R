#'Read in BBS Relative Abundance Shapefile
#'
#'@param SpeciesCode = x - vector of species alpha codes
#'@return returns a shapefile with relative abundance information; 
#'if more than one alpha code is supplied returns a list of shapfiles named
#'with the alpha code
#'@usage data(Birds)
#'@export
BBSmap<-function(SpeciesCode){
  x<-SpeciesCode
  y<-destfile<-exdir<-rep(NA,length(x))  
  shapes<-vector('list',length(x))
  for(i in 1:length(x)){
    if(x[i] %in% Birds[,4]==FALSE) print("Alpha Code not in list")
    y[i]<-sprintf("%05d",Birds[grep(Birds[,4],pattern=x[i]),2])   
    # Download the shapefile to working directory with name CODE.zip#
    destfile[i]<-paste(getwd(),"/",x[i],".zip",sep="")
    download.file(paste("http://www.mbr-pwrc.usgs.gov/bbs/ra12/ra",y[i],".zip",sep=""),destfile=destfile[i],mode="wb")
    exdir[i]<-paste(getwd(),"/",x[i],"_unzip",sep="")
    unzip(paste(x[i],".zip",sep=""),exdir=exdir[i])
    shapes[[i]]<-raster::shapefile(paste(exdir[i],"/","ra",y[i],".shp",sep=""))
  }
  names(shapes)<-c(x)
  if(length(shapes)==1) return(shapes[[1]]) else return(shapes)
  
}
