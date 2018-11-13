#helpers

# #plot a map helper
# lights<<-readRDS('./data/lightFullCountry.rds')
# map<<-readRDS("./data/moz_d.rds")
# locations<<-readRDS('./data/distcenters.rds')
# summary_data<<-readRDS('./data/summary_data.rds')
# source('./helpers.R')


#Filter Summary
filter_summary <-function(data, province, district.type, branches, branchLo, branchHi, density, densityHi,densityLo, pop.level, popHi, popLo){ 
  
  out<-data
  if(province !='All'){
    out<-out %>% filter(Province == province)
  } 
  if(district.type != 'All'){
   out<- out  %>% filter(District.Type == district.type)  
  }
  if(branches ==TRUE){
    out<- out  %>% filter(Number.Bank.Branches >= branchLo , Number.Bank.Branches < branchHi)  
  }
  if(density ==TRUE){
    out<- out  %>% filter(Population.Density >= densityLo , Population.Density < densityHi)  
  }
  if(pop.level ==TRUE){
    out<- out  %>% filter(Population.Level >= popLo , Population.Level < popHi)  
  }
  return(out)  
}

#Summary Plot
summary_plot<-function(data) {
  #plot summaries data 
  ggplot(data=data, aes(y=District, x=Average.Light.Intensity)) +
    geom_point()+
    theme_economist()+
    scale_colour_economist(guide=FALSE) +
    labs(x="Light Intensity",y='District') +
    guides(size=FALSE) #+
    #theme(text = element_text(size = 20))
}

#Scatterplot
scatter_plot<-function(data) {
ggplot(data, aes(x=Population.Level, y=Population.Density, size=Average.Light.Intensity, label=District)) +
  geom_point() +
  geom_text(aes(label=District), hjust=0.5, vjust=1.5, size=4) +
  guides( colour=FALSE)+
  theme_economist()
}

plotOptimisedMap<-function(map, lights, name, gamma, numBranches){
  #Extract region and plot
  #moz.d<-readRDS("./data/moz_d.rds")
  #hiResLights<-readRDS('./data/lightFullCountry.rds')
  
  #Crop Map
  district_map<-(map[map$NAME_2==name,])
  e=extent(district_map) 
  r=crop(lights,e) 
  p = rasterToPoints(r)
  df = data.frame(p)
  colnames(df) = c("lon", "lat", "Nightlight.Intensity")
  df_plot<- df %>% filter(Nightlight.Intensity <=10)
  totalLights<-sum(df$Nightlight.Intensity)
  
  #Kmeans cluster
  kmeans<-as.data.frame(kmeansW(p[,1:2], numBranches, weight = exp(gamma*p[,3]) )$centers)
  
  #GGplot
  g = ggplot(data=df_plot) + geom_point(aes(x=lon, y=lat, color=Nightlight.Intensity)) +
    #theme_bw() + 
    geom_polygon(data=district_map,aes(x=long,y=lat, group=group), color='white', fill=NA) +
    theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())+
    geom_point(data=kmeans, aes(x=kmeans$x, y=kmeans$y), color='red', size=5)
   
  returnList<-list()
  returnList$g<-g
  returnList$coords<-data.frame('lng' = kmeans$x, 'lat' = kmeans$y)
  returnList$raster<-p
  returnList$totalLights<-totalLights
  returnList$df<-df
  return(returnList)
  #plot(r)
  #plot(map, add=T)  
}

estimateCoverage = function(totalLights, coords, radius, lights_df){
  ratios<-list()
  
  #Iterate through each branch
  for(i in 1: dim(coords)[1]){
    #make circle around the center of 10km
    circle<-destPoint(coords[i,], b=1:365, d=radius*1000)
    #filter the lights to the circle
    circle_df<-lights_df %>% filter(lon < max(circle[,1]) & lon > min(circle[,1]) & lat < max(circle[,2]) & lat > min(circle[,2]))
    #Calc relative light intensity
    lightInCircle<-sum(circle_df$Nightlight.Intensity)
    ratios[[i]]<-lightInCircle / totalLights
  }
  return(unlist(ratios))
}

distanceCalculator = function(coords){

  
}

# #Spatial Kernel

# spatialKernel = function(centroid, width){

#  read in centroids of banks
#  fit a circle around the centers
#  calculate euclidean distance of all pixels
#  transform them to convex function
#  multiply them by light reading
#  Sum the weighted light intensities


# }














#Set default to All
#work on population data 
#work on other inputs
#Work on Maps...
#Turn Scales to percentiles
#Buttons for binary classifications
#Text size larger

#Plot Mao Highlighting the chosen region

#Plotting Feature for maps