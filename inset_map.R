
library(ggplot2)
library(raster)
library(gridExtra)
library(grid)
library(maptools)

mx <- readShapePoly("mexstates/mexstates.shp")

new <- mx[grep("Mexico|Tlaxcala|Michoacan", mx@data$ADMIN_NAME),]
class(new)

states <- coordinates(new)
states <- data.frame(states)
states$label <- new@data$ADMIN_NAME

pol<-data.frame(xmin=-103.7455,xmax=-97.61348 ,ymin=17.92083 ,ymax=20.39417)

p1<-ggplot()+geom_polygon(data=new, aes(long+0.008,lat-0.005, group=group), fill="#9ecae1")+
  geom_polygon(data=new, aes(long,lat, group=group), colour="grey10",fill="#fff7bc")+
  geom_text(data=states, aes(x=X1, y=X2,label=label), size=3, colour="grey20")+
  coord_equal()+theme_bw()+xlab("")+ylab("")+
  scale_x_continuous(breaks=seq(121.8,122.2, 0.1), labels=c(paste(seq(121.8,122.2, 0.1),"°E", sep="")))+
  scale_y_continuous(breaks=seq(13.2,13.6, 0.1), labels=c(paste(seq(13.2,13.6, 0.1),"°N", sep="")))+
  theme(axis.text.y =element_text(angle = 90, hjust=0.5))

p1

p2<-ggplot()+geom_polygon(data=mx, aes(long,lat,group=group),colour="grey10",fill="#fff7bc")+
  coord_equal()+theme_bw()+labs(x=NULL,y=NULL)+
  scale_x_continuous(breaks=seq(117.5,125, 2.5), labels=c(paste(seq(117.5,125, 2.5),"°E", sep="")))+
  scale_y_continuous(breaks=seq(5,20, 5), labels=c(paste(seq(5,20, 5),"°N", sep="")))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank())
p2


png(file="states.jpeg",w=1800,h=1800, res=300)
grid.newpage()
v1<-viewport(width = 0.5, height = 0.5, x = 0.6, y = 0.6) #plot area for the main map
v2<-viewport(width = 0.5, height = 0.5, x = 0.25, y = 0.28) #plot area for the inset map
print(p1,vp=v1) 
print(p2,vp=v2)
dev.off()

















