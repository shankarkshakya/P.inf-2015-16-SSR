rm(list = ls())
library(ggplot2)
library(raster)
library(gridExtra)
library(grid)
library(maptools)

mx <- readShapePoly("mexstates/mexstates.shp")
mystates <- mx[grep("Mexico|Tlaxcala|Michoacan", mx@data$ADMIN_NAME),]

states <- coordinates(mystates)
states <- data.frame(states)
states$label <- mystates@data$ADMIN_NAME

site <- read.csv("Dgeo_MX.csv", header = TRUE)
colnames(site) <- c("label", "X2", "X1")
states <- rbind(states, site[c(3,1,2)])
states <- states[4:9, ]
colnames(states)[3] <- "Sites"

pol <- data.frame(xmin=-103.7455,xmax=-97.61348 ,ymin=17.92083 ,ymax=20.39417)

#mystates <- fortify(mystates, region = "ADMIN_NAME")
mystates <- fortify(mystates)

colnames(mystates)[6] <- "States"
head(mystates)

mystates$States[mystates$States == 18] <- "Michoacan"
mystates$States[mystates$States == 19] <- "Central Mexico"
mystates$States[mystates$States == 20] <- "Tlaxcala"

mycol <- rainbow(6)
mycol[2] <- "#000000"


statemap <- ggplot() + 
  geom_polygon(data=mystates, aes(x = long, y = lat, group=group, fill = States)) +
  coord_equal() + 
  theme_bw() + labs(x="Longitude", y = "Latitude") +
  geom_point(data = states, aes(x=X1, y=X2, color = Sites), size = 1.9) + 
  scale_color_manual(values = mycol) +
  theme(axis.text = element_text(size = 10, face = "bold", family = "Microsoft Sans Serif", colour = "black")) +
  theme(axis.title = element_text(size = 10, face = "bold", family = "Microsoft Sans Serif")) +
  theme(text = element_text(size =10, face = "bold", family = "Microsoft Sans Serif"))

#statemap <- statemap + coord_fixed(ratio = 2)

ggsave(statemap, height = 9, width = 7, filename = "statemap.png", )

ggsave("statemap.tiff", plot = last_plot(), width = 7, height = 8, dpi = 600)


mx_map <- ggplot()+
  geom_polygon(data=mx, aes(long,lat,group=group),colour="grey10",fill="#fff7bc") +
  coord_equal()+
  theme_bw()+labs(x="Longitude",y = "Latitude") + 
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype = 1) 

ggsave(mx_map, height = 3, width = 4, filename = "mxmap.png")

# 
# maptheme <- theme(
#   #axis.text = element_blank(),
#   axis.ticks = element_blank(),
#   panel.grid = element_blank(),
#   panel.border = element_rect(fill = NA, colour = "black"),
#   panel.background = element_blank()
# )
# 
# grid.mystatespage()
# v1 <- viewport(width = 0.5, height =1, x = 0.5, y = 0.5)  # the larger map
# v2 <- viewport(width = 0.3, height = 0.2, x = 0.58, y = 0.76)  # the inset in upper left
# print(statemap + maptheme, vp = v1)
# print(mx_map + maptheme, vp = v2)



library(cowplot)
p <- ggdraw() +
  draw_plot(statemap, 0, 0.1, 1, 1) +
  draw_plot(mx_map,  .4, 0, .3, .2) 

+
  draw_plot_label(c("A", "B"), c(0, 0), c(1, 0.5), size = 15)

ggsave(p, filename = "combine.pdf", height = 8, width = 8)



grid.mystatespage()
pushViewport(viewport(layout = grid.layout(2, 2)))
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
}

print(statemap, vp = define_region(1, 1:2))
print(mx_map, vp = define_region(2,1))











