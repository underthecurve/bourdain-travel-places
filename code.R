library('dplyr')
library('readr')
library('ggplot2')
library('reshape2')
library('RColorBrewer')

places <- read_csv('bourdain_travel_places.csv')
lat.long <- colsplit(places$coordinates, ",", names = c("lat", "long"))
places <- cbind(places, lat.long)
places <- places %>% select(-coordinates)

## World Map

mdat <- map_data('world')

gmap <- ggplot() + 
  geom_polygon(dat = mdat, 
               aes(long, lat, group = group), fill="#e3e3e3") + 
  borders("world", colour = "white", size=0.1)

gmap.points <- gmap + 
  geom_point(data = places, 
             aes(long, 
                 lat, colour = show, alpha = 0.75), size = 1) + 
  scale_color_manual(values = c('#000000', '#e41a1c', 'deepskyblue3')) + 
  ggtitle('"If I am an advocate for anything, it is to move."') +
  labs(subtitle = "–Anthony Bourdain")

gmap.formatted <- gmap.points + 
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = 'bottom', 
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        legend.key = element_rect(fill=NA),
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) + 
  guides(alpha = FALSE)

gmap.formatted

ggsave('bourdain_travel_map.png', width = 6, height = 4.5)


## US Map with states - to be added










