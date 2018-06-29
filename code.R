library('dplyr')
library('readr')
library('ggplot2')
library('reshape2')
library('ggalt')
library('rgdal')
library('maptools')

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

# a different projection and look, via https://cfss.uchicago.edu/dataviz_geospatial.html#geospatial_visualization
gmap.formatted + 
  coord_map(projection = "polyconic") # too crazy-looking

gmap.formatted + 
  coord_map(projection = "mollweide", xlim=c(-180, 180)) # better 

ggsave('bourdain_travel_map_mollweide.png', width = 6, height = 4.5)

# robinson projection via https://gis.stackexchange.com/questions/44387/use-proj4-to-specify-robinson-projection-with-r-ggmap-and-ggplot2-packages
# see also https://www.jessesadler.com/post/gis-with-r-intro/
gmap.formatted + coord_proj("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

ggsave('bourdain_travel_map_robinson.png', width = 6, height = 4.5)

## US Map with states via https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
# code adapted from https://cfss.uchicago.edu/dataviz_geospatial.html#using_shapefiles

usa <- readOGR("cb_2017_us_state_20m/cb_2017_us_state_20m.shp")

fortify(usa) %>%
  head()

gpclibPermit()

usa2 <- usa %>%
    fortify(region = "NAME") %>%
    as_tibble() %>%
    left_join(usa@data, by = c("id" = "NAME"))

usamap <- ggplot(data = usa2 %>% filter(id != "Alaska", id != "Hawaii", id != "Puerto Rico"), mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "#e3e3e3")

# add in A Cook's Tour locations, which Shane Turbeville added to my original dataset: https://public.tableau.com/profile/shanetville#!/vizhome/BourdainsTravels/BourdainsTravels

cooks.tour <- read_csv('Map_data.csv')

cooks.tour <- cooks.tour %>% filter(Show == "A Cook's Tour")

usamap.points <- usamap + 
  geom_point(data = cooks.tour %>% filter(Country == "United States"), 
             aes(Longitude, 
                 Latitude, colour = Show, alpha = 0.75, group = Country), size = 2) + 
  geom_point(data = places %>% filter(!is.na(state) & state != "Hawaii"), 
             aes(long, 
                 lat, colour = show, alpha = 0.75, group = country), size = 2) +
  scale_color_manual(values = c('#264484', '#000000', '#e41a1c', 'deepskyblue3')) +
  ggtitle("Anthony Bourdain's U.S. travels") 

usamap.formatted <- usamap.points + 
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

usamap.formatted

ggsave('bourdain_travel_usa_contiguous.png', width = 6, height = 4.5)

# Hawaii
hawaiimap <- ggplot(data = usa2 %>% filter(id == "Hawaii"), mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "#e3e3e3")

hawaiimap.points <- hawaiimap + 
  geom_point(data = places %>% filter(state == "Hawaii"), 
             aes(long, 
                 lat, colour = show, alpha = 0.75, group = country), size = 2) +
  scale_color_manual(values = c('#000000', '#e41a1c', 'deepskyblue3')) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) + 
  guides(alpha = FALSE)

hawaiimap.points

ggsave('bourdain_travel_hawaii.png', width = 1.5, height = 1.5)

# Puerto Rico
prmap <- ggplot(data = usa2 %>% filter(id == "Puerto Rico"), mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "white", fill = "#e3e3e3")

prmap.points <- prmap + 
  geom_point(data = places %>% filter(grepl("Puerto Rico", city_or_area)), 
             aes(long, 
                 lat, colour = show, alpha = 0.75, group = country), size = 2) +
  scale_color_manual(values = c('#000000', '#e41a1c', 'deepskyblue3')) +
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 14),
        axis.line = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = 'none',
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        plot.background = element_blank()) + 
  guides(alpha = FALSE)

prmap.points

ggsave('bourdain_travel_puertorico.png', width = 1.5, height = 1.5)
