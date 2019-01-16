library(tidyverse)
library(rgeos)
library(scales)
library(rworldmap)
library(reshape2)
library(scatterpie)
library(RColorBrewer)

## Colored Mapping
## Dataset for countries' longitude and latitude
world <- map_data("world")
## Raw data variable 'countries' is separated by comma
Plotmap <- (Dataset) %>% 
  mutate(`Country(ies)` = strsplit(`Country(ies)`, ", ")) %>% 
  unnest(`Country(ies)`) %>%
  filter(!is.na(`Country(ies)`)) %>%
  group_by(`Country(ies)`) %>%
  summarise(count=n()) %>%
  left_join(world, by=c("Country(ies)"="region")) %>%
  rename("region"=`Country(ies)`)
world_unique <- data.frame(region=unique(world$region)) %>%
  arrange(region)
ggplot() +
  geom_map(data=world, map=world, aes(long, lat, map_id=region), fill="white", color="#f2f2f2", fill=TRUE,
           lwd=0.4) +
  geom_map(data=Plotmap, map=Plotmap, aes(long, lat, map_id=region, fill=count), color="#f2f2f2",
           lwd=0.05) +
  scale_fill_gradientn(colours = c('#F9E53F', '#7FD157', '#2A8A8C', '#404E88', '#461863')
                       ,values = scales::rescale(c(0,10,50,250,500))
                       ,labels = comma
                       ,breaks = c(0,10,50,250,500)
  ) +
  labs(fill="Label", y="", x="") +
  ggtitle("") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(0, 0, -2, 0), "cm"))


## Pie chart on map
## central point for pie chart, may could get a better dataset
centroids <- as.data.frame(gCentroid(getMap(resolution="high"), byid=TRUE))
centroids$region <- rownames(centroids)
centroids <- arrange(centroids, region)
Gendermap <- Datajoin %>% 
  mutate(`Country(ies)` = strsplit(`Country(ies)`, ", ")) %>% 
  unnest(`Country(ies)`) %>%
  filter(!is.na(`Country(ies)`)) %>%
  melt(id.vars = c("Author", "Country(ies)"), 
       measure.vars = c("Man", "Woman", "Trans", "Genderqueer", 
                        "Agender", "OtherGender")) %>%
  filter(value==1) %>%
  group_by(`Country(ies)`, variable) %>%
  summarise(count=n()) %>%
  dcast(`Country(ies)` ~ variable) %>%
  mutate(Man=replace(Man, is.na(Man), 0), Woman=replace(Woman, is.na(Woman), 0),
         Trans=replace(Trans, is.na(Trans), 0), 
         Genderqueer=replace(Genderqueer, is.na(Genderqueer), 0),
         OtherGender=replace(OtherGender, is.na(OtherGender), 0),
         Total=Man+Woman+Trans+Genderqueer+OtherGender) %>%
  left_join(centroids, by=c("Country(ies)"="region")) %>%
  rename("region"=`Country(ies)`) %>%
  filter(Total>10)
ggplot() +
  geom_map(data=world, map=world, aes(long, lat, map_id=region), fill="white",
           color="azure4", lwd=0.2, fill=TRUE) +
  geom_scatterpie(data = Gendermap, 
                  aes(x, y, r = log(Total)*2),
                  cols = c("Man", "Woman", "Trans", "Genderqueer", "OtherGender"), 
                  alpha = 0.5) +
  scale_fill_manual(
    breaks = c("Man", "Woman", "Trans", "Genderqueer", "OtherGender"),
    labels = c("Man", "Woman", "Trans", "Genderqueer", "OtherGender"),
    values = c("Man" = "orange",
               "Woman" = "white",
               "Trans" = "black",
               "Genderqueer" = "cyan",
               "OtherGender" = "red")
  ) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(0.96, 0.2), legend.justification = c(1, 0),
        panel.grid = element_blank(), panel.border = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(size=10)) +
  labs(fill="Gender") +
  ggtitle("")