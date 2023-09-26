#############################################################
#### Panel F of Figure 1 - Country Summaries             ####
#### Created by: Olivia Smith                            #### 
#### Modified by: Courtney Davis                         ####
#### Last checked: 9 Sep 2023                            ####
#############################################################


############ Load packages
library(here)
library(tidyverse)
library(magrittr)
library(sf)
library(tmap)
library(grid)
library(tidytext)
library(janitor)
library(readxl)

############ Load data
data<-read_xlsx(here("Data","Dataset 4. Who studies bias.xlsx"))
ogdata<- subset(data, Demographic == "Country")


data<-as.data.frame(lapply(ogdata, rep, ogdata$Total))
data$Total<-as.numeric(data$Total)

data$Total<-as.numeric(data$Total)

# rename all uppercase country names to capitalizing only the first letter
data %<>% mutate(name = str_to_title(Demographic.category))

############ Set global options and theme
options(scipen = 10) # use exponential notation for numbers larger than 10 digits 
theme_set(theme_minimal())

# use world basemap from tmap
data("World")
World$name

# change relevant country names for the tmap::World basemap
# we change country names from loaded excel datafile to match names of country of Worldmap
World %<>% mutate(name = as.character(name))
World$name[41] <- "Czech Republic"
World$name[88] <- "South Korea"
World$name[45] <- "Dominican Republic"
World$name[104] <- "North Macedonia"
World$name[136] <- "Russian Federation"
World$name[153] <- "Swaziland (Eswatini)"
World$name[172] <- "Viet Nam"
World$name[161] <- "Viet Nam"
World$name[161] <- "Trinidad And Tobago"

#data$name <- as.factor(data$name)
##remove countries not in the world basemap
data2 <- data %>% filter((name != "Singapore") %>% replace_na(TRUE))
data %<>% filter(name != "Singapore") # Singapore is not included in world basemap, so need to filter out so the geometry works for mapping

data2 <- data %>% filter((name != "Hong Kong") %>% replace_na(TRUE))
data %<>% filter(name != "Hong Kong") # Hong Kong is not included in world basemap, so need to filter out so the geometry works for mapping

data2 <- data %>% filter((name != "Bahrain") %>% replace_na(TRUE))
data %<>% filter(name != "Bahrain")

data2 <- data %>% filter((name != "Barbados") %>% replace_na(TRUE))
data %<>% filter(name != "Barbados")

data2 <- data %>% filter((name != "Malta") %>% replace_na(TRUE))
data %<>% filter(name != "Malta")

#######################################################################################################

# load world map from tmap package
# sf_use_s2(TRUE)
# join world + journal data frames

world_data <- left_join(data, World, by = "name") %>% 
  st_as_sf()

# count journal by country then plot

world_data_count <- world_data %>% 
  count(name, sort = TRUE)

# set tmap mode

tmap_mode("plot")

# plot journal counts by country for world and for Europe
b <- st_bbox(c(xmin = -160, xmax = 165, ymax = 90, ymin = -60), crs = st_crs(4326))
b.eur <- st_bbox(c(xmin = -9, xmax = 43, ymax = 72, ymin = 34), crs = st_crs(4326))

########

data_count <- data %>% 
  count(name, Dataset, sort = TRUE)

# join world + journal data frames

data_count_sf <- left_join(data_count, World, by = "name") %>% 
  st_as_sf()

### Plot individual panels for entire world
# Meta-analysis authors who submitted manuscripts
data_count_sfA<- subset(data_count_sf, Dataset == "Manuscripts submitted" )

tiff(here("Figures","country maps manuscripts submitted.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfA, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "jenks",
          labels = c("1 to 973", "974 to 3,180", "3,181 to 6,139", "6,140 to 12,390", "12,391 to 25,562")) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.005), legend.title.size = 0.44, legend.text.size = 0.375)  
jif.map
dev.off()

# Meta-analysis authors with accepted manuscripts
data_count_sfB<- subset(data_count_sf, Dataset == "Manuscripts accepted" )

tiff(here("Figures","country maps manuscripts accepted.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfB, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "jenks",
          labels = c("1 to 133", "134 to 338", "339 to 621", "622 to 1,020", "1,021 to 3,383")) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.005), legend.title.size = 0.44, legend.text.size = 0.375)  
jif.map
dev.off()

# Authors of studies
data_count_sfC<- subset(data_count_sf, Dataset == "All studies on bias" )

tiff(here("Figures","country maps all studies on bias.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfC, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "pretty") +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.005), legend.title.size = 0.44, legend.text.size = 0.375)  
jif.map
dev.off()

# Authors studied problem
data_count_sfD<- subset(data_count_sf, Dataset == "Studies on bias for dem" )

tiff(here("Figures","country maps studies on bias for dem.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfD, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "pretty") +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.005), legend.title.size = 0.44, legend.text.size = 0.375)  
jif.map
dev.off()

# Authors studied solution
data_count_sfE<- subset(data_count_sf, Dataset == "Studies on solution for dem" )

tiff(here("Figures","country maps studies on solution for dem.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfE, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "pretty") +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.005), legend.title.size = 0.44, legend.text.size = 0.375)  
jif.map
dev.off()





### Plot individual panels for Europe insets
# Meta-analysis authors who submitted manuscripts
data_count_sfA<- subset(data_count_sf, Dataset == "Manuscritps submitted" )

tiff(here("Figures","europe maps manuscripts submitted.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfA, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "fixed",
          breaks = c(1,1001,3001,6001,12001,26001))+
  #labels = c("1 to 973", "974 to 3,180", "3,181 to 6,139", "6,140 to 12,390", "12,391 to 25,562")) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.75), legend.title.size = 0.3, legend.text.size = 0.2)  
jif.map
dev.off()

# Meta-analysis authors with accepted manuscripts
data_count_sfB<- subset(data_count_sf, Dataset == "Manuscripts accepted" )

tiff(here("Figures","europe maps manuscripts accepted.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfB, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "pretty")+
  #labels = c("1 to 133", "134 to 338", "339 to 621", "622 to 1,020", "1,021 to 3,383")) +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01, 0.75), legend.title.size = 0.3, legend.text.size = 0.2)  
jif.map
dev.off()

# Authors of studies
data_count_sfC<- subset(data_count_sf, Dataset == "All studies on bias" )

tiff(here("Figures","europe maps all studies on bias.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfC, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "pretty") +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01, 0.75), legend.title.size = 0.3, legend.text.size = 0.2)  
jif.map
dev.off()

# Authors studied problem
data_count_sfD<- subset(data_count_sf, Dataset == "Studies on bias for dem" )

tiff(here("Figures","europe maps studies on bias for dem.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfD, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "pretty") +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01, 0.75), legend.title.size = 0.3, legend.text.size = 0.2)  
jif.map
dev.off()

# Authors studied solution
data_count_sfE<- subset(data_count_sf, Dataset == "Studies on solution for dem" )

tiff(here("Figures","europe maps studies on solution for dem.tiff"), width = 3.25, height = 1.2, units = 'in', res = 600, compression = 'lzw')
jif.map <- tm_shape(World, bb = b.eur, projection = "+proj=robin") +
  tm_borders(col = "black", lwd = 0.25) +
  tm_shape(data_count_sfE, projection = "+proj=robin") +
  tm_fill(col = "n", title = "# of manuscripts", 
          alpha = 0.75, palette = "viridis", style = "pretty") +
  tm_layout(legend.outside = FALSE, panel.show = F,
            legend.position = c(0.01,0.75), legend.title.size = 0.3, legend.text.size = 0.2)  
jif.map
dev.off()
