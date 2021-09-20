# MUSA 508, Fall 2021
# Lab 2 - Urban Growth Boundaries
# From text Chapter 3 - https://urbanspatial.github.io/PublicPolicyAnalytics/

# Load libraries

options(scipen=999)

library(tidyverse)
library(sf)
library(gridExtra)
library(grid)
library(kableExtra)

# Load functions

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

# Load Lancaster County Data

# Note the st_read function

lancCounty <- st_read(file.path(root.dir,"/Chapter2/LancasterCountyBoundary.geojson")) %>%
  st_transform('ESRI:102728')

uga <- st_read(file.path(root.dir,"/Chapter2/Urban_Growth_Boundary.geojson")) %>% 
  st_transform('ESRI:102728')

studyAreaTowns <- st_read(file.path(root.dir,"/Chapter2/StudyAreaTowns.geojson")) %>% 
  st_transform('ESRI:102728')
buildings <- st_read(file.path(root.dir,"/Chapter2/LancasterCountyBuildings.geojson")) %>% 
  st_transform('ESRI:102728')
greenSpace <- st_read(file.path(root.dir,"/Chapter2/LancasterGreenSpace.geojson")) %>% 
  st_transform('ESRI:102728')

# Take a quick look at the UGA
# What does it look like?

plot(uga)

# Union the geometries and apply a teeny-tiny buffer to simplify the geometry

uga_union <- 
  st_union(uga) %>%
  st_buffer(1) %>%
  st_sf()

# Create buffers for both inside and outside the UGA, 
# What's the unit in st_buffer? What's the projection? How do you find out?

outsideBuffer <-
  st_buffer(uga_union, 660) %>%
  st_difference(uga_union) %>%
  mutate(Legend = "Outside")

insideBuffer <- 
  st_buffer(uga_union, dist = -660) %>%
  st_difference(uga_union, .) %>%
  mutate(Legend = "Inside")

# Put the buffers together - we want to geoprocess these inside/outside metrics later

bothBuffers <- rbind(insideBuffer, outsideBuffer)

# Plot them

ggplot() + 
  geom_sf(data = bothBuffers, aes(fill = Legend)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) + 
  labs(title = "1/8mi buffer inside & outside UGA") +
  mapTheme()

# Joining the buffers to the towns
# Filter out those which aren't both inside and outside

buffersAndTowns <- 
  st_intersection(st_make_valid(studyAreaTowns), bothBuffers) %>%
  filter(MUNI != "MOUNTVILLE BOROUGH" & MUNI != "MILLERSVILLE BOROUGH")

# Challenge #1

# Make the Map in Figure 2.10

# Can we screenshare this?

# Next step - calculate building density inside and outside the buffer

# Take building centroids
buildingCentroids <- 
  st_centroid(buildings) %>%
  mutate(counter = 1) %>% 
  dplyr::select(counter)

# Summarize them by buffersAndTonws, bind them back to the buffersAndTowns sf
# Calculate Area
buffersAndTowns_Buildings <- 
  aggregate(buildingCentroids, buffersAndTowns, sum) %>%
  cbind(buffersAndTowns) %>%
  mutate(counter = replace_na(counter, 0),
         Area = as.numeric(st_area(.))) 

# Summarize the number of buildings

buffersAndTowns_Buildings_Summarize <-
  buffersAndTowns_Buildings %>%
  group_by(MUNI, Legend) %>%
  summarize(Building_Count = sum(counter),
            Area = sum(Area)) %>%
  mutate(Building_Density = Building_Count / Area)

# Calculate the density... look at how we use the spread command

buildingDifferenceTable <-
  st_drop_geometry(buffersAndTowns_Buildings_Summarize) %>%
  dplyr::select(MUNI, Legend, Building_Density) %>%
  spread(Legend, Building_Density) %>%
  mutate(Building_Difference = Inside - Outside) %>%
  arrange(desc(Building_Difference)) 

# Skip to section 2.3, and build some discontinuity plots

# Create a multiple ring buffer

multipleRing <-
  rbind(
    multipleRingBuffer(uga_union, -14520, -660) %>%
      mutate(Legend = "Inside the UGA"),
    multipleRingBuffer(uga_union, 26400, 660) %>%
      mutate(Legend = "Outside the UGA")) 

# Now relate the rings and towns
RingsAndTowns <- 
  st_intersection(multipleRing, st_make_valid(studyAreaTowns))

# What is the building density at various distances?

buildings.in.RingsAndTowns <- 
  aggregate(buildingCentroids, 
            RingsAndTowns, sum) %>%
  cbind(RingsAndTowns) %>%
  dplyr::select(-geometry.1) %>% 
  mutate(counter = replace_na(counter, 0),
         Area = as.numeric(st_area(.))) 

# Challenge # 2: 

# Can you create Figure 2.13 or 2.14?