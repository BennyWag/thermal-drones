# drone vs ground - data cleaning -----------------------------------------

library(openxlsx)
library(RColorBrewer)
library(sf)
library(sfheaders)
library(chron)
library(stringi)
library(tidyverse)
library(patchwork)

spotlighting<-read.xlsx('data/detect/3_spotlighting_master_checked.xlsx', detectDates = T)%>%select(-obs_id)

drone<-read.xlsx('data/detect/4_drone_timefix.xlsx', detectDates = T)

loadshapelist<-function(dir, extension = '.gpkg'){
  
  z<-getwd()
  setwd(dir)
  
  #
  print('listing files in folder') 
  temp<-list.files(pattern = paste0('*',extension), full.names=FALSE)
  
  #
  print('compiling names')
  names_plots<-temp
  names_plots<-str_replace(names_plots, extension,'')
  
  #
  print('load all files into list')
  allshapes <- lapply(temp, st_read)
  
  #
  print('set stack names')
  names(allshapes)<-names_plots
  
  setwd(z)
  return(allshapes)
  
}

saveshapelist <- function(shape, name, outdir, extension = '.gpkg'){
  
  shapename<-name
  
  st_write(shape, file.path(outdir, paste0(name, extension)))
  
}

# fix names etc  ----------------------------------------------------------

sort(unique(spotlighting$site_ID))
sort(unique(drone$site_ID))

sort(unique(spotlighting$species))
sort(unique(drone$commonName))

#ground data

spotlighting_fix<-spotlighting%>%
  mutate(species = case_when(grepl('oob', species) ~ 'Southern boobook',
                             grepl('ountain', species) ~ 'Mountain crazytail devil',
                             grepl('reater', species) ~ 'Southern greater glider',
                             grepl('ing', species) ~ 'Common ringtail possum',
                             grepl('eather', species) ~ 'Feathertail glider',
                             grepl('ugar', species) ~ 'Sugar glider',
                             grepl('ellow', species) ~ 'Yellow-bellied glider',
                             grepl('rush', species) ~ 'Common brushtail possum',
                             TRUE ~ species),
         obs_type = case_when(obs_type %in% c('heard', 'Heard') ~ 'heard',
                              obs_type %in% c('seen', 'Seen') ~ 'seen',
                              TRUE ~ obs_type))%>%
  mutate(species = str_trim(species, side = 'right'),
         species = case_when(grepl('evil', species) ~ 'Mountain brushtail possum',
                             TRUE ~ species),
         time = obs_time*24,
         date = as_date(date))

sort(unique(spotlighting_fix$species))

table(spotlighting_fix$species)

#drone data

drone_fix<-drone%>%
  mutate(species = case_when(grepl('ountain', commonName) ~ 'Mountain crazytail devil',
                             grepl('rush', commonName) ~ 'Common brushtail possum',
                             grepl('reater', commonName) ~ 'Southern greater glider',
                             grepl('ing', commonName) ~ 'Common ringtail possum',
                             grepl('eather', commonName) ~ 'Feathertail glider',
                             grepl('ugar', commonName) ~ 'Sugar glider',
                             grepl('rebird', commonName) ~ 'Superb lyrebird',
                             grepl('Liar', commonName) ~ 'Superb lyrebird',
                             grepl('oala', commonName) ~ 'Koala',
                             grepl('omb', commonName) ~ 'Bare-nosed wombat',
                             grepl('Magpies', commonName) ~ 'Australian magpie',
                             grepl('Cat', commonName) ~ 'Feral cat',
                             grepl('oob', commonName) ~ 'Southern boobook',
                             grepl('nkn', commonName) ~ 'unknown',
                             commonName %in% c('Other bird', 'Owl', 'Parrots', 'Possum', 'Undeterminded', 'Unidentified', 'UNKNOWN') ~ 'unknown',
                             
                             
                             TRUE ~ commonName))%>%
  mutate(species = str_trim(species, side = 'right'),
         species = case_when(grepl('evil', species) ~ 'Mountain brushtail possum',
                             TRUE ~ species),
         time = as.numeric(obs_time)/10,
         date = as_date(dateObserv),
         time_alt = stri_sub_replace(obs_time, 3 , 2, value = ':'),
         time_alt = paste0(time_alt, ':00'),
         time_alt_calc = as.times(time_alt))

sort(unique(drone_fix$species))

table(drone_fix$species)

#collate coupe and site ID

sites<-drone_fix%>%select(coupeNumbe, site_ID)%>%distinct()

write.csv(sites, 'outputs/coupeID_site_ID.csv', row.names = F)

#extract survey results

# ground survey transects -------------------------------------------------

#get survey areas

ground_track<-loadshapelist('data/detect/spatial/ground_transects')

ground_track_trans<-map(ground_track, st_transform, crs = 28355)

buff_poly<-function(line, buffer_width = 25){
  
  buffered <- st_buffer(line, buffer_width)
  
  poly<-buffered%>%st_union()%>%st_convex_hull()
  
  return(poly)
  
}

ground_track_polys<-map(ground_track_trans, buff_poly)

#calc area

ground_areas<-map(ground_track_polys, st_area)%>%
  unlist()%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(site_ID = 1, area_m2 = 2)%>%
  mutate(area_ha = round(area_m2/10000))

sum(ground_areas$area_ha)

#drone obs within ground survey area

drone_fix_sf<-st_as_sf(drone_fix, coords = c('speciesEas', 'speciesNor'), crs = 4326)%>%
  st_transform(crs = 28355)

drone_subset<-map2(list(drone_fix_sf), ground_track_polys, st_intersection)

names(drone_subset)<-names(ground_track_polys)

drone_subset_df<-bind_rows(drone_subset) #this has to be filtered for multiple surveys!

table(drone_subset_df$species)

#save polygons

#dir.create('outputs/detect/spatial/ground')

map2(ground_track_polys, plotnames, 
     saveshapelist, 
     outdir = 'outputs/detect/spatial/ground/')

# flight paths and drone survey area --------------------------------------

flight_paths<-loadshapelist('data/detect/spatial/flight_paths')

flight_paths_trans<-map(flight_paths, st_transform, crs = 28355)

#flight_track_polys<-map(flight_paths_trans, buff_poly, buffer_width = 50)

#save

#dir.create('outputs/detect/spatial')

#saveRDS(flight_track_polys, 'outputs/detect/spatial/flight_areas.rds')

#dir.create('outputs/detect/spatial/flights')

# map2(flight_track_polys, plotnames, 
#      saveshapelist, 
#      outdir = 'outputs/detect/spatial/flights/')

# osb_poly<-buff_poly(flight_paths_trans$Osborne, buffer_width = 50)
# 
# st_write(osb_poly, 'outputs/detect/spatial/flights/Osborne.gpkg', delete_layer = T)

flight_track_polys<-loadshapelist('outputs/detect/spatial/flights')

flight_areas<-map(flight_track_polys, st_area)%>%
  unlist()%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(site_ID = 1, area_m2 = 2)%>%
  mutate(area_ha = round(area_m2/10000))

sum(flight_areas$area_ha)

# spotlighting data - individual obs --------------------------------------

table(spotlighting_fix$obs_type)

spotlighting_fix_obs1<-spotlighting_fix%>%filter(obs_type == 'seen')%>%
  filter(seen_by_obs1 == 1 & seen_by_obs2 == 0)

spotlighting_fix_obs2<-spotlighting_fix%>%filter(obs_type == 'seen')%>%
  filter(seen_by_obs1 == 0 & seen_by_obs2 == 1)

spotlighting_fix_both1<-spotlighting_fix%>%filter(obs_type == 'seen')%>%
  filter(seen_by_both == 1 & observer_no == 1)

spotlighting_fix_both2<-spotlighting_fix%>%filter(obs_type == 'seen')%>%
  filter(seen_by_both == 1 & observer_no == 2)

spotlighting_fix_unique<-bind_rows(spotlighting_fix_obs1, spotlighting_fix_obs2, spotlighting_fix_both1)

table(spotlighting_fix_unique$species)


# summary data for progress report (Feb 24) -------------------------------

#species

drone_species_count<-drone_fix%>%group_by(species)%>%
  tally()

write.csv(drone_species_count, 'outputs/detect/drone_species.csv', row.names = F)

drone_species_count_site<-drone_fix%>%group_by(species, site_ID)%>%
  tally()

ground_species_count<-spotlighting_fix_unique%>%group_by(species)%>%
  tally()

write.csv(ground_species_count, 'outputs/detect/ground_species.csv', row.names = F)

#drone species in ground footprint

drone_ground<-drone_subset_df%>%st_drop_geometry()%>%
  group_by(site_ID)%>%
  tally()

ground_site<-spotlighting_fix_unique%>%
  group_by(site_ID)%>%
  tally()

#join

obs_compare<-left_join(drone_ground, ground_site, by = 'site_ID')%>%
  rename(drone_obs = 2, ground_obs = 3)%>%
  mutate(ground_obs = case_when(is.na(ground_obs) ~ 0,
                                TRUE ~ ground_obs),
         diff = ground_obs - drone_obs)

#for GGs

drone_ground_GG<-drone_subset_df%>%st_drop_geometry()%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID)%>%
  tally()

ground_site_GG<-spotlighting_fix_unique%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID)%>%
  tally()

obs_compare_GG<-full_join(drone_ground_GG, ground_site_GG, by = 'site_ID')%>%
  rename(drone_obs = 2, ground_obs = 3)%>%
  mutate(ground_obs = case_when(is.na(ground_obs) ~ 0,
                                TRUE ~ ground_obs),
         drone_obs = case_when(is.na(drone_obs) ~ 0,
                                TRUE ~ drone_obs),
         diff = ground_obs - drone_obs)

#graph

a<-obs_compare%>%pivot_longer(2:3)%>%
ggplot(aes(x = reorder(site_ID, value), y = value, fill = name))+
  geom_bar(stat = 'identity', position = 'dodge')+
  coord_cartesian(expand = F)+
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'No. of species', x = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position="bottom")
  

b<-obs_compare_GG%>%pivot_longer(2:3)%>%
  ggplot(aes(x = reorder(site_ID, value), y = value, fill = name))+
  geom_bar(stat = 'identity', position = 'dodge')+
  coord_cartesian(expand = F)+
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'No. of SGG obs.', x = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position="bottom")

a/b+ plot_annotation(tag_levels = 'A') +
  plot_layout(guides = 'collect') & theme(legend.position = 'bottom')

ggsave('obs_comp.svg',path = 'figures/detect/', width = 25, height = 20, units = 'cm', dpi = 600)


#map

spotlighting_fix_sf<-st_as_sf(spotlighting_fix_unique, coords = c('long', 'lat'), crs = 4326)%>%
  st_transform(crs = 28355)

ginters_ground<-st_intersection(spotlighting_fix_sf, flight_track_polys$Ginters)

ginters_drone<-st_intersection(drone_fix_sf, flight_track_polys$Ginters)

ggplot()+
  geom_sf(data = flight_track_polys$Ginters, fill = 'white')+
  geom_sf(data = flight_paths_trans$Ginters, size = 0.7, color = 'lightgrey')+
  geom_sf(data = ginters_drone, aes(color = 'dodgerblue'), size = 0.5)+
  geom_sf(data = ground_track_polys$Ginters, color = 'blue', lwd = 1.5)+
  geom_sf(data = ground_track_trans$Ginters, color = 'grey', lwd = 0.7)+
  geom_sf(data = drone_subset$Ginters, aes(color = 'red'), size = 3, pch = 4)+
  geom_sf(data = ginters_ground, aes(color = 'darkgreen'), size = 3, pch = 3 )+
  scale_color_identity(name = '',
                       breaks = c('red', 'darkgreen', 'dodgerblue'),
                       labels = c("Drone obs.", "Ground obs.", "All drone obs"),
                       guide = 'legend')+
  theme_minimal()+
  theme(legend.position="bottom")


#dir.create('figures/detect')

ggsave('survey_example.svg',path = 'figures/detect/', width = 20, height = 25, units = 'cm', dpi = 600)

#time

#ground surveys

spot_survey_raw<-read.xlsx('data/detect/2_survey_master.xlsx', detectDates = T)%>%
  rename(end_time = survey_end)%>%
  mutate(diff_time = case_when(start_time < end_time ~ end_time-start_time,
                               end_time<start_time ~ 1-(start_time-end_time)),
         start = as.times(start_time),
         end = as.times(end_time),
         time_pass = as.times(diff_time))
  
spot_survey_times<-spot_survey_raw%>%group_by(site_ID, transect)%>%
  summarise(max = max(time_pass))%>%
  group_by(site_ID)%>%
  summarise(survey_time = sum(max),
            survey_dec = as.numeric(survey_time))%>%
  mutate(survey_time = case_when(survey_dec>0.2 ~ survey_time/3,
                                 TRUE ~ survey_time),
         survey_char = as.character(survey_time))%>%
  tidyr::separate(survey_char, into = c("hours", "min", 'sec'), sep = ":")%>%
  mutate(survey_length = as.numeric(hours) + as.numeric(min)/60)%>%
  select(1,2,7)%>%
  ungroup()

#combine with no of obs

obs_compare_time<-left_join(obs_compare, spot_survey_times%>%select(1,3), by = 'site_ID')%>%
  rename(ground_time = 5)%>%
  mutate(ground_effort = ground_obs/(ground_time*2))

#add area

obs_compare_time<-cbind(obs_compare_time, ground_areas$area_ha)%>%rename(area = 7)

write.csv(obs_compare_time, 'outputs/detect/ground_effort.csv', row.names = F)

#drone

drone_times<-drone_fix%>%
  mutate(time_num = as.numeric(time_alt_calc))%>%
  group_by(site_ID, dateObserv)%>%
  summarise(start_time= min(time_num),
            end_time = max(time_num))%>%
  mutate(diff_time = case_when(start_time < end_time ~ end_time-start_time,
                               end_time<start_time ~ 1-(start_time-end_time)),
         diff_time = case_when(diff_time>0.5 ~ 1-diff_time,
                               TRUE ~ diff_time))%>%
  mutate_at(vars(3:5), as.times)%>%
  mutate(diff_char = as.character(diff_time))%>%
  tidyr::separate(diff_char, into = c("hours", "min", 'sec'), sep = ":")%>%
  mutate(survey_length = as.numeric(hours) + as.numeric(min)/60)%>%
  na.omit()

#species obs by plot and date

drone_tally_all<-drone_fix%>%
  group_by(site_ID, dateObserv)%>%
  tally()

#join

drone_effort<-left_join(drone_tally_all, drone_times%>%select(1,2,9), by = c('site_ID', 'dateObserv'))%>%
  mutate(effort = n/(survey_length))%>%
  filter(survey_length>2)

#add area

drone_effort<-left_join(drone_effort, flight_areas%>%select(1,3), by = c('site_ID'))

write.csv(drone_effort, 'outputs/detect/drone_effort.csv', row.names = F)





# stuff -------------------------------------------------------------------

alfred_drone<-st_intersection(drone_fix_sf,   st_transform(flight_track_polys$Alfred, crs = 28355))

mohican_3_drone<-st_intersection(drone_fix_sf,   st_transform(flight_track_polys$Mohican_3, crs = 28355))

