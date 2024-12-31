# spotlighting data - calc coordinates - sort out double obs --------------

library(openxlsx)
library(mrds)
library(Distance)
library(geosphere)
library(sf)
library(MASS)
library(tidyverse)

#functions

perpendicular_distance <- function(transect_bearing, distance, compass_bearing) {
  # Convert bearings from degrees to radians
  transect_bearing_rad <- transect_bearing * pi / 180
  compass_bearing_rad <- compass_bearing * pi / 180
  
  # Calculate coordinates of the observation point
  Q_x <- distance * cos(compass_bearing_rad)
  Q_y <- distance * sin(compass_bearing_rad)
  
  # Direction vector of the transect line
  v_x <- cos(transect_bearing_rad)
  v_y <- sin(transect_bearing_rad)
  
  # Project Q onto the transect line
  dot_product <- Q_x * v_x + Q_y * v_y
  P_x <- dot_product * v_x
  P_y <- dot_product * v_y
  
  # Calculate the perpendicular distance
  perpendicular_dist <- sqrt((Q_x - P_x)^2 + (Q_y - P_y)^2)
  
  return(perpendicular_dist)
}

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

sort_spot<-function(data, obs = 'seen', sp, trans){
  
  spot_mrds<-data%>%filter(obs_type == obs, species == sp, transect %in% trans)%>%
    mutate(distance = perpendicular_distance(transect_bearing, as.numeric(distance_to_animal), 
                                             as.numeric(bearing_to_animal)))
  
  spot_mrds_both<-spot_mrds%>%filter(observer_no == 1 & seen_by_both == 1)%>%mutate(object = row_number()*12)
  spot_mrds_1<-spot_mrds%>%filter(seen_by_obs1 == 1 & seen_by_obs2 == 0)%>%mutate(object = row_number()*121)
  spot_mrds_2<-spot_mrds%>%filter(seen_by_obs1 == 0 & seen_by_obs2 == 1)%>%mutate(object = row_number()*262)
  
  spot_mrds_both_dup<-rbind(spot_mrds_both%>%mutate(detected = 1), spot_mrds_both%>%mutate(observer_no = 2, detected = 1))
  spot_mrds_1_dup<-rbind(spot_mrds_1%>%mutate(detected = 1), spot_mrds_1%>%mutate(observer_no = 2, detected = 0))
  spot_mrds_2_dup<-rbind(spot_mrds_2%>%mutate(detected = 1), spot_mrds_2%>%mutate(observer_no = 1, detected = 0))
  
  spot_mrds_all<-bind_rows(spot_mrds_both_dup, spot_mrds_1_dup, spot_mrds_2_dup)%>%
    mutate(max_dist = max(distance))%>%
    select(1, 2, observer = 5, 11, 12, 26:29)
  
  return(spot_mrds_all)
  
}

sort_spot_unique<-function(data, obs = 'seen', sp, trans){
  
  spot_mrds<-data%>%filter(obs_type == obs, species %in% sp, transect %in% trans)%>%
    mutate(distance = perpendicular_distance(transect_bearing, as.numeric(distance_to_animal), 
                                             as.numeric(bearing_to_animal)))
  
  spot_mrds_both<-spot_mrds%>%filter(observer_no == 1 & seen_by_both == 1)%>%mutate(object = row_number()*12)
  spot_mrds_1<-spot_mrds%>%filter(seen_by_obs1 == 1 & seen_by_obs2 == 0)%>%mutate(object = row_number()*121)
  spot_mrds_2<-spot_mrds%>%filter(seen_by_obs1 == 0 & seen_by_obs2 == 1)%>%mutate(object = row_number()*262)
  
  spot_mrds_both_dup<-rbind(spot_mrds_both%>%mutate(detected = 1), spot_mrds_both%>%mutate(observer_no = 2, detected = 1))
  spot_mrds_1_dup<-rbind(spot_mrds_1%>%mutate(detected = 1), spot_mrds_1%>%mutate(observer_no = 2, detected = 0))
  spot_mrds_2_dup<-rbind(spot_mrds_2%>%mutate(detected = 1), spot_mrds_2%>%mutate(observer_no = 1, detected = 0))
  
  spot_mrds_all<-bind_rows(spot_mrds_both_dup, spot_mrds_1_dup, spot_mrds_2_dup)%>%
    mutate(max_dist = max(distance))
  
  return(spot_mrds_all)
  
}

mrds_listfun<-function(dataset){
  
  width<-max(dataset$distance)
  
  mrds_mod<- ddf(method="io",dsmodel= ~cds(key="hr"),
                 mrmodel=~glm(link="logit",formula= ~distance),
                 data=dataset, meta.data= list(width=width))
  
  det_tables <- det.tables(mrds_mod)
  
  results_list<-list(mrds_mod, det_tables)
  names(results_list)<-c('model', 'det_table')
  
  return(results_list)
}

dens_fun<-function(obs_data, 
                   model,
                   modname,
                   site_vector = sitevec,
                   sitecol = site_ID, 
                   samplab = 1, 
                   targetarea = 10000, 
                   translength = 1500){
  
  sample<-obs_data%>%
    mutate(Region.Label = as.numeric(as.factor({{sitecol}})),
           Sample.Label = samplab, 
           Area = targetarea,
           Effort = translength)
  
  tables <-Distance:::checkdata(sample[sample$observer==1,]) 
  
  abund <- dht(model=model,
               region=tables$region.table,
               sample=tables$sample.table,
               obs=tables$obs.table,
               se=TRUE) 
  
  
  
  den<-abund$individuals$N%>%slice(1:(n()-1))%>%
    mutate(Region.Label = as.numeric(Label))
  
  #add individuals
  
  n<-abund$individuals$summary%>%slice(1:(n()-1))%>%
    mutate(Region.Label = as.numeric(Region))%>%
    rename(n_obs = n)%>%select(10,5)
  
  den_n<-left_join(den, n, by = 'Region.Label')
  
  #add site name and sites with 0 obs
  
  den_n_site<-left_join(den_n,
                        sample%>%select(site_ID, Region.Label),
                        by = 'Region.Label')%>%unique()%>%
    select(site_ID, n_obs, 2:7)%>%
    mutate(model = modname)
  
  missing<-site_vector%>%
    as.data.frame()%>%rename(site_ID = 1)%>%
    filter(!site_ID %in% den_n_site$site_ID)%>%
    mutate(Estimate = 0,
           se = 0,
           cv = 0,
           lcl = 0,
           ucl = 0,
           df = 0,
           n_obs = 0,
           model = modname)%>%
    select(site_ID, n_obs, 2:7, model)
  
  den_n_site_all<-rbind(den_n_site, missing)
  
  list<-list(abund, den_n_site_all)
  names(list)<-c('model', 'den_table')
  
  return(list)
  
}

buff_poly<-function(line, buffer_width = 25){
  
  buffered <- st_buffer(line, buffer_width)
  
  poly<-buffered%>%st_union()%>%st_convex_hull()
  
  return(poly)
  
}

# load and clean data -----------------------------------------------------

spotlighting<-read.xlsx('data/detect/3_spotlighting_master_checked_bearings.xlsx', detectDates = T)%>%select(-obs_id)


spotlighting_fix<-spotlighting%>%
  mutate(species = case_when(grepl('oob', species) ~ 'Southern boobook',
                             grepl('ountain', species) ~ 'xyz',
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
         species = case_when(grepl('xyz', species) ~ 'Mountain brushtail possum',
                             TRUE ~ species),
         time = obs_time*24,
         date = as_date(date))


sort(unique(spotlighting_fix$species))
table(spotlighting_fix$species)

# calculate observation coordinates ---------------------------------------

# add perp distance

spot_seen<-spotlighting_fix%>%
  filter(obs_type == 'seen')%>%
  mutate(perp_distance = perpendicular_distance(transect_bearing, 
                                                as.numeric(distance_to_animal), 
                                                as.numeric(bearing_to_animal)))%>%
  rename(long_obs = long, lat_obs = lat)

# calculate x + y location of observastion - off transect

spot_seen_loc<-spot_seen%>%
  st_as_sf(coords = c('long_obs', 'lat_obs'), crs = 4326, remove = F)%>%
  mutate(as.data.frame(destPoint(st_coordinates(.), bearing_to_animal, distance_to_animal)))

#sort out SGG and RTP and save - observer loc

st_write(spot_seen_loc%>%filter(species == 'Southern greater glider'), 
         'outputs/detect/spatial/spotlighting_SGG.gpkg', delete_layer = T)

st_write(spot_seen_loc%>%filter(species == 'Common ringtail possum'), 
         'outputs/detect/spatial/spotlighting_CRP.gpkg', delete_layer = T)

#convert to calculated location

spot_seen_loc_cal<-spot_seen_loc%>%st_drop_geometry()%>%
  st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = F)
  
st_write(spot_seen_loc_cal%>%filter(species == 'Southern greater glider'), 
         'outputs/detect/spatial/spotlighting_loc_SGG.gpkg', delete_layer = T)

st_write(spot_seen_loc_cal%>%filter(species == 'Common ringtail possum'), 
         'outputs/detect/spatial/spotlighting_loc_CRP.gpkg', delete_layer = T)

#combine ground transects and save

ground_track<-loadshapelist('data/detect/spatial/ground_transects')

ground_track_trans<-map(ground_track, st_transform, crs = 28355)

ground_track_all<-bind_rows(ground_track_trans)%>%st_transform(crs = 4326)

st_write(ground_track_all, 'outputs/detect/spatial/ground_tracks_combined.gpkg', delete_layer = T)

# mrds models -------------------------------------------------------------

spotlighting_reduced<-read.xlsx('data/detect/3_spotlighting_master_checked_bearings_reduced.xlsx', detectDates = T)%>%select(-obs_id)


spotlighting_fix_reduced<-spotlighting_reduced%>%
  mutate(species = case_when(grepl('oob', species) ~ 'Southern boobook',
                             grepl('ountain', species) ~ 'xyz',
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
         species = case_when(grepl('xyz', species) ~ 'Mountain brushtail possum',
                             TRUE ~ species),
         time = obs_time*24,
         date = as_date(date),
         transect_fix = case_when(is.na(trans_ordered) ~ transect,
                              !is.na(trans_ordered) ~ trans_ordered),
         transect = transect_fix)%>%
  select(-c(trans_ordered, transect_fix))


sort(unique(spotlighting_fix_reduced$species))
table(spotlighting_fix_reduced$species)

spotlighting_fix_reduced_uncount<-spotlighting_fix_reduced%>%uncount(n_animals)

sitevec<-unique(spotlighting_fix_reduced$site_ID)

#setup data

GG_all<-sort_spot(spotlighting_fix_reduced, 
          sp = 'Southern greater glider',
          trans = c('1', '2', '3'))

GG_one<-sort_spot(spotlighting_fix_reduced, 
                  sp = 'Southern greater glider',
                  trans = c('1'))

CRP_all<-sort_spot(spotlighting_fix_reduced, 
                  sp = 'Common ringtail possum',
                  trans = c('1', '2', '3'))

CRP_one<-sort_spot(spotlighting_fix_reduced, 
                  sp = 'Common ringtail possum',
                  trans = c('1'))

mrds_list<-list(GG_all, GG_one, CRP_all, CRP_one)
names(mrds_list)<-c('GG_all', 'GG_one', 'CRP_all', 'CRP_one')

#run models

mrds_calc<-map(mrds_list, mrds_listfun)


# Occupancy modeling (likelihood of sp detection) -------------------------

all_sites<-unique(spotlighting_fix$site_ID)

#create dataset of obs 1 and obs 2 detections

# GG_all_det<-spotlighting_fix_reduced%>%
#   filter(species == 'Southern greater glider', transect %in% c('1', '2', '3'))%>%
#   group_by(site_ID, observer_no)%>%
#   summarise(n = sum(n_animals))%>%
#   ungroup() %>%
#   complete(site_ID, observer_no, fill = list(n = 0))%>%
#   mutate(n = case_when(n > 0 ~ 1,
#                        TRUE ~ 0))%>%
#   pivot_wider(names_from = 2, values_from = 3)%>%
#   rename(first = 2, second = 3)
# 
# missing_GG_all<-as.data.frame(unique(spotlighting_fix$site_ID))%>%
#   rename(site_ID = 1)%>%
#   filter(!site_ID %in% GG_all_det$site_ID)%>%
#   mutate(first = rep(0, length(site_ID)),
#          second = rep(0, length(site_ID)))
# 
# GG_all_det<-rbind(GG_all_det, missing_GG_all)%>%
#   mutate()

#wrap function

compile_det_mat<-function(data, sp, trans, sites){
  
  det_frame<-data%>%
    filter(species == sp, transect %in% trans)%>%
    group_by(site_ID, observer_no)%>%
    summarise(n = sum(n_animals))%>%
    ungroup() %>%
    complete(site_ID, observer_no, fill = list(n = 0))%>%
    mutate(n = case_when(n > 0 ~ 1,
                         TRUE ~ 0))%>%
    pivot_wider(names_from = 2, values_from = 3)%>%
    rename(first = 2, second = 3)
  
  missing_frame<-as.data.frame(sites)%>%
    rename(site_ID = 1)%>%
    filter(!site_ID %in% det_frame$site_ID)%>%
    mutate(first = rep(0, length(site_ID)),
           second = rep(0, length(site_ID)))
  
  all_frame<-rbind(det_frame, missing_frame)%>%
    mutate()
  
  return(all_frame)
}

GG_det_3<-compile_det_mat(data = spotlighting_fix_reduced,
                sp = 'Southern greater glider',
                trans = c('1', '2', '3'),
                sites = all_sites)

GG_det_1<-compile_det_mat(data = spotlighting_fix_reduced,
                          sp = 'Southern greater glider',
                          trans = c('1'),
                          sites = all_sites)

CRP_det_3<-compile_det_mat(data = spotlighting_fix_reduced,
                          sp = 'Common ringtail possum',
                          trans = c('1', '2', '3'),
                          sites = all_sites)

CRP_det_1<-compile_det_mat(data = spotlighting_fix_reduced,
                          sp = 'Common ringtail possum',
                          trans = c('1'),
                          sites = all_sites)

#models

GG_all_det_mod <- occu(~ 1 ~ 1, data = as.matrix(GG_det_3%>%select(2:3))%>%unmarkedFrameOccu())
GG_one_det_mod <- occu(~ 1 ~ 1, data = as.matrix(GG_det_1%>%select(2:3))%>%unmarkedFrameOccu())

backTransform(GG_all_det_mod, type = "det")
backTransform(GG_one_det_mod, type = "det")

CRP_all_det_mod <- occu(~ 1 ~ 1, data = as.matrix(CRP_det_3%>%select(2:3))%>%unmarkedFrameOccu())
CRP_one_det_mod <- occu(~ 1 ~ 1, data = as.matrix(CRP_det_1%>%select(2:3))%>%unmarkedFrameOccu())

backTransform(CRP_all_det_mod, type = "det")
backTransform(CRP_one_det_mod, type = "det")
 
# save unique observations for mapping ------------------------------------

unique_obs<-sort_spot_unique(spotlighting_fix_reduced,
                             sp = c('Southern greater glider', 'Common ringtail possum'),
                             trans = c('1', '2', '3'))

unique_obs_sf<-unique_obs%>%filter(observer_no == 1)%>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326, remove = F)%>%
  st_transform(crs = 28355)

st_write(unique_obs_sf, 'outputs/detect/spatial/spotlighting_GG+CRP.gpkg', delete_layer = T)

# density calculations ----------------------------------------------------

#derive area sampled per plot based on distances

area_surveyed<-mrds_list$GG_all%>%
  filter(observer == 1)%>%
  group_by(site_ID)%>%
  summarise(max_dist = case_when(max(distance)<30 ~ 35,
                              TRUE ~ max(distance)),
         area_surveyed = (max_dist*2*1500)/10000,
         animals = sum(n_animals),
         p = 0.76,
         animals_adj = animals/p,
         dens = animals_adj/area_surveyed)

area_surveyed_single<-mrds_list$GG_one%>%
  filter(observer == 1)%>%
  group_by(site_ID)%>%
  summarise(max_dist = case_when(max(distance)<30 ~ 35,
                                 TRUE ~ max(distance)),
            area_surveyed = (max_dist*2*500)/10000,
            animals = sum(n_animals),
            p = 0.76,
            animals_adj = animals/p,
            dens = animals_adj/area_surveyed)

area_surveyed<-mrds_list$GG_all%>%
  filter(observer == 1)%>%
  group_by(site_ID)%>%
  summarise(max_dist = 35,
            area_surveyed = (max_dist*2*1500)/10000,
            animals = sum(n_animals),
            p = 0.76,
            animals_adj = animals/p,
            dens = animals_adj/area_surveyed)

#get density estimates - step by step code

GG_all_sample<-mrds_list$GG_all%>%
  mutate(Region.Label = as.numeric(as.factor(site_ID)),
         Sample.Label = 1, 
         Area = 10000,
         Effort = 1500)

GG_all_table <-Distance:::checkdata(GG_all_sample[GG_all_sample$observer==1,]) 

GG_all_abund <- dht(model=mrds_calc$GG_all$model,
                    region=GG_all_table$region.table,
                    sample=GG_all_table$sample.table,
                    obs=GG_all_table$obs.table,
                    se=TRUE) 

GG_all_abund

GG_all_abund_den<-GG_all_abund$individuals$N%>%slice(1:(n()-1))%>%
  mutate(Region.Label = as.numeric(Label))

#add individuals

GG_all_abund_n<-GG_all_abund$individuals$summary%>%slice(1:(n()-1))%>%
  mutate(Region.Label = as.numeric(Region))%>%
  rename(n_obs = n)%>%select(10,5)

GG_all_abund_n<-left_join(GG_all_abund_den, GG_all_abund_n, by = 'Region.Label')

#add site name and sites with 0 obs

GG_all_abund_n_site<-left_join(GG_all_abund_n,
                               GG_all_sample%>%select(site_ID, Region.Label),
                               by = 'Region.Label')%>%unique()%>%
  select(site_ID, n_obs, 2:7)%>%
  mutate(model = 'GG_all')

GG_all_missing<-unique(spotlighting_fix_reduced$site_ID)%>%
  as.data.frame()%>%rename(site_ID = 1)%>%
  filter(!site_ID %in% GG_all_abund_n_site$site_ID)%>%
  mutate(Estimate = 0,
         se = 0,
         cv = 0,
         lcl = 0,
         ucl = 0,
         df = 0,
         n_obs = 0,
         model = 'GG_all')%>%
  select(site_ID, n_obs, 2:7, model)

GG_all_dens<-rbind(GG_all_abund_n_site, GG_all_missing)


# density calculations for all --------------------------------------------

#using function - see above


dens_GG_all<-dens_fun(obs_data = mrds_list$GG_all,
               model = mrds_calc$GG_all$model,
               modname = 'GG_all')

dens_GG_one<-dens_fun(obs_data = mrds_list$GG_one,
                      model = mrds_calc$GG_one$model,
                      modname = 'GG_one')

dens_CRP_all<-dens_fun(obs_data = mrds_list$CRP_all,
                      model = mrds_calc$CRP_all$model,
                      modname = 'CRP_all')

dens_CRP_one<-dens_fun(obs_data = mrds_list$CRP_one,
                      model = mrds_calc$CRP_one$model,
                      modname = 'CRP_one')

dens_calc_all<-bind_rows(dens_GG_all$den_table%>%mutate(species = 'GG', survey = '3'),
                         dens_GG_one$den_table%>%mutate(species = 'GG', survey = '1'),
                         dens_CRP_all$den_table%>%mutate(species = 'CRP', survey = '3'),
                         dens_CRP_one$den_table%>%mutate(species = 'CRP', survey = '1'))


dens_calc_all%>%filter(Estimate>0)%>%
  ggplot(aes(x = species, y=Estimate, fill = survey))+
  geom_boxplot()

#dir.create('outputs/compare')

write.csv(dens_calc_all, 'outputs/compare/spot_dens.csv', row.names = F)

# get survey areas - single + 3 transects ---------------------------------

ground_track_single<-loadshapelist('data/detect/spatial/ground_transects_single')

ground_track_single_trans<-map(ground_track_single, st_transform, crs = 28355)

max_dist<-GG_all%>%group_by(site_ID)%>%summarise(max_dist = max(distance),
                                                 mean_dist = mean(distance))

ground_track_polys_single<-map(ground_track_single_trans, buff_poly)

#dir.create('outputs/detect/spatial/ground/single')

map2(ground_track_polys_single, names(ground_track_polys_single), 
     saveshapelist, 
     outdir = 'outputs/detect/spatial/ground/single')

#ground_track_polys<-loadshapelist('outputs/detect/spatial/ground')

ground_areas<-map(ground_track_polys, st_area)%>%
  unlist()%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(site_ID = 1, area_m2 = 2)%>%
  mutate(area_ha = round(area_m2/10000))

ground_areas_single<-map(ground_track_polys_single, st_area)%>%
  unlist()%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(site_ID = 1, area_m2 = 2)%>%
  mutate(area_ha_single = round(area_m2/10000))

ground_areas<-cbind(ground_areas, ground_areas_single%>%select(area_m2_single = 2, area_ha_single))

write.csv(ground_areas, 'outputs/compare/ground_areas.csv', row.names = F)

# spotlighting densities based on ground area -----------------------------

mrds_calc$GG_all$model
mrds_calc$GG_one$model

mrds_calc$CRP_all$model
mrds_calc$CRP_one$model


#GG

n_animals_GG<-mrds_list$GG_all%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  filter(observer == 1)%>%
  group_by(site_ID)%>%
  summarise(GGs_all = sum(n_animals))

n_animals_GG_single<-mrds_list$GG_one%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  filter(observer == 1)%>%
  group_by(site_ID)%>%
  summarise(GGs_single = sum(n_animals))

n_GGs<-left_join(n_animals_GG, n_animals_GG_single, by = 'site_ID')

n_GGs_ground<-left_join(ground_areas, n_GGs, by = 'site_ID')%>%
  mutate(across(c(6:7), ~ replace_na(., 0)),
         p_all = 0.76,
         p_single = 0.9,
         dens_all = (GGs_all/p_all)/area_ha,
         dens_single = (GGs_single/p_single)/area_ha_single)

#CRP

n_animals_CRP<-mrds_list$CRP_all%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  filter(observer == 1)%>%
  group_by(site_ID)%>%
  summarise(CRP_all = sum(n_animals))

n_animals_CRP_single<-mrds_list$CRP_one%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  filter(observer == 1)%>%
  group_by(site_ID)%>%
  summarise(CRP_single = sum(n_animals))

n_CRPs<-left_join(n_animals_CRP, n_animals_CRP_single, by = 'site_ID')

n_CRPs_ground<-left_join(ground_areas, n_CRPs, by = 'site_ID')%>%
  mutate(across(c(6:7), ~ replace_na(., 0)),
         p_all = 0.43,
         p_single = 0.5,
         dens_all = (CRP_all/p_all)/area_ha,
         dens_single = (CRP_single/p_single)/area_ha_single)


#combine

ground_calc_all<-rbind(n_GGs_ground%>%mutate(species = 'GG', survey = 3, method = 'spot')%>%
                         select(site_ID, species, n_obs = GGs_all, dens = dens_all, survey, method),
                       n_GGs_ground%>%mutate(species = 'GG', survey = 1, method = 'spot')%>%
                         select(site_ID, species, n_obs = GGs_single, dens = dens_single, survey, method),
                       n_CRPs_ground%>%mutate(species = 'CRP', survey = 3, method = 'spot')%>%
                         select(site_ID, species, n_obs = CRP_all, dens = dens_all, survey, method),
                       n_CRPs_ground%>%mutate(species = 'CRP', survey = 1, method = 'spot')%>%
                         select(site_ID, species, n_obs = CRP_single, dens = dens_single, survey, method))

write.csv(ground_calc_all, 'outputs/compare/spot_dens_ground-area.csv', row.names = F)

                     
# survey effort -----------------------------------------------------------

spot_unique<-sort_spot_unique(spotlighting_fix_reduced,
                              sp = unique(spotlighting_fix_reduced$species),
                              trans = c('1', '2', '3'))%>%
  filter(observer_no == 1)%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))

spot_survey_raw<-read.xlsx('data/detect/2_survey_master.xlsx', detectDates = T)%>%
  rename(end_time = survey_end)%>%
  mutate(diff_time = case_when(start_time < end_time ~ end_time-start_time,
                               end_time<start_time ~ 1-(start_time-end_time)),
         start = as.times(start_time),
         end = as.times(end_time),
         time_pass = as.times(diff_time))

#three transects:

spot_survey_times<-spot_survey_raw%>%group_by(site_ID, transect)%>%
  summarise(max = max(time_pass))%>%
  group_by(site_ID)%>%
  summarise(survey_time = sum(max),
            survey_dec = as.numeric(survey_time))%>%
  mutate(survey_time = case_when(survey_dec>0.2 ~ survey_time/3,
                                 TRUE ~ survey_time),
         survey_num = as.numeric(survey_time))%>%
  mutate(survey_length = survey_num*24)%>%
  select(1,2,5)%>%
  ungroup()

#one transect

spot_survey_times_single<-spot_survey_raw%>%group_by(site_ID, transect)%>%
  summarise(survey_time = max(time_pass),
            survey_dec = as.numeric(survey_time))%>%
  filter(transect %in% c(0,1))%>%
  mutate(survey_time = case_when(transect == 0 ~ survey_time/3,
                                 TRUE ~ survey_time),
         survey_num = as.numeric(survey_time))%>%
  mutate(survey_length = survey_num*24,
         survey_length = case_when(survey_length>3 ~ survey_length/3,
                                   TRUE ~ survey_length))%>%
  select(1,3,6)%>%
  ungroup()
  

#get unique obs per site:

spot_site_obs<-spot_unique%>%
  group_by(site_ID, species)%>%
  summarise(n_obs = sum(n_animals))%>%
  group_by(site_ID)%>%
  mutate(all_obs = sum(n_obs))%>%
  pivot_wider(names_from = 'species', values_from = 'n_obs')%>%
  dplyr::select(1:3,5)%>%
  mutate_at(vars(3,4), ~ replace_na(., 0))%>%
  rename(gg_obs = 3, crp_obs = 4)

spot_site_obs_single<-spot_unique%>%
  filter(transect == 1)%>%
  group_by(site_ID, species)%>%
  summarise(n_obs = sum(n_animals))%>%
  group_by(site_ID)%>%
  mutate(all_obs = sum(n_obs))%>%
  pivot_wider(names_from = 'species', values_from = 'n_obs')%>%
  dplyr::select(1:3,5)%>%
  mutate_at(vars(3,4), ~ replace_na(., 0))%>%
  rename(gg_obs = 3, crp_obs = 4)

spot_effort_all<-left_join(spot_survey_times, spot_site_obs,
                                 by = 'site_ID')%>%
  mutate_at(vars(4:6), ~ replace_na(., 0))%>%
  mutate(effort_all = all_obs/survey_length,
         effort_GG = gg_obs/survey_length,
         effort_crp = crp_obs/survey_length,
         area = 10)

spot_effort_single<-left_join(spot_survey_times_single, spot_site_obs_single,
                           by = 'site_ID')%>%
  mutate_at(vars(4:6), ~ replace_na(., 0))%>%
  mutate(effort_all = all_obs/survey_length,
         effort_GG = gg_obs/survey_length,
         effort_crp = crp_obs/survey_length,
          area = 4)

#models length and obs

hist(spot_effort_all$all_obs)
hist(spot_effort_all$crp_obs)
hist(spot_effort_all$gg_obs)


summary(glm(all_obs ~ survey_length, family = poisson, data = spot_effort_all))
summary(glm.nb(all_obs ~ survey_length, data = spot_effort_all)) 

summary(glm.nb(gg_obs ~ survey_length, data = spot_effort_all)) 
summary(glm(gg_obs ~ survey_length, family = poisson, data = spot_effort_all))

summary(glm.nb(crp_obs ~ survey_length, data = spot_effort_all)) 
summary(glm(crp_obs ~ survey_length, family = poisson, data = spot_effort_all))


ggplot(spot_effort_all, aes(x = survey_length, y = all_obs))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(spot_effort_all, aes(x = survey_length, y = crp_obs))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(spot_effort_all, aes(x = survey_length, y = gg_obs))+
  geom_point()+
  geom_smooth(method = 'lm')


#summary stats

spot_effort_summary<-spot_effort_all%>%
  summarise(av_length = mean(survey_length, na.rm = T),
            sd_length = sd(survey_length, na.rm = T),
            av_animals = mean(effort_all, na.rm = T),
            sd_animals = sd(effort_all, na.rm = T),
            av_gg = mean(effort_GG, na.rm = T),
            sd_gg = sd(effort_GG, na.rm = T),
            av_crp = mean(effort_crp, na.rm = T),
            sd_crp = sd(effort_crp, na.rm = T))

spot_effort_summary<-spot_effort%>%
  summarise(across(3:9,
                   list(mean = mean, sd = sd, min = min, max = max ), 
                   .names = "{.col}_{.fn}"))%>%
  mutate(survey = 'three transects')



spot_effort_summary_single<-spot_effort_single%>%
  summarise(av_length = mean(survey_length, na.rm = T),
            sd_length = sd(survey_length, na.rm = T),
            av_animals = mean(effort_all, na.rm = T),
            sd_animals = sd(effort_all, na.rm = T),
            av_gg = mean(effort_GG, na.rm = T),
            sd_gg = sd(effort_GG, na.rm = T),
            av_crp = mean(effort_crp, na.rm = T),
            sd_crp = sd(effort_crp, na.rm = T))

spot_effort_summary_single<-spot_effort_single%>%
  summarise(across(3:9,
                   list(mean = mean, sd = sd, min = min, max = max ), 
                   .names = "{.col}_{.fn}"))%>%
  mutate(survey = 'one transect')

spot_effort_all<-rbind(spot_effort_summary_single, spot_effort_summary)%>%
  pivot_longer(cols = 1:28)

write.csv(spot_effort_all, 'tables/spot_effort_summary.csv', row.names = F)

# cumulative obs

spot_times_start<-spot_survey_raw%>%
  mutate(start_fix = case_when(start_time<0.05 ~ 1+start_time,
                               TRUE ~ start_time))%>%
  group_by(site_ID)%>%
  slice_min(start_fix, with_ties = F)

spot_times_obs<-left_join(spot_unique, spot_times_start%>%dplyr::select(site_ID, start_fix),
                          by = 'site_ID')%>%
  mutate(obs_time_fix = case_when(obs_time<0.2 ~ 1+obs_time,
                                  TRUE ~ obs_time),
         time_diff = (obs_time_fix-start_fix)*24,
         time_diff = case_when(time_diff < 0 ~ 0,
                               TRUE ~ time_diff))

spot_times_frac<-spot_times_obs%>%
  mutate(obs_hour = case_when(time_diff <= 1 ~ 1,
                              time_diff > 1 & time_diff <= 2 ~ 2,
                              time_diff > 2 & time_diff <= 3 ~ 3,
                              time_diff > 3 & time_diff <= 4 ~ 4,
                              time_diff > 4 & time_diff <= 5 ~ 5,
                              time_diff > 5 & time_diff <= 6 ~ 6,
                              time_diff > 6 & time_diff <= 7 ~ 7,
                              is.na(time_diff) ~ NA,
                              TRUE ~ 8))


full_hours_spot<-spot_survey_times%>%dplyr::select(site_ID, survey_length)%>%
  mutate(length = round(survey_length))%>%
  rowwise()%>%
  do(data.frame(
    site_ID = .$site_ID,
    obs_hour = 0:.$length))%>%
  as_tibble()

spot_obs_hr<-spot_times_frac%>%
  #filter(species == 'Common ringtail possum')%>%
  group_by(site_ID, obs_hour)%>%
  dplyr::count()

spot_obs_hr_complete<-left_join(full_hours_spot, spot_obs_hr, by = c('site_ID', 'obs_hour'))%>%
  mutate(n = replace_na(n, 0))%>%
  group_by(site_ID)%>%
  arrange(site_ID, obs_hour)%>%
  mutate(cum = cumsum(n),
         plateau = check_plateau(cum, threshold = 0.10, n_last = 2))

spot_plateau_sites <- spot_obs_hr_complete %>%
  group_by(site_ID) %>%
  summarize(has_plateaued = any(plateau))

ggplot(spot_obs_hr_complete, aes(x = obs_hour, y = cum, color = site_ID)) +
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = spot_obs_hr_complete %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(title = "Cumulative Counts of Observations Over Time",
       x = "Date", y = "Cumulative Count")
