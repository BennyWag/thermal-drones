# drone data analyses -----------------------------------------------------

library(openxlsx)
library(RColorBrewer)
library(sf)
library(sfheaders)
library(spdep)
library(spatstat)
library(chron)
library(stringi)
library(unmarked)
library(MASS)
library(tidyverse)
library(patchwork)

drone<-read.xlsx('data/detect/4_drone_timefix.xlsx', detectDates = T)

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
         time_alt_calc = as.times(time_alt),
         speciesNor = case_when(speciesNor>0 ~ speciesNor*(-1),
                                TRUE~speciesNor))

flight_track_polys<-loadshapelist('outputs/detect/spatial/flights')

flight_areas<-map(flight_track_polys, st_area)%>%
  unlist()%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(site_ID = 1, area_m2 = 2)%>%
  mutate(area_ha_drone = round(area_m2/10000))

write.csv(flight_areas, 'outputs/compare/flight_areas.csv', row.names = F)

table(drone_fix$species)

# calculate 'naive' densities ---------------------------------------------

#tally n obs per site and survey

colnames(drone_fix)

drone_results<-drone_fix%>%
  mutate(site_ID = case_when(site_ID == 'United' ~ 'Merger',
                             TRUE ~ site_ID),
         species = case_when(grepl('brush', species) ~ 'Brushtail possum',
                             TRUE ~ species))%>%
  group_by(site_ID, startDate, species)%>%
  summarise(n = sum(countNumbe))

#join in area

drone_results_area<-left_join(drone_results, flight_areas, by = 'site_ID')%>%
  mutate(naive_dens = n/area_ha_drone)

table(drone_results_area$species)

# derive detectability through occupancy modeling -------------------------

#sites with two flights

sites_double<-c('Babbington', 'Canvas', 'Mohican_1', 'Mohican_3', 'Narbethong', 'Davis',
                "Alfred", "Ginters", "Mohican_2", "Murrindindi", "Quodpod", "Tarrango",   
                "Troop", "Yellowdindi")

double_fun<-function(data, sp, sites_df){
  
  double<-data%>%filter(species == sp)%>%
    group_by(site_ID)%>%
    mutate(survey_no = row_number())%>%
    select(1, 4, 8)%>%
    ungroup()%>%
    pivot_wider(names_from = survey_no, values_from = n)%>%
    filter(!is.na(`2`))%>%
    select(-4)%>%
    rename(first = 2, second = 3)
  
  missing_df<-as.data.frame(sites_df)%>%
    rename(site_ID = 1)%>%
    filter(!site_ID %in% double$site_ID)%>%
    mutate(first = rep(0, length(site_ID)),
           second = rep(0, length(site_ID)))
  
  
  double_all<-rbind(double, missing_df)
  
  return(double_all)
  
}

double_fun(drone_results_area, sp = 'Southern greater glider', sites_df = sites_double)
double_fun(drone_results_area, sp = 'Common ringtail possum', sites_df = sites_double)
double_fun(drone_results_area, sp = 'Sugar glider', sites_df = sites_double)
double_fun(drone_results_area, sp = 'Feathertail glider', sites_df = sites_double)
double_fun(drone_results_area, sp = 'Brushtail possum', sites_df = sites_double)


#derive detect

det_oc<-function(data, k){
  
  mat<-as.matrix(data%>%select(-1), nrow = nrow(data), byrow = T )
  
  umf <- unmarkedFramePCount(y = mat)
  
  occmodel <- pcount(~ 1 ~ 1, data = umf, K = k)
  
  summary(occmodel)
  backTransform(occmodel, type = "det") 
  
}

det_oc(double_fun(drone_results_area, sp = 'Southern greater glider', sites_df = sites_double), k = 22)
det_oc(double_fun(drone_results_area, sp = 'Common ringtail possum', sites_df = sites_double), k = 147)
det_oc(double_fun(drone_results_area, sp = 'Sugar glider', sites_df = sites_double), k = 56)
det_oc(double_fun(drone_results_area, sp = 'Feathertail glider', sites_df = sites_double), k = 20)
det_oc(double_fun(drone_results_area, sp = 'Brushtail possum', sites_df = sites_double), k = 6)

# correct densities for GG and CRP - add sites with no obs ----------------

#whole area

det_gg<-0.79
det_CRP<-0.59

sitevec_drone<-unique(drone_fix$site_ID)

correct_dens<-function(dataset, sp, det, site_vec){
  
  results_adjusted<-dataset%>%
    filter(species %in% c(sp))%>%
    mutate(p = det,
           N = n/p,
           den_adj = N/area_ha_drone)%>%
    group_by(site_ID)%>%
    mutate(survey_no = row_number())%>%
    select(1,11,3:10)
  
  missing<-site_vec%>%
    as.data.frame()%>%rename(site_ID = 1)%>%
    filter(!site_ID %in% results_adjusted$site_ID)%>%
    mutate(survey_no = 1,
           species = sp,
           n = 0,
           area_m2 = 0,
           area_ha_drone = 0,
           naive_dens = 0,
           p = det,
           N = 0,
           den_adj = 0)
  
  combined<-rbind(results_adjusted, missing)
  
  return(combined)
  
}

drone_gg<-correct_dens(drone_results_area,
                   sp = 'Southern greater glider',
                   det = det_gg,
                   site_vec = sitevec_drone)

drone_crp<-correct_dens(drone_results_area,
                       sp = 'Common ringtail possum',
                       det = det_gg,
                       site_vec = sitevec_drone)

#combine - check if correlated

drone_gg_crp<-left_join(drone_gg, 
                        drone_crp%>%select(den_adj_crp = den_adj, site_ID, survey_no), 
                        by = c('site_ID', 'survey_no'))

# calculate densities in ground footprint ---------------------------------

#ground_track_polys<-loadshapelist('outputs/detect/spatial/ground')
#ground_track_polys_single<-loadshapelist('outputs/detect/spatial/ground/single')

drone_fix_sf<-st_as_sf(drone_fix, coords = c('speciesEas', 'speciesNor'), crs = 4326)%>%
  st_transform(crs = 28355)

drone_subset_ground<-map2(list(drone_fix_sf), ground_track_polys, st_intersection)
drone_subset_ground_single<-map2(list(drone_fix_sf), ground_track_polys_single, st_intersection)

names(drone_subset_ground)<-names(ground_track_polys)
names(drone_subset_ground_single)<-names(ground_track_polys_single)

drone_subset_ground_df<-bind_rows(drone_subset_ground)%>%
  mutate(site_ID = case_when(site_ID == 'United' ~ 'Merger',
                             TRUE ~ site_ID))%>%
  group_by(site_ID, startDate, species)%>%
  summarise(n = sum(countNumbe))%>%
  st_drop_geometry() 

drone_subset_ground_single_df<-bind_rows(drone_subset_ground_single)%>%
  mutate(site_ID = case_when(site_ID == 'United' ~ 'Merger',
                             TRUE ~ site_ID))%>%
  group_by(site_ID, startDate, species)%>%
  summarise(n = sum(countNumbe))%>%
  st_drop_geometry()

#add areas

#ground_areas<-read.csv('outputs/compare/ground_areas.csv')

drone_footprint<-left_join(drone_subset_ground_df, ground_areas%>%select(1:3), by = 'site_ID')%>%
  mutate(naive_dens = n/area_ha)%>%
  rename(area_ha_drone = area_ha)

drone_footprint_single<-left_join(drone_subset_ground_single_df, ground_areas%>%select(1, 4, 5), by = 'site_ID')%>%
  mutate(naive_dens = n/area_ha_single)%>%
  rename(area_ha_drone = area_ha_single,
         area_m2 = area_m2_single)

#run calcs

drone_gg_footprint<-correct_dens(drone_footprint,
                       sp = 'Southern greater glider',
                       det = det_gg,
                       site_vec = sitevec_drone)

drone_gg_footprint_single<-correct_dens(drone_footprint_single,
                                 sp = 'Southern greater glider',
                                 det = det_gg,
                                 site_vec = sitevec_drone)

drone_crp_footprint<-correct_dens(drone_footprint,
                                 sp = 'Common ringtail possum',
                                 det = det_CRP,
                                 site_vec = sitevec_drone)

drone_crp_footprint_single<-correct_dens(drone_footprint_single,
                                        sp = 'Common ringtail possum',
                                        det = det_CRP,
                                        site_vec = sitevec_drone)

drone_calc_all<-bind_rows(drone_gg%>%mutate(species = 'GG', survey = 'coupe'),
                          drone_gg_footprint%>%mutate(species = 'GG', survey = '3'),
                          drone_gg_footprint_single%>%mutate(species = 'GG', survey = '1'),
                          drone_crp%>%mutate(species = 'CRP', survey = 'coupe'),
                          drone_crp_footprint%>%mutate(species = 'CRP', survey = '3'),
                          drone_crp_footprint_single%>%mutate(species = 'CRP', survey = '1'))

drone_calc_all%>%filter(den_adj>0)%>%
  ggplot(aes(x = species, y=den_adj, fill = survey))+
  geom_boxplot()

write.csv(drone_calc_all, 'outputs/compare/drone_dens.csv', row.names = F)

#save obs for mapping

drone_fix_sf_gg_crp<-drone_fix_sf%>%filter(species %in% c('Southern greater glider', 'Common ringtail possum'))

st_write(drone_fix_sf_gg_crp, 'outputs/detect/spatial/drone_GG+CRP.gpkg', delete_layer = T)


# calculate aggregation - NNI ---------------------------------------

#get each survey result

#GGs

drone_fix_sf_latlong<-drone_fix_sf%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID, startDate)%>%
  group_split()

drone_fix_gg_sites<-drone_fix%>%
  filter(species == 'Southern greater glider')

site_ids<-unique(drone_fix_gg_sites[, c('site_ID', 'startDate')])%>%
  as.data.frame()%>%
  arrange(site_ID)%>%
  mutate(namedate = paste0(site_ID, '__', startDate))

names(drone_fix_sf_latlong)<-site_ids$namedate

#CRPs

drone_fix_sf_latlong_crp<-drone_fix_sf%>%
  filter(species == 'Common ringtail possum')%>%
  group_by(site_ID, startDate)%>%
  group_split()

drone_fix_crp_sites<-drone_fix%>%
  filter(species == 'Common ringtail possum')

site_ids_crp<-unique(drone_fix_crp_sites[, c('site_ID', 'startDate')])%>%
  as.data.frame()%>%
  arrange(site_ID)%>%
  mutate(namedate = paste0(site_ID, '__', startDate))

names(drone_fix_sf_latlong_crp)<-site_ids_crp$namedate

#test on one

troop_GGs<-drone_fix_sf_latlong$`Troop__2023-03-15`
coords_troop<-st_coordinates(troop_GGs)
ppp_troop<-as.ppp(coords_troop, W = owin(range(coords_troop[,1]), range(coords_troop[,2])))
nni_troop <- nndist(ppp_troop, k = 1)

observed_mean_distance <- mean(nni_troop)
expected_mean_distance <- 0.5 / sqrt(nrow(troop_GGs) / area.owin(ppp_troop$window))
nni_value <- observed_mean_distance / expected_mean_distance

#function for all obs

nni_calc<-function(obs, k = 1){
  
  spec_loc<-obs
  coords<-st_coordinates(spec_loc)
  ppp<-as.ppp(coords, W = owin(range(coords[,1]), range(coords[,2])))
  nni <- nndist(ppp, k = k)
  
  observed_mean_distance <- mean(nni)
  expected_mean_distance <- 0.5 / sqrt(nrow(spec_loc) / area.owin(ppp$window))
  nni_value <- observed_mean_distance / expected_mean_distance
  
  n_obs = nrow(spec_loc)
  
  results<-data.frame(n = n_obs,
                      mean_dist = observed_mean_distance, 
                      expected_dist = expected_mean_distance, 
                      nni = nni_value)
  
  return(results)
  
}

nni_GG<-map(drone_fix_sf_latlong, nni_calc)%>%
  bind_rows(.id = 'survey')%>%
  separate(survey, into = c("site_ID", "startDate"), sep = "__", convert = TRUE)%>%
  mutate(species = 'GG')

nni_CRP<-map(drone_fix_sf_latlong_crp, nni_calc)%>%
  bind_rows(.id = 'survey')%>%
  separate(survey, into = c("site_ID", "startDate"), sep = "__", convert = TRUE)%>%
  mutate(species = 'GG')



# derive survey length and get correct start times ------------------------

#time spent per survey + check start times

flight_times<-read.csv('data/detect/5_drone-flighttimes_fix.csv')%>%
  mutate(time = as.numeric(flight_start)/10,
         time_alt = stri_sub_replace(flight_start, 3 , 2, value = ':'),
         time_alt = paste0(time_alt, ':00'),
         time_alt_calc = as.times(time_alt),
         time_num = as.numeric(time_alt_calc),
         time_num_24 = case_when(time_num<0.2 ~ 1+time_num,
                                 TRUE ~ time_num),
         startDate = dmy(date_start))

drone_times_survey<-drone_fix%>%
  mutate(time_num = as.numeric(time_alt_calc),
         time_num_24 = case_when(time_num<0.2 ~ 1+time_num,
                                 TRUE ~ time_num))%>%
  group_by(site_ID, startDate)%>%
  summarise(start_time= min(time_num_24),
            end_time = max(time_num_24))%>%
  mutate(diff_time= (end_time-start_time)*24)

#combine

drone_start_check<-left_join(drone_times_survey%>%select(1:3),
                             flight_times%>%select(site_ID, startDate, time_num_24),
                             by = c('site_ID', 'startDate'))%>%
  mutate(diff = start_time - time_num_24)

write.csv(drone_start_check, 'outputs/detect/drone_start.csv', row.names = F) # export for quick fix in table

#derive survey length and effort

flight_times_end<-read.xlsx('data/detect/5_drone-flighttimes_fix.xlsx', detectDates = T)%>%
  mutate(start_alt = stri_sub_replace(flight_start, 3 , 2, value = ':'),
         start_alt = paste0(start_alt, ':00'),
         end_alt = stri_sub_replace(flight_end, 3 , 2, value = ':'),
         end_alt = paste0(end_alt, ':00'),
         start_alt_calc = as.times(start_alt),
         end_alt_calc = as.times(end_alt),
         start_num = as.numeric(start_alt_calc),
         end_num = as.numeric(end_alt_calc),
         end_num = case_when(end_num<0.2 ~ 1+end_num,
                                 TRUE ~ end_num),
         diff_time= (end_num-start_num)*24)

mean(flight_times_end$diff_time, na.rm = T)

hist(flight_times_end$diff_time)

# drone observations per survey hr ----------------------------------------

start_times<-read.csv('outputs/detect/drone_start_fix.csv')%>%
  mutate(startDate = dmy(startDate))

drone_fix_start<-left_join(drone_fix, start_times%>%
                             dplyr::select(site_ID, startDate, start_time_fix),
                           by = c('site_ID', 'startDate'))

drone_fix_start_end<-left_join(drone_fix, flight_times_end%>%
                                 dplyr::select(site_ID, startDate = date_start, 
                                               start_time_fix = start_num, end_num, length = diff_time),
                           by = c('site_ID', 'startDate'))

drone_times_obs<-drone_fix_start_end%>%
  mutate(time_num = as.numeric(time_alt_calc),
         time_num_24 = case_when(time_num<0.2 ~ 1+time_num,
                                 TRUE ~ time_num))%>%
  group_by(site_ID, startDate)%>%
  mutate(first_obs= min(time_num_24),
         time_diff = (time_num_24-start_time_fix)*24,
         time_diff = case_when(time_diff<0 ~ 0,
                               TRUE ~ time_diff),
         time_diff_first = (time_num_24-first_obs)*24)

drone_times_obs_GG<-drone_times_obs%>%filter(species == 'Southern greater glider')
drone_times_obs_CRP<-drone_times_obs%>%filter(species == 'Common ringtail possum')

hist(drone_times_obs$time_diff)
hist(drone_times_obs$time_diff_first)

hist(drone_times_obs_GG$time_diff) #check this for ground?
hist(drone_times_obs_GG$time_diff_first)

hist(drone_times_obs_CRP$time_diff)

#add survey area - test against obs

drone_times_obs_area<-left_join(drone_times_obs, flight_areas%>%select('site_ID', 'area_ha_drone'), by = 'site_ID')

time_obs_area<-drone_times_obs_area%>%
  #filter(species == 'Southern greater glider')%>%
  #filter(species == 'Common ringtail possum')%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  filter(area_ha_drone<150)

summary(glm(length ~ n, data = time_obs_area))

ggplot(time_obs_area, aes(x = length, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')

summary(glm(length ~ area_ha_drone, data = time_obs_area))

ggplot(time_obs_area, aes(x = length, y = area_ha_drone))+
  geom_point()+
  geom_smooth(method = 'lm')

ggplot(time_obs_area, aes(x = area_ha_drone, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')

hist(time_obs_area$n)

summary(glm(n ~ area_ha_drone, family = poisson, data = time_obs_area))
summary(glm.nb(n ~ area_ha_drone, data = time_obs_area)) # for ggs
summary(glm(n ~ length, family = poisson, data = time_obs_area))
summary(glm.nb(n ~ length, data = time_obs_area)) # for ggs

# #only surveys 2-6 hrs
# 
# hist(flight_times_end$diff_time)
# summary(flight_times_end$diff_time)
# 
# drone_times_obs_2_6<-drone_times_obs%>%filter(length>=1, length<5, time_diff < 6)
# 
# drone_times_obs_2_6_gg<-drone_times_obs%>%
#   filter(length>=1, length<5, time_diff < 6)%>%
#   filter(species == 'Southern greater glider')
# 
# hist(drone_times_obs_2_6$time_diff)
# hist(drone_times_obs_2_6_gg$time_diff)

#calc fraction of observations within each hour

drone_times_frac<-drone_times_obs%>%
  mutate(obs_hour = case_when(time_diff <= 1 ~ 1,
                              time_diff > 1 & time_diff <= 2 ~ 2,
                              time_diff > 2 & time_diff <= 3 ~ 3,
                              time_diff > 3 & time_diff <= 4 ~ 4,
                              time_diff > 4 & time_diff <= 5 ~ 5,
                              time_diff > 5 & time_diff <= 6 ~ 6,
                              time_diff > 6 & time_diff <= 7 ~ 7,
                              is.na(time_diff) ~ NA,
                              TRUE ~ 8))

table(drone_times_frac$obs_hour)


drone_frac_all<-drone_times_frac%>%
  filter(!is.na(obs_hour))%>%
  group_by(obs_hour)%>%
  summarise(n = sum(countNumbe))%>%
  mutate(n_all = 800,
         obs_frac = n/n_all)

drone_frac_sp<-drone_times_frac%>%
  filter(!is.na(obs_hour))%>%
  group_by(species, obs_hour)%>%
  summarise(n = n())%>%
  group_by(species)%>%
  mutate(n_obs= sum(n))%>%
  ungroup()%>%
  mutate(obs_frac = n/n_obs)

#by site/date

drone_frac_sp<-drone_times_frac%>%
  filter(!is.na(obs_hour))%>%
  group_by(site_ID, startDate, species, obs_hour)%>%
  summarise(n = n())%>%
  group_by(site_ID, startDate, species)%>%
  mutate(n_obs= sum(n))%>%
  ungroup()%>%
  mutate(obs_frac = n/n_obs)

#cumulative count per site/date - all species

full_hours<-flight_times_end%>%dplyr::select(site_ID, date_start, diff_time)%>%
  mutate(length = round(diff_time))%>%
  na.omit()%>%
  rowwise()%>%
  do(data.frame(
    site_ID = .$site_ID,
    startDate = .$date_start,
    obs_hour = 0:.$length))%>%
  as_tibble()

drone_obs_hr<-drone_times_frac%>%
  filter(!is.na(obs_hour))%>%
  group_by(site_ID, startDate, obs_hour)%>%
  dplyr::count()

drone_obs_hr_complete<-left_join(full_hours, drone_obs_hr, by = c('site_ID', 'startDate', 'obs_hour'))%>%
  mutate(n = replace_na(n, 0))%>%
  group_by(site_ID, startDate)%>%
  mutate(ID = paste(site_ID, startDate, sep = '_'))%>%
  arrange(site_ID, startDate, obs_hour)%>%
  mutate(cum = cumsum(n))

ggplot(drone_obs_hr_complete, aes(x = obs_hour, y = cum, color = ID))+
  geom_point(show.legend = F)+
  #geom_line(show.legend = F)+
  geom_smooth(method = 'loess', se = F, show.legend = F)

#check for plateaus

check_plateau <- function(cumulative_count, threshold = 0.05, n_last = 3) {
  if (length(cumulative_count) < n_last) {
    return(FALSE)
  }
  last_counts <- tail(cumulative_count, n_last)
  max_count <- max(last_counts)
  min_count <- min(last_counts)
  (max_count - min_count) / max_count <= threshold
}

drone_obs_hr_complete_plat<-drone_obs_hr_complete%>%
  group_by(ID)%>%
  mutate(plateau = check_plateau(cum, threshold = 0.10, n_last = 2))

plateau_sites <- drone_obs_hr_complete_plat %>%
  group_by(ID) %>%
  summarize(has_plateaued = any(plateau))

ggplot(drone_obs_hr_complete_plat, aes(x = obs_hour, y = cum, color = ID)) +
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = drone_obs_hr_complete_plat %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(title = "Cumulative Counts of Observations Over Time",
       x = "Date", y = "Cumulative Count")

# drone survey effort -----------------------------------------------------

drone_obs<-drone_fix%>%
  group_by(site_ID, startDate, species)%>%
  summarise(n_obs = sum(countNumbe))%>%
  mutate(species = case_when(grepl('Yellow', species) ~ 'Yellow-bellied glider',
                             TRUE ~ species))%>%
  group_by(site_ID, startDate)%>%
  mutate(all_obs = sum(n_obs))%>%
  pivot_wider(names_from = 'species', values_from = 'n_obs')%>%
  select(1:3, 6, 5)%>%
  mutate_at(vars(4,5), ~ replace_na(., 0))%>%
  rename(gg_obs = 4, crp_obs = 5)
  
drone_effort<-left_join(flight_times_end%>%rename(startDate = 3), 
                        drone_obs,
                        by = c('site_ID', 'startDate'))%>%
  mutate(effort_all = all_obs/diff_time,
         effort_GG = gg_obs/diff_time,
         effort_crp = crp_obs/diff_time)

drone_effort_summary<-drone_effort%>%
  summarise(av_length = mean(diff_time, na.rm = T),
            sd_length = sd(diff_time, na.rm = T),
            av_animals = mean(effort_all, na.rm = T),
            sd_animals = sd(effort_all, na.rm = T),
            av_gg = mean(effort_GG, na.rm = T),
            sd_gg = sd(effort_GG, na.rm = T),
            av_crp = mean(effort_crp, na.rm = T),
            sd_crp = sd(effort_crp, na.rm = T))

drone_effort_summary<-drone_effort%>%
  filter(rowSums(is.na(select(., 16:22))) == 0)%>%
  summarise(across(16:22,
                   list(mean = mean, sd = sd, min = min, max = max ), 
                   .names = "{.col}_{.fn}"))%>%
  mutate(survey = 'drone')%>%
  pivot_longer(cols = 1:28)

write.csv(drone_effort_summary, 'tables/drone_effort_summary.csv', row.names = F)


# CODE-DUMP after this ----------------------------------------------------

# add in ground results - single transect ---------------------------------

ground_results_gg<-gg.abund_single$individuals$N%>%slice(1:(n()-1))%>%mutate(Region.Label = as.numeric(Label))

#add individuals

ground_obs_gg<-gg.abund_single$individuals$summary%>%slice(1:(n()-1))%>%
  mutate(Region.Label = as.numeric(Region))%>%
  rename(n_ground = n)%>%select(10,5)

ground_results_gg_n<-left_join(ground_results_gg, ground_obs_gg, by = 'Region.Label')

#combine

ground_results_gg_plot<-left_join(ground_results_gg_n, 
                                  spot_mrds_single%>%select(site_ID, Region.Label)%>%distinct, 
                                  by = 'Region.Label')%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  select(site_ID, n_ground, Estimate)

#compare

drone_results_area_gg_adjusted<-drone_results_area_gg%>%
  mutate(p = 0.77,
         N = n/p,
         den_adj = N/area_ha_drone)

drone_ground_abund_adj<-left_join(drone_results_area_gg_adjusted, ground_results_gg_plot, by = 'site_ID')%>%
  mutate_at(11:12,  replace_na, 0)


# add in ground results - all transects -----------------------------------

ground_results_gg_all<-gg.abund_all_fixed$individuals$N%>%slice(1:(n()-1))%>%mutate(Region.Label = as.numeric(Label))

#add individuals

ground_obs_gg_all<-gg.abund_all_fixed$individuals$summary%>%slice(1:(n()-1))%>%
  mutate(Region.Label = as.numeric(Region))%>%
  rename(n_ground = n)%>%select(10,5)

ground_results_gg_n_all<-left_join(ground_results_gg_all, ground_obs_gg_all, by = 'Region.Label')

#combine

ground_results_gg_plot_all<-left_join(ground_results_gg_n_all, 
                                      spot_mrds_all_sample_fixed%>%select(site_ID, Region.Label)%>%distinct, 
                                  by = 'Region.Label')%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  select(site_ID, n_ground, Estimate)

#compare

drone_ground_abund_adj_all<-left_join(drone_results_area_gg_adjusted, ground_results_gg_plot_all, by = 'site_ID')%>%
  mutate_at(11:12,  replace_na, 0)


# observations in same footprint ------------------------------------------

#ground_track_polys<-loadshapelist('outputs/detect/spatial/ground')

#drone obs within ground survey area

drone_fix_sf<-st_as_sf(drone_fix, coords = c('speciesEas', 'speciesNor'), crs = 4326)%>%
  st_transform(crs = 28355)

drone_subset<-map2(list(drone_fix_sf), ground_track_polys, st_intersection)

names(drone_subset)<-names(ground_track_polys)

drone_subset_df<-bind_rows(drone_subset) 

drone_results_footprint<-drone_subset_df%>%
  mutate(site_ID = case_when(site_ID == 'United' ~ 'Merger',
                             TRUE ~ site_ID))%>%
  group_by(site_ID, startDate, species)%>%
  tally()%>%st_drop_geometry()

#get ground areas

ground_areas<-map(ground_track_polys, st_area)%>%
  unlist()%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(site_ID = 1, area_m2 = 2)%>%
  mutate(area_ha = round(area_m2/10000))

#join in area

drone_results_footprint_area<-left_join(drone_results_footprint, ground_areas, by = 'site_ID')%>%
  mutate(dens = n/area_ha)

drone_results_footprint_area_gg<-drone_results_footprint_area%>%filter(species == 'Southern greater glider')%>%
  mutate(p = 0.77,
         N = n/p,
         den_adj = N/area_ha)

#join ground data

drone_ground_abund_adj_all_footprint<-left_join(drone_results_footprint_area_gg, ground_results_gg_plot_all, by = 'site_ID')%>%
  mutate_at(11:12,  replace_na, 0)

# compare all density estimates -------------------------------------------

drone_ground_all<-drone_ground_abund_adj_all%>%select(site_ID, startDate, species, 
                                                   area_drone = area_ha, detect_drone = p,
                                                  n_drone = n, n_drone_adj = N, dens_drone = den_adj,
                                                  n_ground_all = n_ground, dens_ground_all = Estimate)

drone_ground_all_single<-left_join(drone_ground_all, drone_ground_abund_adj%>%
                                     select(site_ID, startDate, n_ground_single = n_ground, dens_ground_single = Estimate), 
                                   by = c('site_ID', 'startDate'))

drone_ground_footprint<-drone_ground_abund_adj_all_footprint%>%select(site_ID, startDate, 
                                                                      area_ground = area_ha, n_drone_footprint = n,
                                                                      n_drone_adj_footprint = N, dens_drone_footprint = den_adj)



drone_ground_est<-left_join(drone_ground_all_single, drone_ground_footprint, by = c('site_ID', 'startDate'))%>%
  mutate(area_ground = case_when(is.na(area_ground) ~ 10,
                                 TRUE ~ area_ground))%>%
  mutate_at(14:16,  replace_na, 0)


