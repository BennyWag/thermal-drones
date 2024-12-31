# tables and figures ------------------------------------------------------

library(sf)
library(raster)
library(ggmap)
library(ggspatial)
library(RColorBrewer)
library(tidyverse)
library(scales)
library(patchwork)

# species obs table -------------------------------------------------------

## Table 1 ##

#drone

drone_spec<-drone_fix%>%
  group_by(site_ID, startDate, species)%>%
  summarise(n_obs = sum(countNumbe))%>%
  mutate(species = case_when(grepl('Yellow', species) ~ 'Yellow-bellied glider',
                             TRUE ~ species))%>%
  group_by(site_ID, species)%>%
  slice_max(n_obs, with_ties = F)%>%
  group_by(species)%>%
  summarise(n_obs_drone = sum(n_obs))

#ground - individual obs

spot_unique<-sort_spot_unique(spotlighting_fix_reduced,
                              sp = unique(spotlighting_fix_reduced$species),
                              trans = c('1', '2', '3'))%>%
  filter(observer_no == 1)

spot_spec_unique<-spot_unique%>%
  group_by(species)%>%
  summarise(n_obs_spot = sum(n_animals))

#ground all obs by type

spot_spec<-spotlighting_fix_reduced%>%
  group_by(species, obs_type)%>%
  summarise(n_obs_spot = sum(n_animals))

#combine arboreals for paper table

unique(drone_spec$species)

arboreals<-c('Common brushtail possum', 'Common ringtail possum',
             'Feathertail glider', 'Koala', "Leadbeater's Possum" ,
             'Mountain brushtail possum', 'Southern greater glider', 
             'Sugar glider', 'Yellow-bellied glider')

spec_all<-left_join(drone_spec%>%filter(species %in% arboreals),
                    spot_spec_unique%>%filter(species %in% arboreals),
                    by = 'species')%>%
  mutate_at(vars(2,3), ~ replace_na(., 0))

#dir.create('tables')

write.csv(spec_all, 'tables/species_obs.csv', row.names = F)

#full obs

drone_spec_all<-drone_fix%>%
  group_by(species)%>%
  summarise(n_obs = sum(countNumbe))%>%
  mutate(species = case_when(grepl('Yellow', species) ~ 'Yellow-bellied glider',
                             TRUE ~ species))

spot_spec_all<-spotlighting_fix_reduced%>%
  group_by(species)%>%
  summarise(n_obs_spot = sum(n_animals))

spec_raw<-left_join(drone_spec_all,
                    spot_spec_all,
                    by = 'species')%>%
  mutate_at(vars(2,3), ~ replace_na(., 0))

write.csv(spec_raw, 'tables/species_obs_raw.csv', row.names = F)

# N obs and density boxplots ----------------------------------------------

## Figure 1 ##

drone_ground_figs<-drone_ground_area%>%
  mutate(area = case_when(survey == 3 ~ 'Three transects',
                          survey == 1 ~ 'Single transect',
                          TRUE ~ 'Coupe'),
         area = fct_relevel(area, 'Single transect', 'Three transects', 'Coupe'),
         method = fct_relevel(method, 'drone', 'spot'))

fig1_crp_dens<-drone_ground_figs%>%
  filter(species == 'CRP', dens >= 0)%>%
  ggplot(aes(x = area, y=dens, fill = method))+
  geom_boxplot()+
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Animals per ha.', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

fig1_crp_obs<-drone_ground_figs%>%
  filter(species == 'CRP', dens >= 0,  n_obs < 50)%>%
  ggplot(aes(x = area, y=n_obs, fill = method))+
  geom_boxplot()+
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Individuals observed', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

fig1_gg_dens<-drone_ground_figs%>%
  filter(species == 'GG')%>%
  ggplot(aes(x = area, y=dens, fill = method))+
  geom_boxplot()+
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Animals per ha.', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

fig1_gg_obs<-drone_ground_figs%>%
  filter(species == 'GG', dens >= 0)%>%
  ggplot(aes(x = area, y=n_obs, fill = method))+
  geom_boxplot()+
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Individuals observed', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

(fig1_crp_obs + theme(axis.text.x = element_blank()) + 
    fig1_gg_obs + theme(axis.text.x = element_blank(),
                        axis.title.y = element_blank())) / 
  (fig1_crp_dens + 
     fig1_gg_dens + theme(axis.title.y = element_blank())) +
  plot_layout(guides = 'collect') +  
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

ggsave('1.obs_dens.svg',path = 'figures/', width = 25, height = 20, units = 'cm', dpi = 600)

#make figutre with CIs - pre review Ecological informatics

fig_1_crp_summary_obs<-drone_ground_figs%>%
  filter(species == 'CRP', dens >= 0,  n_obs < 50)%>%
  group_by(method, area)%>%
  summarize(mean = mean(n_obs),
            lower_ci = mean(n_obs) - qt(0.975, df = n() - 1) * (sd(n_obs) / sqrt(n())),
            upper_ci = mean(n_obs) + qt(0.975, df = n() - 1) * (sd(n_obs) / sqrt(n())),
            .groups = 'drop')

fig_1_crp_summary_dens<-drone_ground_figs%>%
  filter(species == 'CRP', dens >= 0)%>%
  group_by(method, area)%>%
  summarize(mean = mean(dens),
            lower_ci = mean(dens) - qt(0.975, df = n() - 1) * (sd(dens) / sqrt(n())),
            upper_ci = mean(dens) + qt(0.975, df = n() - 1) * (sd(dens) / sqrt(n())),
            .groups = 'drop')

fig_1_gg_summary_obs<-drone_ground_figs%>%
  filter(species == 'GG', dens >= 0)%>%
  group_by(method, area)%>%
  summarize(mean = mean(n_obs),
            lower_ci = mean(n_obs) - qt(0.975, df = n() - 1) * (sd(n_obs) / sqrt(n())),
            upper_ci = mean(n_obs) + qt(0.975, df = n() - 1) * (sd(n_obs) / sqrt(n())),
            .groups = 'drop')

fig_1_gg_summary_dens<-drone_ground_figs%>%
  filter(species == 'GG')%>%
  group_by(method, area)%>%
  summarize(mean = mean(dens),
            lower_ci = mean(dens) - qt(0.975, df = n() - 1) * (sd(dens) / sqrt(n())),
            upper_ci = mean(dens) + qt(0.975, df = n() - 1) * (sd(dens) / sqrt(n())),
            .groups = 'drop')
  
dodge <- position_dodge(width = 0.8)

fig1_crp_obs_ci<-drone_ground_figs%>%
  filter(species == 'CRP', dens >= 0,  n_obs < 50)%>%
  ggplot()+
  geom_boxplot(aes(x = area, y=n_obs, fill = method), outlier.colour = "red", alpha = 0.5, position = dodge)+
  geom_errorbar(data = fig_1_crp_summary_obs, 
                aes(x = area, ymin = lower_ci, ymax = upper_ci, color = method), 
                width = 0.2, size = 1.2, 
                position = dodge,
                show.legend = FALSE) +
  geom_point(data = fig_1_crp_summary_obs, 
             aes(x = area, y = mean, color = method), 
             size = 3, 
             position = dodge,
             show.legend = FALSE) +
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  scale_color_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Individuals observed', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

fig1_crp_dens_ci<-drone_ground_figs%>%
  filter(species == 'CRP', dens >= 0)%>%
  ggplot()+
  geom_boxplot(aes(x = area, y=dens, fill = method), outlier.colour = "red", alpha = 0.5, position = dodge)+
  geom_errorbar(data = fig_1_crp_summary_dens, 
                aes(x = area, ymin = lower_ci, ymax = upper_ci, color = method), 
                width = 0.2, size = 1.2, 
                position = dodge,
                show.legend = FALSE) +
  geom_point(data = fig_1_crp_summary_dens, 
             aes(x = area, y = mean, color = method), 
             size = 3, 
             position = dodge,
             show.legend = FALSE) +
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  scale_color_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Animals per ha', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")


fig1_gg_obs_ci<-drone_ground_figs%>%
  filter(species == 'GG', dens >= 0)%>%
  ggplot()+
  geom_boxplot(aes(x = area, y=n_obs, fill = method), outlier.colour = "red", alpha = 0.5, position = dodge)+
  geom_errorbar(data = fig_1_gg_summary_obs, 
                aes(x = area, ymin = lower_ci, ymax = upper_ci, color = method), 
                width = 0.2, size = 1.2, 
                position = dodge,
                show.legend = FALSE) +
  geom_point(data = fig_1_gg_summary_obs, 
             aes(x = area, y = mean, color = method), 
             size = 3, 
             position = dodge,
             show.legend = FALSE) +
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  scale_color_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Individuals observed', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

fig1_gg_dens_ci<-drone_ground_figs%>%
  filter(species == 'GG')%>%
  ggplot()+
  geom_boxplot(aes(x = area, y=dens, fill = method), outlier.colour = "red", alpha = 0.5, position = dodge)+
  geom_errorbar(data = fig_1_gg_summary_dens, 
                aes(x = area, ymin = lower_ci, ymax = upper_ci, color = method), 
                width = 0.2, size = 1.2, 
                position = dodge,
                show.legend = FALSE) +
  geom_point(data = fig_1_gg_summary_dens, 
             aes(x = area, y = mean, color = method), 
             size = 3, 
             position = dodge,
             show.legend = FALSE) +
  scale_fill_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  scale_color_brewer(palette = 'Dark2', labels = c("Drone survey", "Ground survey"))+
  labs(fill = NULL, y = 'Individuals observed', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position="bottom")

(fig1_crp_obs_ci + theme(axis.text.x = element_blank()) + 
    fig1_gg_obs_ci + theme(axis.text.x = element_blank(),
                        axis.title.y = element_blank())) / 
  (fig1_crp_dens_ci + 
     fig1_gg_dens_ci + theme(axis.title.y = element_blank())) +
  plot_layout(guides = 'collect') +  
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

ggsave('1.obs_dens_ci.svg',path = 'figures/', width = 25, height = 20, units = 'cm', dpi = 600)

#presentation

dir.create('figures/presentation')

fig1_crp_obs + fig1_gg_obs +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')  
  

ggsave('1.obs.svg',path = 'figures/presentation/', width = 25, height = 20, units = 'cm', dpi = 600)

fig1_crp_dens + fig1_gg_dens +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')  


ggsave('2.dens.svg',path = 'figures/presentation/', width = 25, height = 20, units = 'cm', dpi = 600)


# coupe density vs detection - GG / CRP -----------------------------------

## Figure 2 ##

drone_dens_gg<-left_join(drone_est%>%filter(species == 'GG', survey == 'coupe'),
                             ground_est_area%>%filter(species == 'GG', survey == 3)%>%
                               mutate(pres = case_when(n_obs>0~1,
                                                       TRUE ~ 0))%>%
                               select(site_ID, pres, dens), 
                             by = 'site_ID')

summary(glm(pres ~ den_adj, data = drone_dens_gg, family = 'binomial'))
summary(glm(pres ~ N, data = drone_dens_gg, family = 'binomial'))

wilcox.test(drone_dens_gg$pres, drone_dens_gg$den_adj, paired = TRUE)


drone_dens_crp<-left_join(drone_est%>%filter(species == 'CRP', survey == 'coupe'),
                         ground_est_area%>%filter(species == 'CRP', survey == 3)%>%
                           mutate(pres = case_when(n_obs>0~1,
                                                   TRUE ~ 0))%>%
                           select(site_ID, pres, dens), 
                         by = 'site_ID')

summary(glm(pres ~ den_adj, data = drone_dens_crp, family = 'binomial'))


fig_2_b<-drone_dens_gg%>%
  ggplot(aes(x = den_adj, y = pres))+
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = F, color = 'black')+
  scale_y_continuous(labels = label_percent())+
  labs(fill = NULL, y = 'Likelihood of detection by ground survey', x = 'Estimated coupe density (animals/ha)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

fig_2_a<-drone_dens_crp%>%
  ggplot(aes(x = den_adj, y = pres))+
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), 
              se = F, color = 'black')+
  scale_y_continuous(labels = label_percent())+
  labs(fill = NULL, y = 'Likelihood of detection by ground survey', x = 'Estimated coupe density (animals/ha)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

fig_2_a + fig_2_b +   plot_annotation(tag_levels = 'A')

ggsave('2.obs_dens.svg',path = 'figures/', width = 30, height = 15, units = 'cm', dpi = 600)

 # example coupes ----------------------------------------------------------

## Figure 3 ##

#drone data - ggs only

drone_sf<-st_as_sf(drone_fix, coords = c('speciesEas', 'speciesNor'), crs = 4326)%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID, startDate)%>%
  group_split()

drone_fix_gg_sites<-drone_fix%>%
  filter(species == 'Southern greater glider')

site_ids<-unique(drone_fix_gg_sites[, c('site_ID', 'startDate')])%>%
  as.data.frame()%>%
  arrange(site_ID)%>%
  mutate(namedate = paste0(site_ID, '__', startDate))

names(drone_sf)<-site_ids$namedate

#ground data - ggs only

ground_sf<-unique_obs%>%filter(observer_no == 1, species == 'Southern greater glider')%>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326, remove = F)%>%
  group_by(site_ID)%>%
  group_split()

ground_gg_sites<-unique_obs%>%
  filter(species == 'Southern greater glider')

site_ids_ground<-unique(unique_obs$site_ID)%>%
  as.data.frame()%>%
  rename(site_ID = 1)%>%
  arrange(site_ID)

names(ground_sf)<-site_ids_ground$site_ID


fig_3_a<-ggplot()+
  geom_sf(data = flight_track_polys$Alfred, fill = 'grey')+
  #geom_sf(data = flight_paths_trans$Alfred, size = 0.7, color = 'lightgrey')+
  geom_sf(data = drone_sf$`Alfred__2023-06-05`, aes(color = 'dodgerblue'), size = 1)+
  geom_sf(data = ground_track_polys$Alfred, color = 'blue', lwd = 1.5)+
  geom_sf(data = ground_track_trans$Alfred, color = 'grey', lwd = 0.7)+
  scale_x_continuous(breaks = c(147.452, 147.456, 147.460))+ 
  #geom_sf(data = ginters_ground, aes(color = 'darkgreen'), size = 3, pch = 3 )+
  scale_color_identity(name = '',
                       breaks = c('red', 'darkgreen', 'dodgerblue'),
                       labels = c("Drone obs.", "Ground obs.", "Drone obs."),
                       guide = NULL)+
  theme_bw()+
  theme(legend.position="bottom")

fig_3_b<-ggplot()+
  geom_sf(data = flight_track_polys$Castella, fill = 'grey')+
  #geom_sf(data = flight_paths_trans$Alfred, size = 0.7, color = 'lightgrey')+
  geom_sf(data = ground_track_polys$Castella, color = 'blue', lwd = 1.5)+
  geom_sf(data = ground_track_trans$Castella, color = 'grey', lwd = 0.7)+
  geom_sf(data = drone_sf$`Castella__2023-08-28`, aes(color = 'dodgerblue'), size = 1)+
  geom_sf(data = ground_sf$Castella, aes(color = 'darkgreen'), size = 3, pch = 3 )+
  scale_x_continuous(breaks = c(145.430, 145.434, 145.438))+
  scale_color_identity(name = '',
                       breaks = c('red', 'darkgreen', 'dodgerblue'),
                       labels = c("Drone obs.", "Ground obs.", "Drone obs."),
                       guide = 'legend')+
  theme_bw()+
  theme(legend.position="bottom")


fig_3_c<-ggplot()+
  geom_sf(data = flight_track_polys$Troop, fill = 'grey')+
  #geom_sf(data = flight_paths_trans$Alfred, size = 0.7, color = 'lightgrey')+
  geom_sf(data = ground_track_polys$Troop, color = 'blue', lwd = 1.5)+
  geom_sf(data = ground_track_trans$Troop, color = 'grey', lwd = 0.7)+
  #geom_sf(data = drone_sf$`Troop__2022-12-15`, aes(color = 'dodgerblue'), size = 1)+
  #geom_sf(data = drone_sf$`Troop__2023-03-15`, aes(color = 'dodgerblue'), size = 1)+
  geom_sf(data = drone_sf$`Troop__2022-12-15`, aes(color = 'dodgerblue'), size = 1)+
  geom_sf(data = ground_sf$Troop, aes(color = 'darkgreen'), size = 3, pch = 3 )+
  scale_color_identity(name = '',
                       breaks = c('red', 'darkgreen', 'dodgerblue'),
                       labels = c("Drone obs.", "Ground obs.", "Drone obs."),
                       guide = NULL)+
  theme_bw()+
  theme(legend.position="bottom")


#compile

fig_3_a + fig_3_c + fig_3_b +   
  plot_layout(guides = 'collect') +  
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

ggsave('3.coupe_compare.svg',path = 'figures/', width = 30, height = 20, units = 'cm', dpi = 600)

#example with all obs and transects - Fig S1

drone_ginters<-st_as_sf(drone_fix, coords = c('speciesEas', 'speciesNor'), crs = 4326)%>%
  filter(site_ID == 'Ginters', startDate == '2023-05-23')

ground_ginters<-unique_obs%>%filter(observer_no == 1, site_ID == 'Ginters')%>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326, remove = F)

ggplot()+
  geom_sf(data = flight_track_polys$Ginters, fill = 'grey')+
  geom_sf(data = flight_paths_trans$Ginters, size = 0.7, color = 'lightgrey')+
  geom_sf(data = ground_track_polys$Ginters, color = 'blue', lwd = 1.5)+
  geom_sf(data = ground_track_trans$Ginters, color = 'grey', lwd = 0.7)+
  geom_sf(data = drone_ginters, aes(color = species), size = 2)+
  geom_sf(data = ground_ginters, aes(color = species), size = 4, pch = 3 )+
  scale_x_continuous(breaks = c(145.584, 145.589, 145.594))+
  labs(color = NULL)+
  theme_bw()+
  theme(legend.position="bottom")

ggsave('S1.coupe_example.svg',path = 'figures/', width = 20, height = 30, units = 'cm', dpi = 600)


## fig 4 ##

#nni graph

drone_dens_ground_nni<-left_join(drone_dens_ground, 
                                 nni_GG%>%filter(n>2)%>%group_by(site_ID)%>%summarise(mean_nni = mean(nni)),
                                 by = 'site_ID')

drone_dens_ground_nni%>%filter(mean_nni<=2.2)%>%
  ggplot(aes(x = mean_nni, y = den_adj))+
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  #geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "Gamma"), se = F, color = 'black')+
  labs(fill = NULL, y = 'Estimated coupe density (animals/ha)', x = 'NNI')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave('4.nni_dens.svg',path = 'figures/', width = 15, height = 10, units = 'cm', dpi = 600)


drone_dens_ground_nni%>%filter(mean_nni<=2.2)%>%
  ggplot(aes(x = mean_nni, y = dens))+
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  #geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "Gamma"), se = F, color = 'black')+
  labs(fill = NULL, y = 'Estimated coupe density (animals/ha)', x = 'NNI')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))



#associated model

summary(glm(den_adj~mean_nni, family = 'Gamma', data = drone_dens_ground_nni%>%filter(mean_nni<=2.2)))
summary(glm(dens_diff~mean_nni, family = 'Gamma', data = drone_dens_ground_nni%>%filter(mean_nni<=2.2)))


# survey area and time ----------------------------------------------------

## Fig 5 ##

# all obs - length x obs

fig_5_a<-drone_times_obs_area%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  filter(area_ha_drone<150)%>%
  ggplot(aes(x = length, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = 'No. of total obs.', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#all obs - length x area

fig_5_b<-drone_times_obs_area%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  filter(area_ha_drone<150)%>%
  ggplot(aes(x = area_ha_drone, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = NULL, x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#crp - length x obs

fig_5_c<-drone_times_obs_area%>%
  filter(species == 'Common ringtail possum')%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  filter(area_ha_drone<150)%>%
  ggplot(aes(x = length, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = 'No. of CRP obs.', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


#crp - area x obs

fig_5_d<-drone_times_obs_area%>%
  filter(species == 'Common ringtail possum')%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  filter(area_ha_drone<150)%>%
  ggplot(aes(x = area_ha_drone, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = NULL, x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#gg - length x obs

fig_5_e<-drone_times_obs_area%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  filter(area_ha_drone<150)%>%
  ggplot(aes(x = length, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = 'No. of SGG obs.', x = 'Drone survey length (h)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#gg - area x obs

fig_5_f<-drone_times_obs_area%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  filter(area_ha_drone<150)%>%
  ggplot(aes(x = area_ha_drone, y = n))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = NULL, x = 'Survey area (ha)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

(fig_5_a + fig_5_b)/
  (fig_5_c + fig_5_d)/
  (fig_5_e + fig_5_f)+  
  plot_annotation(tag_levels = 'A') 

ggsave('5.drone_length_obs.svg',path = 'figures/', width = 30, height = 25, units = 'cm', dpi = 600)

#redo with predicted CIs - pre review Ecological informatics

fig_5_a_pred_df<-drone_times_obs_area%>%
  group_by(site_ID, startDate, length, area_ha_drone)%>%
  summarise(n = sum(countNumbe))%>%
  na.omit()

fig_5_a_model<-lm(n~length, data = fig_5_a_pred_df)

fig_5_a_pred_preds<- predict(fig_5_a_model, newdata = fig_5_a_pred_df, interval = "confidence")
fig_5_a_pred_df$fit <- fig_5_a_pred_preds[, "fit"]
fig_5_a_pred_df$lower_ci <- fig_5_a_pred_preds[, "lwr"]
fig_5_a_pred_df$upper_ci <- fig_5_a_pred_preds[, "upr"]


fig_5_a_ci<-fig_5_a_pred_df%>%
  ggplot(aes(x = length, y = n))+
  geom_point()+
  geom_line(aes(y = fit), color = "blue")+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue")+
  labs(y = 'No. of total obs.', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#make function

ci_graph<-function(dataset, variable, ylabel = NULL, xlabel = NULL){
  
  #redo with actual CIs
  
  pred_df<-dataset%>%
    group_by(site_ID, startDate, length, area_ha_drone)%>%
    summarise(n = sum(countNumbe))%>%
    filter(area_ha_drone<150)%>%
    na.omit()%>%
    mutate(modvar = {{ variable }})
  
  model<-lm(n~modvar, data = pred_df)
  
  preds<- predict(model, newdata = pred_df, interval = "confidence")
  pred_df$fit <- preds[, "fit"]
  pred_df$lower_ci <- preds[, "lwr"]
  pred_df$upper_ci <- preds[, "upr"]
  
  
  graph<-pred_df%>%
    ggplot(aes(x = {{ variable }}, y = n))+
    geom_line(aes(y = fit), color = "black", size = 1.1)+
    geom_point()+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue")+
    labs(y = ylabel, x = xlabel)+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
          axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  return(graph)
  
}

ci_graph_filter<-function(dataset, spp, variable,ylabel = NULL, xlabel = NULL){
  
  #redo with actual CIs
  
  pred_df<-dataset%>%
    filter(species == spp)%>%
    group_by(site_ID, startDate, length, area_ha_drone)%>%
    summarise(n = sum(countNumbe))%>%
    filter(area_ha_drone<150)%>%
    na.omit()%>%
    mutate(modvar = {{ variable }})
  
  model<-lm(n~modvar, data = pred_df)
  
  preds<- predict(model, newdata = pred_df, interval = "confidence")
  pred_df$fit <- preds[, "fit"]
  pred_df$lower_ci <- preds[, "lwr"]
  pred_df$upper_ci <- preds[, "upr"]
  
  
  graph<-pred_df%>%
    ggplot(aes(x = {{ variable }}, y = n))+
    geom_line(aes(y = fit), color = "black", size = 1.1)+
    geom_point()+
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2, fill = "blue")+
    labs(y = ylabel, x = xlabel)+
    theme_bw()+
    theme(plot.title = element_text(size =18, face='bold'),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14), 
          axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
          axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  return(graph)
  
}


ci_graph(drone_times_obs_area, area_ha_drone)
ci_graph_filter(drone_times_obs_area, spp = 'Southern greater glider', length)

fig_5_a_ci<-ci_graph(drone_times_obs_area, length, ylabel = 'No. of total obs.')
fig_5_b_ci<-ci_graph(drone_times_obs_area, area_ha_drone)
fig_5_c_ci<-ci_graph_filter(drone_times_obs_area, spp = 'Common ringtail possum', length, ylabel = 'No. of CRP obs.')
fig_5_d_ci<-ci_graph_filter(drone_times_obs_area, spp = 'Common ringtail possum', area_ha_drone)
fig_5_e_ci<-ci_graph_filter(drone_times_obs_area, spp = 'Southern greater glider', length, ylabel = 'No. of SGG obs.', xlabel = 'Drone survey length (h)')
fig_5_f_ci<-ci_graph_filter(drone_times_obs_area, spp = 'Southern greater glider', area_ha_drone, xlabel = 'Survey length (ha)')

(fig_5_a_ci + fig_5_b_ci)/
  (fig_5_c_ci + fig_5_d_ci)/
  (fig_5_e_ci + fig_5_f_ci)+  
  plot_annotation(tag_levels = 'A') 

ggsave('5.drone_length_obs_ci.svg',path = 'figures/', width = 30, height = 25, units = 'cm', dpi = 600)


#for pres

(fig_5_a + fig_5_b)/
  (fig_5_c + fig_5_d) 

ggsave('3.effort_all_crp.svg',path = 'figures/presentation/', width = 25, height = 15, units = 'cm', dpi = 600)

(fig_5_e + fig_5_f)
  
ggsave('4.effort_all_gg.svg',path = 'figures/presentation/', width = 25, height = 10, units = 'cm', dpi = 600)

# ground surveys

fig_s5_a<-ggplot(spot_effort_all, aes(x = survey_length, y = all_obs))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = 'No. of total obs.', x = 'Ground survey length (h)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

fig_s5_b<-ggplot(spot_effort_all, aes(x = survey_length, y = crp_obs))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = 'No. of CRP obs.', x = 'Ground survey length (h)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

fig_s5_c<-ggplot(spot_effort_all, aes(x = survey_length, y = gg_obs))+
  geom_point()+
  geom_smooth(method = 'lm')+
  labs(y = 'No. of SGG obs.', x = 'Ground survey length (h)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

fig_s5_a / fig_s5_b / fig_s5_c +  
  plot_annotation(tag_levels = 'A') 

ggsave('S5.effort_length_ground.svg',path = 'figures/', width = 15, height = 25, units = 'cm', dpi = 600)


# cumulative observations -------------------------------------------------

## Fig 6 ##

full_hours<-flight_times_end%>%dplyr::select(site_ID, date_start, diff_time)%>%
  mutate(length = round(diff_time))%>%
  na.omit()%>%
  rowwise()%>%
  do(data.frame(
    site_ID = .$site_ID,
    startDate = .$date_start,
    obs_hour = 0:.$length))%>%
  as_tibble()

#gg

drone_obs_hr_gg<-drone_times_frac%>%
  filter(!is.na(obs_hour), species == 'Southern greater glider')%>%
  group_by(site_ID, startDate, obs_hour)%>%
  dplyr::count()

drone_obs_hr_gg_complete<-left_join(full_hours, drone_obs_hr_gg, by = c('site_ID', 'startDate', 'obs_hour'))%>%
  mutate(n = replace_na(n, 0))%>%
  group_by(site_ID, startDate)%>%
  mutate(ID = paste(site_ID, startDate, sep = '_'))%>%
  arrange(site_ID, startDate, obs_hour)%>%
  mutate(cum = cumsum(n))%>%
  group_by(ID)%>%
  mutate(plateau = check_plateau(cum, threshold = 0.10, n_last = 2))

plateau_sites_gg <- drone_obs_hr_gg_complete %>%
  group_by(ID) %>%
  summarize(has_plateaued = any(plateau))

fig_6_e<-ggplot(drone_obs_hr_gg_complete, aes(x = obs_hour, y = cum, color = ID))+
  geom_point(show.legend = F)+
  #geom_line(show.legend = F)+
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = drone_obs_hr_gg_complete %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(y = 'No of cumulative SGG obs.', x = 'Survey length (h)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#crp

drone_obs_hr_crp<-drone_times_frac%>%
  filter(!is.na(obs_hour), species == 'Common ringtail possum')%>%
  group_by(site_ID, startDate, obs_hour)%>%
  dplyr::count()

drone_obs_hr_crp_complete<-left_join(full_hours, drone_obs_hr_crp, by = c('site_ID', 'startDate', 'obs_hour'))%>%
  mutate(n = replace_na(n, 0))%>%
  group_by(site_ID, startDate)%>%
  mutate(ID = paste(site_ID, startDate, sep = '_'))%>%
  arrange(site_ID, startDate, obs_hour)%>%
  mutate(cum = cumsum(n))%>%
  group_by(ID)%>%
  mutate(plateau = check_plateau(cum, threshold = 0.10, n_last = 2))

plateau_sites_crp <- drone_obs_hr_crp_complete %>%
  group_by(ID) %>%
  summarize(has_plateaued = any(plateau))


fig_6_c<-ggplot(drone_obs_hr_crp_complete, aes(x = obs_hour, y = cum, color = ID))+
  geom_point(show.legend = F)+
  #geom_line(show.legend = F)+
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = drone_obs_hr_crp_complete %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(y = 'No of cumulative CRP obs.', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


#all

drone_obs_hr<-drone_times_frac%>%
  filter(!is.na(obs_hour))%>%
  group_by(site_ID, startDate, obs_hour)%>%
  dplyr::count()

drone_obs_hr_complete<-left_join(full_hours, drone_obs_hr, by = c('site_ID', 'startDate', 'obs_hour'))%>%
  mutate(n = replace_na(n, 0))%>%
  group_by(site_ID, startDate)%>%
  mutate(ID = paste(site_ID, startDate, sep = '_'))%>%
  arrange(site_ID, startDate, obs_hour)%>%
  mutate(cum = cumsum(n))%>%
  group_by(ID)%>%
  mutate(plateau = check_plateau(cum, threshold = 0.10, n_last = 2))

plateau_sites_all <- drone_obs_hr_complete %>%
  group_by(ID) %>%
  summarize(has_plateaued = any(plateau))

fig_6_a<-ggplot(drone_obs_hr_complete, aes(x = obs_hour, y = cum, color = ID))+
  geom_point(show.legend = F)+
  #geom_line(show.legend = F)+
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = drone_obs_hr_complete %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(y = 'No of cumulative obs.', x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))


#add ground results

full_hours_spot<-spot_survey_times%>%dplyr::select(site_ID, survey_length)%>%
  mutate(length = round(survey_length))%>%
  rowwise()%>%
  do(data.frame(
    site_ID = .$site_ID,
    obs_hour = 0:.$length))%>%
  as_tibble()

#all

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

spot_plateau_all <- spot_obs_hr_complete %>%
  group_by(site_ID) %>%
  summarize(has_plateaued = any(plateau))

fig_6_b<-ggplot(spot_obs_hr_complete, aes(x = obs_hour, y = cum, color = site_ID))+
  geom_point(show.legend = F)+
  #geom_line(show.legend = F)+
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = spot_obs_hr_complete %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(y = NULL, x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#crp

spot_obs_hr_crp<-spot_times_frac%>%
  filter(species == 'Common ringtail possum')%>%
  group_by(site_ID, obs_hour)%>%
  dplyr::count()

spot_obs_hr_complete_crp<-left_join(full_hours_spot, spot_obs_hr_crp, by = c('site_ID', 'obs_hour'))%>%
  mutate(n = replace_na(n, 0))%>%
  group_by(site_ID)%>%
  arrange(site_ID, obs_hour)%>%
  mutate(cum = cumsum(n),
         plateau = check_plateau(cum, threshold = 0.10, n_last = 2))

spot_plateau_crp <- spot_obs_hr_complete_crp %>%
  group_by(site_ID) %>%
  summarize(has_plateaued = any(plateau))

fig_6_d<-ggplot(spot_obs_hr_complete_crp, aes(x = obs_hour, y = cum, color = site_ID))+
  geom_point(show.legend = F)+
  #geom_line(show.legend = F)+
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = spot_obs_hr_complete_crp %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(y = NULL, x = NULL)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#gg

spot_obs_hr_gg<-spot_times_frac%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID, obs_hour)%>%
  dplyr::count()

spot_obs_hr_complete_gg<-left_join(full_hours_spot, spot_obs_hr_gg, by = c('site_ID', 'obs_hour'))%>%
  mutate(n = replace_na(n, 0))%>%
  group_by(site_ID)%>%
  arrange(site_ID, obs_hour)%>%
  mutate(cum = cumsum(n),
         plateau = check_plateau(cum, threshold = 0.10, n_last = 2))

spot_plateau_gg <- spot_obs_hr_complete_gg %>%
  group_by(site_ID) %>%
  summarize(has_plateaued = any(plateau))

fig_6_f<-ggplot(spot_obs_hr_complete_gg, aes(x = obs_hour, y = cum, color = site_ID))+
  geom_point(show.legend = F)+
  #geom_line(show.legend = F)+
  geom_smooth(method = 'loess', se = F, show.legend = F)+
  geom_point(data = spot_obs_hr_complete_gg %>% filter(plateau), shape = 8, size = 4, show.legend = F) +
  labs(y = NULL, x = 'Survey length (h)')+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

(fig_6_a + fig_6_b) /
  (fig_6_c + fig_6_d) /
  (fig_6_e + fig_6_f) +
plot_annotation(tag_levels = 'A')

ggsave('6.drone_cum_obs.svg',path = 'figures/', width = 25, height = 25, units = 'cm', dpi = 600)


# obs histogram - not used ------------------------------------------------

#drone

drone_times_obs_GG<-drone_times_obs%>%filter(species == 'Southern greater glider')
drone_times_obs_CRP<-drone_times_obs%>%filter(species == 'Common ringtail possum')

hist(drone_times_obs$time_diff)
hist(drone_times_obs_GG$time_diff)
hist(drone_times_obs_CRP$time_diff)


drone_times_obs%>%
  ggplot(aes(time_diff))+
  geom_histogram(color = 'black', fill = 'dodgerblue', binwidth = 1, boundary = 0)+
  scale_x_continuous(breaks = seq(0, 8, by = 1))+
  scale_y_continuous(breaks = seq(0, 400, by = 50))+
  labs(x = 'Time after survey start (h)', y = 'No. of observations')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
  

drone_times_obs_GG%>%
  ggplot(aes(time_diff))+
  geom_histogram(color = 'black', fill = 'dodgerblue', binwidth = 1, boundary = 0)+
  scale_x_continuous(breaks = seq(0, 7, by = 1))+
  scale_y_continuous(breaks = seq(0, 65, by = 15))+
  labs(x = 'Time after survey start (h)', y = 'No. of SGGs')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

drone_times_obs_CRP%>%
  ggplot(aes(time_diff))+
  geom_histogram(color = 'black', fill = 'dodgerblue', binwidth = 1, boundary = 0)+
  scale_x_continuous(breaks = seq(0, 8, by = 1))+
  scale_y_continuous(breaks = seq(0, 200, by = 50))+
  labs(x = 'Time after survey start (h)', y = 'No. of SGGs')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14), 
        axis.text.x = element_text(size = 12, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 12, face = 'bold', color = 'black'),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

# site map ----------------------------------------------------------------

plot(flight_track_polys$Alfred$geom)

alf_cent<-st_centroid(flight_track_polys$Alfred)

plot(alf_cent$geom, add = T)

#get all centroids

flight_track_centroids<-map(flight_track_polys, st_transform, crs = 4326)
flight_track_centroids<-map(flight_track_centroids, st_centroid)%>%
  bind_rows()

plot(flight_track_centroids$geom)

gint_4326<-flight_track_polys$Ginters%>%st_transform(crs = 4326)

plot(gint_4326, col = 'red', axes = T)


flight_track_centroids$Long<-st_coordinates(flight_track_centroids)[,1]
flight_track_centroids$Lat<-st_coordinates(flight_track_centroids)[,2]


#make inset

aus<-st_read('data/spatial/map/Australia_proj.shp')%>%
  #filter(STATENAME == "Victoria")%>%
  st_transform(crs = 4326)

inset<-ggplot(data = aus, show.legend = "point")+
  geom_sf(fill = 'white', color = 'black')+
  geom_rect(xmin = extent(flight_track_centroids)[1], xmax = extent(flight_track_centroids)[2],
            ymin = extent(flight_track_centroids)[3], ymax = extent(flight_track_centroids)[4],
            color = 'red', fill = NA, size = 1.5)+
  labs(x = NULL, y = NULL, title = NULL)+
  coord_sf(expand = F)+
  theme_void()

register_stadiamaps('b911e5ce-ac12-4c16-8133-b30a12cb39a3', write = T)

baselayer <- get_stadiamap(bbox = c(left = extent(flight_track_centroids)[1],
                                    bottom = extent(flight_track_centroids)[3],
                                    right = extent(flight_track_centroids)[2],
                                    top = extent(flight_track_centroids)[4]),
                           maptype = "stamen_terrain", 
                           crop = F)

map<-ggmap(baselayer) + labs(x = "", y = "") +
  geom_point(data = flight_track_centroids, aes(x = Long, y = Lat), 
             color = 'black', fill = 'dodgerblue', shape = 25, size = 3) +
  coord_sf(crs = 4326)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.position = 'bottom',
        plot.margin = margin(0,1,0,0, unit = 'cm'))+
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'br', style = 'ticks', text_col = 'black')

map+annotation_custom(ggplotGrob(inset), xmin = 147, xmax = 147.6, ymin = -37.19, ymax = -37.5)

ggsave('S1.map.svg',path = 'figures', width = 40, height = 20,
       units = 'cm', dpi = 120)
