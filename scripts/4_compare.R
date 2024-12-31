# ground and drone results comparison -------------------------------------

library(tidyverse)

ground_est<-read.csv('outputs/compare/spot_dens.csv')%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID),
         method = 'spot')

ground_est_area<-read.csv('outputs/compare/spot_dens_ground-area.csv')

drone_est<-read.csv('outputs/compare/drone_dens.csv')%>%
  mutate(method = 'drone')

drone_est_filter<-read.csv('outputs/compare/drone_dens.csv')%>%
  mutate(method = 'drone')%>%
  group_by(site_ID, species, survey)%>%
  slice_max(n, with_ties = F)%>%
  ungroup()


# combine data - compare estimate -----------------------------------------

drone_ground<-rbind(ground_est%>%select(site_ID, species, n_obs, dens = Estimate, survey, method),
                    drone_est_filter%>%select(site_ID, species, n_obs = n, dens = den_adj, survey, method))

drone_ground_area<-rbind(ground_est_area,
                         drone_est_filter%>%select(site_ID, species, n_obs = n, dens = den_adj, survey, method))

drone_ground%>%filter(dens > 0, dens < 6)%>%
  mutate(method = fct_relevel(method, 'drone', 'spot'))%>%
  ggplot(aes(x = survey, y=dens, fill = method))+
  geom_boxplot()+
  facet_wrap(~species)

drone_ground_area%>%filter(dens >= 0, dens < 4)%>%
  mutate(method = fct_relevel(method, 'drone', 'spot'))%>%
  ggplot(aes(x = survey, y=dens, fill = method))+
  geom_boxplot()+
  facet_wrap(~species)

drone_ground%>%filter(n_obs > 0, n_obs < 50)%>%
  mutate(method = fct_relevel(method, 'drone', 'spot'))%>%
  ggplot(aes(x = survey, y=n_obs, fill = method))+
  geom_boxplot()+
  facet_wrap(~species)

drone_ground_area%>%filter(n_obs >= 0 & n_obs < 40)%>%
  mutate(method = fct_relevel(method, 'drone', 'spot'))%>%
  ggplot(aes(x = survey, y=n_obs, fill = method))+
  geom_boxplot()+
  facet_wrap(~species)

#density vs probability of detection

drone_dens_ground<-left_join(drone_est%>%filter(species == 'GG', survey == 'coupe'),
                             ground_est_area%>%filter(species == 'GG', survey == 3)%>%
                               mutate(pres = case_when(n_obs>0~1,
                                                       TRUE ~ 0))%>%
                               select(site_ID, pres, dens), 
                             by = 'site_ID')

#model

dens_presabs<-glm(pres ~ den_adj, data = drone_dens_ground, family = 'binomial')
summary(dens_presabs)

# drone_dens_ground<-left_join(drone_est_filter%>%filter(species == 'GG', survey == 'coupe'),
#                              ground_est_area%>%filter(species == 'GG', survey == 3)%>%
#                                mutate(pres = case_when(n_obs>0~1,
#                                                        TRUE ~ 0))%>%
#                                select(site_ID, pres, dens), 
#                              by = 'site_ID')%>%
#   mutate(dens_diff = dens-den_adj)

#add nni

drone_dens_ground_nni<-left_join(drone_dens_ground, 
                                 nni_GG%>%filter(n>2)%>%group_by(site_ID)%>%summarise(mean_nni = mean(nni)),
                                 by = 'site_ID')

drone_dens_ground%>%
  ggplot(aes(x = den_adj, y = pres))+
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = F, color = 'black')
  
drone_dens_ground%>%
  filter(dens > 0 )%>%
  ggplot(aes(x = den_adj, y = dens))+
  geom_point()+
  geom_smooth(se = F, method = 'gam')

drone_dens_ground_nni%>%filter(mean_nni<=2.15)%>%
  ggplot(aes(x = mean_nni, y = den_adj))+
  geom_jitter(height = 0.05, width = 0, alpha = 0.5) +
  #geom_point()+
  stat_smooth(method = "glm", method.args = list(family = "Gamma"), se = F, color = 'black')

summary(glm(den_adj~mean_nni, family = Gamma(link = 'log'), data = drone_dens_ground_nni%>%filter(mean_nni<=2.2)))


# sign tests between methods ----------------------------------------------

wilcox.test(drone_dens_ground$den_adj, drone_dens_ground$dens, paired = TRUE)

#get individual estimates

#gg

GG_sign_obs<-drone_ground_area%>%filter(species == 'GG')%>%select(1,3,5,6)%>%
  pivot_wider(names_from = c('method', 'survey'), values_from = 'n_obs')

wilcox.test(GG_sign_obs$spot_3, GG_sign_obs$drone_3, paired = TRUE)
wilcox.test(GG_sign_obs$spot_1, GG_sign_obs$drone_1, paired = TRUE)
wilcox.test(GG_sign_obs$spot_3, GG_sign_obs$drone_coupe, paired = TRUE)

GG_sign_dens<-drone_ground_area%>%filter(species == 'GG')%>%select(1,4:6)%>%
  pivot_wider(names_from = c('method', 'survey'), values_from = 'dens')

wilcox.test(GG_sign_dens$spot_3, GG_sign_dens$drone_3, paired = TRUE)
wilcox.test(GG_sign_dens$spot_1, GG_sign_dens$drone_1, paired = TRUE)
wilcox.test(GG_sign_dens$spot_3, GG_sign_dens$drone_coupe, paired = TRUE)

#crp

CRP_sign_obs<-drone_ground_area%>%filter(species == 'CRP')%>%select(1,3,5,6)%>%
  pivot_wider(names_from = c('method', 'survey'), values_from = 'n_obs')

wilcox.test(CRP_sign_obs$spot_3, CRP_sign_obs$drone_3, paired = TRUE)
wilcox.test(CRP_sign_obs$spot_1, CRP_sign_obs$drone_1, paired = TRUE)
wilcox.test(CRP_sign_obs$spot_3, CRP_sign_obs$drone_coupe, paired = TRUE)

CRP_sign_dens<-drone_ground_area%>%filter(species == 'CRP')%>%select(1,4:6)%>%
  pivot_wider(names_from = c('method', 'survey'), values_from = 'dens')

wilcox.test(CRP_sign_dens$spot_3, CRP_sign_dens$drone_3, paired = TRUE)
wilcox.test(CRP_sign_dens$spot_1, CRP_sign_dens$drone_1, paired = TRUE)
wilcox.test(CRP_sign_dens$spot_3, CRP_sign_dens$drone_coupe, paired = TRUE)

# occupancy modeling ground vs drone --------------------------------------

coupes_gg<-drone_dens_ground%>%filter(n>0)%>%select(site_ID)%>%
  mutate(drone = 1)

coupes_crp<-drone_est%>%
  filter(species == 'CRP', survey == 'coupe', n > 0)%>%
  select(site_ID)%>%
  unique()%>%
  mutate(drone = 1)

#GG model

GG_det_drone<-left_join(GG_det_3%>%
                          mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                                                     TRUE ~ site_ID)), 
                        coupes_gg, by = 'site_ID')%>%
  mutate(ground = first + second,
         ground_pres = case_when(ground>0 ~ 1,
                                 TRUE ~ 0),
         drone_pres = case_when(is.na(drone) ~ 0,
                                TRUE ~ drone))%>%
  select(1, 6, 7)


GG_drone_mod <- occu(~ 1 ~ 1, data = as.matrix(GG_det_drone%>%select(2:3))%>%unmarkedFrameOccu())

backTransform(GG_drone_mod, type = "det")

#CRP model

CRP_det_drone<-left_join(CRP_det_3%>%
                          mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                                                     TRUE ~ site_ID)), 
                         coupes_crp, by = 'site_ID')%>%
  mutate(ground = first + second,
         ground_pres = case_when(ground>0 ~ 1,
                                 TRUE ~ 0),
         drone_pres = case_when(is.na(drone) ~ 0,
                                TRUE ~ drone))%>%
  select(1, 6, 7)

CRP_drone_mod <- occu(~ 1 ~ 1, data = as.matrix(CRP_det_drone%>%select(2:3))%>%unmarkedFrameOccu())

backTransform(CRP_drone_mod, type = "det")

#contingency tables

table(GG_det_drone$ground_pres, GG_det_drone$drone_pres)
table(CRP_det_drone$ground_pres, CRP_det_drone$drone_pres)


# descriptive comparison --------------------------------------------------

#n species by survey

drone_results_sp<-drone_fix%>%
  filter(species != 'unknown')%>%
  mutate(site_ID = case_when(site_ID == 'United' ~ 'Merger',
                             TRUE ~ site_ID))%>%
  group_by(site_ID, startDate, species)%>%
  summarise(n = sum(countNumbe))%>%
  group_by(site_ID, startDate)%>%
  tally()%>%
  group_by(site_ID)%>%
  mutate(survey_no = row_number())%>%
  select(site_ID, survey_no, spec_drone = n)

drone_no_obs<-sitevec_drone%>%as.data.frame()%>%rename(site_ID = 1)%>%
  filter(!site_ID %in% drone_results_sp$site_ID)%>%
  mutate(survey_no = 1,
         spec_drone = 0)

drone_results_sp_all<-rbind(drone_results_sp, drone_no_obs)

ground_no_obs<-spotlighting_fix%>%filter(n_animals == 0)

ground_results_sp<-spotlighting_fix_reduced%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  #filter(obs_type == 'seen')%>%
  group_by(site_ID, species)%>%
  summarise(n = sum(n_animals))%>%
  group_by(site_ID)%>%
  tally()%>%
  mutate(spec_ground = case_when(site_ID %in% ground_no_obs$site_ID ~ 0,
                       TRUE ~ n))

drone_ground_sp<-left_join(drone_results_sp_all, ground_results_sp%>%select(site_ID, spec_ground), by = 'site_ID')

drone_ground_sp%>%
  ggplot(aes(spec_ground, spec_drone))+
  geom_point()+
  #geom_jitter(width = .1)+
  scale_x_continuous(limits = c(0,10), breaks = seq(0, 10, by = 1))+
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, by = 1))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
  theme_bw()

#n obs in same footprint

drone_obs_ground<-bind_rows(drone_subset_ground)%>%
  filter(species != 'unknown')%>%
  mutate(site_ID = case_when(site_ID == 'United' ~ 'Merger',
                             TRUE ~ site_ID))%>%
  group_by(site_ID, startDate, species)%>%
  summarise(n = sum(countNumbe))%>%
  st_drop_geometry()%>%
  group_by(site_ID)%>%
  mutate(survey_no = dense_rank(startDate))%>%
  select(site_ID, survey_no, species, n_drone = n)

#get unique ground observations - visual only!

spot_obs<-spotlighting_fix_reduced%>%filter(obs_type == 'seen')

spot_obs_both<-spot_obs%>%filter(observer_no == 1 & seen_by_both == 1)%>%mutate(object = row_number()*12)
spot_obs_1<-spot_obs%>%filter(seen_by_obs1 == 1 & seen_by_obs2 == 0)%>%mutate(object = row_number()*121)
spot_obs_2<-spot_obs%>%filter(seen_by_obs1 == 0 & seen_by_obs2 == 1)%>%mutate(object = row_number()*262)

spot_obs_unique<-bind_rows(spot_obs_both, spot_obs_1, spot_obs_2)

spot_obs_ground<-spot_obs_unique%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  group_by(site_ID, species)%>%
  summarise(n_ground = sum(n_animals))

# all

drone_obs_ground_all<-drone_obs_ground%>%
  group_by(site_ID, survey_no)%>%
  summarise(drone_obs = sum(n_drone))

ground_obs_all<-spot_obs_ground%>%
  group_by(site_ID)%>%
  summarise(ground_obs = sum(n_ground))

drone_spot_ground_all<-left_join(drone_obs_ground_all, ground_obs_all, by = 'site_ID')%>%
  mutate(ground_obs = case_when(is.na(ground_obs) ~ 0,
                                TRUE ~ ground_obs))

drone_spot_ground_all%>%
  ggplot(aes(ground_obs, drone_obs))+
  geom_point()+
  #geom_jitter(width = .1)+
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, by = 1))+
  scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by = 1))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
  theme_bw()

# GG

drone_obs_ground_gg<-drone_obs_ground%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID, survey_no)%>%
  summarise(drone_obs = sum(n_drone))

drone_no_obs_gg<-sitevec_drone%>%as.data.frame()%>%rename(site_ID = 1)%>%
  filter(!site_ID %in% drone_obs_ground_gg$site_ID)%>%
  mutate(survey_no = 1,
         drone_obs = 0)

drone_obs_ground_gg_comb<-rbind(drone_obs_ground_gg, drone_no_obs_gg)

ground_obs_gg<-spot_obs_ground%>%
  filter(species == 'Southern greater glider')%>%
  group_by(site_ID)%>%
  summarise(ground_obs = sum(n_ground))

drone_spot_ground_gg<-left_join(drone_obs_ground_gg_comb, ground_obs_gg, by = 'site_ID')%>%
  mutate(ground_obs = case_when(is.na(ground_obs) ~ 0,
                                TRUE ~ ground_obs))

drone_spot_ground_gg%>%
  ggplot(aes(ground_obs, drone_obs))+
  geom_point()+
  #geom_jitter(width = .1)+
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, by = 1))+
  scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by = 1))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
  theme_bw()

# CRP

drone_obs_ground_crp<-drone_obs_ground%>%
  filter(species == 'Common ringtail possum')%>%
  group_by(site_ID, survey_no)%>%
  summarise(drone_obs = sum(n_drone))

drone_no_obs_crp<-sitevec_drone%>%as.data.frame()%>%rename(site_ID = 1)%>%
  filter(!site_ID %in% drone_obs_ground_crp$site_ID)%>%
  mutate(survey_no = 1,
         drone_obs = 0)

drone_obs_ground_crp_comb<-rbind(drone_obs_ground_crp, drone_no_obs_crp)

ground_obs_crp<-spot_obs_ground%>%
  filter(species == 'Common ringtail possum')%>%
  group_by(site_ID)%>%
  summarise(ground_obs = sum(n_ground))

drone_spot_ground_crp<-left_join(drone_obs_ground_crp_comb, ground_obs_crp, by = 'site_ID')%>%
  mutate(ground_obs = case_when(is.na(ground_obs) ~ 0,
                                TRUE ~ ground_obs))

drone_spot_ground_crp%>%
  ggplot(aes(ground_obs, drone_obs))+
  geom_point()+
  #geom_jitter(width = .1)+
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, by = 1))+
  scale_y_continuous(limits = c(0,30), breaks = seq(0, 30, by = 1))+
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed")+
  theme_bw()
