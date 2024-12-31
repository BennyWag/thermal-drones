# derive veg density effects --------------------------------------------------

library(openxlsx)
library(unmarked)
library(mrds)
library(Distance)
library(lmtest) #for lrtest
library(tidyverse)

veg_dens<-read.xlsx('data/detect/1_transect_master.xlsx')%>%
  mutate(veg_dens_num = case_when(grepl('ow', veg_density_estimate) ~ 1,
                                  grepl('ed', veg_density_estimate) ~ 2,
                                  grepl('ig', veg_density_estimate) ~ 3))

veg_dens_av<-veg_dens%>%
  group_by(site_ID)%>%
  summarise(mean_vd = mean(veg_dens_num))

veg_dens_av_transect<-veg_dens%>%
  group_by(site_ID, transect)%>%
  summarise(mean_vd = mean(veg_dens_num))

# influence on detectability ----------------------------------------------

#add veg density to model data

drone_double_sgg<-double_fun(drone_results_area, sp = 'Southern greater glider', sites_df = sites_double)%>%
  left_join(.,veg_dens_av, by = 'site_ID')%>%filter(site_ID !='Troop')

drone_double_crp<-double_fun(drone_results_area, sp = 'Common ringtail possum', sites_df = sites_double)%>%
  left_join(.,veg_dens_av, by = 'site_ID')

drone_double_sg<-double_fun(drone_results_area, sp = 'Sugar glider', sites_df = sites_double)%>%
  left_join(.,veg_dens_av, by = 'site_ID')

drone_double_fg<-double_fun(drone_results_area, sp = 'Feathertail glider', sites_df = sites_double)%>%
  left_join(.,veg_dens_av, by = 'site_ID')

drone_double_bt<-double_fun(drone_results_area, sp = 'Brushtail possum', sites_df = sites_double)%>%
  left_join(.,veg_dens_av, by = 'site_ID')

#include vegdens in function

det_oc_veg<-function(data, k){
  
  mat<-as.matrix(data%>%select(-c(1,4)), nrow = nrow(data), byrow = T )
  
  umf <- unmarkedFramePCount(y = mat, siteCovs = data.frame(veg_density = data$mean_vd))
  
  occmodel <- pcount(~ veg_density ~ 1, data = umf, K = k)
  
  summary(occmodel)

}

det_oc_veg(drone_double_sgg, k = 30)
det_oc_veg(drone_double_crp, k = 150)
det_oc_veg(drone_double_sg, k = 60)
det_oc_veg(drone_double_fg, k = 30)
det_oc_veg(drone_double_bt, k = 10)

# influence of veg dens on ground surveys ---------------------------------

mrds_listfun_veg<-function(dataset, wd){
  
  mrds_mod<- ddf(method="io",dsmodel= ~cds(key="hr"),
                 mrmodel=~glm(link="logit",formula= ~distance+mean_vd),
                 data=dataset, meta.data= list(width=wd))
  
  det_tables <- det.tables(mrds_mod)
  
  results_list<-list(mrds_mod, det_tables)
  names(results_list)<-c('model', 'det_table')
  
  return(results_list)
}


#get veg density for model data

GG_all_veg<-GG_all%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  left_join(., veg_dens_av_transect, by = c('site_ID', 'transect'))%>%
  mutate(mean_vd = case_when(is.na(mean_vd) ~ 1,
                             TRUE ~ mean_vd))

GG_one_veg<-GG_one%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  left_join(., veg_dens_av_transect, by = c('site_ID', 'transect'))%>%
  mutate(mean_vd = case_when(is.na(mean_vd) ~ 1,
                             TRUE ~ mean_vd))

CRP_all_veg<-CRP_all%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  left_join(., veg_dens_av_transect, by = c('site_ID', 'transect'))%>%
  mutate(mean_vd = case_when(is.na(mean_vd) ~ 1,
                             TRUE ~ mean_vd))

CRP_one_veg<-CRP_one%>%
  mutate(site_ID = case_when(site_ID == '299-944-0002 (NAR)' ~ '299-944-0002',
                             TRUE ~ site_ID))%>%
  left_join(., veg_dens_av_transect, by = c('site_ID', 'transect'))%>%
  mutate(mean_vd = case_when(is.na(mean_vd) ~ 1,
                             TRUE ~ mean_vd))

#gg models

# all transects

mrds_GG_veg<-mrds_listfun_veg(GG_all_veg, wd = 36)

summary(mrds_calc$GG_all$model)
summary(mrds_GG_veg$model)

lrtest(mrds_calc$GG_all$model, mrds_GG_veg$model)

# one transect

mrds_GG_veg_one<-mrds_listfun_veg(GG_one_veg, wd = 36)

summary(mrds_calc$GG_one$model)
summary(mrds_GG_veg_one$model)

lrtest(mrds_calc$GG_one$model, mrds_GG_veg_one$model)

#crp models

# all transects

mrds_CRP_veg<-mrds_listfun_veg(CRP_all_veg, wd = 30)

summary(mrds_calc$CRP_all$model)
summary(mrds_CRP_veg$model)

lrtest(mrds_calc$CRP_all$model, mrds_CRP_veg$model)

#one transect

mrds_CRP_veg_one<-mrds_listfun_veg(CRP_one_veg, wd = 30)

summary(mrds_calc$CRP_one$model)
summary(mrds_CRP_veg_one$model)

lrtest(mrds_calc$CRP_one$model, mrds_CRP_veg_one$model)
