### Figure 2
# Testing accuracy of impacts

# Working directory
setwd("/Volumes/Citadel/Clark_et_al_2022_PNAS_SM")

# Libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(Hmisc)
library(cowplot)
library(reshape2)
library(readr)

# Subsetting to products we use in the main analsyis
used.products <- 
  read_csv(paste0(getwd(),'/Managed_Data/NutriScore for radar plots 21January2022 Log2.csv'))
  

# Getting estimated impacts for scaling
estimated.impacts <-
  read_csv(paste0(getwd(),'/Managed_Data/Impacts by Product 21January2022 Log2.csv')) %>%
  mutate(scaled_GHG = mean_GHG / max(.$mean_GHG),
        scaled_Eut = mean_Eut / max(.$mean_Eut),
        scaled_WatScar = mean_WatScar / max(.$mean_WatScar),
        scaled_Land = mean_Land / max(.$mean_Land),
        scaled_lower_ci_GHG = lower_ci_GHG / max(.$mean_GHG),
        scaled_lower_ci_Eut = lower_ci_Eut / max(.$mean_Eut),
        scaled_lower_ci_Land = lower_ci_Land / max(.$mean_Land),
        scaled_lower_ci_WatScar = lower_ci_WatScar / max(.$mean_WatScar),
        scaled_upper_ci_GHG = upper_ci_GHG / max(.$mean_GHG),
        scaled_upper_ci_Eut = upper_ci_Eut / max(.$mean_Eut),
        scaled_upper_ci_Land = upper_ci_Land / max(.$mean_Land),
        scaled_upper_ci_WatScar = upper_ci_WatScar / max(.$mean_WatScar)) %>%
  mutate(scaled_tot_env = (scaled_GHG + scaled_Eut + scaled_WatScar + scaled_Land) / 4,
         scaled_tot_env_lower = (scaled_lower_ci_GHG + scaled_lower_ci_Eut + scaled_lower_ci_Land + scaled_lower_ci_WatScar) / 4,
         scaled_tot_env_upper = (scaled_upper_ci_GHG + scaled_upper_ci_Eut + scaled_upper_ci_Land + scaled_upper_ci_WatScar) / 4) %>%
  filter(!grepl('coffee.*maker|coffee.*filter|coffee.*press|aero.*press|coffee.*paper', product_name, ignore.case = TRUE))

# Validated estiamtes
validate.dat <-
  read_csv(paste0(getwd(),'/Managed_Data/Validating Impacts by Product 6Feb2022.csv')) %>%
  filter(paste0(id, product_name) %in% paste0(used.products$id, used.products$product_name)) %>%
  mutate(scaled_GHG = mean_GHG / max(estimated.impacts$mean_GHG),
         scaled_Eut = mean_Eut / max(estimated.impacts$mean_Eut),
         scaled_WatScar = mean_WatScar / max(estimated.impacts$mean_WatScar),
         scaled_Land = mean_Land / max(estimated.impacts$mean_Land),
         scaled_lower_ci_GHG = lower_ci_GHG / max(estimated.impacts$mean_GHG),
         scaled_lower_ci_Eut = lower_ci_Eut / max(estimated.impacts$mean_Eut),
         scaled_lower_ci_Land = lower_ci_Land / max(estimated.impacts$mean_Land),
         scaled_lower_ci_WatScar = lower_ci_WatScar / max(estimated.impacts$mean_WatScar),
         scaled_upper_ci_GHG = upper_ci_GHG / max(estimated.impacts$mean_GHG),
         scaled_upper_ci_Eut = upper_ci_Eut / max(estimated.impacts$mean_Eut),
         scaled_upper_ci_Land = upper_ci_Land / max(estimated.impacts$mean_Land),
         scaled_upper_ci_WatScar = upper_ci_WatScar / max(estimated.impacts$mean_WatScar)) %>%
  mutate(scaled_tot_env = (scaled_GHG + scaled_Eut + scaled_WatScar + scaled_Land) / 4,
         scaled_tot_env_lower = (scaled_lower_ci_GHG + scaled_lower_ci_Eut + scaled_lower_ci_Land + scaled_lower_ci_WatScar) / 4,
         scaled_tot_env_upper = (scaled_upper_ci_GHG + scaled_upper_ci_Eut + scaled_upper_ci_Land + scaled_upper_ci_WatScar) / 4) %>%
  mutate(scaled_tot_env = scaled_tot_env / max(estimated.impacts$scaled_tot_env),
         scaled_tot_env_lower = scaled_tot_env_lower / max(estimated.impacts$scaled_tot_env),
         scaled_tot_env_upper = scaled_tot_env_upper / max(estimated.impacts$scaled_tot_env))

# Panel A ----
# Distribution of log ratio of impacts
# Getting known impacts from using back of package info
known.estimates <-
  validate.dat %>%
  filter(info_validate %in% 'none') %>%
  group_by(id, product_name) %>%
  dplyr::summarise(mean_GHG_known = mean(mean_GHG),
                   mean_Land_known = mean(mean_Land),
                   mean_WatScar_known = mean(mean_WatScar),
                   mean_Eut_known = mean(mean_Eut),
                   scaled_tot_env_known = mean(scaled_tot_env)) %>%
  unique(.) 

# Getting estimated impacts when no info is knonw
estimated.estimates <-
  validate.dat %>% 
  filter(n_percent_known %in% 0 & percent_known %in% 0) %>%
  group_by(id, product_name) %>%
  dplyr::summarise(mean_GHG = mean(mean_GHG),
                   mean_Land = mean(mean_Land),
                   mean_WatScar = mean(mean_WatScar),
                   mean_Eut = mean(mean_Eut),
                   scaled_tot_env = mean(scaled_tot_env)) %>%
  unique(.)

# Getting ratio
ratio.impacts <-
  left_join(estimated.estimates %>% dplyr::select(id, product_name, scaled_tot_env, mean_GHG, mean_Land, mean_Eut, mean_WatScar), 
            known.estimates %>% dplyr::select(id, product_name, scaled_tot_env_known, mean_GHG_known, mean_Land_known, mean_Eut_known, mean_WatScar_known)) %>%
  mutate(ratio_tot_env = scaled_tot_env / scaled_tot_env_known) %>%
  mutate(ratio_tot_env = log2(ratio_tot_env)) %>%
  filter(!is.na(ratio_tot_env))

# Quick tests before plotting
# Is the ratio significantly different from 0?
# No it is not
t.test(ratio.impacts$ratio_tot_env)

# Absolute difference in impacts between 'more accurate' and 'less accurate' estimates
mean(abs(ratio.impacts$mean_GHG[abs(ratio.impacts$ratio_tot_env)<=.5849] - ratio.impacts$mean_GHG_known[abs(ratio.impacts$ratio_tot_env)<=.5849]))
mean(abs(ratio.impacts$mean_GHG[abs(ratio.impacts$ratio_tot_env)>.5849] - ratio.impacts$mean_GHG_known[abs(ratio.impacts$ratio_tot_env)>.5849]))
mean(abs(ratio.impacts$mean_GHG[abs(ratio.impacts$ratio_tot_env)>1] - ratio.impacts$mean_GHG_known[abs(ratio.impacts$ratio_tot_env)>1]))

mean(abs(ratio.impacts$mean_Land[abs(ratio.impacts$ratio_tot_env)<=.5849] - ratio.impacts$mean_Land_known[abs(ratio.impacts$ratio_tot_env)<=.5849]))
mean(abs(ratio.impacts$mean_Land[abs(ratio.impacts$ratio_tot_env)>.5849] - ratio.impacts$mean_Land_known[abs(ratio.impacts$ratio_tot_env)>.5849]))
mean(abs(ratio.impacts$mean_Land[abs(ratio.impacts$ratio_tot_env)>1] - ratio.impacts$mean_Land_known[abs(ratio.impacts$ratio_tot_env)>1]))

# What about paired t tests?
# These are also not significantly different
t.test(ratio.impacts$scaled_tot_env, ratio.impacts$scaled_tot_env_known)
t.test(ratio.impacts$mean_GHG, ratio.impacts$mean_GHG_known)
t.test(ratio.impacts$mean_Eut, ratio.impacts$mean_Eut_known)
t.test(ratio.impacts$mean_Land, ratio.impacts$mean_Land_known)
t.test(ratio.impacts$mean_WatScar, ratio.impacts$mean_WatScar_known)

# Number of products within x%%% difference
# Within 10%
sum(ratio.impacts$ratio_tot_env >= log2(1/1.1) & ratio.impacts$ratio_tot_env <= log2(1.1)) / nrow(ratio.impacts)

# Within 25%
sum(ratio.impacts$ratio_tot_env >= log2(1/1.25) & ratio.impacts$ratio_tot_env <= log2(1.25)) / nrow(ratio.impacts)

# Outside of 50%
1 - sum(ratio.impacts$ratio_tot_env >= log2(1/1.5) & ratio.impacts$ratio_tot_env <= log2(1.5)) / nrow(ratio.impacts)

# Outside of 100%
1 - sum(ratio.impacts$ratio_tot_env >= log2(1/2) & ratio.impacts$ratio_tot_env <= log2(2)) / nrow(ratio.impacts)

mean(abs(ratio.impacts$mean_GHG[abs(ratio.impacts$ratio_tot_env) > 1] - ratio.impacts$mean_GHG_known[abs(ratio.impacts$ratio_tot_env) > 1]))
mean(abs(ratio.impacts$mean_Land[abs(ratio.impacts$ratio_tot_env) > 1] - ratio.impacts$mean_Land_known[abs(ratio.impacts$ratio_tot_env) > 1]))
mean(abs(ratio.impacts$mean_GHG-ratio.impacts$mean_GHG_known))
mean(abs(ratio.impacts$mean_Land-ratio.impacts$mean_Land_known))

# And plotting
ggplot() +
  geom_density(dat = ratio.impacts, aes(x = ratio_tot_env), colour = 'black') +
  theme_classic() +
  coord_cartesian(xlim = c(-6,6), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,5), expand = c(0,0),
                     breaks = c(0,1,2,3,4),
                     labels = c(0,10,20,30,40)) +
  geom_vline(xintercept = log2(1.5)) +
  geom_vline(xintercept = log2(1/1.5)) +
  geom_vline(xintercept = log2(1.25)) +
  geom_vline(xintercept = log2(1/1.25)) +
  geom_vline(xintercept = log2(1.1)) +
  geom_vline(xintercept = log2(1/1.1)) +
  geom_vline(xintercept = 0, linetype = 2, colour = 'grey') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  labs(x = 'Log2 ratio of the estimated and\nknown environmental impact scores',
       y = 'Density (percent of dataset)')

# Saving
ggsave(paste0(getwd(),"/Figures/Fig 2a 6Feb2022 Log2 Ratio of Accuracy.pdf"),
width = 13, height = 5, units = 'cm')

# Panel B ----
# Boxplots for each indicator, using known and estimated data
# Showing ratio for each indicator
ratio.impacts <-
  ratio.impacts %>%
  mutate(ratio_ghg = log2(mean_GHG / mean_GHG_known),
         ratio_land = log2(mean_Land / mean_Land_known),
         ratio_WatScar = log2(mean_WatScar / mean_WatScar_known),
         ratio_Eut = log2(mean_Eut / mean_Eut_known))

# Getting wide data frame
boxplot.dat <-
  ratio.impacts %>%
  as.data.frame(.) %>%
  dplyr::select(id, product_name, ratio_tot_env, ratio_ghg, ratio_land, ratio_WatScar, ratio_Eut) %>%
  melt(id.vars = c('id','product_name'), variable.name = 'ratio') %>%
  mutate(ratio = gsub("ratio_","",ratio)) %>%
  mutate(ratio = ifelse(ratio %in% 'Eut', 'Eutrophication\nPotential',
                        ifelse(ratio %in% 'ghg', 'Greenhouse Gas\nEmissions',
                               ifelse(ratio %in% 'WatScar', 'Water Scarcity',
                                      ifelse(ratio %in% 'land','Land Use', 'Total Environmental\nImpact'))))) %>%
  transform(ratio = factor(ratio, levels = c('Total Environmental\nImpact','Greenhouse Gas\nEmissions','Land Use','Water Scarcity','Eutrophication\nPotential')))


# And plotting
fig.2b <-
  ggplot(boxplot.dat, aes(x = ratio, y = value, group = ratio)) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 1) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-.75,.75)) +
  # stat_summary(fun = 'mean', geom = 'point', colour = 'red') +
  # stat_summary(fun.data = 'mean_cl_normal', geom = 'errorbar', colour = 'red') +
  labs(y = 'Log2 ratio of the estimated and\nknown environmental impact scores',
       x = 'Environmental Indicator') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 90,hjust =1,vjust = .5))


# Variance based on number of ingredients in the product
# Merging in number of ingredients
ratio.impacts <-
  left_join(ratio.impacts,
            validate.dat %>% dplyr::select(id, product_name, n_ingredients) %>% unique(.))

# And plotting
fig.2c <-
  ggplot(ratio.impacts, aes(x = n_ingredients, y = (ratio_tot_env), group = n_ingredients)) +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = 1) +
  scale_x_continuous(breaks = c(2:11)) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-.75,.75), xlim = c(1.5,11.5)) +
  # stat_summary(fun = 'mean', geom = 'point', colour = 'red') +
  # stat_summary(fun.data = 'mean_cl_normal', geom = 'errorbar', colour = 'red') +
  labs(y = 'Log2 ratio of the estimated and\nknown environmental impact scores',
       x = 'Number of Ingredients in the Product') +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7))


tmp.sum <-
  ratio.impacts %>%
  group_by(n_ingredients) %>%
  dplyr::summarise(ratio_env = mean(abs(ratio_tot_env)))
summary(lm(abs(tmp.sum$ratio_env) ~ tmp.sum$n_ingredients))

  
# And variance based on total percent known
validate.dat <-
  validate.dat %>%
  filter(info_validate != 'none') %>%
  group_by(id, product_name, variable_removed_validate, percent_known) %>%
  dplyr::summarise(scaled_tot_env = mean(scaled_tot_env, na.rm = TRUE)) %>%
  as.data.frame(.) %>%
  mutate(percent_known_binned = floor(percent_known/10) * 10) %>%
  mutate(percent_known_cat = paste0('>',percent_known_binned,'-',percent_known_binned + 10,'%')) %>%
  mutate(percent_known_cat = ifelse(percent_known %in% 0,'0%',percent_known_cat)) %>%
  filter(percent_known_binned != 100) %>%
  transform(percent_known_cat = factor(percent_known_cat, 
                                       levels = c('0%',">0-10%",'>10-20%','>20-30%','>30-40%','>40-50%','>50-60%','>60-70%','>70-80%','>80-90%','>90-100%'))) %>%
  left_join(., known.estimates %>% dplyr::select(id, product_name, scaled_tot_env_known)) %>%
  mutate(ratio_impact = log2(scaled_tot_env / scaled_tot_env_known)) %>%
  unique(.)
  
# And plotting
fig.2d <-
  ggplot(validate.dat, aes(x = percent_known_cat, y = ratio_impact, group = percent_known_cat)) +
  theme_classic() +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-.75,.75)) +
  # stat_summary(fun = 'mean', geom = 'point', colour = 'red') +
  # stat_summary(fun.data = 'mean_cl_normal', geom = 'errorbar', colour = 'red') +
  scale_x_discrete(labels = gsub("%","",levels(validate.dat$percent_known_cat))) +
  # geom_errorbar(inherit.aes = FALSE, data = dat.fig2cd %>% filter(outcome %in% 'Percent Difference'), aes(x = percent_known_cat, ymin = dif - se_dif, ymax = dif + se_dif), position = position_dodge(1), colour = 'red') +
  # geom_boxplot(outlier.colour = 'white', outlier.alpha = 0) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 7)) +
  theme(legend.text = element_text(size = 7)) +
  theme(legend.title = element_text(size = 7)) +
  theme(legend.position = c(.25,.75)) +
  theme(axis.text.x = element_text(angle = 90,hjust =1,vjust = .5)) +
  # facet_wrap(.~Indicator, scales = 'free') +
  # coord_cartesian(ylim = c(-1.5, 3)) +
  labs(x = 'Percent of Product Known',
       y = 'Percent difference between estimated and\nknown environmental impact score',
       colour = 'Accuracy Measurement')

tmp.sum <-
  validate.dat %>%
  group_by(percent_known) %>%
  dplyr::summarise(ratio_env = mean(abs(ratio_impact)))


summary(lm(abs(tmp.sum$ratio_env) ~ tmp.sum$percent_known))
summary(lm(abs(validate.dat$ratio_impact) ~ validate.dat$percent_known))

# And sticking figures together ----
fig2 <-
  plot_grid(fig.2b, fig.2c, fig.2d,
            nrow = 1,
            align = 'hv',
            labels = 'auto', label_size = 8)

fig2
Sys.sleep(10)

# Saving data files
if(!('Figure Data' %in% list.files(getwd()))) {
  dir.create(paste0(getwd(),'/Figure Data'))
}

write.csv(validate.dat %>% dplyr::select(figure_id_num = product_name, percent_known_binned, ratio_impact) %>% mutate(figure_id_num = 1:nrow(.)),
          paste0(getwd(),'/Figure Data/Data Fig 2d 6Feb2022.csv'), row.names = FALSE)
write.csv(ratio.impacts %>% as.data.frame(.) %>% dplyr::select(figure_id_num = product_name, ratio_tot_env, n_ingredients) %>% mutate(figure_id_num = 1:nrow(.)),
          paste0(getwd(),'/Figure Data/Data Fig 2a,c 6Feb2022.csv'), row.names = FALSE)
write.csv(boxplot.dat %>% dplyr::select(figure_id_num = product_name, ratio, value) %>% mutate(figure_id_num = 1:nrow(.)),
          paste0(getwd(),'/Figure Data/Data Fig 2b 6Feb2022.csv'), row.names = FALSE)


ggsave(paste0(getwd(),"/Figures/Fig2 b-d 2 6Feb2022 Testing Impacts.pdf"),
       width = 13, height = 5, units = 'cm')


# Tests for overlaps between the mean estimated impact and the cis
# Getting estimated impacts for scaling
overlap.cis <-
  read.csv(paste0(getwd(),'/Managed_Data/Validating Impacts by Product 6Feb2022 Log2.csv'), stringsAsFactors = FALSE) %>%
  mutate(scaled_GHG = mean_GHG / max(estimated.impacts$mean_GHG),
         scaled_Eut = mean_Eut / max(estimated.impacts$mean_Eut),
         scaled_WatScar = mean_WatScar / max(estimated.impacts$mean_WatScar),
         scaled_Land = mean_Land / max(estimated.impacts$mean_Land),
         scaled_lower_ci_GHG = lower_ci_GHG / max(estimated.impacts$mean_GHG),
         scaled_lower_ci_Eut = lower_ci_Eut / max(estimated.impacts$mean_Eut),
         scaled_lower_ci_Land = lower_ci_Land / max(estimated.impacts$mean_Land),
         scaled_lower_ci_WatScar = lower_ci_WatScar / max(estimated.impacts$mean_WatScar),
         scaled_upper_ci_GHG = upper_ci_GHG / max(estimated.impacts$mean_GHG),
         scaled_upper_ci_Eut = upper_ci_Eut / max(estimated.impacts$mean_Eut),
         scaled_upper_ci_Land = upper_ci_Land / max(estimated.impacts$mean_Land),
         scaled_upper_ci_WatScar = upper_ci_WatScar / max(estimated.impacts$mean_WatScar)) %>%
  mutate(scaled_tot_env = (scaled_GHG + scaled_Eut + scaled_WatScar + scaled_Land) / 4,
         scaled_tot_env_lower = (scaled_lower_ci_GHG + scaled_lower_ci_Eut + scaled_lower_ci_Land + scaled_lower_ci_WatScar) / 4,
         scaled_tot_env_upper = (scaled_upper_ci_GHG + scaled_upper_ci_Eut + scaled_upper_ci_Land + scaled_upper_ci_WatScar) / 4) %>%
  mutate(scaled_tot_env = scaled_tot_env / max(estimated.impacts$scaled_tot_env),
         scaled_tot_env_lower = scaled_tot_env_lower / max(estimated.impacts$scaled_tot_env),
         scaled_tot_env_upper = scaled_tot_env_upper / max(estimated.impacts$scaled_tot_env))

# Getting known
known.cis <-
  overlap.cis %>%
  filter(info_validate %in% 'none') %>%
  group_by(id, product_name) %>%
  dplyr::summarise(scaled_tot_env_known = mean(scaled_tot_env),
                   scaled_tot_env_low_known = mean(scaled_tot_env_lower),
                   scaled_tot_env_high_known = mean(scaled_tot_env_upper),
                   scaled_upper_ghg_known = mean(scaled_upper_ci_GHG),
                   scaled_upper_land_known = mean(scaled_upper_ci_Land),
                   scaled_upper_wat_known = mean(scaled_upper_ci_WatScar),
                   scaled_upper_eut_known = mean(scaled_upper_ci_Eut),
                   scaled_lower_ghg_known = mean(scaled_lower_ci_GHG),
                   scaled_lower_land_known = mean(scaled_lower_ci_Land),
                   scaled_lower_wat_known = mean(scaled_lower_ci_WatScar),
                   scaled_lower_eut_known = mean(scaled_lower_ci_Eut))

# Getting estimated
estimated.cis <-
  overlap.cis %>%
  filter(n_percent_known %in% 0 & percent_known %in% 0) %>%
  group_by(id, product_name) %>%
  dplyr::summarise(scaled_tot_env = mean(scaled_tot_env),
                   scaled_tot_env_low = mean(scaled_tot_env_lower),
                   scaled_tot_env_high = mean(scaled_tot_env_upper),
                   scaled_upper_ghg = mean(scaled_upper_ci_GHG),
                   scaled_upper_land = mean(scaled_upper_ci_Land),
                   scaled_upper_wat = mean(scaled_upper_ci_WatScar),
                   scaled_upper_eut = mean(scaled_upper_ci_Eut),
                   scaled_lower_ghg = mean(scaled_lower_ci_GHG),
                   scaled_lower_land = mean(scaled_lower_ci_Land),
                   scaled_lower_wat = mean(scaled_lower_ci_WatScar),
                   scaled_lower_eut = mean(scaled_lower_ci_Eut))

# Merging
overlap.cis.check <-
  left_join(estimated.cis, known.cis) %>%
  filter(!is.na(scaled_tot_env_high_known)) %>%
  filter(!is.na(scaled_tot_env)) %>%
  filter(!is.na(scaled_tot_env_high)) %>%
  filter(!is.na(scaled_tot_env_known)) %>%
  mutate(overlap.cis = 0) %>%
  mutate(overlap.cis = ifelse(scaled_tot_env_low <= scaled_tot_env_high_known & scaled_tot_env_low >= scaled_tot_env_low_known, 1, overlap.cis)) %>%
  mutate(overlap.cis = ifelse(scaled_tot_env_high <= scaled_tot_env_high_known & scaled_tot_env_high >= scaled_tot_env_low_known, 1, overlap.cis)) %>%
  mutate(overlap.cis.ghg = ifelse(scaled_lower_ghg <= scaled_upper_ghg_known & scaled_lower_ghg >= scaled_lower_ghg_known, 1, 0)) %>%
  mutate(overlap.cis.ghg = ifelse(scaled_upper_ghg <= scaled_upper_ghg_known & scaled_upper_ghg >= scaled_upper_ghg_known, 1, overlap.cis.ghg)) %>%
  mutate(overlap.cis.eut = ifelse(scaled_lower_eut <= scaled_upper_eut_known & scaled_lower_eut >= scaled_lower_eut_known, 1, 0)) %>%
  mutate(overlap.cis.eut = ifelse(scaled_upper_eut <= scaled_upper_eut_known & scaled_upper_eut >= scaled_upper_eut_known, 1, overlap.cis.eut)) %>%
  mutate(overlap.cis.wat = ifelse(scaled_lower_wat <= scaled_upper_wat_known & scaled_lower_wat >= scaled_lower_wat_known, 1, 0)) %>%
  mutate(overlap.cis.wat = ifelse(scaled_upper_wat <= scaled_upper_wat_known & scaled_upper_wat >= scaled_upper_wat_known, 1, overlap.cis.wat)) %>%
  mutate(overlap.cis.land = ifelse(scaled_lower_land <= scaled_upper_land_known & scaled_lower_land >= scaled_lower_land_known, 1, 0)) %>%
  mutate(overlap.cis.land = ifelse(scaled_upper_land <= scaled_upper_land_known & scaled_upper_land >= scaled_upper_land_known, 1, overlap.cis.land)) %>%
  mutate(no.overlaps.cis = overlap.cis.eut + overlap.cis.wat + overlap.cis.land + overlap.cis.ghg + overlap.cis)

# And checking
sum(overlap.cis.check$overlap.cis, na.rm = TRUE)/nrow(overlap.cis.check)
sum(overlap.cis.check$overlap.cis.eut)
sum(overlap.cis.check$overlap.cis.ghg)
sum(overlap.cis.check$overlap.cis.wat)
sum(overlap.cis.check$overlap.cis.land)
sum(overlap.cis.check$no.overlaps.cis)

hist(overlap.cis.check$no.overlaps.cis)
table(overlap.cis.check$no.overlaps.cis)
sum(overlap.cis.check$overlap.cis %in% 0 & overlap.cis.check$overlap.cis.ghg %in% 0 & overlap.cis.check$overlap.cis.eut %in% 0 & overlap.cis.check$overlap.cis.land %in% 0 & overlap.cis.check$overlap.cis.wat %in% 0) / nrow(overlap.cis.check)


