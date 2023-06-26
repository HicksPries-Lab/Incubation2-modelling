## GRAPHS FOR WANG THESIS 2023
## Author: Michelle S. Wang, michelle.s.wang.th@dartmouth.edu

# Load packages + functions
library(tidyverse)
library(ggsci) 
library(ggrepel)
library(scales)

library(FME)
library(ggpubr)
library(car)

# Nature color palette: https://nanx.me/ggsci/reference/pal_npg.html; show_col(pal_npg("nrc")(10))

# Theme
theme_C <- theme_light() + 
  theme(panel.grid.minor = element_blank(), 
        text = element_text(size = 20), #for facetwrapped plots
        strip.background = element_rect(color="black", fill="#93C5FF", size=1.5, linetype="solid"),
        #legend.position = "none",
        plot.title = element_text(hjust = 0.5),
  ) 

# Theme
theme_bar <-  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),  # center title
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    panel.spacing = unit(.9, 'lines'),
    text = element_text(size = 20)
  )

INC1_colors <- c('SOIL' = '#7E6148FF', 'CS1' = '#91D1C2FF', 'AD1' = '#8491B4FF', 'HLFB1' = '#F39B7FFF')
INC2_colors <- c('SOIL' = '#7E6148FF', 'CS2' = '#00A087FF', 'AD2' = '#3C5488FF', 'HLFB1' = '#F39B7FFF', 'HLFB2' = '#E64B35FF', 'HLFB3' = '#DC0000FF')
INCtot_colors <- c('SOIL' = '#7E6148FF', 'CS1' = '#91D1C2FF', 'AD1' = '#8491B4FF', 'CS2' = '#00A087FF', 'AD2' = '#3C5488FF', 'HLFB1' = '#F39B7FFF', 'HLFB2' = '#E64B35FF', 'HLFB3' = '#DC0000FF')
################################################################################################
# ONE TIME INPUT GRAPHS

# Initials
ADconv = .5
DASEconv = .35

# Read in data
onetime_data0 <- read.csv("onetime_data.csv", stringsAsFactors = FALSE, header = TRUE) # scan in document formatted like example

onetime_data0 <- onetime_data0 %>%
  #select(-'X', -'X.1') %>% # get rid of weird extra column
  select(-'GWC20', -'CCBP_P', -'CCBP_V', -'DASE_C', -'DASE_O')

# total treatments: c('DASE_C',	'DASE_O',	'DASE_AVG',	'AD_S',	'POET_S',	'NREL_S',	'AD_N',	'POET_N',	'NREL_N',	'CS_N',	'GWC16',	'GWC20',	'PALOUSE',	'CS_1P',	'AD_1P',	'CCBP_P',	'DASE_1P',	'VERSHIRE',	'CS_1V',	'AD_1V',	'CCBP_V',	'DASE_1V')

CinitsINC2 <- c((1199.218125+1198.987771)/2, 1202.890795, 1208.769544, 1280.327308, 869.183259, 871.99067, 907.076619, 880.866037, 540.873971) # these numbers reflect if I average C per treatment, Information from INC3 -> CombinedIRMS -> Treatment_Calculations
# CinitsINC2 key = 'DASE_AVG',	'AD_S',	'POET_S',	'NREL_S',	'AD_N',	'POET_N',	'NREL_N',	'CS_N',	'GWC16'

CinitsP <- c(514.4336596, 1145.656643, 1059.347782, 1188.126723) # these numbers reflect if I average C per treatment, Information from INC2 -> IRMS -> "IRMS_summary" -> IRMS_Pre
CinitsV <- c(1113.366093, 1752.370126, 1651.682688, 1783.200554)
CinitsINC1 <- c(CinitsP, CinitsV)
# CinitsINC1 key =  'PALOUSE',	'CS_1P',	'AD_1P',	'DASE_1P',	'VERSHIRE',	'CS_1V',	'AD_1V',	'DASE_1V'

onetime_data <- onetime_data0 %>%
  pivot_longer(
    cols = c('DASE_AVG',	'AD_S',	'POET_S',	'NREL_S',	'AD_N',	'POET_N',	'NREL_N',	'CS_N',	'GWC16',	'PALOUSE',	'CS_1P',	'AD_1P',	'DASE_1P',	'VERSHIRE',	'CS_1V',	'AD_1V',	'DASE_1V'),
    names_to = 'treatment',
    values_to = 'cummCO2resp'
  )

# ONE TIME INPUT INC1 
onetime_data_INC1 <- onetime_data %>%
  filter(treatment == 'PALOUSE' | treatment ==	'CS_1P' | treatment == 'AD_1P' | treatment ==	'DASE_1P' | treatment ==	'VERSHIRE' | treatment ==	'CS_1V' | treatment ==	'AD_1V' | treatment ==	'DASE_1V') %>%
  mutate(soil = ifelse(treatment == c('PALOUSE',	'CS_1P',	'AD_1P',	'DASE_1P'), 'P', 'V')) %>%
  mutate(type = case_when(treatment == 'PALOUSE' | treatment == 'VERSHIRE' ~ 'SOIL', 
                     treatment == 'CS_1P' | treatment == 'CS_1V' ~ 'CS1',
                     treatment == 'AD_1P' | treatment == 'AD_1V' ~ 'AD1',
                     treatment == 'DASE_1P' | treatment == 'DASE_1V' ~ 'HLFB1')) %>%
  mutate(cummCO2respCONV = case_when(type == 'SOIL' ~ cummCO2resp,
                                     type == 'CS1' ~ cummCO2resp, 
                                     type == 'AD1' ~ ADconv*cummCO2resp,
                                     type == 'HLFB1' ~ DASEconv*cummCO2resp)) %>%
  mutate(soil = factor(soil, labels = c('Palouse', 'Vershire'))) %>%
  mutate(init_totalC = case_when(treatment == 'PALOUSE' ~ CinitsINC1[1],
                                 treatment == 'CS_1P' ~ CinitsINC1[2] ,	
                                 treatment == 'AD_1P' ~ CinitsINC1[3],	
                                 treatment == 'DASE_1P' ~ CinitsINC1[4], 
                                 treatment == 'VERSHIRE' ~ CinitsINC1[5], 
                                 treatment == 'CS_1V' ~ CinitsINC1[6], 
                                 treatment == 'AD_1V' ~ CinitsINC1[7], 
                                 treatment == 'DASE_1V' ~ CinitsINC1[8])) %>%
  mutate(Cret = init_totalC - cummCO2resp) %>%
  mutate(init_totalCCONV = case_when(type == 'SOIL' ~ init_totalC,
                                     type == 'CS1' ~ init_totalC, 
                                     type == 'AD1' ~ ADconv*init_totalC,
                                     type == 'HLFB1' ~ DASEconv*init_totalC)) %>%
  mutate(CretCONV = init_totalCCONV - cummCO2respCONV)

data_ends <- onetime_data_INC1 %>%
  group_by(treatment) %>%
  top_n(1, YEAR)

#to isolate each soil type use this code in ggplot data =
#onetime_data_INC1 %>%
 # filter(soil == 'Palouse')

onetime_INC1_plot <- ggplot(data = onetime_data_INC1, aes(x = YEAR, y = cummCO2resp, group = treatment)) +
  geom_line(aes(col = type, linetype = factor(soil)), size = 2) + 
  scale_linetype_manual(values = c('Palouse' = 'solid', 'Vershire' = 'dotdash')) +
  scale_color_manual(values = INC1_colors) + #c('SOIL' = '#7E6148FF', 'CS2' = '#91D1C2FF', 'AD2' = '#8491B4FF', 'DASE1' = '#F39B7FFF', 'DASE2' = '#E64B35FF', 'DASE3' = '#DC0000FF')) +
  theme_C +
  #xlim(0, 25) +
  labs(x = 'Years', y = 'Total C Respired [mg]', 
       #title = '100 Year Projections of C Respired in Incubation 1 Treatments', 
       col = 'Treatment', linetype = 'Soil Type') 
onetime_INC1_plot
ggsave("onetimeINC1_plot.png", plot = onetime_INC1_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# residue conversion yield accounted for
onetimeCONV_INC1_plot <- ggplot(data = onetime_data_INC1 , aes(x = YEAR, y = cummCO2respCONV, group = treatment)) +
  geom_line(aes(col = type, linetype = soil), size = 2) + 
  scale_color_manual(values = INC1_colors) +
  theme_C +
 # xlim(0, 25) +
  labs(x = 'Years', y = 'Total C Respired [mg]', 
       # title = '100 Year Projections of C Respired \n in Incubation 1 Treatments with Conversion Rates', 
       col = 'Treatment', linetype = 'Soil Type') 
onetimeCONV_INC1_plot
ggsave("VERonetimeCONV_INC1_plot.png", plot = onetimeCONV_INC1_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# INC1 Retained version of above plots
onetimeRET_INC1_plot <- ggplot(data = onetime_data_INC1, aes(x = YEAR, y = Cret, group = treatment)) +
  geom_line(aes(col = type, linetype = factor(soil)), size = 2) + 
  scale_color_manual(values = INC1_colors) +
  theme_C +
  xlim(0, 50) +
  labs(x = 'Years', y = 'Total C Retained [mg]', title = '100 Year Projections of C Retained in Incubation 1 Treatments', col = 'Treatment', linetype = 'Soil Type') 
onetimeRET_INC1_plot
ggsave("onetimeRET50_INC1_plot.png", plot = onetimeRET_INC1_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# residue conversion yield accounted for
onetimeRET_CONV_INC1_plot <- ggplot(data = onetime_data_INC1, aes(x = YEAR, y = CretCONV, group = treatment)) +
  geom_line(aes(col = type, linetype = soil), size = 2) + 
  scale_color_manual(values = INC1_colors) +
  theme_C +
  xlim(0, 50) +
  labs(x = 'Years', y = 'Total C Retained [mg]', title = '100 Year Projections of C Retained \n in Incubation 1 Treatments with Conversion Rates', col = 'Treatment', linetype = 'Soil Type') 
onetimeRET_CONV_INC1_plot
ggsave("onetimeRET50_CONV_INC1_plot.png", plot = onetimeRET_CONV_INC1_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# ONE TIME INPUT INC2
# Nature color palette
#E64B35FF # orange red
#F39B7FFF # salmon red
#DC0000FF # deep red

onetime_data_INC2 <- onetime_data %>%
  filter(treatment == 'DASE_AVG' | treatment ==	'AD_S' | treatment == 'POET_S' | treatment ==	'NREL_S' | treatment ==	'AD_N' | treatment ==	'POET_N' | treatment ==	'NREL_N' | treatment ==	'CS_N' | treatment ==	'GWC16') %>%
  mutate(dose = case_when(treatment == 'GWC16' ~ 'SOIL', 
                          treatment == 'CS_N' | treatment == 'AD_N'  
                          | treatment == 'POET_N' | treatment == 'NREL_N' ~ 'N',
                          treatment == 'DASE_AVG' | treatment == 'AD_S' 
                          | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'S')) %>%
  mutate(type = case_when(treatment == 'GWC16' ~ 'SOIL', 
                          treatment == 'CS_N' ~ 'CS',
                          treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD',
                          treatment == 'DASE_AVG' | treatment == 'POET_N' | treatment == 'NREL_N' | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'DASE')) %>%
  mutate(type2 = case_when(treatment == 'GWC16' ~ 'SOIL', 
                          treatment == 'CS_N' ~ 'CS2',
                          treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD2',
                          treatment == 'DASE_AVG' ~ 'HLFB1',
                          treatment == 'POET_N' | treatment == 'POET_S' ~ 'HLFB3',
                          treatment == 'NREL_N' | treatment == 'NREL_S' ~ 'HLFB2')) %>%
  mutate(cummCO2respCONV = case_when(type == 'SOIL' ~ cummCO2resp,
                                     type == 'CS' ~ cummCO2resp, 
                                     type == 'AD' ~ ADconv*cummCO2resp,
                                     type == 'DASE' ~ DASEconv*cummCO2resp)) %>%
  mutate(dose = factor(dose, labels = c('Reduced', 'Standard', 'Soil'))) %>% 
  mutate(init_totalC = case_when(treatment == 'DASE_AVG' ~ CinitsINC2[1],
                                 treatment == 'AD_S' ~ CinitsINC2[2] ,	
                                 treatment == 'POET_S' ~ CinitsINC2[3],	
                                 treatment == 'NREL_S' ~ CinitsINC2[4], 
                                 treatment == 'AD_N' ~ CinitsINC2[5], 
                                 treatment == 'POET_N' ~ CinitsINC2[6], 
                                 treatment == 'NREL_N' ~ CinitsINC2[7], 
                                 treatment == 'CS_N' ~ CinitsINC2[8],
                                 treatment == 'GWC16' ~ CinitsINC2[9])) %>%
  mutate(Cret = init_totalC - cummCO2resp) %>%
  mutate(init_totalCCONV = case_when(type == 'SOIL' ~ init_totalC,
                                     type == 'CS' ~ init_totalC, 
                                     type == 'AD' ~ ADconv*init_totalC,
                                     type == 'DASE' ~ DASEconv*init_totalC)) %>%
  mutate(CretCONV = init_totalCCONV - cummCO2respCONV)

# code so only dosage group
# onetime_data_INC2 %>%
#   filter(soil == 'Vershire')

onetime_INC2_plot <- ggplot(data = onetime_data_INC2, aes(x = YEAR, y = cummCO2resp, group = treatment)) +
  geom_line(aes(linetype = factor(dose), colour = type2), size = 2) + 
  scale_linetype_manual(values = c('Standard' = 'solid', 'Reduced' = 'twodash', 'Soil' = 'dotted')) +
  scale_color_manual(values = INC2_colors) +
  theme_C +
  xlim(0, 25) +
  labs(x = 'Years', y = 'Total C Respired [mg]', 
       #title = '25 Year Projections of C Respired in Incubation 2 Treatments', 
       linetype = 'Dosage', color = 'Treatment') 
onetime_INC2_plot
ggsave("onetimeINC2_plot.png", plot = onetime_INC2_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# residue conversion yield accounted for
onetimeCONV_INC2_plot <- ggplot(data = onetime_data_INC2 , aes(x = YEAR, y = cummCO2respCONV, group = treatment)) +
  geom_line(aes(linetype = dose, colour = type2), size = 2) + 
  scale_linetype_manual(values = c('Standard' = 'solid', 'Reduced' = 'twodash', 'Soil' = 'dotted')) +
  scale_color_manual(values = INC2_colors) +
  theme_C +
  xlim(0, 25.2) +
  labs(x = 'Years', y = 'Total C Respired [mg]', 
       # title = '25 Year Projections of C Respired \n in Incubation 2 Treatments with Conversion Rates', 
       linetype = 'Dosage', col = 'Treatment') 
onetimeCONV_INC2_plot
ggsave("onetimeCONV_INC2_plot.png", plot = onetimeCONV_INC2_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# INC2 RETAINED VERSION OF ABOVE PLOTS
onetimeRET_INC2_plot <- ggplot(data = onetime_data_INC2, aes(x = YEAR, y = Cret, group = treatment)) +
  geom_line(aes(linetype = factor(dose), colour = type2), size = 2) + 
  scale_color_manual(values = INC2_colors) +
  theme_C +
  xlim(0, 25) +
  labs(x = 'Years', y = 'Total C Retained [mg]', title = '25 Year Projections of C Retained in Incubation 2 Treatments', linetype = 'Dosage', color = 'Treatment') 
onetimeRET_INC2_plot
ggsave("onetimeRET_INC2_plot.png", plot = onetimeRET_INC2_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# residue conversion yield accounted for
onetimeRET_CONV_INC2_plot <- ggplot(data = onetime_data_INC2, aes(x = YEAR, y = CretCONV, group = treatment)) +
  geom_line(aes(linetype = dose, colour = type2), size = 2) + 
  scale_color_manual(values = INC2_colors) +
  theme_C +
  xlim(0, 25.2) +
  labs(x = 'Years', y = 'Total C Respired [mg]', title = '25 Year Projections of C Respired \n in Incubation 2 Treatments with Conversion Rates', linetype = 'Dosage', col = 'Treatment') 
onetimeRET_CONV_INC2_plot
ggsave("onetimeRET_CONV_INC2_plot.png", plot = onetimeRET_CONV_INC2_plot, width = 30, height = 20, units = "cm")  # change this accordingly


####################################################################################################
# LONGTERM MODELLING GRAPHS
longterm_data0 <- read.csv("Longterm_data.csv", stringsAsFactors = FALSE, header = TRUE) # scan in document formatted like example

longterm_data0 <- longterm_data0 %>%
  select(-'CCBP_P', -'CCBP_V', -'GWC16', -'PALOUSE', -'VERSHIRE')

# c('DASE_C',	'DASE_O',	'DASE_AVG',	'AD_S',	'POET_S',	'NREL_S',	'AD_N',	'POET_N',	'NREL_N',	'CS_N',	'GWC16',	'GWC20',	'PALOUSE',	'CS_1P',	'AD_1P',	'CCBP_P',	'DASE_1P',	'VERSHIRE',	'CS_1V',	'AD_1V',	'CCBP_V',	'DASE_1V')

longterm_data <- longterm_data0 %>%
  pivot_longer(
    cols = c('DASE_AVG',	'AD_S',	'POET_S',	'NREL_S',	'AD_N',	'POET_N',	'NREL_N',	'CS_N',	'CS_1P',	'AD_1P',	'DASE_1P',	'CS_1V',	'AD_1V',	'DASE_1V'),
    names_to = 'treatment',
    values_to = 'cummCO2resp'
  )

# longterm INC1
longterm_data_INC1 <- longterm_data %>%
  filter(treatment ==	'CS_1P' | treatment == 'AD_1P' | treatment ==	'DASE_1P' | treatment ==	'CS_1V' | treatment ==	'AD_1V' | treatment ==	'DASE_1V') %>%
  mutate(soil = ifelse(treatment == c('CS_1P',	'AD_1P',	'DASE_1P'), 'P', 'V')) %>%
  mutate(type = case_when(treatment == 'CS_1P' | treatment == 'CS_1V' ~ 'CS1',
                          treatment == 'AD_1P' | treatment == 'AD_1V' ~ 'AD1',
                          treatment == 'DASE_1P' | treatment == 'DASE_1V' ~ 'HLFB1')) %>%
  mutate(cummCO2respCONV = case_when(type == 'CS1' ~ cummCO2resp, 
                                     type == 'AD1' ~ ADconv*cummCO2resp,
                                     type == 'HLFB1' ~ DASEconv*cummCO2resp)) %>%
  mutate(soil = factor(soil, labels = c('Palouse', 'Vershire')))

data_ends <- onetime_data_INC1 %>%
  group_by(treatment) %>%
  top_n(1, YEAR)

longterm_INC1_plot <- ggplot(data = longterm_data_INC1, 
                            # %>% filter(soil == 'Vershire')   ,
                             aes(x = YEAR, y = cummCO2resp, group = treatment)) +
  geom_smooth(aes(col = type, linetype = soil), size = 2, alpha = 0) + 
  scale_linetype_manual(values = c('Palouse' = 'solid', 'Vershire' = 'dotdash')) +
  scale_color_manual(values = INC1_colors) +
  theme_C +
  coord_trans( y="log2") +  # otherwise DASE1 overwhelms plot 
  #xlim(0, 25) +
  labs(x = 'Years', y = 'Total C Retained in Treatments \n log2([mg C])', 
       #title = '100 Year Steady State Projections \n of C Retained in Incubation 1 Treatments', 
       linetype = 'Soil Type', color = 'Treatment') 
longterm_INC1_plot
ggsave("VERlongtermINC1_plot.png", plot = longterm_INC1_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# check INC1 data 
ggp <- ggplot(longterm_data_INC1, aes(x = YEAR, y = cummCO2resp, group = treatment)) +  
  stat_smooth(aes(col = treatment)) 
ggp
ggp_data <- ggplot_build(ggp)
head(ggp_data$data[[1]])

write.csv(ggp_data$data[[1]], 'inc1ggp_data.csv') # spits out data in color order, so graph it to see what number corresponds w/ what
# palouse, standard -> 1: AD1, 3: CS1, 5: HLFB1
# vershire, standard -> 2: AD1, 4: CS1, 6: HLFB1

# longterm INC2
longterm_data_INC2 <- longterm_data %>%
  filter(treatment == 'DASE_AVG' | treatment ==	'AD_S' | treatment == 'POET_S' | treatment ==	'NREL_S' | treatment ==	'AD_N' | treatment ==	'POET_N' | treatment ==	'NREL_N' | treatment ==	'CS_N' | treatment ==	'GWC16') %>%
  mutate(dose = case_when(treatment == 'CS_N' | treatment == 'AD_N'  
                          | treatment == 'POET_N' | treatment == 'NREL_N' ~ 'N',
                          treatment == 'DASE_AVG' | treatment == 'AD_S' 
                          | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'S')) %>%
  mutate(type = case_when(treatment == 'CS_N' ~ 'CS',
                          treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD',
                          treatment == 'DASE_AVG' | treatment == 'POET_N' | treatment == 'NREL_N' | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'DASE')) %>%
  mutate(type2 = case_when(treatment == 'CS_N' ~ 'CS2',
                           treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD2',
                           treatment == 'DASE_AVG' ~ 'HLFB1',
                           treatment == 'POET_N' | treatment == 'POET_S' ~ 'HLFB3',
                           treatment == 'NREL_N' | treatment == 'NREL_S' ~ 'HLFB2')) %>%
  mutate(cummCO2respCONV = case_when(type == 'CS' ~ cummCO2resp, 
                                     type == 'AD' ~ ADconv*cummCO2resp,
                                     type == 'DASE' ~ DASEconv*cummCO2resp)) %>%
  mutate(dose = factor(dose, labels = c('Reduced', 'Standard')))

longterm_INC2_plot <- ggplot(data = longterm_data_INC2
                             %>% filter(dose == 'Reduced'), 
                             aes(x = YEAR, y = cummCO2resp, group = treatment)) +
  scale_linetype_manual(values = c('Standard' = 'solid', 'Reduced' = 'twodash')) +
  geom_smooth(aes(linetype = dose, colour = type2), size = 2, alpha = 0) + 
  scale_color_manual(values = INC2_colors) +
  theme_C +
  #coord_trans( y="log2") +  # otherwise DASE3 overwhelms plot 
  labs(x = 'Years', y = 'Total C Retained in Treatments [mg C]', 
       #title = '100 Year Steady State Projections \n of C Retained in Incubation 2 Treatments',  
       linetype = 'Dosage', color = 'Treatment') 
longterm_INC2_plot
ggsave("REDlongtermINC2_plot.png", plot = longterm_INC2_plot, width = 30, height = 20, units = "cm")  # change this accordingly

# output geom_smooth data to check SS values
ggp <- ggplot(longterm_data_INC2, aes(x = YEAR, y = cummCO2resp, group = treatment)) +  
  stat_smooth(aes(col = treatment)) 
ggp
ggp_data <- ggplot_build(ggp)
head(ggp_data$data[[1]])

write.csv(ggp_data$data[[1]], 'inc2ggp_data.csv') # spits out data in color order, so graph it to see what number corresponds w/ what
# reduced -> groups 1: AD2, 3: CS2, 5: HLFB2, 7: HLFB3

####################################################################################################
# 13C PARTITIONING

# Read in data
thirteenC_data0 <- read.csv("thirteenC_data.csv", stringsAsFactors = FALSE, header = TRUE) # scan in document formatted like example

thirteenC_data <- thirteenC_data0 %>%
  filter(treatment != 'CCBP_1P' & treatment != 'CCBP_1V' & treatment != 'TWENTY' ) %>% #& treatment != 'ONESIX' & treatment != 'PALOUSE' & treatment != 'VERSHIRE'
  #select(-'X', -'X.1', -'X.2', -'X.3', -'X.4') %>%
  filter(sample != 'V5C' & sample != 'V5A' & sample != 'P3B' & sample != 'V1A') %>%
  group_by(treatment, time, source) %>%
  summarize_all(list(mean, sd)) %>%
  select(-'num_fn1', -'sample_fn1', -'rep_fn1', -'num_fn2', -'sample_fn2', -'rep_fn2', -'inc_fn2') #%>% 
  #mutate(source = factor(source, levels = c('soil', 'res')))

thirteenC_err_data0 <- read.csv("thirteenC_err_data.csv", stringsAsFactors = FALSE, header = TRUE) # scan in document formatted like example

thirteenC_err_data <- thirteenC_err_data0 %>%
  filter(treatment != 'CCBP_1P', treatment != 'CCBP_1V', treatment != 'ONESIX', treatment != 'TWENTY', treatment != 'PALOUSE', treatment != 'VERSHIRE') %>%
  filter(sample != 'V5C' & sample != 'V5A' & sample != 'P3B' & sample != 'V1A') %>%
  group_by(treatment, time) %>%
  summarize_all(list(mean, sd)) %>%
  select(-'num_fn1', -'sample_fn1', -'rep_fn1', -'num_fn2', -'sample_fn2', -'rep_fn2', -'inc_fn2') 

# priming data 
priming_data <- thirteenC_data %>%
  mutate(amt_fn1 = 1000*amt_fn1) %>%
  mutate(amt_fn2 = 1000*amt_fn2) %>%
  group_by(treatment, source) %>%
  arrange(time) %>%
  mutate(diff = amt_fn1 - lag(amt_fn1, default = first(amt_fn1))) %>%
  mutate(diff = -1*diff) %>%
  mutate(stdev = sqrt(amt_fn2^2 + (lag(amt_fn2, default = first(amt_fn2)))^2)) %>% # standard deviation
  filter(diff != 0) %>%
  mutate(diff = ifelse(diff < 0, 0, diff))  # if priming says somehow soil gained carbon from incubation, correct to 0 



#write.csv(priming_data, file="priming_data.csv", row.names = FALSE)

# PRIMING STATISTICS
loss_data <- thirteenC_data0 %>%
  filter(treatment != 'CCBP_1P' & treatment != 'CCBP_1V' & treatment != 'ONESIX' & treatment != 'TWENTY' & treatment != 'PALOUSE' & treatment != 'VERSHIRE') %>%
  #select(-'X', -'X.1', -'X.2', -'X.3', -'X.4') %>%
  filter(sample != 'V5C' & sample != 'V5A' & sample != 'P3B' & sample != 'V1A') %>%
  mutate(amt = 1000*amt) %>%
  group_by(sample, source) %>%
  arrange(time) %>%
  mutate(diff = amt - lag(amt, default = first(amt))) %>%
  mutate(diff = -1*diff) %>%
  filter(diff != 0) %>%
  mutate(diff = ifelse(diff < 0, 0, diff)) #if priming says somehow soil gained carbon, correct to 0  

                                 
priming_stats_data <- loss_data %>%
  filter(source == 'soil')

res_stats_data <- loss_data %>%
  filter(source == 'res')

# SOIL LOSS, INC 1, SOIL TYPE = PALOUSE
res.aov_priming <- aov(diff ~ treatment, data = priming_stats_data %>% filter(inc == '1' & soil_type == 'P')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') # use because of unbalanced design
TukeyHSD(res.aov_priming) 

# SOIL LOSS, INC 1, SOIL TYPE = VERSHIRE
res.aov_priming <- aov(diff ~ treatment, data = priming_stats_data %>% filter(inc == '1' & soil_type == 'V')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') 
TukeyHSD(res.aov_priming)  

# SOIL LOSS, INC 2, NEW/REDUCED
res.aov_priming <- aov(diff ~ treatment, data = priming_stats_data %>% filter(inc == '2' & dose == 'N')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') 
TukeyHSD(res.aov_priming)  

# SOIL LOSS, INC 2, STANDARD
res.aov_priming <- aov(diff ~ treatment, data = priming_stats_data %>% filter(inc == '2' & dose == 'S')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') 
TukeyHSD(res.aov_priming)  

###############################################################################################################

# RES LOSS, INC 1, SOIL TYPE = PALOUSE
res.aov_priming <- aov(diff ~ treatment, data = res_stats_data %>% filter(inc == '1' & soil_type == 'P')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') 
TukeyHSD(res.aov_priming) 

# RES LOSS, INC 1, SOIL TYPE = VERSHIRE
res.aov_priming <- aov(diff ~ treatment, data = res_stats_data %>% filter(inc == '1' & soil_type == 'V')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') 
TukeyHSD(res.aov_priming)  

# RES LOSS, INC 2, NEW/REDUCED
res.aov_priming <- aov(diff ~ treatment, data = res_stats_data %>% filter(inc == '2' & dose == 'N')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') 
TukeyHSD(res.aov_priming)  

# RES LOSS, INC 2, STANDARD
res.aov_priming <- aov(diff ~ treatment, data = res_stats_data %>% filter(inc == '2' & dose == 'S')) # test interaction btwn Num and Typ
Anova(res.aov_priming, type = 'III') 
TukeyHSD(res.aov_priming) 


# 13C INC1
thirteenC_data_INC1 <- thirteenC_data %>%
  filter(inc_fn1 == '1') %>%
  #filter(treatment ==	'CS_1P' | treatment == 'AD_1P' | treatment ==	'DASE_1P' | treatment ==	'CS_1V' | treatment ==	'AD_1V' | treatment ==	'DASE_1V') %>%
  mutate(soil = ifelse(treatment == 'CS_1P' | treatment ==	'AD_1P' | treatment ==	'DASE_1P', 'P', 'V')) %>%
  mutate(type = case_when(treatment == 'CS_1P' | treatment == 'CS_1V' ~ 'CS1',
                          treatment == 'AD_1P' | treatment == 'AD_1V' ~ 'AD1',
                          treatment == 'DASE_1P' | treatment == 'DASE_1V' ~ 'HLFB1')) %>%
  mutate(amt_fn1 = 1000*amt_fn1) %>%
  mutate(amt_fn2 = 1000*amt_fn2)  %>%
  mutate(err_max = amt_fn1+amt_fn2) %>%
  mutate(err_min = amt_fn1-amt_fn2) %>%
  mutate(soil = factor(soil, levels = c('P', 'V'), labels = c('Palouse', 'Vershire'))) 

thirteenC_err_data_INC1 <- thirteenC_err_data %>%
  filter(inc_fn1 == '1') %>%
  #filter(treatment ==	'CS_1P' | treatment == 'AD_1P' | treatment ==	'DASE_1P' | treatment ==	'CS_1V' | treatment ==	'AD_1V' | treatment ==	'DASE_1V') %>%
  mutate(soil = ifelse(treatment == 'CS_1P' | treatment ==	'AD_1P' | treatment ==	'DASE_1P', 'P', 'V')) %>%
  mutate(type = case_when(treatment == 'CS_1P' | treatment == 'CS_1V' ~ 'CS1',
                          treatment == 'AD_1P' | treatment == 'AD_1V' ~ 'AD1',
                          treatment == 'DASE_1P' | treatment == 'DASE_1V' ~ 'HLFB1')) %>%
  mutate(resC_fn1 = 1000*resC_fn1) %>%
  mutate(soilC_fn1 = 1000*soilC_fn1) %>%
  mutate(resC_fn2 = 1000*resC_fn2) %>%
  mutate(soilC_fn2 = 1000*soilC_fn2) 

thirteenC_data_INC1 <- merge(x = thirteenC_data_INC1, y = thirteenC_err_data_INC1, by = c('treatment', 'time')) 
thirteenC_data_INC1 <- thirteenC_data_INC1 %>%
  mutate(v_adj = ifelse(source == 'soil', 0, soilC_fn1))  #this needs to be amt_fn1 of the other source, 0)) 

thirteenC_INC1_plot <- ggplot(thirteenC_data_INC1, aes(fill=factor(source, levels = c('soil', 'res')), y=amt_fn1, x=time)) + 
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,2000)) +
  geom_errorbar(aes(ymin=err_min+v_adj, ymax=err_max+v_adj), col = 'black', width=.2, position = 'identity') + # in aes(col = factor(source, levels = c('soil', 'res'))) to check if err bars are on right bars
                #position=position_dodge(.9))  +
  facet_grid(soil.x ~ type.x) + 
  #scale_fill_discrete(limits = c("res", "source"), labels = c("Residue", "Soil")) +
  scale_fill_npg( labels = c("Soil", "Residue")) +
  labs(y = 'C in Treatments [mg C]', 
      # title = 'C Partitioning of Initial and Remaining C in Treatments Pre and Post Incubation 1',
       fill = 'Source') +
  theme_bar 
  #geom_text(aes(label = amt_fn2))
thirteenC_INC1_plot
ggsave("thirteenC_INC1_plot.png", plot = thirteenC_INC1_plot, width = 30, height = 15, units = "cm")  # change this accordingly
  
# priming INC1
priming_INC1 <- priming_data %>%
  filter(inc_fn1 == '1') %>%
  mutate(soil = ifelse(treatment == 'CS_1P' | treatment ==	'AD_1P' | treatment ==	'DASE_1P' | treatment == 'PALOUSE', 'P', 'V')) %>%
  mutate(type = case_when(treatment == 'CS_1P' | treatment == 'CS_1V' ~ 'CS1',
                          treatment == 'AD_1P' | treatment == 'AD_1V' ~ 'AD1',
                          treatment == 'DASE_1P' | treatment == 'DASE_1V' ~ 'HLFB1')) %>%
  filter(source == 'soil') %>%
  mutate(soil = factor(soil, levels = c('P', 'V'), labels = c('Palouse', 'Vershire'))) %>%
  rename(old_diff = diff) %>%
  mutate(control = ifelse(soil == 'Palouse', 	21, 0)) %>% # values here from diff column of priming data of PALOUSE and VERSHIRE
  mutate(diff = old_diff-control) %>%
  filter(treatment != 'PALOUSE', treatment != 'VERSHIRE')

priming_INC1_plot <- ggplot(priming_INC1, aes(x = type, y = diff, fill = type)) + # change "diff" to "old_diff" if you want to just see soil derived losses
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin=diff-stdev, ymax=diff+stdev), col = 'black', width=.2, position = 'identity') +
  theme_bar +
  labs(y = 'Soil Priming [mg]', fill = 'Substrate') +
  #labs(y = 'Soil Derived Carbon Loss [mg]', fill = 'Substrate') +
  #theme(axis.text.x = element_text()) +
  facet_grid(soil ~ type, scales = "free_x") +
  scale_fill_manual(values = INC1_colors) 
priming_INC1_plot 
ggsave("adj_priming_INC1_plot.png", plot = priming_INC1_plot , width = 30, height = 15, units = "cm")  # change this accordingly

# RESIDUE LOSS
priming_INC1 <- priming_data %>%
  filter(inc_fn1 == '1') %>%
  mutate(soil = ifelse(treatment == 'CS_1P' | treatment ==	'AD_1P' | treatment ==	'DASE_1P', 'P', 'V')) %>%
  mutate(type = case_when(treatment == 'CS_1P' | treatment == 'CS_1V' ~ 'CS1',
                          treatment == 'AD_1P' | treatment == 'AD_1V' ~ 'AD1',
                          treatment == 'DASE_1P' | treatment == 'DASE_1V' ~ 'HLFB1')) %>%
  filter(source == 'res') %>%
  mutate(soil = factor(soil, levels = c('P', 'V'), labels = c('Palouse', 'Vershire'))) 

priming_INC1_plot <- ggplot(priming_INC1, aes(x = type, y = diff, fill = type)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin=diff-stdev, ymax=diff+stdev), col = 'black', width=.2, position = 'identity') +
  theme_bar +
  labs(y = 'Residue Derived Carbon Loss [mg]', fill = 'Substrate') +
  #theme(axis.text.x = element_text()) +
  facet_grid(soil ~ type, scales = "free_x") +
  scale_fill_manual(values = INC1_colors) 
priming_INC1_plot 
ggsave("resloss_INC1_plot.png", plot = priming_INC1_plot , width = 30, height = 15, units = "cm")  # change this accordingly


# 13C INC2
thirteenC_data_INC2 <- thirteenC_data %>%
  filter(inc_fn1 == '2') %>%
  mutate(dose = case_when(treatment == 'CS_N' | treatment == 'AD_N'  
                          | treatment == 'POET_N' | treatment == 'NREL_N' ~ 'N',
                          treatment == 'DASE_AVG' | treatment == 'AD_S' 
                          | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'S')) %>%
  mutate(type = case_when(treatment == 'CS_N' ~ 'CS',
                          treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD',
                          treatment == 'DASE_AVG' | treatment == 'POET_N' | treatment == 'NREL_N' | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'DASE')) %>%
  mutate(type2 = case_when(treatment == 'CS_N' ~ 'CS2',
                           treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD2',
                           treatment == 'DASE_AVG' ~ 'HLFB1',
                           treatment == 'POET_N' | treatment == 'POET_S' ~ 'HLFB3',
                           treatment == 'NREL_N' | treatment == 'NREL_S' ~ 'HLFB2')) %>%
  mutate(dose = factor(dose, levels = c('S', 'N'), labels = c('Standard', 'Reduced'))) %>%
  mutate(amt_fn1 = 1000*amt_fn1) %>%
  mutate(amt_fn2 = 1000*amt_fn2)  %>%
  mutate(err_max = amt_fn1+amt_fn2) %>%
  mutate(err_min = amt_fn1-amt_fn2) 

thirteenC_err_data_INC2 <- thirteenC_err_data %>%
  filter(inc_fn1 == '2') %>%
  mutate(dose = case_when(treatment == 'CS_N' | treatment == 'AD_N'  
                          | treatment == 'POET_N' | treatment == 'NREL_N' ~ 'N',
                          treatment == 'DASE_AVG' | treatment == 'AD_S' 
                          | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'S')) %>%
  mutate(type = case_when(treatment == 'CS_N' ~ 'CS',
                          treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD',
                          treatment == 'DASE_AVG' | treatment == 'POET_N' | treatment == 'NREL_N' | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'DASE')) %>%
  mutate(type2 = case_when(treatment == 'CS_N' ~ 'CS2',
                           treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD2',
                           treatment == 'DASE_AVG' ~ 'HLFB1',
                           treatment == 'POET_N' | treatment == 'POET_S' ~ 'HLFB3',
                           treatment == 'NREL_N' | treatment == 'NREL_S' ~ 'HLFB2')) %>%
  mutate(dose = factor(dose, levels = c('S', 'N'))) %>%
  mutate(resC_fn1 = 1000*resC_fn1) %>%
  mutate(soilC_fn1 = 1000*soilC_fn1) %>%
  mutate(resC_fn2 = 1000*resC_fn2) %>%
  mutate(soilC_fn2 = 1000*soilC_fn2) 

thirteenC_data_INC2 <- merge(x = thirteenC_data_INC2, y = thirteenC_err_data_INC2, by = c('treatment', 'time')) 
thirteenC_data_INC2 <- thirteenC_data_INC2 %>%
  mutate(v_adj = ifelse(source == 'soil', 0, soilC_fn1))  #this needs to be amt_fn1 of the other source, 0)) 

thirteenC_INC2_plot <- ggplot(thirteenC_data_INC2, aes(fill=factor(source, levels = c('soil', 'res')), y=amt_fn1, x=time)) + 
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1500)) +
  facet_grid(dose.x ~ type2.x) + 
  geom_errorbar(aes(ymin=err_min+v_adj, ymax=err_max+v_adj),col = 'black', width=.2, position = 'identity') + # in aes(col = factor(source, levels = c('soil', 'res'))) to check if err bars are on right bars
  #scale_fill_discrete(limits = c("res", "source"), labels = c("Residue", "Soil")) +
  scale_fill_npg(labels = c("Soil", "Residue")) +
  labs(y = 'C in Treatments [mg C]', 
       #title = 'C Partitioning of Initial and Remaining C in Treatments Pre and Post Incubation 2',
       fill = 'Source') +
  theme_bar
thirteenC_INC2_plot
ggsave("thirteenC_INC2_plot.png", plot = thirteenC_INC2_plot, width = 30, height = 15, units = "cm")  # change this accordingly

# priming INC2
priming_INC2 <- priming_data %>%
  filter(inc_fn1 == '2') %>%
  mutate(dose = case_when(treatment == 'CS_N' | treatment == 'AD_N'  
                          | treatment == 'POET_N' | treatment == 'NREL_N' ~ 'N',
                          treatment == 'DASE_AVG' | treatment == 'AD_S' 
                          | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'S')) %>%
  mutate(type = case_when(treatment == 'CS_N' ~ 'CS',
                          treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD',
                          treatment == 'DASE_AVG' | treatment == 'POET_N' | treatment == 'NREL_N' | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'DASE')) %>%
  mutate(type2 = case_when(treatment == 'CS_N' ~ 'CS2',
                           treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD2',
                           treatment == 'DASE_AVG' ~ 'HLFB1',
                           treatment == 'POET_N' | treatment == 'POET_S' ~ 'HLFB3',
                           treatment == 'NREL_N' | treatment == 'NREL_S' ~ 'HLFB2')) %>%
  mutate(dose = factor(dose, levels = c('S', 'N'), labels = c('Standard', 'Reduced'))) %>%
  filter(source == 'soil') %>%
  rename(old_diff = diff) %>%
  mutate(control = 65) %>%
  mutate(diff = old_diff-control) %>%
  filter(treatment != 'ONESIX')

priming_INC2_plot <- ggplot(priming_INC2, aes(x = type2, y = diff, fill = type2)) + # change "diff" to "old_diff" if you are interested in just soil derived losses
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin=diff-stdev, ymax=diff+stdev), col = 'black', width=.2, position = 'identity') +
  theme_bar +
  labs(y = 'Soil Priming [mg]', fill = 'Substrate') +
  #labs(y = 'Soil Derived Carbon Loss [mg]', fill = 'Substrate') +
  #theme(axis.text.x = element_text()) +
  facet_grid(dose ~ type2, scales = "free_x") +
  scale_fill_manual(values = INC2_colors) 
priming_INC2_plot 
ggsave("adj_priming_INC2_plot.png", plot = priming_INC2_plot , width = 30, height = 15, units = "cm")  # change this accordingly

# old priming plot w/ new color, no control correction
priming_INC2_plot <- ggplot(priming_INC2, aes(x = type2, y = old_diff, fill = type2)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin=old_diff-stdev, ymax=old_diff+stdev), col = 'black', width=.2, position = 'identity') +
  theme_bar +
  labs(y = 'Soil Derived Carbon Loss [mg]', fill = 'Substrate') +
  #theme(axis.text.x = element_text()) +
  facet_grid(dose ~ type2, scales = "free_x") +
  scale_fill_manual(values = INC2_colors) 
priming_INC2_plot 
ggsave("priming_INC2_plot.png", plot = priming_INC2_plot , width = 30, height = 15, units = "cm")  # change this accordingly


# res loss
priming_INC2 <- priming_data %>%
  filter(inc_fn1 == '2') %>%
  mutate(dose = case_when(treatment == 'CS_N' | treatment == 'AD_N'  
                          | treatment == 'POET_N' | treatment == 'NREL_N' ~ 'N',
                          treatment == 'DASE_AVG' | treatment == 'AD_S' 
                          | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'S')) %>%
  mutate(type = case_when(treatment == 'CS_N' ~ 'CS',
                          treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD',
                          treatment == 'DASE_AVG' | treatment == 'POET_N' | treatment == 'NREL_N' | treatment == 'POET_S' | treatment == 'NREL_S' ~ 'DASE')) %>%
  mutate(type2 = case_when(treatment == 'CS_N' ~ 'CS2',
                           treatment == 'AD_N' | treatment == 'AD_S' ~ 'AD2',
                           treatment == 'DASE_AVG' ~ 'HLFB1',
                           treatment == 'POET_N' | treatment == 'POET_S' ~ 'HLFB3',
                           treatment == 'NREL_N' | treatment == 'NREL_S' ~ 'HLFB2')) %>%
  mutate(dose = factor(dose, levels = c('S', 'N'), labels = c('Standard', 'Reduced'))) %>%
  filter(source == 'res')

priming_INC2_plot <- ggplot(priming_INC2, aes(x = type2, y = diff, fill = type2)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin=diff-stdev, ymax=diff+stdev), col = 'black', width=.2, position = 'identity') +
  theme_bar +
  labs(y = 'Residue Derived Carbon Loss [mg]', fill = 'Substrate') +
  #theme(axis.text.x = element_text()) +
  facet_grid(dose ~ type2, scales = "free_x") +
  scale_fill_manual(values = INC2_colors) 
priming_INC2_plot 
ggsave("ressloss_INC2_plot.png", plot = priming_INC2_plot , width = 30, height = 15, units = "cm")  # change this accordingly

####################################################################################################
# CORRELATIONS

correlations_data0 <- read.csv("correlations_data.csv", stringsAsFactors = FALSE, header = TRUE) # scan in document formatted like example

correlations_data <- correlations_data0 %>%
  mutate(perSol = 100*perSol) %>%
  mutate(Cret_tot = 100*Cret_tot)

theme_point <- theme_bw() + theme(
  text = element_text(size = 20)
)

# C:N plot / analysis
C2N_plot <- ggplot(correlations_data, aes(x = C2N, y = Cret_tot)) +
  geom_point(aes(col = id), size = 2) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth", alpha = .25) +
  theme_point +
  labs(x = 'C:N of Substrate', 
       y = '% of Carbon Retained in \n Treatment Containing Substrate [%]',
       col = 'Substrate') +
  ylim(65, 100) +
  scale_color_manual(values = INCtot_colors) 
C2N_plot
ggsave("C2N_plot.png", plot = C2N_plot , width = 20, height = 15, units = "cm")  # change this accordingly

mod_C2N <- lm(Cret_tot ~ C2N, data = correlations_data)
anova(mod_C2N)
summary(mod_C2N)

# %lignin plot / analysis
lig_plot <- ggplot(correlations_data, aes(x = perLig, y = Cret_tot)) +
  geom_point(aes(col = id), size = 2) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth", alpha = .25) +
  theme_point +
  labs(x = '% Lignin of Substrate', 
       y = '% of Carbon Retained in \n Treatment Containing Substrate [%]',
       col = 'Substrate') +
  ylim(65, 100) +
  scale_color_manual(values = INCtot_colors) 
lig_plot
ggsave("lig_plot.png", plot = lig_plot , width = 20, height = 15, units = "cm")  # change this accordingly

mod_lig <- lm(Cret_tot ~ perLig, data = correlations_data)
anova(mod_lig)
summary(mod_lig)

# %solubilization / analysis
sol_plot <- ggplot(correlations_data, aes(x = perSol, y = Cret_tot)) +
  geom_point(aes(col = id), size = 2) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth", alpha = .25) +
  theme_point +
  labs(x = '% Solubilization of Substrate', 
       y = '% of Carbon Retained in \n Treatment Containing Substrate [%]',
       col = 'Substrate') +
  ylim(65, 100) +
  scale_color_manual(values = INCtot_colors) 
sol_plot
ggsave("sol_plot.png", plot = sol_plot , width = 20, height = 15, units = "cm")  # change this accordingly

mod_sol <- lm(Cret_tot ~ perSol, data = correlations_data)
anova(mod_sol)
summary(mod_sol)

# C:N, lignin, and solubilization
#lm(%retained ~ C:N, data = )
#anova (mod)
# if model is good, it'll be signficiant
# summary(mod) -> R^2 and p-value
# 
# it all depends on how much has been decomposed
# C:N, lignin, %sol

####### EXTRA
# onetime_INC1_plot <- ggplot(data = onetime_data_INC1, aes(x = YEAR, y = cummCO2resp, group = treatment)) +
#   geom_line(aes(col = type, linetype = factor(soil)), size = 2) + 
#   scale_linetype_manual(values = c('Palouse' = 'solid', 'Vershire' = 'dotdash')) +
#   # geom_text(data = data_ends, aes(label = treatment, 
#   #                                                              x = YEAR + 0.5, 
#   #                                                              y = cummCO2resp, 
#   #                                                              color = treatment)) +
#   #geom_text_repel(aes(label = type), data = data_ends, color = 'black', size = 4,
#   #nudge_x = 8, direction = "x", hjust = "right") +
#   # geom_label_repel(aes(label = type), data = data_ends,
#   #                   nudge_x = .1, size = 5,
#   #                   na.rm = TRUE) +
#   #scale_y_continuous(sec.axis = sec_axis(~ ., breaks = data_ends)) +
#   #geom_text(data = data_ends, aes(label = treatment, colour = treatment, x = Inf, y = cummCO2resp), hjust = -.1) +
# scale_color_manual(values = c('SOIL' = '#7E6148FF', 'CS1' = '#91D1C2FF', 'AD1' = '#8491B4FF', 'DASE1' = '#F39B7FFF')) + #c('SOIL' = '#7E6148FF', 'CS2' = '#91D1C2FF', 'AD2' = '#8491B4FF', 'DASE1' = '#F39B7FFF', 'DASE2' = '#E64B35FF', 'DASE3' = '#DC0000FF')) +
#   theme_C +
#   #xlim(0, 25) +
#   labs(x = 'Years', y = 'Total C Respired [mg]', 
#        #title = '100 Year Projections of C Respired in Incubation 1 Treatments', 
#        col = 'Treatment', linetype = 'Soil Type') 
# onetime_INC1_plot
# ggsave("onetimeINC1_plot.png", plot = onetime_INC1_plot, width = 30, height = 20, units = "cm")  # change this accordingly
