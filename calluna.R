#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤#
########### CALLUNA #####################
#¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤¤#

library(readxl)
library(tidyverse)
library(devtools)
library(patchwork)
library(lme4)
library(nlme)
library(patternplot)
library(egg)



# import metadata on plots 
plotID <- read_xlsx('Data/drought.plots.xlsx') # these data 


# import data on growth of Calluna 2016-2019
calluna <- read_xlsx('Data/DE.2_Calluna-traits_Dataset.xlsx', na = "NA") %>% 
  filter(!(plot == 'a' | plot == '9.1.b')) %>% 
  left_join(plotID, by = c('plot', 'site')) %>% 
  mutate_each (funs(as.numeric), 7:28) %>% 
  mutate(stem1 = ifelse(individual == '16.1.5.' & year == '2016', 5.5, 
                              ifelse(individual == '1.1.6.' & year == '2018', 2.5, 
                                     ifelse(individual == '18.1.9.' & year == '2016', 0.5, 
                                            ifelse(individual == '18.1.9.' & year == '2017', 0.8, stem1)))),
         stem2 = ifelse(individual == '1.1.6.' & year == '2018', 3.2, stem2),
         stem3 = ifelse(individual == '1.1.6.' & year == '2018', 2.4, stem3),
         stem4 = ifelse(individual == '1.1.6.' & year == '2018', 2.6, stem4),
         stem5 = ifelse(individual == '1.1.6.' & year == '2018', 2.2, stem5)) %>% 
  filter(!(individual == '18.3.0.' & year == '2018'),
         !(individual == '18.1.9.' & year == '2019')) %>% 
  mutate(stem = rowMeans(.[, c('stem1','stem2','stem3', 'stem4','stem5')], na.rm = TRUE),
         branch = rowMeans(.[, c('branch1','branch2','branch3', 'branch4','branch5')], na.rm = TRUE),
         terminal = rowMeans(.[, c('terminal1','terminal2','terminal3', 'terminal4','terminal5')], na.rm = TRUE),
         geography = recode(geography, north = 'Northern Norway', south = 'Southern Norway'),
         phase = factor(phase, levels = c('pioneer', 'building', 'mature')),
         phase = fct_recode(phase, "Young" = 'pioneer', "Intermediate" = 'building', 'Old' = 'mature'),
         year.exp2 = ifelse(year == '2016', 0,
                           ifelse (year == '2017', 1,
                           ifelse (year == '2018', 2,
                                   ifelse (year == '2019', 3, year))))) %>% 
  mutate (year.exp = as.numeric(year.exp2)) %>% 
  filter(stem != 'NaN',
         stem > -100,
         stem < 150)
# 
ggplot(calluna %>% filter(year == '2019'), aes(phase, stem, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~geography) +
  scale_fill_manual(values = c("0" = 'darkblue', "60" = 'deepskyblue3', "90" = "mediumturquoise"), 
                    name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
  theme_classic() +
  ylab('Stem diameter (mm)')



#######################

stem <- 
  ggplot(calluna, aes(year.exp, stem, colour = phase, linetype = treatment, shape = treatment)) +
  geom_jitter(size = 1.6) +
  geom_smooth(method = lm, se = FALSE, size = 2.5) +
  scale_colour_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                      name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"),
                        name = "Drought treatment",
                        breaks= c("0", "60", "90"),
                        labels = c("Ambient", "Moderate", "Extreme")) +
  scale_shape_manual(values = c(16, 17, 15),
                     name = "Drought treatment",
                     breaks= c("0", "60", "90"),
                     labels = c("Ambient", "Moderate", "Extreme")) +
  theme_classic() +
  facet_wrap(~geography) +
  ylab('Diameter (mm)') +
  xlab('Years since start') +
  ggtitle('a)      Stem diameter') +
  theme(strip.text = element_text(size=15),
        strip.background =element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)) +
  guides(linetype = guide_legend(override.aes= list(color = "black", size = 2)),
  shape = guide_legend(override.aes = list(colour = 'red', size = 5)))


my_tag <- c("Year* \n Succession", "Year:Drought** \n Succession** \n Year:Succession: \n Drought**")

fig_stem <- 
  tag_facet(stem, 
            x = 2, y = 500, 
            vjust = 1, hjust = 0.5,
            open = "", close = "",
            #fontface = 4,
            size = 4,
            #family = "serif",
            tag_pool = my_tag)


#height <- 
ggplot(calluna, aes(year.exp, height, colour = phase, linetype = treatment, shape = treatment)) +
  geom_jitter(size = 1.6) +
  geom_smooth(method = lm, se = FALSE, size = 2.5) +
  scale_colour_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                      name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted"),
                        name = "Drought treatment",
                        breaks= c("0", "60", "90"),
                        labels = c("Ambient", "Moderate", "Extreme")) +
  scale_shape_manual(values = c(16, 17, 15),
                     name = "Drought treatment",
                     breaks= c("0", "60", "90"),
                     labels = c("Ambient", "Moderate", "Extreme")) +
  theme_classic() +
  facet_wrap(~geography) +
  ylab('Height of stand (mm)') +
  xlab('Years since start') +
  ggtitle('b)      Height') +
  theme(strip.text = element_text(size=15),
        strip.background =element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)) +
  guides(linetype = guide_legend(override.aes= list(color = "black", size = 2)),
         shape = guide_legend(override.aes = list(colour = 'red', size = 5)))


my_tag <- c("Year* \n Succession", "Year:Drought** \n Succession** \n Year:Succession: \n Drought**")

fig_height <- 
  tag_facet(height, 
            x = 2, y = 500, 
            vjust = 1, hjust = 0.5,
            open = "", close = "",
            #fontface = 4,
            size = 4,
            #family = "serif",
            tag_pool = my_tag)


#### STEM ####

p <-
  ggplot(calluna, aes(year, log(stem), fill = interaction(treatment,phase))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0.Young" = "lightskyblue1", "60.Young" = "lightgreen", "90.Young" = "orange", "0.Intermediate" = "lightseagreen", "60.Intermediate" = "limegreen", "90.Intermediate" = "darkorange", "0.Old" = "steelblue", "60.Old" = "forestgreen", "90.Old" = "chocolate3"), name = "Phase and drought frequency", labels = c("Young, ambient", "Young, moderate", "Young, extreme", "Intermediate, ambient", "Intermediate, moderate", "Intermediate, extreme", "Old, ambient", "Old, moderate", "Old, extreme")) +
  theme_classic() +
  # annotate("text", x = '2016', y = 13,
  #          label = "Phase:Year***") +
  # annotate("text", x = '2016', y = 12.5,
  #          label = "Phase***") +
  # annotate("text", x = '2016', y = 12,
  #          label = "Year***") +
  facet_grid(~geography, scales = 'free') +
  ylab('log Stem diameter (mm)') +
  xlab('Year') 


my_tag <- c("Year:Phase*** \n Year***", "Year:Phase*** \n Year*** \n Phase***")
fig <- tag_facet(p, 
          x = '2017', y = 2.1, 
          vjust = c(-2, -1), hjust = 1,
          open = "", close = "",
          #fontface = 4,
          size = 3,
          #family = "serif",
          tag_pool = my_tag)

tiff("Figures/stem_south_boxplot.tiff", units="in", width=10, height=5, res=600)
fig
dev.off() 

#### TERMINAL ####

p <-
  ggplot(calluna, aes(year, terminal, fill = interaction(treatment,phase))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0.Young" = "lightskyblue1", "60.Young" = "lightgreen", "90.Young" = "orange", "0.Intermediate" = "lightseagreen", "60.Intermediate" = "limegreen", "90.Intermediate" = "darkorange", "0.Old" = "steelblue", "60.Old" = "forestgreen", "90.Old" = "chocolate3"), name = "Phase and drought frequency", labels = c("Young, ambient", "Young, moderate", "Young, extreme", "Intermediate, ambient", "Intermediate, moderate", "Intermediate, extreme", "Old, ambient", "Old, moderate", "Old, extreme")) +
  theme_classic() +
  # annotate("text", x = '2016', y = 13,
  #          label = "Phase:Year***") +
  # annotate("text", x = '2016', y = 12.5,
  #          label = "Phase***") +
  # annotate("text", x = '2016', y = 12,
  #          label = "Year***") +
  facet_grid(~geography, scales = 'free') +
  ylab('Terminal shoot (mm)') +
  xlab('Year') 


my_tag <- c("Year:Phase*** \n Year:Treatment* \n Year***  \n Phase**", "Year:Phase:Treatment* \n Year:Phase* \n Year:Treatment*** \n Year*** \n Phase***")

fig <- 
  tag_facet(p, 
                 x = '2019', y = 150, 
                 vjust = c(1, 1), hjust = 1,
                 open = "", close = "",
                 #fontface = 4,
                 size = 3,
                 #family = "serif",
                 tag_pool = my_tag)

tiff("Figures/terminal_boxplot.tiff", units="in", width=10, height=5, res=600)
fig
dev.off() 



#### HEIGHT ####

p <-
  ggplot(calluna, aes(year, height, fill = interaction(treatment,phase))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0.Young" = "lightskyblue1", "60.Young" = "lightgreen", "90.Young" = "orange", "0.Intermediate" = "lightseagreen", "60.Intermediate" = "limegreen", "90.Intermediate" = "darkorange", "0.Old" = "steelblue", "60.Old" = "forestgreen", "90.Old" = "chocolate3"), name = "Phase and drought frequency", labels = c("Young, ambient", "Young, moderate", "Young, extreme", "Intermediate, ambient", "Intermediate, moderate", "Intermediate, extreme", "Old, ambient", "Old, moderate", "Old, extreme")) +
  theme_classic() +
  # annotate("text", x = '2016', y = 13,
  #          label = "Phase:Year***") +
  # annotate("text", x = '2016', y = 12.5,
  #          label = "Phase***") +
  # annotate("text", x = '2016', y = 12,
  #          label = "Year***") +
  facet_grid(~geography, scales = 'free') +
  ylab('Stand height (mm)') +
  xlab('Year') 


my_tag <- c("Year:Phase*** \n  Year*** \n Phase***", "Year:Phase*** \n  Year*** \n Phase***")

fig <- 
  tag_facet(p, 
            x = '2016', y = c(400, 600), 
            vjust = 1, hjust = 0.5,
            open = "", close = "",
            #fontface = 4,
            size = 3,
            #family = "serif",
            tag_pool = my_tag)

tiff("Figures/height_boxplot.tiff", units="in", width=10, height=5, res=600)
fig
dev.off() 


  # geom_text(
  #   data  = dat_text,
  #   mapping = aes(x = year, y = stem, label = label),
  #              inherit.aes = FALSE) 
  # geom_text(data = ann_text, aes(x = '2017', y = 10, label = label), label = c("a", "Phase***"),
  #           inherit.aes = FALSE) +
  # geom_text(data = ann_text, aes(x = '2017', y = 9, label = label), label = c("a", "Phase***"),
  #           inherit.aes = FALSE) +
  # geom_text(data = ann_text, aes(x = '2017', y = 8, label = label), label = c("a","Phase***"),
  #           inherit.aes = FALSE) 
  #ylab('Height of Calluna stand (mm)')
  #ylab('Stem diameter (mm)') 
  #ylab('New shoot length (mm)') 
  #ylab('Stretched length of Calluna stand (mm)') +
  #ylab('Canopy height (mm)') +
  #ylab('Canopy damage (%)')

# 
# pattern.type<-c('nwlines', 'blank', 'nelines')
# pattern.color=c('black','black', 'black')
# background.color=c("lightseagreen","limegreen", "darkorange")
# frame.color=c('black', 'black', 'black')
# pattern.line.size<-c(3, 1,3)
# density<-c(6, 1, 8)
# group <- calluna$phase
# x = calluna$year
# y = calluna$stem
# 
# patternboxplot(calluna, x, y, group, pattern.type=pattern.type,pattern.line.size=pattern.line.size,
#                pattern.color=pattern.color,background.color=background.color,frame.color=frame.color,
#                density=density,legend.h=2,legend.x.pos=1.1,legend.y.pos=0.495,legend.pixel=10, legend.w=0.2,
#                legend.label=c("Orange", "Strawberry", "Watermelon"))      


############  annual change #########
# The variation between stands is large. Increasin resolution by investigating annual change

# yes yes, I know you are not going to like the look of this, Richard. 

# first, extract each year and then put it together to simpler calculate differences between them. Doing this manually, could maybe have been done with pivot_wider also?

c2016 <- calluna %>% 
 filter(year == '2016') %>% 
  mutate(stem16 = stem,
         terminal16 = terminal,
         height16 = height,
         length16 = length) %>% 
  dplyr::select(site, year, plot, block, individual, phase, geography, treatment, stem16, length16, height16, terminal16)

c2017 <- calluna %>% 
  filter(year == '2017') %>% 
  mutate(stem17 = stem,
         terminal17 = terminal,
         height17 = height,
         length17 = length) %>% 
  dplyr::select(site, year, plot, block, individual, phase, geography, treatment, stem17, length17, height17, terminal17)

c2018 <- calluna %>% 
  filter(year == '2018') %>% 
  mutate(stem18 = stem,
         terminal18 = terminal,
         height18 = height,
         length18 = length) %>% 
  dplyr::select(site, year, plot, block, individual, phase, geography,treatment, stem18, length18, height18, terminal18)

c2019 <- calluna %>% 
  filter(year == '2019') %>% 
  mutate(stem19 = stem,
         terminal19 = terminal,
         height19 = height,
         length19 = length) %>% 
  dplyr::select(site, year, plot, block, individual, phase, geography, treatment, stem19, length19, height19, terminal19)

annual <- c2016 %>% 
  left_join(c2017, by = c('site', 'plot', 'block', 'individual', 'phase', 'geography', 'treatment')) %>% 
  left_join(c2018, by = c('site', 'plot', 'block', 'individual', 'phase', 'geography', 'treatment')) %>% 
  left_join(c2019, by = c('site', 'plot', 'block', 'individual', 'phase', 'geography', 'treatment')) %>% 
  mutate(stem16_17 = stem17 - stem16,
         stem17_18 = stem18 - stem17,
         stem18_19 = stem18 - stem19,
        terminal16_17 = terminal17 - terminal16,
         terminal17_18 = terminal18 - terminal17,
         terminal18_19 = terminal19 - terminal18,
        height16_17 = height17 - height16,
        height17_18 = height18 - height17,
        height18_19 = height18 - height19,
        length16_17 = length17 - length16,
        length17_18 = length18 - length17,
        length18_19 = length18 - length19
        ) %>% 
  dplyr::select(site,
         plot, 
         block,
         individual, 
         phase, 
         geography, 
         treatment, 
         stem16_17, 
         stem17_18, 
         stem18_19, 
         stem16,
         stem17,
         stem18,
         stem19,
         terminal16_17, 
         terminal17_18, 
         terminal18_19,
         terminal16,
         terminal17,
         terminal18,
         terminal19,
         height16_17, 
         height17_18,
         height18_19,
         height16,
         height17,
         height18,
         height19,
         length16_17,
         length17_18,
        length18_19,
        length16,
        length17,
        length18) %>% 
filter(!(site == 'BUO')) # %>% 
  # pivot_longer(c(-site, -plot, -individual, -phase, -geography, -treatment), names_to = 'what', values_to = 'value')

stem <- annual %>% 
  dplyr::select(site,
         plot, 
         block,
         individual, 
         phase, 
         geography, 
         treatment, 
         stem16_17, 
         stem17_18, 
         stem18_19, 
         stem16,
         stem17,
         stem18,
         stem19) %>% 
  mutate('2017' = stem16_17, # / stem16 *100,
         '2018' = stem17_18, # / stem17 * 100,
         '2019' = stem18_19,
         'total' = stem19-stem16)# %>%  # / stem18 * 100) %>% 
  #  tidyr::pivot_longer(c(-site, -plot, -block, -individual, -phase, -geography, -treatment), names_to = 'year', values_to = 'stem') %>% 
  # filter(year %in% c('2017', '2018', '2019'))
  
terminal <- annual %>% 
  dplyr::select(site,
         plot, 
         block,
         individual, 
         phase, 
         geography, 
         treatment, 
         terminal16_17, 
         terminal17_18, 
         terminal18_19, 
         terminal16,
         terminal17,
         terminal18,
           terminal19) %>% 
  mutate('2017' = terminal17 ,
         '2018' = terminal18 ,
         '2019' = terminal19 ,
         'total' = terminal19-terminal16) #%>% 
  # pivot_longer(c(-site, -plot, -block, -individual, -phase, -geography, -treatment), names_to = 'year', values_to = 'terminal') %>% 
  # filter(year %in% c('2016', '2017', '2018', '2019'))

height <- annual %>% 
  dplyr::select(site,
         plot,
         block,
         individual, 
         phase, 
         geography, 
         treatment, 
         height16_17, 
         height17_18, 
         height18_19, 
         height16,
         height17,
         height18,
         height19) %>% 
  mutate('2017' = height16_17 / height16 *100,
         '2018' = height17_18 / height17 * 100,
         '2019' = height18_19 / height18 * 100,
         'total' = height19-height16) #%>% 
  # pivot_longer(c(-site, -plot, -block,  -individual, -phase, -geography, -treatment), names_to = 'year', values_to = 'height') %>% 
  # filter(year %in% c('2016', '2017', '2018', '2019'))

length <- annual %>% 
  dplyr::select(site,
         plot, 
         block,
         individual, 
         phase, 
         geography, 
         treatment, 
         length16_17, 
         length17_18, 
         length18_19, 
         length16,
         length17,
         length18) %>% 
  mutate('2017' = length16_17 / length16 *100,
         '2018' = length17_18 / length17 * 100,
         '2019' = length18_19 / length18 * 100) %>% 
  pivot_longer(c(-site, -plot, -block, -individual, -phase, -geography, -treatment), names_to = 'year', values_to = 'length') %>% 
  filter(year %in% c('2016', '2017', '2018', '2019')) %>% 
  filter(!(individual == '1.1.6.' & year == '2018')) #weird values, probably misidentified specimen


#########################


stem2 <- stem %>% 
  group_by(site, geography, phase, treatment) %>% 
  summarise(mean = mean(total, na.rm = TRUE), sd = sd(total, na.rm = TRUE), n = n())

Stem_boks <- 
  ggplot(stem, aes(phase, total, fill = treatment)) +
  geom_boxplot() +
  # scale_fill_manual(values = c('0' = "#E69F00", '60' = "#56B4E9", '90' = "#009E73"),
  #                   name = "Drought treatment",
  #                   labels = c("Ambient", "Moderate", "Extreme")) +
  scale_fill_manual(values = c("0" = 'darkblue', "60" = 'deepskyblue3', "90" = "mediumturquoise"), 
                    name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
  theme_classic() +
  facet_wrap(~geography) +
  #ylab('log plant matter drymass(gm-2)') +
    labs(y = expression(paste('',Delta,' Stem diameter 2016-2019 (mm)'))) +
  xlab('Successional phase') +
  #ggtitle('Total') +
  theme(strip.text = element_text(size=15),
        strip.background =element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)) +
    ggtitle('a)')



  Terminal_boks <- 
  ggplot(calluna %>%  filter(year == 2019), aes(phase, log(terminal), fill = treatment)) +
    geom_boxplot() +
    # scale_fill_manual(values = c('0' = "#E69F00", '60' = "#56B4E9", '90' = "#009E73"),
    #                   name = "Drought treatment",
    #                   labels = c("Ambient", "Moderate", "Extreme")) +
    scale_fill_manual(values = c("0" = 'darkblue', "60" = 'deepskyblue3', "90" = "mediumturquoise"), 
                      name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
    theme_classic() +
    facet_wrap(~geography) +
    #ylab('log plant matter drymass(gm-2)') +
    labs(y = 'log Annual shoot growth 2019 (mm)') +
    xlab('Successional phase') +
    #ggtitle('Total') +
    theme(strip.text = element_text(size=15),
          strip.background =element_blank(),
          text = element_text(size=15),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13)) +
    ggtitle('c)')
  
  Height_boks <- 
  ggplot(height, aes(phase, total, fill = treatment)) +
    geom_boxplot() +
   # geom_jitter() +
    # scale_fill_manual(values = c('0' = "#E69F00", '60' = "#56B4E9", '90' = "#009E73"),
    #                   name = "Drought treatment",
    #                   labels = c("Ambient", "Moderate", "Extreme")) +
    scale_fill_manual(values = c("0" = 'darkblue', "60" = 'deepskyblue3', "90" = "mediumturquoise"), 
                      name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
    theme_classic() +
    facet_wrap(~geography) +
    #ylab('log plant matter drymass(gm-2)') +
    labs(y = expression(paste('',Delta,' Stand height 2016-2019 (mm)'))) +
    xlab('Successional phase') +
    #ggtitle('Total') +
    theme(strip.text = element_text(size=15),
          strip.background =element_blank(),
          text = element_text(size=15),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13)) +
    ggtitle('b)')

  
  # linear stem
  
  mod <- lmer(total ~ treatment*phase + (1|plot), 
                   data = stem %>% filter(geography == 'Northern Norway'),
                   REML = TRUE,
                   na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(total ~ treatment*phase + (1|plot), 
              data = stem %>% filter(geography == 'Southern Norway'),
              REML = TRUE,
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(total ~ geography*treatment  + (1|plot), 
              data = stem %>% filter(phase == 'Intermediate'),
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(total ~ geography*treatment  + (1|plot), 
              data = stem %>% filter(phase == 'Old'),
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  # linear terminal
  
  mod <- lmer(log(terminal19) ~ treatment*phase + (1|plot), 
              data = terminal %>% filter(geography == 'Northern Norway'),
              REML = TRUE,
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(log(terminal19) ~ treatment*phase + (1|plot), 
              data = terminal %>% filter(geography == 'Southern Norway'),
              REML = TRUE,
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  mod <- lmer(log(terminal19) ~ geography*treatment  + (1|plot), 
              data = terminal %>% filter(phase == 'Intermediate'),
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(log(terminal19) ~ geography*treatment  + (1|plot), 
              data = terminal %>% filter(phase == 'Old'),
              na.action=na.omit)  
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  # linear height
  
  mod <- lmer(total ~ treatment*phase + (1|plot), 
              data = height %>% filter(geography == 'Northern Norway'),
              REML = TRUE,
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(total ~ treatment*phase + (1|plot), 
              data = height %>% filter(geography == 'Southern Norway'),
              REML = TRUE,
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  mod <- lmer(total ~ treatment*phase + (1|plot), 
              data = stem %>% filter(geography == 'Northern Norway'),
              REML = TRUE,
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(total ~ treatment*phase + (1|plot), 
              data = stem %>% filter(geography == 'Southern Norway'),
              REML = TRUE,
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(total ~ geography*treatment  + (1|plot), 
              data = height %>% filter(phase == 'Intermediate'),
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  mod <- lmer(total ~ geography*treatment  + (1|plot), 
              data = height %>% filter(phase == 'Old'),
              na.action=na.omit)
  
  anova(mod)
  
  summary(mod)
  plot(mod) 
  qqnorm(residuals(mod))
  qqline(residuals(mod))
  
  
  
  my_tag_Y <- c("", "a")
  my_tag_I <- c("a A", "b B")
  my_tag_O <- c("b A", "a B")
  
  
  Stem_boks_ann <- 
    Stem_boks %>% 
    tag_facet(x = 'Young', y =  6.2,
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_Y) %>%
    tag_facet(x = 'Intermediate', y = c(7, 3.7), 
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_I) %>% 
    tag_facet(x = 'Old', y = c(3, 6), 
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_O) + 
    theme(strip.text = element_text()) # add this to get the facet header back (bacause tag_facet fuction removes them)
  
  
  
  tiff('Figures/Calluna.stem.box.tif', units = 'px', height = 3000, width = 3000, res = 300)
  Stem_boks_ann
  dev.off()
  
  
  
  my_tag_Y <- c("", "a")
  my_tag_I <- c("a A", "a B")
  my_tag_O <- c("a A", "a B")
  
  
  Terminal_boks_ann <- 
    Terminal_boks %>% 
    tag_facet(x = 'Young', y = 5,
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_Y) %>%
    tag_facet(x = 'Intermediate', y = c(4, 5), 
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_I) %>% 
    tag_facet(x = 'Old', y = c(4.5, 5), 
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_O) + 
    theme(strip.text = element_text()) # add this to get the facet header back (bacause tag_facet fuction removes them)
  
  
  
  tiff('Figures/Calluna.term.box.tif', units = 'px', height = 3000, width = 3000, res = 300)
  Terminal_boks_ann
  dev.off()
  
  
  my_tag_Y <- c("", "a")
  my_tag_I <- c("a A", "a B")
  my_tag_O <- c("a A", "a B")
  
  
 Height_boks_ann <- 
    Height_boks %>% 
    tag_facet(x = 'Young', y = 300,
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_Y) %>%
    tag_facet(x = 'Intermediate', y = c(200, 250), 
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_I) %>% 
    tag_facet(x = 'Old', y = c(180, 300), 
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              fontface = 1,
              size = 5,
              #family = "serif",
              tag_pool = my_tag_O) + 
    theme(strip.text = element_text())  # add this to get the facet header back (bacause tag_facet fuction removes them) +
 
  
  
  tiff('Figures/Calluna.height.box.tif', units = 'px', height = 3000, width = 3000, res = 300)
  Height_boks_ann
  dev.off()
  

  
  ##### TukeyHSD
  
  
  tni <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = stem %>% filter(geography == 'Northern Norway' & phase == 'Intermediate'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'North',
           phase = 'Intermediate',
           levels = c('60-0', '90-0', '90-60'))
  
  
  tno <- (TukeyHSD(aov(lm(total ~ treatment, 
                           data = stem %>% filter(geography == 'Northern Norway' & phase == 'Old'),
                           na.action=na.omit))))$`treatment`[,1:4] %>% 
            as.data.frame() %>% 
            mutate(geography = 'North',
                   phase = 'Old',
                   levels = c('60-0', '90-0', '90-60'))
  
  tsy <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = stem %>% filter(geography == 'Southern Norway' & phase == 'Young'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Young',
           levels = c('60-0', '90-0', '90-60'))
  
  tsi <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = stem %>% filter(geography == 'Southern Norway' & phase == 'Intermediate'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Intermediate',
           levels = c('60-0', '90-0', '90-60'))
  
  
  tso <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = stem %>% filter(geography == 'Southern Norway' & phase == 'Old'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Old',
           levels = c('60-0', '90-0', '90-60'))
  
  
  
  tukey_full_c <- rbind(tni, tno, tsy, tsi, tso) %>% 
    mutate(phase = fct_relevel(phase, 'Young', 'Intermediate', 'Old'))
  
  tukey_c_stem <-
    ggplot(tukey_full_c, aes(x = diff, y = levels, xmin = lwr, xmax = upr)) +
    geom_point() +
    geom_errorbarh(height = 0.3) +
    geom_vline(xintercept = 0, linetype="dotted", size = 0.8) +
    facet_grid(phase ~geography) +
    theme_classic() +
    xlab('Differences in mean levels of drought treatment') +
    ylab('Drought-level pairs') +
    ggtitle('d)')
  
  tiff('Figures/Tukey_stem.tif', units = 'px', height = 1000, width = 1500, res = 300)
  tukey_c_stem
  dev.off()
  
  tni <- (TukeyHSD(aov(lm(log(terminal19) ~ treatment, 
                          data = terminal %>% filter(geography == 'Northern Norway' & phase == 'Intermediate'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'North',
           phase = 'Intermediate',
           levels = c('60-0', '90-0', '90-60'))
  
  
  tno <- (TukeyHSD(aov(lm(log(terminal19) ~ treatment, 
                          data = terminal %>% filter(geography == 'Northern Norway' & phase == 'Old'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'North',
           phase = 'Old',
           levels = c('60-0', '90-0', '90-60'))
  
  tsy <- (TukeyHSD(aov(lm(log(terminal19) ~ treatment, 
                          data = terminal %>% filter(geography == 'Southern Norway' & phase == 'Young'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Young',
           levels = c('60-0', '90-0', '90-60'))
  
  tsi <- (TukeyHSD(aov(lm(log(terminal19) ~ treatment, 
                          data = terminal %>% filter(geography == 'Southern Norway' & phase == 'Intermediate'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Intermediate',
           levels = c('60-0', '90-0', '90-60'))
  
  
  tso <- (TukeyHSD(aov(lm(log(terminal19) ~ treatment, 
                          data = terminal %>% filter(geography == 'Southern Norway' & phase == 'Old'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Old',
           levels = c('60-0', '90-0', '90-60'))
  
  
  
  tukey_full_c <- rbind(tni, tno, tsy, tsi, tso) %>% 
    mutate(phase = fct_relevel(phase, 'Young', 'Intermediate', 'Old'))
  
  tukey_c_terminal <-
    ggplot(tukey_full_c, aes(x = diff, y = levels, xmin = lwr, xmax = upr)) +
    geom_point() +
    geom_errorbarh(height = 0.3) +
    geom_vline(xintercept = 0, linetype="dotted", size = 0.8) +
    facet_grid(phase ~geography) +
    theme_classic() +
    xlab('Differences in mean levels of drought treatment') +
    ylab('Drought-level pairs') +
    ggtitle('f)')
  
  tiff('Figures/Tukey_terminal.tif', units = 'px', height = 1000, width = 1500, res = 300)
  tukey_c_terminal
  dev.off()
  
  
  tni <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = height %>% filter(geography == 'Northern Norway' & phase == 'Intermediate'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'North',
           phase = 'Intermediate',
           levels = c('60-0', '90-0', '90-60'))
  
  
  tno <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = height %>% filter(geography == 'Northern Norway' & phase == 'Old'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'North',
           phase = 'Old',
           levels = c('60-0', '90-0', '90-60'))
  
  tsy <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = height %>% filter(geography == 'Southern Norway' & phase == 'Young'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Young',
           levels = c('60-0', '90-0', '90-60'))
  
  tsi <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = height %>% filter(geography == 'Southern Norway' & phase == 'Intermediate'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Intermediate',
           levels = c('60-0', '90-0', '90-60'))
  
  
  tso <- (TukeyHSD(aov(lm(total ~ treatment, 
                          data = height %>% filter(geography == 'Southern Norway' & phase == 'Old'),
                          na.action=na.omit))))$`treatment`[,1:4] %>% 
    as.data.frame() %>% 
    mutate(geography = 'South',
           phase = 'Old',
           levels = c('60-0', '90-0', '90-60'))
  
  
  
  tukey_full_c <- rbind(tni, tno, tsy, tsi, tso) %>% 
    mutate(phase = fct_relevel(phase, 'Young', 'Intermediate', 'Old'))
  
  tukey_c_height <-
    ggplot(tukey_full_c, aes(x = diff, y = levels, xmin = lwr, xmax = upr)) +
    geom_point() +
    geom_errorbarh(height = 0.3) +
    geom_vline(xintercept = 0, linetype="dotted", size = 0.8) +
    facet_grid(phase ~geography) +
    theme_classic() +
    xlab('Differences in mean levels of drought treatment') +
    ylab('Drought-level pairs')+
    ggtitle('e)')
  
  tiff('Figures/Tukey_height.tif', units = 'px', height = 1000, width = 1500, res = 300)
  tukey_c_height
  dev.off()
  
  
  calluna_full  <-
    (Stem_boks_ann / Height_boks_ann / Terminal_boks_ann & theme(legend.position = "bottom")) + plot_layout(guides = "collect")

  
  
  layout<-"
AAD
AAE
AAG

"
  
  ny <- 
    wrap_plots(A = calluna_full, D = tukey_c_stem, G = tukey_c_terminal, E = tukey_c_height, design = layout)
  
  
  tiff('Figures/Calluna.full.box.tukey.tif', units = 'px', height = 5000, width = 3000, res = 300)
  ny
  dev.off()

########################
stem$year <- as.numeric(stem$year)

#p.stem <- 
  ggplot(stem, aes(year, stem, colour = phase, linetype = treatment, shape = treatment, fill = phase)) +
  geom_jitter(size = 2, alpha=0.3, colour='black', width = 0.25) +
  #geom_point(shape= c(21, 24, 22), color="black", size=4, stroke=1.5) +
  geom_smooth(method = lm, se = FALSE, size = 2, alpha = 1) +
  scale_colour_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                      name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
  scale_linetype_manual(values = c("solid", "longdash", "dashed"),
                        name = "Drought treatment",
                        breaks= c("0", "60", "90"),
                        labels = c("Ambient", "Moderate", "Extreme")) +
  scale_shape_manual(values = c(16, 17, 15),
                     name = "Drought treatment",
                     breaks= c("0", "60", "90"),
                     labels = c("Ambient", "Moderate", "Extreme")) +
  scale_shape_manual(values = c(21, 24, 22),
                     name = "Drought treatment",
                     breaks= c("0", "60", "90"),
                     labels = c("Ambient", "Moderate", "Extreme")) +
  scale_fill_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                    name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
  theme_classic() +
  facet_wrap(~geography) +
  ylab('log diameter (mm)') +
  xlab('Year') +
  ggtitle('a)      Stem diameter') +
  theme(strip.text = element_text(size=15),
        strip.background =element_blank(),
        text = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13)) +
  guides(linetype = guide_legend(override.aes= list(color = "black", size = 2)),
         shape = guide_legend(override.aes = list(colour = 'red', size = 5))) +
    scale_x_continuous(breaks=c(2016, 2017, 2018, 2019),
                     labels=c("2016", "2017", "2018", "2019"))

  
  
  stem1 <- lmer(stem ~ treatment*phase*year + (1|plot), data = stem %>% filter(geography == 'Southern Norway'), na.action=na.omit, REML = TRUE)
  
  anova(stem1)
  
  my_tag <- c("Year:Phase*** \n  Year*** \n Phase***", "Year:Phase*** \n  Year*** \n Phase***")
  
  fig <- 
    tag_facet(p, 
              x = '2016', y = c(400, 600), 
              vjust = 1, hjust = 0.5,
              open = "", close = "",
              #fontface = 4,
              size = 3,
              #family = "serif",
              tag_pool = my_tag)
  
  tiff("Figures/height_boxplot.tiff", units="in", width=10, height=5, res=600)
  fig
  dev.off() 
  
  
  
  
  
  
  
  
  terminal$year <- as.numeric(terminal$year)
  
  p.terminal <- 
  ggplot(terminal, aes(year, terminal, colour = phase, linetype = treatment, shape = treatment, fill = phase)) +
    geom_jitter(size = 2, alpha=0.3, colour='black', width = 0.25) +
    #geom_point(shape= c(21, 24, 22), color="black", size=4, stroke=1.5) +
    geom_smooth(method = lm, se = FALSE, size = 2, alpha = 1) +
    scale_colour_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                        name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
    scale_linetype_manual(values = c("solid", "longdash", "dashed"),
                          name = "Drought treatment",
                          breaks= c("0", "60", "90"),
                          labels = c("Ambient", "Moderate", "Extreme")) +
    scale_shape_manual(values = c(16, 17, 15),
                       name = "Drought treatment",
                       breaks= c("0", "60", "90"),
                       labels = c("Ambient", "Moderate", "Extreme")) +
    scale_shape_manual(values = c(21, 24, 22),
                       name = "Drought treatment",
                       breaks= c("0", "60", "90"),
                       labels = c("Ambient", "Moderate", "Extreme")) +
    scale_fill_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                      name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
    theme_classic() +
    facet_wrap(~geography) +
    ylab('Length (mm)') +
    xlab('Year') +
    ggtitle('b)      Annual shoot') +
    theme(strip.text = element_text(size=15),
          strip.background =element_blank(),
          text = element_text(size=15),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13)) +
    guides(linetype = guide_legend(override.aes= list(color = "black", size = 2)),
           shape = guide_legend(override.aes = list(colour = 'red', size = 5))) +
    scale_x_continuous(breaks=c(2016, 2017, 2018, 2019),
                       labels=c("2016", "2017", "2018", "2019"))
  
  
  height$year <- as.numeric(height$year)
  
  p.height <- 
  ggplot(height, aes(year, log(height), colour = phase, linetype = treatment, shape = treatment, fill=phase)) +
    geom_jitter(size = 2, alpha=0.3, colour='black', width = 0.25) +
    #geom_point(shape= c(21, 24, 22), color="black", size=4, stroke=1.5) +
    geom_smooth(method = lm, se = FALSE, size = 2, alpha = 1) +
    scale_colour_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                        name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
    scale_linetype_manual(values = c("solid", "longdash", "dashed"),
                          name = "Drought treatment",
                          breaks= c("0", "60", "90"),
                          labels = c("Ambient", "Moderate", "Extreme")) +
    scale_shape_manual(values = c(16, 17, 15),
                       name = "Drought treatment",
                       breaks= c("0", "60", "90"),
                       labels = c("Ambient", "Moderate", "Extreme")) +
    scale_shape_manual(values = c(21, 24, 22),
                       name = "Drought treatment",
                       breaks= c("0", "60", "90"),
                       labels = c("Ambient", "Moderate", "Extreme")) +
    scale_fill_manual(values = c("Young" = "#E69F00", "Intermediate" = "#56B4E9", "Old" = "#009E73"),
                      name = "Successional stage", labels = c("Young", "Intermediate", "Old")) +
    theme_classic() +
    facet_wrap(~geography) +
    ylab('log height (mm)') +
    xlab('Year') +
    ggtitle('c)      Stand height') +
    theme(strip.text = element_text(size=15),
          strip.background =element_blank(),
          text = element_text(size=15),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13)) +
    guides(linetype = guide_legend(override.aes= list(color = "black", size = 2)),
           shape = guide_legend(override.aes = list(colour = 'red', size = 5))) +
    scale_x_continuous(breaks=c(2016, 2017, 2018, 2019),
                       labels=c("2016", "2017", "2018", "2019"))
  
  
library(patchwork)
  
j <-  p.stem / p.terminal / p.height

tiff("Figures/calluna_scatters.tiff", units="in", width=7, height=12, res=300)
j
dev.off() 

#######################


#visualising data

stem.p <- 
  ggplot(stem, aes(year, stem, fill = treatment)) +
  geom_boxplot() +
    scale_fill_manual(values = c("0" = "lightseagreen", "50" = "limegreen", "90" = "darkorange"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
    theme_classic()+
    xlab('Year') +
    ylab('Percentage change from last year') +
    ggtitle('Stem diameter') +
  facet_grid(phase~geography, scales = 'free')

height.p <- 
  ggplot(height, aes(year, height, fill = treatment)) +
  geom_boxplot()+
    scale_fill_manual(values = c("0" = "lightseagreen", "50" = "limegreen", "90" = "darkorange"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
    theme_classic()+
    xlab('Year') +
    ylab('Percentage change from last year') +
    ggtitle('Stand height') +
  facet_grid(phase~geography, scales = 'free')

length.p <- 
  ggplot(length, aes(year, length, fill = treatment)) +
  geom_boxplot()+
    scale_fill_manual(values = c("0" = "lightseagreen", "50" = "limegreen", "90" = "darkorange"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
    theme_classic()+
    xlab('Year') +
    ylab('Percentage change from last year') +
    ggtitle('Stretched length') +
  facet_grid(phase~geography, scales = 'free')

terminal.p <- 
  ggplot(terminal, aes(year, terminal, fill = treatment)) +
  geom_boxplot()+
    scale_fill_manual(values = c("0" = "lightseagreen", "50" = "limegreen", "90" = "darkorange"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
    theme_classic()+
    xlab('Year') +
    ylab('Hrowth since last year (mm)') +
    ggtitle('Shoot length') +
  facet_grid(phase~geography, scales = 'free')

#stem.p + height.p + length.p + terminal.p #patchwork


############# LINEAR MODELS WITH FIXED VARIABLES AND RANDOM FACTORS ###########

library(multcomp) # for post hoc
library(lmerTest) # for extended output on lmer to also include p-values

calluna$treatment <- as.factor(calluna$treatment)

# South


stem1 <- lmer(stem ~ treatment*phase + (1|year) + (1|plot), data = calluna %>% filter(geography == 'Southern Norway'), na.action=na.omit)

anova(stem1)


stem2 <- lmer(stem ~ phase + (1|year) + (1|plot), data = calluna %>% filter(geography == 'Southern Norway'), na.action=na.omit) # geography/  

anova(stem2)

anova(stem1,stem2) # use stem2

posthoc_stem <- glht(stem2, linfct = mcp(phase = 'Tukey'))
summary(posthoc_stem)


# should it be:

stem1 <- lmer(stem ~ treatment*phase*year + (1|plot/individual), data = calluna %>% filter(geography == 'Southern Norway'), na.action=na.omit)

anova(stem1)


terminal1 <- lmer(terminal ~ treatment*phase + (1|year) + (1|plot), data = calluna %>% filter(geography == 'Southern Norway')) # geography/  %>% 

anova(terminal1)

terminal2 <- lmer(terminal ~ phase + (1|year) + (1|plot), data = calluna %>% filter(geography == 'Southern Norway'))

anova(terminal2)

anova(terminal1, terminal2) #bruk terminal2

posthoc_term <- glht(terminal2, linfct = mcp(phase = 'Tukey'))
summary(posthoc_term)





height1 <- lmer(height ~ treatment*phase + (1|year) + (1|plot), data = calluna %>% filter(geography == 'Southern Norway'))

anova(height1)

height2 <- lmer(height ~ phase + (1|year) + (1|plot), data = calluna %>% filter(geography == 'Southern Norway'))

anova(height2)

anova(height1, height2) # bruk height2


posthoc_height <- glht(height2, linfct = mcp(phase = 'Tukey'))
summary(posthoc_height)





# North

calluna_north <- calluna %>% filter(geography == 'Northern Norway', !(site == 'BUO'))

terminal1 <- lmer(terminal ~ treatment*phase + (1|year) + (1|plot), data = calluna_north) 

anova(terminal1)

summary(terminal1)

terminal2 <- lmer(terminal ~ phase + (1|year) + (1|plot), data = calluna_north) 

anova(terminal2)

anova(terminal1, terminal2) #bruk terminal2

posthoc_term <- glht(terminal1, linfct = mcp(treatment = 'Tukey'))
summary(posthoc_term)



stem1 <- lmer(stem ~ treatment*phase + (1|year) + (1|plot), data = calluna_north) 

anova(stem1)


stem2 <- lmer(stem ~ treatment + (1|year) + (1|plot), data = calluna_north) 

anova(stem2)

anova(stem1,stem2) # use stem2

posthoc_stem <- glht(stem2, linfct = mcp(treatment = 'Tukey'))

summary(posthoc_stem)




height1 <- lmer(height ~ treatment*phase + (1|year) + (1|plot), data = calluna_north) 

anova(height1)

height2 <- lmer(height ~ phase + (1|year) + (1|plot), data = calluna_north) 

anova(height2)

anova(height1, height2) # bruk height2

posthoc_height <- glht(height2, linfct = mcp(phase = 'Tukey'))
summary(posthoc_height)



#### ggplot boxplot with asteriks #####

a <- 'a'
ab <- 'ab'
b <- 'b'
bc <- 'bc'
c <- 'c'
ac <- 'ac'

#south.stem <- 
  ggplot(calluna %>% filter(geography == 'Southern Norway'), aes(phase, stem)) +
  geom_boxplot(fill = 'tomato') +
  annotate("text",  x = 'Young', y = 14,label = a, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Intermediate', y = 12,label = b, size = 6,
             parse = TRUE)  +
  annotate("text",  x = 'Old', y = 16,label = c, size = 6,
             parse = TRUE)  +
  theme_classic() +
  ylab('Percentage change') +
  xlab('Post-fire stage') 

  
#south.terminal <- 
ggplot(calluna %>% filter(geography == 'Southern Norway'), aes(phase, terminal)) +
  geom_boxplot(fill = 'tomato') +
  annotate("text",  x = 'Young', y = 155,label = a, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Intermediate', y = 140,label = b, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Old', y = 115,label = b, size = 6,
           parse = TRUE)  +
  theme_classic() +
  ylab('Annual growth (mm)') +
  xlab('Post-fire stage') 

south.height <- 
  ggplot(calluna %>% filter(geography == 'Southern Norway'), aes(phase, height)) +
  geom_boxplot(fill = 'tomato') +
  annotate("text",  x = 'Young', y = 500,label = a, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Intermediate', y = 700,label = b, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Old', y = 720,label = b, size = 6,
           parse = TRUE)  +
  theme_classic() +
  ylab('Percentage change') +
  xlab('Post-fire stage') 



# north

north.stem <- 
  ggplot(calluna_north, aes(treatment, stem)) +
  geom_boxplot(fill = 'steelblue') +
  # annotate("text",  x = 'Young', y = 15,label = a, size = 6,
  #          parse = TRUE)  +
  # annotate("text",  x = 'Intermediate', y = 15,label = b, size = 6,
  #          parse = TRUE)  +
  # annotate("text",  x = 'Old', y = 18,label = c, size = 6,
  #          parse = TRUE)  +
  theme_classic() +
  ylab('Percentage change') +
  xlab('Drought treatment') +
  scale_x_discrete(breaks=c("0","50","90"),
                   labels=c("Ambient", "Moderate", "Extreme"))


north.terminal <- 
  ggplot(calluna_north, aes(phase, terminal)) +
  geom_boxplot(fill = 'steelblue') +
  # annotate("text",  x = 'Young', y = 155,label = a, size = 6,
  #          parse = TRUE)  +
  annotate("text",  x = 'Intermediate', y = 170,label = a, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Old', y = 100,label = b, size = 6,
           parse = TRUE)  +
  theme_classic() +
  ylab('Annual growth (mm)') +
  xlab('Post-fire stage') 

north.height <- 
  ggplot(calluna_north, aes(phase, height)) +
  geom_boxplot(fill = 'steelblue') +
  # annotate("text",  x = 'Young', y = 500,label = a, size = 6,
  #          parse = TRUE)  +
  annotate("text",  x = 'Intermediate', y = 300,label = a, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Old', y = 340,label = b, size = 6,
           parse = TRUE)  +
  theme_classic() +
  ylab('Percentage change') +
  xlab('Post-fire stage') 


library(patchwork)

(south.stem | south.height | south.terminal) / (north.stem | north.height | north.terminal)



############### Linear models ########

library(nlme)
library(patchwork)

a <- 'a'
ab <- 'ab'
b <- 'b'
bc <- 'bc'
c <- 'c'
ac <- 'ac'

# #use this because of year already being a fixed factor
# a <- lme(stem ~ treatment*phase*year.exp, 
#          random = (~1|plot/individual), 
#          data = data.south,
#          na.action=na.omit) 
# 
# # this would be fine otherwise (if year was something else)
# b <- lme(stem ~ treatment*phase*year.exp, 
#          random = list(individual = ~1, year = ~1), 
#          data = data.south,
#          correlation=corAR1(),
#          na.action=na.omit) 
# 
# # this would specify time
# c <- lme(stem ~ treatment*phase*year.exp, 
#          random = (~1|plot/individual), 
#          data = data.south,
#          correlation=corAR1(form = ~year|plot/individual),
#          na.action=na.omit) 
# 
# # and this do exactely the same thing as the one above
# d <- lme(stem ~ treatment*phase*year.exp, 
#          random = (~1|plot/individual), 
#          data = data.south,
#          correlation=corAR1(form = ~year),
#          na.action=na.omit) 
# 
# anova (a, b, c, d)

#output: 
# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# a     1 21 2894.739 2993.142 -1426.370                        
# b     2 22 2861.528 2964.617 -1408.764 1 vs 2 35.21149  <.0001
# c     3 22 2886.105 2989.194 -1421.052    


# STEM

data.south <-  calluna %>% filter(geography == 'Southern Norway' & stem > 0) #%>%
  mutate(year = as.numeric(year))


stem1 <- lme(stem ~ treatment*phase*year.exp, 
             random = (~1|plot/individual), 
             data = data.south ,
             na.action=na.omit, 
             method = 'ML')

anova(stem1)  # remove treatment:phase:year.exp 

stem2 <- lme(stem ~ treatment:phase + phase:year.exp + treatment:year.exp + treatment + year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.south ,
             na.action=na.omit, 
             method = 'ML')

anova(stem2) # remove treatment:phase


stem3 <- lme(stem ~ phase:year.exp + treatment:year.exp + treatment + year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.south ,
             na.action=na.omit, 
             method = 'ML') 

anova(stem3) # remove treatment:year.exp 

stem4 <- lme(stem ~ phase:year.exp + treatment + year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.south ,
             na.action=na.omit, 
             method = 'ML')

anova(stem4) # remove treatment

stem5 <- lme(stem ~ phase:year.exp +  year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.south ,
             na.action=na.omit, 
             method = 'ML')

anova(stem5) # remove treatment:year.exp 


anova(stem1, stem2, stem3, stem4, stem5)


posthoc_stem <- glht(stem5, linfct = mcp(phase = 'Tukey'))
summary(posthoc_stem)


stem.inter <- data.south %>% 
  #ungroup() %>% 
  #mutate(year = as.numeric(as.character(year))) %>%  # year must be continuous for drawing line
  group_by(year.exp, phase) %>% 
  summarise(sd = sd(stem, na.rm = TRUE), stem = mean(stem))


stem.south.inter <-
  ggplot(stem.inter, aes(year.exp, stem, ymin = stem-sd, ymax = stem+sd, col = phase)) + 
  #geom_boxplot()
  geom_pointrange(size = 0.8) +
  geom_errorbar(width=0.2, size = 0.8) +
    annotate("text", x = 0.3, y = 10,
             label = "Phase:Year***") +
    annotate("text", x = 0.3, y = 9.5,
             label = "Phase***") +
    annotate("text", x = 0.3, y = 9,
             label = "Year***") +
  #geom_path(data = bags, aes(x = months, y = rate, group=plot)) +
  #geom_smooth( se = TRUE, alpha = 0.1) + #span = 1, 
  geom_line() +
  theme_classic() +
  theme(strip.text = element_text(),
        strip.background =element_blank()) +
  scale_x_continuous(breaks=c(0, 1, 2, 3)) +
  scale_colour_manual(values = c("Young" = "gold3", "Intermediate" = "tomato", "Old" = "mediumorchid3"), name = "Successional phase") +
  #scale_colour_manual(values = c("0" = "steelblue2", "60" = "springgreen4", "90" = "orange2"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) #+
  #facet_grid(geography ~ phase) +
  xlab('Year since experiment start') +
  ylab('Stem diameter (mm)')
  
  
  tiff("Figures/stem_south_effect.tiff", units="in", width=6, height=5, res=300)
  stem.south.inter
  dev.off() 


# stem.phase <- data.south %>% 
#   #ungroup() %>% 
#   #mutate(year = as.numeric(as.character(year))) %>%  # year must be continuous for drawing line
#   group_by(phase) %>% 
#   summarise(sd = sd(stem, na.rm = TRUE), stem = mean(stem))
# 
# stem.south.phase <-
#   ggplot(stem.phase, aes(phase, stem, ymin = stem-sd, ymax = stem+sd, col = phase)) + 
#   #geom_boxplot()
#   geom_pointrange(size = 0.8) +
#   geom_errorbar(width=0.2, size = 0.8) +
#   annotate("text",  x = 'Young', y = 5.6,label = a, size = 6,
#            parse = TRUE)  +
#   annotate("text",  x = 'Intermediate', y = 6.5,label = b, size = 6,
#            parse = TRUE)  +
#   annotate("text",  x = 'Old', y = 9,label = c, size = 6,
#            parse = TRUE)  +
#   #geom_path(data = bags, aes(x = months, y = rate, group=plot)) +
#   #geom_smooth( se = TRUE, alpha = 0.1) + #span = 1, 
#   #geom_line() +
#   theme_classic() +
#   #theme(strip.text = element_text(),
#   #      strip.background =element_blank()) +
#   #scale_x_continuous(breaks=c(0, 1, 2, 3)) +
#   scale_colour_manual(values = c("Young" = "gold3", "Intermediate" = "tomato", "Old" = "mediumorchid3"), name = "Successional phase") +
#   #scale_colour_manual(values = c("0" = "steelblue2", "60" = "springgreen4", "90" = "orange2"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) #+
#   #facet_grid(geography ~ phase) +
#   xlab('Successional phase') +
#   ylab('Stem diameter (mm)')
# 
# stem.year <- data.south %>% 
#   #ungroup() %>% 
#   #mutate(year = as.numeric(as.character(year))) %>%  # year must be continuous for drawing line
#   group_by(year.exp) %>% 
#   summarise(sd = sd(stem, na.rm = TRUE), stem = mean(stem))
# 
# stem.south.year <-
#   ggplot(stem.year, aes(year.exp, stem, ymin = stem-sd, ymax = stem+sd)) + 
#   #geom_boxplot()
#   geom_pointrange(size = 0.8) +
#   geom_errorbar(width=0.2, size = 0.8) +
#   #geom_path(data = bags, aes(x = months, y = rate, group=plot)) +
#   #geom_smooth( se = TRUE, alpha = 0.1) + #span = 1, 
#   #geom_line() +
#   theme_classic() +
#   #theme(strip.text = element_text(),
#   #      strip.background =element_blank()) +
#   #scale_x_continuous(breaks=c(0, 1, 2, 3)) +
#   scale_colour_manual(values = c("Young" = "gold3", "Intermediate" = "tomato", "Old" = "mediumorchid3"), name = "Successional phase") +
#   #scale_colour_manual(values = c("0" = "steelblue2", "60" = "springgreen4", "90" = "orange2"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) #+
#   #facet_grid(geography ~ phase) +
#   xlab('Year since experiment start') +
#   ylab('Stem diameter (mm)')
# 
# (stem.south.inter | (stem.south.phase / stem.south.year))


# TERMINAL

data.south <-  calluna %>% filter(geography == 'Southern Norway' & terminal > 0) %>% 
  mutate(year = as.numeric(year))

terminal1 <- lme(terminal ~ treatment*phase*year.exp, 
                 random = (~1|plot/individual), 
                 data = data.south ,
                 na.action=na.omit, 
                 method = 'ML') 

anova(terminal1) 


term.inter <- data.south %>% 
  #ungroup() %>% 
  #mutate(year = as.numeric(as.character(year))) %>%  # year must be continuous for drawing line
  group_by(year.exp, phase, treatment) %>% 
  summarise(sd = sd(stem, na.rm = TRUE), stem = mean(stem))


term.south.inter <-
  ggplot(term.inter, aes(year.exp, stem, ymin = stem-sd, ymax = stem+sd, col = phase)) + 
  #geom_boxplot()
  geom_pointrange(size = 0.8) +
  geom_errorbar(width=0.2, size = 0.8) +
  annotate("text", x = 0.3, y = 10,
           label = "Phase:Year***") +
  annotate("text", x = 0.3, y = 9.5,
           label = "Phase***") +
  annotate("text", x = 0.3, y = 9,
           label = "Year***") +
  #geom_path(data = bags, aes(x = months, y = rate, group=plot)) +
  #geom_smooth( se = TRUE, alpha = 0.1) + #span = 1, 
  geom_line() +
  theme_classic() +
  theme(strip.text = element_text(),
        strip.background =element_blank()) +
  scale_x_continuous(breaks=c(0, 1, 2, 3)) +
  scale_colour_manual(values = c("Young" = "gold3", "Intermediate" = "tomato", "Old" = "mediumorchid3"), name = "Successional phase") +
  #scale_colour_manual(values = c("0" = "steelblue2", "60" = "springgreen4", "90" = "orange2"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) #+
  #facet_grid(geography ~ phase) +
  xlab('Year since experiment start') +
  ylab('Stem diameter (mm)') +
  facet_wrap(~treatment)


tiff("Figures/stem_south_effect.tiff", units="in", width=10, height=5, res=300)
term.south.inter
dev.off() 


# HEIGHT


data.south <-  calluna %>% filter(geography == 'Southern Norway' & height > 0) %>% 
  mutate(year = as.numeric(year))

height1 <- lme(height ~ treatment*phase*year.exp, 
               random = (~1|plot/individual), 
               data = data.south ,
               na.action=na.omit, 
               method = 'ML') 

anova(height1) #remove full interaction term



height2 <- lme(height ~ treatment:phase + phase:year.exp + treatment:year.exp + treatment + year.exp + phase, 
               random = (~1|plot/individual), 
               data = data.south ,
               na.action=na.omit, 
               method = 'ML') 

anova(height2) #remove treatment:phase


height3 <- lme(height ~ phase:year.exp + treatment:year.exp + treatment + year.exp + phase, 
               random = (~1|plot/individual), 
               data = data.south ,
               na.action=na.omit, 
               method = 'ML') 

anova(height3) #remove treatment:year.exp



height4 <- lme(height ~ phase:year.exp + treatment + year.exp + phase, 
               random = (~1|plot/individual), 
               data = data.south ,
               na.action=na.omit, 
               method = 'ML') 

anova(height4) #remove treatment



height5 <- lme(height ~ phase:year.exp + year.exp + phase, 
               random = (~1|plot/individual), 
               data = data.south ,
               na.action=na.omit, 
               method = 'ML') 

anova(height5) 

anova(height1, height2, height3, height4, height5)


###### NORD

data.north<-  calluna %>% filter(geography == 'Northern Norway' & stem > 0 & phase != 'Young') %>% 
  mutate(year = as.numeric(year))

# STEM

stem1 <- lme(stem ~ treatment*phase*year.exp, 
             random = (~1|plot/individual), 
             data = data.north ,
             na.action=na.omit, 
             method = 'ML') 


anova(stem1)  # remove treatment:phase:year.exp 

stem2 <- lme(stem ~ treatment:phase + phase:year.exp + treatment:year.exp + treatment + year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.north ,
             na.action=na.omit, 
             method = 'ML') 

anova(stem2) # remove treatment:phase


stem3 <- lme(stem ~ phase:year.exp + treatment:year.exp + treatment + year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.north ,
             na.action=na.omit, 
             method = 'ML') 

anova(stem3) # remove treatment:year.exp 

stem4 <- lme(stem ~ phase:year.exp + treatment + year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.north ,
             na.action=na.omit, 
             method = 'ML') 

anova(stem4) # remove treatment

stem5 <- lme(stem ~ phase:year.exp +  year.exp + phase, 
             random = (~1|plot/individual), 
             data = data.north ,
             na.action=na.omit, 
             method = 'ML') 

anova(stem5) # remove treatment:year.exp 

anova(stem1, stem2, stem3, stem4, stem5)




# TERMINAL


data.north<-  calluna %>% filter(geography == 'Northern Norway' & terminal > 0 & phase != 'Young') %>% 
  mutate(year = as.numeric(year))


terminal1 <- lme(terminal ~ treatment*phase*year.exp, 
                 random = (~1|plot/individual), 
                 data = data.north ,
                 na.action=na.omit, 
                 method = 'ML') 


anova(terminal1)  # remove treatment:phase:year.exp 



terminal2 <- lme(terminal ~ treatment:phase + phase:year.exp + treatment:year.exp + treatment + year.exp + phase,
                 random = (~1|plot/individual), 
                 data = data.north ,
                 na.action=na.omit, 
                 method = 'ML') 


anova(terminal2)  # remove treatment:phase



terminal3 <- lme(terminal ~ phase:year.exp + treatment:year.exp + treatment + year.exp + phase,
                 random = (~1|plot/individual), 
                 data = data.north ,
                 na.action=na.omit, 
                 method = 'ML') 

anova(terminal3)  # remove treatment:year.exp


terminal4 <- lme(terminal ~ phase:year.exp + treatment + year.exp + phase,
                 random = (~1|plot/individual), 
                 data = data.north ,
                 na.action=na.omit, 
                 method = 'ML') 


anova(terminal4)  

terminal5 <- lme(terminal ~ phase:year.exp + year.exp + phase,
                 random = (~1|plot/individual), 
                 data = data.north ,
                 na.action=na.omit, 
                 method = 'ML') 


anova(terminal5)

anova(terminal1, terminal2, terminal3, terminal4, terminal5)

# HEIGHT


data.north<-  calluna %>% filter(geography == 'Northern Norway' & height > 0 & phase != 'Young') %>% 
  mutate(year = as.numeric(year))


height1 <- lme(height ~ treatment*phase*year.exp, 
               random = (~1|plot/individual), 
               data = data.north ,
               na.action=na.omit, 
               method = 'ML') 

anova(height1) # remove treatment:phase:year.exp 



height2 <- lme(height ~ treatment:phase + phase:year.exp + treatment:year.exp + treatment + year.exp + phase,
               random = (~1|plot/individual), 
               data = data.north ,
               na.action=na.omit, 
               method = 'ML') 

anova(height2) # remove treatment:phase



height3 <- lme(height ~  phase:year.exp + treatment:year.exp + treatment + year.exp + phase,
               random = (~1|plot/individual), 
               data = data.north ,
               na.action=na.omit, 
               method = 'ML') 

anova(height3) 

height4 <- lme(height ~  phase:year.exp + treatment:year.exp + year.exp + phase,
               random = (~1|plot/individual), 
               data = data.north ,
               na.action=na.omit, 
               method = 'ML') 

anova(height3) 



anova(height1, height2, height3)


############## plot asterix plots ############

a <- 'a'
ab <- 'ab'
b <- 'b'
bc <- 'bc'
c <- 'c'
ac <- 'ac'

#south.stem <- 
ggplot(calluna %>% filter(geography == 'Southern Norway'), aes(phase, stem)) +
  geom_boxplot(fill = 'tomato') +
  annotate("text",  x = 'Young', y = 14,label = a, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Intermediate', y = 12,label = b, size = 6,
           parse = TRUE)  +
  annotate("text",  x = 'Old', y = 16,label = c, size = 6,
           parse = TRUE)  +
  theme_classic() +
  ylab('Percentage change') +
  xlab('Post-fire stage') 


##################### only terminal paper 2 #################

# per m2

# 53.3% C from IGN samples
meta <- read_xlsx('Data/shoots.xlsx') %>% 
  filter(site == 'LYG') %>% 
  mutate(sample = paste(plot.i.felt., replicate, sep = '', prefix = '.'))

CN <- read_xlsx('Data/CN14022018.xlsx') %>% 
  filter(str_detect(prøve, "L")) %>% 
  mutate(sample = str_sub(prøve, 2,7)) %>% 
  left_join(meta, by = 'sample') %>% 
  left_join(plotID, by = 'plot') %>% 
  mutate(length = as.numeric(length.shoot.mm),
         gmm = as.numeric(weight.shoot.mg) / length)

mean(CN$gmm, na.rm = TRUE)
#1.14 mg/mm

mean(as.numeric(CN$C))
# 52,79 %

1.14*0.5279
# ca 0.6 mg C per mm 


ggplot(CN, aes(phase, as.numeric(C), fill = treatment)) +
  geom_boxplot()

ggplot(CN, aes(as.numeric(C), length)) +
  geom_point()



calluna %>% 
  group_by()

terminal.fig <- 
  ggplot(calluna %>%  filter(site == 'LYG' & year == '2019'), aes(phase, terminal * 0.0006, fill = treatment)) +
  geom_boxplot() +
  scale_fill_brewer(palette = 'Reds',
                    name = 'Drought intensity',
                    labels = c('Ambient', 'Moderate', 'Extreme')) +
  theme_classic() +
  #ylab(bquote('Carbon accumulation'~(gm^-2~yr^-1))) +
  ylab(bquote('Carbon accumulation (g per shoot)')) +
  xlab ('Successional phase') +
  ggtitle('Calluna shoot')
