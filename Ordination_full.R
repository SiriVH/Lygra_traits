library(tidyverse)
library(vegan)
#library(ggplot2)
library(readxl)
#library(data)

# Need rutedata sheet 'frequency' for species abundances. Call it 'veg'
# Need rutedata sheet 'names' for taxa. Call it 'taxa'
# Need drought.plots for metadata on plots. Call it 'PlotID'

veg <- readRDS('cleandata/community.rds') #community data cleaned
PlotID <- read_xlsx('Data/drought.plots.xlsx')
taxa <- read_xlsx('Data/DE.1_Community_2020.xlsx', sheet = 'names')

prep <- veg %>%
 # filter (site == 'LYG') %>%  
  filter(experiment == 'drought' | experiment == 'Drought') %>% 
  replace (. == '.+' | . == 'plas', 1) %>%              # Replace <1 abundances with 1
  dplyr::rename(Latin_name = species) %>%                    
  left_join (taxa, by='Latin_name') %>% 
  filter (!(Group=='bryophyte' | Group=='lichen')) %>% 
  dplyr::rename (species = Latin_name) %>% 
  #select (year, plot, species, cover) %>% 
  group_by(site, year, plot, species) %>% 
  summarise_at(.vars = 'cover', sum, na.rm = TRUE) %>% 
  spread (species, cover, fill = NA, convert = FALSE, drop = TRUE, sep = NULL) %>% 
  #mutate_each (funs(as.numeric), 4:53) %>% 
  replace (is.na(.), 0) %>% 
  ungroup(year, site, plot)

com <- metaMDS(prep %>%
                 dplyr::select (-year, -plot, -site))

env <- prep %>% 
  dplyr::select (year, plot) %>% 
  mutate (year = as.character(year)) %>% 
  left_join (PlotID, by='plot') %>% 
  mutate(phase = fct_recode(phase, "Young" = 'pioneer', "Intermediate" = 'building', 'Old' = 'mature'),
         geography = fct_recode(geography, "North" = 'north', "South" = 'south'))

data.scores <- as.data.frame(scores(com)) %>%   #Using the scores function from vegan to extract the site scores and convert to a data.frame
                 mutate(plot = prep$plot,
                        year = as.character(prep$year))%>% 
                 left_join(env, by=c('plot', 'year')) 


data.scores$phase<-factor(data.scores$phase, levels=c("Young", "Intermediate", "Old"))
levels(data.scores$phase) <- c("Young", "Intermediate", "Old") 
  
species.scores <- as.data.frame(scores(com, "species")) %>% #Using the scores function from vegan to extract the species scores and convert to a data.frame
                  mutate(species = rownames(.))
  
reddata <- data.scores %>% 
  mutate(region = geography) %>% 
  dplyr::select(-phase, -geography) 

reddata1 <- data.scores %>% 
 filter(geography == 'North') %>% 
  dplyr::select(-phase, -geography) 

reddata2 <- data.scores %>% 
  filter(geography == 'South') %>% 
  dplyr::select(-phase, -geography)


#p <-
ggplot(data=data.scores,aes(x=NMDS1,y=NMDS2, shape= year, colour= treatment ),alpha=0.5 ) + 
  geom_point(data= reddata1, colour = 'gray75', size=5) +
  geom_point(data= reddata2, colour = 'gray90', size=5) +
  geom_point(size = 6) +
  #geom_path(data=reddata,aes(x=NMDS1,y=NMDS2, group=plot)) +
  #scale_color_brewer(palette="Set1") +
  #scale_colour_manual(values=c("0" = "#0072B2", "60" = "#F0E442", "90" = "#D55E00"), name = "Drought frequency", labels = c("Ambient", "Moderate", "Extreme")) +
  scale_colour_manual(values = c("0" = 'darkblue', "60" = 'deepskyblue3', "90" = "mediumturquoise"), 
                      name = "Drought intensity", labels = c("Ambient", "Moderate", "Extreme")) +
  scale_shape_manual(values=c(19, 19, 19, 19, 18))+
  coord_equal() +
  geom_path(data=data.scores,aes(x=NMDS1,y=NMDS2, group=plot)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        #legend.box.just = 'bottom',
        legend.margin=unit(0.5, "cm"),
        strip.text = element_text(size = 30),
        strip.background =element_blank(),
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 20)
        )+
  labs(shape="Year", colour="Treatment") +
  facet_grid(geography~phase) 

tiff("Figures/ordination_shades.tiff", units="px", width = 6000, height = 4000, res=300)
p
dev.off() 


# ANlysis of variance

# Species-site matrix

com2 <- prep %>% 
  dplyr::select(- site, -year, -plot)


# Environmental meata data

env2 <- env %>% 
  mutate(site2 = paste(geography, phase, sep='_')) %>% 
  mutate(site3 = fct_relevel(site2, levels = c('North_Young', 'North_Intermediate', 'North_Old', 'South_Young', 'South_Intermediate', 'South_Old')))


#anosim 

ano <- anosim(com2, env2$site3, permutations = 999, distance = "bray", strata = NULL,
       parallel = getOption("mc.cores"))


#Plot

plot(x=ano$class.vec, y=ano$dis.rank, xlab = 'Sites', ylab = 'Dissimilarity rank', ylim = c(0,40000))


tiff('Figures/anosim.tif', units = 'px', height = 1500, width = 3500, res = 300)
plot(x=ano$class.vec, y=ano$dis.rank, xlab = 'Sites', ylab = 'Dissimilarity rank', ylim = c(0,40000))
dev.off()


# same with adonis

a2 <- adonis2(com2~env2$site2)


# check pair-wise dissimilarities

permutest(betadisper(dist(com2), env2$site2), pairwise = TRUE)

