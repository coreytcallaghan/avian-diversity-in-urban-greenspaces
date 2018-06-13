## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")

### Packages
library(dplyr)
library(ggplot2)
library(readr)
library(ggpubr)
library(patchwork)

### Reading in study sites
Final_study_sites <- read_csv("Data/Final_study_sites/Final_study_sites.csv")

# First, load the RData file in
load("Data/modelling_data.RData")

### Read in bird data
load("Data/Bird_data/bird_data.RData")

## collapse all sites to polygon ids
analysis_all <- predictor_variables
analysis_all$species_richness <- as.numeric(analysis_all$species_richness)
analysis_all <- na.exclude(analysis_all)

### Preliminary EDA for RESULTS section of paper
## Number of sites
length(unique(analysis_all$Polygon_id))

## Number of cities
length(unique(analysis_all$Urban_area))

## comparing richness and checklists among dataset 1
location_richness <- Final_study_sites %>%
  dplyr::select(Polygon_id, LOCALITY_ID) %>%
  right_join(., analysis_all, by="Polygon_id") %>%
  inner_join(., bird_data, by="LOCALITY_ID") %>%
  group_by(Polygon_id) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  summarise(species_richness = length(unique(COMMON_NAME)),
            checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

figure_1 <- Final_study_sites %>%
  dplyr::select(W, L, R, Urban_area, Polygon_id) %>%
  mutate(L=L+R) %>%
  distinct(Polygon_id, .keep_all=TRUE) %>%
  right_join(., location_richness, by="Polygon_id") %>%
  group_by(Urban_area) %>%
  summarise(Waterbirds = mean(W), Landbirds = mean(L), Richness = mean(species_richness),
            sd_w = sd(W), sd_L = sd(L), sd_richness = sd(species_richness), 
            sample_size=length(Urban_area)) %>%
  arrange(desc(Richness))


fig1_richness <- 
  ggplot(figure_1, aes(x=reorder(Urban_area, Richness), y=Richness))+
  geom_point()+
  geom_errorbar(aes(x=reorder(Urban_area, Richness), 
                    ymin=Richness-sd_richness, ymax=Richness+sd_richness))+
  theme_bw()+
  ylab("Total species richness")+
  xlab("City")+
  theme(axis.text.x=element_text(size=5, color="black"))+
  theme(axis.text.y=element_text(size=5, color="black"))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  coord_flip()


fig1_waterbird_richness <-
  ggplot(figure_1, aes(x=reorder(Urban_area, Waterbirds), y=Waterbirds))+
  geom_point()+
  geom_errorbar(aes(x=reorder(Urban_area, Waterbirds), 
                    ymin=Waterbirds-sd_w, ymax=Waterbirds+sd_w))+
  theme_bw()+
  ylab("Waterbird richness")+
  xlab("City")+
  theme(axis.text.x=element_text(size=5, color="black"))+
  theme(axis.text.y=element_text(size=5, color="black"))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  coord_flip()

fig1_landbird_richness <-
  ggplot(figure_1, aes(x=reorder(Urban_area, Landbirds), y=Landbirds))+
  geom_point()+
  geom_errorbar(aes(x=reorder(Urban_area, Landbirds), 
                    ymin=Landbirds-sd_L, ymax=Landbirds+sd_L))+
  theme_bw()+
  ylab("Landbird richness")+
  xlab("City")+
  theme(axis.text.x=element_text(size=5, color="black"))+
  theme(axis.text.y=element_text(size=5, color="black"))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.title.x=element_text(size=12))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  coord_flip()


ggarrange(fig1_richness, fig1_landbird_richness, fig1_waterbird_richness, widths=c(4,4,4))


plot <- fig1_richness + fig1_landbird_richness + fig1_waterbird_richness + plot_layout(ncol = 1)

fig1_landbird_richness + fig1_waterbird_richness - fig1_richness + plot_layout(ncol = 1)


ggsave(filename="H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Submissions/Ecosphere Attempt/Final Submission/Figure1.tiff",
       dpi=300, width=6, height=7, units="in")












