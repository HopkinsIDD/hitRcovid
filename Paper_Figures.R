## Load R packages
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(cowplot)
library(reshape2)

##########################
#Figure 2
##########################

#Import hit-covid dataset
long_data=read.csv('hit-covid/data/hit-covid-longdata.csv')

#Remove "no updates" entries and USA county-level data
long_data=long_data[which(long_data$update=='Update' &
                            is.na(long_data$usa_county)),]

#Update the country names
long_data$country_name=as.character(long_data$country_name)
long_data[which(long_data$country_name=='Hong Kong'),]$country_name='China'
long_data[which(long_data$country_name=='Korea Republic of'),]$country_name='South Korea'
long_data[which(long_data$country_name=='Bolivia Plurinational State of'),]$country_name='Bolivia'
long_data[which(long_data$country_name=='Congo Democratic Republic of the'),]$country_name='Democratic Republic of the Congo'
long_data[which(long_data$country_name=='C^ote dIvoire'),]$country_name='Ivory Coast'
long_data[which(long_data$country_name=='Czechia'),]$country_name='Czech Republic'
long_data[which(long_data$country_name=='Iran Islamic Republic of'),]$country_name='Iran'
long_data[which(long_data$country_name=='Lao Peoples Democratic Republic'),]$country_name='Laos'
long_data[which(long_data$country_name=='Russian Federation'),]$country_name='Russia'
long_data[which(long_data$country_name=='Saint Vincent and the Grenadines'),]$country_name='Saint Vincent'
long_data[which(long_data$country_name=='South Georgia and the South Sandwich Islands'),]$country_name='South Georgia'
long_data[which(long_data$country_name=='Tanzania United Republic of'),]$country_name='Tanzania'
long_data[which(long_data$country_name=='United Kingdom of Great Britain and Northern Ireland'),]$country_name='UK'
long_data[which(long_data$country_name=='United States of America'),]$country_name='USA'
long_data[which(long_data$country_name=='Venezuela Bolivarian Republic of'),]$country_name='Venezuela'
long_data[which(long_data$country_name=='Virgin Islands US'),]$country_name='Virgin Islands'
long_data[which(long_data$country_name=='GuineaBissau'),]$country_name='Guinea-Bissau'
long_data[which(long_data$country_name=='Eswatini'),]$country_name='Swaziland'
long_data[which(long_data$country_name=='Congo'),]$country_name='Republic of Congo'

#Get percentage of interventions reported at national level by country
mapping_data=long_data %>%
  group_by(country_name,national_entry) %>%
  summarise(intervention_group=n()) %>%
  dcast(country_name~national_entry, fun.aggregate = sum) %>%
  group_by(country_name) %>%
  summarise(percentage_of_national_interventions=Yes/(No+Yes)*100)

#Load world map
world <- map_data("world")

#Remove Antarctica from the world map
world=world[which(
  !world$region=='Antarctica'
),]

#Merge world map with mapping dataset
world_joined <- left_join(world, mapping_data, by = c('region' = 'country_name'))

#Label and create color schemes
world_joined$color_group='No data'
world_joined[which(world_joined$percentage_of_national_interventions<=10),]$color_group='0-10 (primarily sub-national)'
world_joined[which(world_joined$percentage_of_national_interventions<=20 &
                     world_joined$percentage_of_national_interventions>10),]$color_group='10-20'
world_joined[which(world_joined$percentage_of_national_interventions<=50 &
                     world_joined$percentage_of_national_interventions>20),]$color_group='20-50'
world_joined[which(world_joined$percentage_of_national_interventions>50),]$color_group='50-100 (primarily national)'
world_joined$color_group=
  ordered(world_joined$color_group,
          levels= c('0-10 (primarily sub-national)',
                    '10-20',
                    '20-50',
                    '50-100',
                    '50-100 (primarily national)'))
custom.col <- c("#F4EDCA",
                "#FFDB6D", 
                "#C4961A",  
                "#D16103", 
                "#999999")

#Create ggplot and export figure2
tiff("figure2.tiff", units="in", width=16, height=5, res=300)

ggplot(data = world_joined, 
       aes(x = long, y = lat,group= group,fill=color_group)) +
  scale_fill_manual(values = custom.col)+
  geom_polygon(data = world[world$order,], 
               aes(x = long, y = lat,group= group),
               lwd=0.1,fill=NA,colour='black',
               inherit.aes = F)+
  geom_polygon(colour='black')+
  theme(panel.background = element_rect(fill = "white")
        ,plot.background = element_rect(fill = "white")
        ,panel.grid = element_blank()
        ,axis.text = element_blank()
        ,axis.title = element_blank())+
  labs(fill = "Percentage of Interventions Recorded \nat National Level (%)")+
  theme(legend.title.align = 0.5)+
  theme(legend.text=element_text(size=12),
        legend.title = element_text(size=14))
dev.off()

##########################
#Figure 3
##########################

#Bring intervention list
intlist<-read_csv("hit-covid/data/intlist.csv") %>%
  rename(intervention_specific_clean=intervention_name)

int_types <- c("Restrictions of travel and movement",
               "Social and physical distancing measures",
               "Surveillance and response measures" ,
               "Other measures")

#Read in data for new zealand and india
fullf3 <- read.csv('hit-covid/data/hit-covid-longdata.csv') %>%
  mutate(date_of_update=as.Date(as.character(date_of_update))) %>%
  filter(country_name=="India" | country_name=="New Zealand") %>%
  select(country_name,
         national_entry,
         date_of_update,
         intervention_specific_clean,
         status_simp
  ) %>%
  left_join(intlist) %>%
  mutate(status_simp=factor(status_simp,
                            levels = c("Strongly Implemented",
                                       "Partially Implemented",
                                       "Implementation Suspended"
                            ))) %>%
  mutate(intervention_type=factor(intervention_type,levels=int_types))%>%
  mutate(intervention_type =
           recode(intervention_type,
                  "Restrictions of travel and movement" = "Restrictions\nof travel and\nmovement",
                  "Other measures" = "Other \nmeasures",
                  "Social and physical distancing measures"="Social and\nphysical\ndistancing\nmeasures",
                  "Surveillance and response measures"="Surveillance\nand response\nmeasures"
           ))  %>%
  
  mutate(country_name=factor(as.character(country_name),
                             levels=c("New Zealand","India")))%>%
  filter(!is.na(intervention_simp)) %>%
  mutate(`Geographic Level`=factor(national_entry,
                                   labels=c("Sub-national","National"))) %>%
  arrange(country_name) %>%
  filter(!(country_name=="New Zealand" & intervention_simp=="Universal facemask policies"))


#Create dataframe for first case
firstcase <- fullf3 %>% distinct(country_name) %>%
  mutate(date_of_update=as.Date(c("2020-02-28","2020-01-30")))

#Create ggplot
f3 <- fullf3 %>% ggplot(aes(x=date_of_update,y=intervention_simp))+
  geom_jitter(aes(col=status_simp,
                  shape=`Geographic Level`)
              ,alpha=0.15,size=2,
              width=0,
              height=0.3
  )+
  facet_grid(intervention_type~country_name,
             scales = "free",
             space="free"
  )+
  theme_bw()+
  theme(strip.text.y = element_text(#size = 5, 
    angle = 0 ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    legend.position="bottom",
    legend.box="vertical",
    legend.margin=margin()
  )+
  ylab("Intervention \nType")+
  xlab("Date of Implementation")+
  scale_color_manual(name="Status",values = c("red","darkorange","black"))+
  geom_vline(
    data=firstcase,
    aes(xintercept = date_of_update),
    lty=2)+
  scale_x_date(
    breaks = as.Date(c("2020-01-01",
                       "2020-02-01",
                       "2020-03-01",
                       "2020-04-01",
                       "2020-05-01",
                       "2020-06-01",
                       "2020-07-01")),
    date_labels="%b",
    limits=as.Date(
      c("2020-01-01","2020-07-01")))

#Produce final plot
ggsave(plot=f3,"figure3.jpg",width=7.2,height=7.2)