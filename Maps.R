##MAPS
# Beillouin Damien
# DataPaper: A global database of land management, land use change 
#and climate change effects on soil organic carbon
# in Scientific Data

#### I/ Initialisation ####

#### library load ####

library(magrittr)    # For the pipes
library(plyr)        # Data management
library(dplyr)       # Data management
library(readxl)      # to read Excel files

library(ggplot2)     # for graphics
library(ggmap)       # maps in ggplot
library(RColorBrewer)# for beautiful colors
library(ggpubr)      # to make beautiful plots
library(scales)      # Graphical scales map data to aesthetics

library(hchinamap)  # for the map of China
library(geobr)      # for the map of Brazil
library(ozmaps)     # For the map of Australia
library(rnaturalearth) # For the world map

library(sf)         # for spatial vector data
library(stringi)    # Character String Processing Facilities
library(maptools)   # for manipulating goegraphical data
library(rgdal)      # connection to the Data Abstraction Library
library(raster)     # Reading, writing, manipulating, analyzing and modeling of spatial data
library(rgeos)      # Interface to Geometry Engine
library(Cairo)      # R graphics device using cairographics
library(mapproj)    # map projections


#### Data Load#####
##
##Adjust the path### ## or set the WD to the right location###

PS         <- read_excel("Data_Base_C_Sol_2021-09-14.xlsx", sheet="Primary_studies") %>% 
                   dplyr::mutate(Country = tolower(trimws(Primarystudies_Country)),
                          Region  = tolower(trimws(Primarystudies_Region))) %>%
                   dplyr::select(DOI, Country, Region) %>% 
                   dplyr::mutate(Country = as.character(Country))

# If there is several countries for a primary studies
s           <- base::strsplit(PS$Country, split = ",")
PS          <- base::data.frame(DOI = rep(PS$DOI, sapply(s, length)),
                      region = rep(PS$Region, sapply(s, length)),
                      Country = unlist(s))
PS$Country <-base::trimws(PS$Country)

# If there is several region for a primary studies
s          <- base::strsplit(PS$region, split = ",")
PS         <- data.frame(DOI = rep(PS$DOI, sapply(s, length)),
                      Country = rep(PS$Country, sapply(s, length)),
                      region = unlist(s))
PS$region <-base::trimws(PS$region)



###II/ Create the maps #####

####Map of the USA#####

# filter the data for USA
USA <-PS %>% dplyr::filter(Country=="usa")  %>%
             dplyr::group_by(region)        %>%
             dplyr::summarise(Unique_Elements = n_distinct(DOI))
USA$FREQ<- (USA$Unique_Elements/ sum(USA$Unique_Elements))*100

# Load the map and join the files
USA_map  <- map_data("state")
USA      <- dplyr::left_join(USA_map, USA, by = "region")

# Create the map
ggplot(USA, aes(long, lat, group = group))+
  geom_polygon(aes(fill = cut(FREQ,breaks= c(0,2, 4, 6, 8,10,15,20), na.rm=TRUE)), 
               color = "gray40")+
  coord_sf()+
  scale_fill_manual(values=c('#f2f0f7',"#dadaeb",'#bcbddc','#9e9ac8','#756bb1','#54278f'), na.value= "white") +
  labs(fill = "# Primary studies")  +
  theme_pubr()+
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))+
  scale_x_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180)) +
  scale_y_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180))

### Map of China ####

# filter the data for China
China<-PS %>% filter(Country=="china") %>%
  group_by(region) %>%
  dplyr::summarise(Unique_Elements = n_distinct(DOI))
China$FREQ<- (China$Unique_Elements/ sum(China$Unique_Elements))*100

# GIS file for China, stores the feature geometry
# to download from https://github.com/GuangchuangYu/chinamap/blob/master/inst/extdata/china/bou2_4p.shp
cnmap     <- readOGR("~/Downloads/bou2_4p.shp")
cnmap     <- shapefile("~/Downloads/bou2_4p.shp")

prov_cn   <- unique(cnmap$NAME)
prov_cn   <- prov_cn[!is.na(prov_cn)]
prov_en   <- c("heilongjiang", "inner mongolia", "xinjiang", "jilin",
             "liaoning", "gansu", "hebei", "beijing", "shanxi",
             "tianjin", "shaanxi", "ningxia", "qinghai", "shandong",
             "tibet", "henan", "jiangsu", "anhui", "sichuan", "hubei",
             "chongqing", "shanghai", "zhejiang", "hunan", "jiangxi",
             "yunnan", "guizhou", "fujian", "guangxi", "taiwan", 
             "guangdong", "hongkong", "hainan")

prov     <- data.frame(prov_cn, prov_en)
id_prov  <- cnmap@data %>%
           dplyr::mutate(prov_en = sapply(NAME, 
                          function(x)
                            prov_en[which(prov_cn == x)])) %>%
           dplyr::mutate(prov_cn = as.character(NAME),
           prov_en = as.character(prov_en))                           

id       <- rownames(id_prov)
id_prov  <- cbind(id=id, id_prov)
id_prov  <- id_prov %>% dplyr::select (id, prov_cn, prov_en)

cnmapdf  <- plyr::join(fortify(cnmap), id_prov, by = "id")

names(China)[1]<-'prov_en'

map2df <- cnmapdf %>%
  plyr::join(China, by = "prov_en") %>%
  mutate(FREQ = as.numeric(FREQ))
cbbPalette <- c('#efcdcd', '#e9b6b3', '#e3a099', '#dd8980', '#d77366', '#d15d4c', '#bf1a00')

map2df %>%
  ggplot(aes(x = long, y = lat, group = group, fill=cut(FREQ,breaks= c(0,2, 4, 6, 8,10,15,20), na.rm=TRUE))) +
  geom_polygon(color = "black", size=0.15) +
  labs(fill='Number of PS') +
  theme_pubr()+
  scale_fill_manual(values=c('#f2f0f7',"#dadaeb",'#bcbddc','#9e9ac8','#756bb1','#54278f'), na.value= "white")+
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))+
  scale_x_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180)) +
  scale_y_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180))

#### map for India #####

# filter the data for India
India<-PS %>% filter(Country=="india") %>%
          dplyr::group_by(region) %>%
          dplyr::summarise(Unique_Elements = n_distinct(DOI))
India$FREQ<- India$Unique_Elements/sum(India$Unique_Elements)*100

# Load the map and join the files
states.shp <- rgdal::readOGR("~/Downloads/IND_adm/IND_adm1.shp")
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()
states.shp.f <- fortify(states.shp, region = "ID_1")


num.states  <-length(states.shp$NAME_1)
mydata      <-data.frame(region=tolower(states.shp$NAME_1),
                   id=states.shp$ID_1)
mydata     <-left_join(mydata, India)


merge.shp.coef <-merge(states.shp.f, mydata, by="id", all.x=TRUE)
final.plot     <-merge.shp.coef[order(merge.shp.coef$order), ] 

ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill= cut(FREQ,breaks= c(0,2, 4, 6, 8,10,15,20), na.rm=TRUE)), 
               color = "black", size = 0.25) + 
  coord_map()+ theme_pubr()+
  scale_fill_manual(values=c('#f2f0f7',"#dadaeb",'#bcbddc','#9e9ac8','#756bb1','#54278f'), na.value= "white")+
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))+
  scale_x_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180)) +
  scale_y_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180))

### Map for Brazil #####

# filter the data for Brazil
Brazil<-PS %>% filter(Country=="brazil") %>%
  group_by(region) %>%
  dplyr::summarise(Unique_Elements = n_distinct(DOI))
Brazil$FREQ<- (Brazil$Unique_Elements/ sum(Brazil$Unique_Elements))*100

# Load the map and join the files
states            <- read_state()
states <- read_state(code_state="all", year=2010)

states$name_state <-stri_trans_general(states$name_state,"Latin-ASCII")
names(Brazil)[1]<- 'name_state'
states <-left_join(states,Brazil)

# plot the map
ggplot() + 
  geom_sf(data=states, aes(fill=cut(FREQ,breaks= c(0,2, 4, 6, 8,10,15,20), na.rm=TRUE)), color="black",size=0.2,
          size=.15, show.legend = TRUE) +
  labs(subtitle="States", size=8) +
  #theme_minimal() +
  #no_axis+
  theme_pubr()+
  scale_fill_manual(values=c('#f2f0f7',"#dadaeb",'#bcbddc','#9e9ac8','#756bb1','#54278f'), na.value= "white") +
  labs(fill = "# Primary studies")  +
  #theme_pubr()+
  theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))+
  scale_x_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180)) +
  scale_y_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180))


####Map for australia ####

# filter the data for australia
Aus<-PS %>% filter(Country=="australia") %>%
  group_by(region) %>%
  dplyr::summarise(Unique_Elements = n_distinct(DOI))
Aus$FREQ<- (Aus$Unique_Elements/ sum(Aus$Unique_Elements))*100

# Load the map and join the files
oz_states <- ozmaps::ozmap_states %>% filter(NAME != "Other Territories")
oz_states$NAME<- tolower(oz_states$NAME)
names(oz_states)[1]<- "region"
oz_states<-left_join(oz_states,Aus)

# plot the map
ggplot() + 
  geom_sf(data = oz_states,color= 'black',size=0.2,
          mapping = aes(fill = cut(FREQ,breaks= c(0,2, 4, 6, 8,10,15,40), na.rm=TRUE)),
          show.legend = TRUE) +
  #geom_sf(data = oz_votes, fill = NA) + 
  coord_sf()+
  scale_fill_manual(values=c('#f2f0f7',"#dadaeb",'#bcbddc','#9e9ac8','#756bb1','#54278f'), na.value= "white") +
  labs(fill = "# Primary studies")  +
  theme_pubr()+
  theme_bw()+theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))+
  scale_x_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180)) +
  scale_y_continuous(breaks = c(-180, -160,-140,-120, -100,-80,  -60,-40, -20,0, 20 ,40, 60,80,100,120, 140 ,160,  180))


#### world #####

# Count the data
COUNT<- PS %>%
  group_by(Country) %>%
  dplyr:: summarize(n=length(unique(DOI))) %>% 
  mutate(Country =tolower(Country))

# Calculate the proportion of data
names(COUNT)[1]<-"geounit"
COUNT$PROP<- COUNT$n/ sum(COUNT$n)

# oLoad the map
world         <- ne_countries(scale = "medium", returnclass = "sf")
world$geounit <-tolower(world$geounit)

# change the names of some countries
world$geounit<-revalue(world$geounit,
                       c("united kingdom"="uk",
                         "united states of america"="usa"))

# merge map and data
world2<-merge(world,COUNT, all=TRUE)

# Create a discrete variable
world2$cut_n<- cut (world2$n, breaks= c(0,50, 100, 200, 400,800, 8000))
brks_scale <- levels(world2$cut_n)

# plot the map
ggplot(world2) +
  geom_sf(aes(fill = cut_n),size=0.2, color="gray20") +     
  #scale_fill_gradientn(colours = c('#461863','#404E88','#2A8A8C','#7FD157','#F9E53F') )+
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'number of experiments'                    # on ajuste les titres, légendes, ....
       ,color = '.'
       ,title = ''
       ,x = NULL
       ,y = NULL) +
theme_bw()+theme(panel.grid.major = element_line(colour = 'grey50', size = 0.3, linetype = 3))+
theme(legend.position="bottom")+
  scale_fill_manual(values=c('#f6e8c3',"#dfc283",'#ba966c','#e5f5e0','#a1d99b','#31a354'), na.value= "white")

