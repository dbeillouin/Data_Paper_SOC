## Descriptive statistics: Number of effect sizes (left) and number of meta-analyses (right) available in the database per type of intervention and land use.

# Beillouin Damien
# DataPaper: A global database of land management, land use change 
#and climate change effects on soil organic carbon
# in Scientific Data

#### I/ Initiailisation####

#### Load Packages####
library("DIZutils") # for data Management
library(tidyr)      # For dataManagement
library(cowplot)    # for plots 

#### load Data####
DATA <- read.csv("data_pour_Table1.csv", sep=";")
DATA %<>% tidyr::pivot_longer(cols=c('MA', 'ES', 'Data'))

DATA$int  <- base::trimws(DATA$int)
DATA$int  <- DIZutils::firstup(DATA$int)

##II/ analysis #######

## Fisrt type of plot####
PLOT1<-ggplot(DATA %>% filter(!LU=="all",name=="MA", outcome=="all"))+
  geom_bar(aes(x= reorder(int, ORDER), y= value, group=LU, fill= LU), stat="identity",
                       position = "dodge2")+ theme_pubr()+ coord_flip()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  labs(x="", y=" Number of meta-analyses")+
  scale_fill_manual(values = c("#E8D399","#C8E8CC", "#99ABE8", "gray60","#E86184"))   +
  theme(legend.position = "none")

PLOT2<- ggplot()+
  geom_bar(data= DATA %>% filter(!LU=="all",name=="ES", outcome=="all"),
                aes(x= reorder(int,ORDER),  y= value, group=LU, fill= LU),alpha=0.5, stat="identity",
           position = "dodge2")+
  geom_bar(data= DATA %>% filter(!LU=="all",name=="ES", outcome=="C"),
           aes(x= reorder(int,ORDER),  y= value, group=LU, fill= LU), stat="identity",
           position = "dodge2")+
  theme_pubr()+ coord_flip()+
  labs(x="", y=" Number of effect_sizes")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values = c("#E8D399","#C8E8CC", "#99ABE8", "gray60","#E86184"))  +
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

PLOT3<-ggplot()+
  geom_bar(data= DATA %>% filter(!LU=="all",name=="Data", outcome=="all"),
           aes(x= reorder(int,ORDER),  y= value, group=LU, fill= LU),alpha=0.5, stat="identity",
           position = "dodge2")+
  geom_bar(data= DATA %>% filter(!LU=="all",name=="Data", outcome=="C"),
           aes(x= reorder(int,ORDER), y= value, group=LU, fill= LU), stat="identity",
           position = "dodge2")+
  theme_pubr()+ coord_flip()+
  labs(x="", y=" Number of paired-data")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values = c("#E8D399","#C8E8CC", "#99ABE8", "gray60","#E86184"))  +
  theme(legend.position = "none")+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plot_grid(PLOT1, PLOT2, PLOT3, ncol=3, rel_widths = c(2,1,1))


## Second type of plot####

PLOTA<-ggplot()+
  geom_hline(aes(yintercept=0), linetype=1, col='gray50', size=0.3)+
  geom_bar(data= DATA %>% filter(!LU=="all",name %in% c("MA"), outcome =="all"),
           aes(x= reorder(int, ORDER), 
               y= value, fill= LU),size=0.2,stat="identity",
           position = "dodge2", color="black")+
  theme_pubr()+ coord_flip()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  labs(x="", y=" Number of meta-analyses")+
  scale_fill_manual(values = c("#E8D399","#C8E8CC", "#99ABE8", "gray60","#E86184"))   +
  theme(legend.position = "none")+
  scale_color_manual(values = c('black',"gray60"))  
  

PLOTB<-ggplot()+
  geom_hline(aes(yintercept=0), linetype=1, col='gray50', size=0.3)+
  geom_bar(data= DATA %>% filter(!LU=="all",name %in% c("ES"), outcome =="all"),
           aes(x= reorder(int, ORDER),
               y= value, fill= LU),alpha= 0.5,size=0.2,stat="identity",
           position = "dodge2")+
  geom_bar(data= DATA %>% filter(!LU=="all",name %in% c("ES"), outcome =="C"),
           aes(x= reorder(int, ORDER),
               y= value, fill= LU),stat="identity",
           position = "dodge2")+
  theme_pubr()+ coord_flip()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  labs(x="", y=" Number of effect-sizes")+
  scale_fill_manual(values = c("#E8D399","#C8E8CC", "#99ABE8", "gray60","#E86184"))   +
  theme(legend.position = "none")+
  scale_color_manual(values = c('black',"gray60"))  +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plot_grid(PLOTA, PLOTB, ncol=2, rel_widths = c(1.35,1))


