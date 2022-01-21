## Main categories and subcategories of other effect-sizes retrieved in the 217 meta-analyses and studied concurrently with SOC
# Beillouin Damien
# DataPaper: A global database of land management, land use change 
#and climate change effects on soil organic carbon
# in Scientific Data

####I/ Initialisation #####

### Load the packages #####

x<-c("magrittr", "tidyverse", "ggplot2","tidyr","dplyr", "readxl", "plotly")
lapply(x, require, character.only = TRUE)

#### Data load ####
##Adjust the path### ## or set the WD to the right location###

ES<- read_excel("Data_Base_C_Sol_2021-09-14.xlsx", sheet="Effect-sizes")   %>%
  mutate (ES=as.numeric(gsub(",", ".", 'Effect size')),
          upper_CI    = as.numeric(gsub(",", ".", upper_CI)),
          lower_CI    = as.numeric(gsub(",", ".", lower_CI)),
          ES_SE       = as.numeric(gsub(",", ".", ES_SE)),
          ES_SE       = as.numeric(as.character(ES_SE)), 
          p_value     = as.numeric(as.character(p_value)), 
          N_paired_data      = as.numeric(as.character(N_paired_data)),
          Sub_cat_outcome    = tolower(Sub_cat_outcome))



##II/ Analyse the Data #####

# Count the number of effect-sizes per categories
COUNT1<- ES                                %>%
  dplyr::mutate(Outcome =tolower(Outcome)) %>% 
  dplyr::group_by(Outcome)                 %>% 
  dplyr::count()                           %>%
  dplyr::filter(!Outcome =="soil carbon")  %>%
  dplyr::mutate(parents= "ALL")            %>%
  dplyr::rename(labels= Outcome, values =n)
COUNT1$colors

# Count the number of effect-sizes per subcategories

COUNT2<- ES                               %>%
  group_by(Outcome,Sub_cat_outcome)       %>%
  dplyr::mutate(Sub_cat_outcome =tolower(Sub_cat_outcome),
         Outcome =tolower(Outcome))       %>%
  dplyr::count()                          %>%
  filter(!Outcome =="soil carbon")        %>%
  dplyr::rename(labels= Sub_cat_outcome,
                parents = Outcome, values =n) %>%
  filter( parents %in% c("ghg", 'plant production',
                         "soil biology", "soil chemistry", 
                         "soil physics", "water quality"))

## Add sub-categories with no details
AA<-data.frame(parents= c("litter", "soil water regulation",'other','soil degradation'),
               labels =c(".", ". ",' .',' . '),
               values =c(232,175, 30, 26))

COUNT2<- bind_rows(COUNT2, AA)

# Count the global number of effect-size
COUNT3 <- data.frame(parents= "", labels= "ALL", values= sum(COUNT1$values))
COUNT3$values= COUNT3$values

# merge all the files
COUNT<-bind_rows(COUNT1,COUNT3,COUNT2)

# plot
fig <- plot_ly(COUNT,labels = ~COUNT$labels, 
               parents = ~COUNT$parents,
               values= COUNT$values,
               type = 'sunburst',
               branchvalues = 'total')
fig
layout(fig, size=24,colorway = c('#1a9850','#ef8a62','#2166ac','#af8dc3','#67a9cf','#4d4d4d','#fee08b','#fee08b','#fee08b'))

