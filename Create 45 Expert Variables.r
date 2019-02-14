library(dplyr)
data=read.csv("clean_data_Final.csv")
####
#add building count , because some owners live in the same address 
dataA = data %>%
  filter(STADDR != "") %>%
  group_by(STADDR) %>%
  summarise(count = n())

data = merge(data, dataA, all.x = T)

data[is.na(data$count),]$count = 1
####

##Layer 1
#Create 3 sizes:LOTAREA,BLDAREA,BLDVOL
data=data %>%
  mutate(LOTAREA= data$LTdepth*data$LTfront)

data=data %>%
  mutate(BLDAREA= data$BLDfront*data$BLDdepth) 

data=data %>%
  mutate(BLDVOL= data$BLDAREA*data$STORIES2/data$count)




####
##Layer 2

#FULLVAL
data$FULLVAL_LOTAREA = data$FULLVAL/data$LOTAREA
data$FULLVAL_BLDAREA = data$FULLVAL/data$BLDAREA
data$FULLVAL_BLDVOL = data$FULLVAL/data$BLDVOL

#AVLAND
data$AVLAND_LOTAREA = data$AVLAND/data$LOTAREA
data$AVLAND_BLDAREA = data$AVLAND/data$BLDAREA
data$AVLAND_BLDVOL = data$AVLAND/data$BLDVOL

#AVTOT
data$AVTOT_LOTAREA = data$AVTOT/data$LOTAREA
data$AVTOT_BLDAREA = data$AVTOT/data$BLDAREA
data$AVTOT_BLDVOL = data$AVTOT/data$BLDVOL


#####
####Layer 3

###ZIP5

##FULLVAL

#FULLVAL_LOTAREA
FULLVAL_LOTAREA_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(FULLVAL_LOTAREA_ZIP5 = mean(FULLVAL_LOTAREA))
#FULLVAL_LOTAREA_ZIP5[is.na(FULLVAL_1_ZIP5$ZIP),]$FULLVAL_1_ZIP5 = mean(D$FULLVAL_LTAREA),not necessary at here since no na in FULLVAL_LOTAREA_ZIP5
data = merge(data, FULLVAL_LOTAREA_ZIP5, all.x = T)
data$FULLVAL_LOTAREA_ZIP5 = data$FULLVAL_LOTAREA/data$FULLVAL_LOTAREA_ZIP5


#FULLVAL_BLDAREA
FULLVAL_BLDAREA_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(FULLVAL_BLDAREA_ZIP5 = mean(FULLVAL_BLDAREA))
data = merge(data, FULLVAL_BLDAREA_ZIP5, all.x = T)
data$FULLVAL_BLDAREA_ZIP5 = data$FULLVAL_BLDAREA/data$FULLVAL_BLDAREA_ZIP5

#FULLVAL_BLDVOL
FULLVAL_BLDVOL_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(FULLVAL_BLDVOL_ZIP5 = mean(FULLVAL_BLDVOL))
data = merge(data, FULLVAL_BLDVOL_ZIP5, all.x = T)
data$FULLVAL_BLDVOL_ZIP5 = data$FULLVAL_BLDVOL/data$FULLVAL_BLDVOL_ZIP5

##AVTOT

#AVTOT_LOTAREA
AVTOT_LOTAREA_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(AVTOT_LOTAREA_ZIP5 = mean(AVTOT_LOTAREA))
data = merge(data, AVTOT_LOTAREA_ZIP5, all.x = T)
data$AVTOT_LOTAREA_ZIP5 = data$AVTOT_LOTAREA/data$AVTOT_LOTAREA_ZIP5


#AVTOT_BLDAREA
AVTOT_BLDAREA_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(AVTOT_BLDAREA_ZIP5 = mean(AVTOT_BLDAREA))
data = merge(data, AVTOT_BLDAREA_ZIP5, all.x = T)
data$AVTOT_BLDAREA_ZIP5 = data$AVTOT_BLDAREA/data$AVTOT_BLDAREA_ZIP5


#AVTOT_BLDVOL
AVTOT_BLDVOL_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(AVTOT_BLDVOL_ZIP5 = mean(AVTOT_BLDVOL))
data = merge(data, AVTOT_BLDVOL_ZIP5, all.x = T)
data$AVTOT_BLDVOL_ZIP5 = data$AVTOT_BLDVOL/data$AVTOT_BLDVOL_ZIP5


##AVLAND
#AVLAND_LOTAREA
AVLAND_LOTAREA_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(AVLAND_LOTAREA_ZIP5 = mean(AVLAND_LOTAREA))
data = merge(data, AVLAND_LOTAREA_ZIP5, all.x = T)
data$AVLAND_LOTAREA_ZIP5 = data$AVLAND_LOTAREA/data$AVLAND_LOTAREA_ZIP5


#AVLAND_BLDAREA
AVLAND_BLDAREA_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(AVLAND_BLDAREA_ZIP5 = mean(AVLAND_BLDAREA))
data = merge(data, AVLAND_BLDAREA_ZIP5, all.x = T)
data$AVLAND_BLDAREA_ZIP5 = data$AVLAND_BLDAREA/data$AVLAND_BLDAREA_ZIP5


#AVLAND_BLDVOL
AVLAND_BLDVOL_ZIP5 = data %>%
  group_by(ZIP5) %>%
  summarise(AVLAND_BLDVOL_ZIP5 = mean(AVLAND_BLDVOL))
data = merge(data, AVLAND_BLDVOL_ZIP5, all.x = T)
data$AVLAND_BLDVOL_ZIP5 = data$AVLAND_BLDVOL/data$AVLAND_BLDVOL_ZIP5


###ZIP3
data$ZIP3 = substr(data$ZIP5,1,3)
##FULLVAL
#FULLVAL_LOTAREA
FULLVAL_LOTAREA_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(FULLVAL_LOTAREA_ZIP3 = mean(FULLVAL_LOTAREA))
data = merge(data, FULLVAL_LOTAREA_ZIP3, all.x = T)
data$FULLVAL_LOTAREA_ZIP3 = data$FULLVAL_LOTAREA/data$FULLVAL_LOTAREA_ZIP3


#FULLVAL_BLDAREA
FULLVAL_BLDAREA_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(FULLVAL_BLDAREA_ZIP3 = mean(FULLVAL_BLDAREA))
data = merge(data, FULLVAL_BLDAREA_ZIP3, all.x = T)
data$FULLVAL_BLDAREA_ZIP3 = data$FULLVAL_BLDAREA/data$FULLVAL_BLDAREA_ZIP3


#FULLVAL_BLDVOL
FULLVAL_BLDVOL_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(FULLVAL_BLDVOL_ZIP3 = mean(FULLVAL_BLDVOL))
data = merge(data, FULLVAL_BLDVOL_ZIP3, all.x = T)
data$FULLVAL_BLDVOL_ZIP3 = data$FULLVAL_BLDVOL/data$FULLVAL_BLDVOL_ZIP3


##AVTOT

#AVTOT_LOTAREA
AVTOT_LOTAREA_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(AVTOT_LOTAREA_ZIP3 = mean(AVTOT_LOTAREA))
data = merge(data, AVTOT_LOTAREA_ZIP3, all.x = T)
data$AVTOT_LOTAREA_ZIP3 = data$AVTOT_LOTAREA/data$AVTOT_LOTAREA_ZIP3


#AVTOT_BLDAREA
AVTOT_BLDAREA_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(AVTOT_BLDAREA_ZIP3 = mean(AVTOT_BLDAREA))
data = merge(data, AVTOT_BLDAREA_ZIP3, all.x = T)
data$AVTOT_BLDAREA_ZIP3 = data$AVTOT_BLDAREA/data$AVTOT_BLDAREA_ZIP3


#AVTOT_BLDVOL
AVTOT_BLDVOL_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(AVTOT_BLDVOL_ZIP3 = mean(AVTOT_BLDVOL))
data = merge(data, AVTOT_BLDVOL_ZIP3, all.x = T)
data$AVTOT_BLDVOL_ZIP3 = data$AVTOT_BLDVOL/data$AVTOT_BLDVOL_ZIP3


##AVLAND
#AVLAND_LOTAREA
AVLAND_LOTAREA_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(AVLAND_LOTAREA_ZIP3 = mean(AVLAND_LOTAREA))
data = merge(data, AVLAND_LOTAREA_ZIP3, all.x = T)
data$AVLAND_LOTAREA_ZIP3 = data$AVLAND_LOTAREA/data$AVLAND_LOTAREA_ZIP3


#AVLAND_BLDAREA
AVLAND_BLDAREA_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(AVLAND_BLDAREA_ZIP3 = mean(AVLAND_BLDAREA))
data = merge(data, AVLAND_BLDAREA_ZIP3, all.x = T)
data$AVLAND_BLDAREA_ZIP3 = data$AVLAND_BLDAREA/data$AVLAND_BLDAREA_ZIP3


#AVLAND_BLDVOL
AVLAND_BLDVOL_ZIP3 = data %>%
  group_by(ZIP3) %>%
  summarise(AVLAND_BLDVOL_ZIP3 = mean(AVLAND_BLDVOL))
data = merge(data, AVLAND_BLDVOL_ZIP3, all.x = T)
data$AVLAND_BLDVOL_ZIP3 = data$AVLAND_BLDVOL/data$AVLAND_BLDVOL_ZIP3


###TAXCLASS
##FULLVAL

#FULLVAL_LOTAREA
FULLVAL_LOTAREA_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(FULLVAL_LOTAREA_TAXCLASS = mean(FULLVAL_LOTAREA))
data = merge(data, FULLVAL_LOTAREA_TAXCLASS, all.x = T)
data$FULLVAL_LOTAREA_TAXCLASS = data$FULLVAL_LOTAREA/data$FULLVAL_LOTAREA_TAXCLASS


#FULLVAL_BLDAREA
FULLVAL_BLDAREA_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(FULLVAL_BLDAREA_TAXCLASS = mean(FULLVAL_BLDAREA))
data = merge(data, FULLVAL_BLDAREA_TAXCLASS, all.x = T)
data$FULLVAL_BLDAREA_TAXCLASS = data$FULLVAL_BLDAREA/data$FULLVAL_BLDAREA_TAXCLASS


#FULLVAL_BLDVOL
FULLVAL_BLDVOL_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(FULLVAL_BLDVOL_TAXCLASS = mean(FULLVAL_BLDVOL))
data = merge(data, FULLVAL_BLDVOL_TAXCLASS, all.x = T)
data$FULLVAL_BLDVOL_TAXCLASS = data$FULLVAL_BLDVOL/data$FULLVAL_BLDVOL_TAXCLASS


##AVTOT 

#AVTOT_LOTAREA
AVTOT_LOTAREA_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(AVTOT_LOTAREA_TAXCLASS = mean(AVTOT_LOTAREA))
data = merge(data, AVTOT_LOTAREA_TAXCLASS, all.x = T)
data$AVTOT_LOTAREA_TAXCLASS = data$AVTOT_LOTAREA/data$AVTOT_LOTAREA_TAXCLASS


#AVTOT_BLDAREA
AVTOT_BLDAREA_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(AVTOT_BLDAREA_TAXCLASS = mean(AVTOT_BLDAREA))
data = merge(data, AVTOT_BLDAREA_TAXCLASS, all.x = T)
data$AVTOT_BLDAREA_TAXCLASS = data$AVTOT_BLDAREA/data$AVTOT_BLDAREA_TAXCLASS


#AVTOT_BLDVOL
AVTOT_BLDVOL_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(AVTOT_BLDVOL_TAXCLASS = mean(AVTOT_BLDVOL))
data = merge(data, AVTOT_BLDVOL_TAXCLASS, all.x = T)
data$AVTOT_BLDVOL_TAXCLASS = data$AVTOT_BLDVOL/data$AVTOT_BLDVOL_TAXCLASS


##AVLAND

#AVLAND_LOTAREA
AVLAND_LOTAREA_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(AVLAND_LOTAREA_TAXCLASS = mean(AVLAND_LOTAREA))
data = merge(data, AVLAND_LOTAREA_TAXCLASS, all.x = T)
data$AVLAND_LOTAREA_TAXCLASS = data$AVLAND_LOTAREA/data$AVLAND_LOTAREA_TAXCLASS


#AVLAND_BLDAREA
AVLAND_BLDAREA_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(AVLAND_BLDAREA_TAXCLASS = mean(AVLAND_BLDAREA))
data = merge(data, AVLAND_BLDAREA_TAXCLASS, all.x = T)
data$AVLAND_BLDAREA_TAXCLASS = data$AVLAND_BLDAREA/data$AVLAND_BLDAREA_TAXCLASS


#AVLAND_BLDVOL
AVLAND_BLDVOL_TAXCLASS = data %>%
  group_by(TAXCLASS) %>%
  summarise(AVLAND_BLDVOL_TAXCLASS = mean(AVLAND_BLDVOL))
data = merge(data, AVLAND_BLDVOL_TAXCLASS, all.x = T)
data$AVLAND_BLDVOL_TAXCLASS = data$AVLAND_BLDVOL/data$AVLAND_BLDVOL_TAXCLASS


###BORO
data$BORO = substr(data$BBLE,1,1)

##FULLVAL

#FULLVAL_LOTAREA
FULLVAL_LOTAREA_BORO = data %>%
  group_by(BORO) %>%
  summarise(FULLVAL_LOTAREA_BORO = mean(FULLVAL_LOTAREA))
data = merge(data, FULLVAL_LOTAREA_BORO, all.x = T)
data$FULLVAL_LOTAREA_BORO = data$FULLVAL_LOTAREA/data$FULLVAL_LOTAREA_BORO


#FULLVAL_BLDAREA
FULLVAL_BLDAREA_BORO = data %>%
  group_by(BORO) %>%
  summarise(FULLVAL_BLDAREA_BORO = mean(FULLVAL_BLDAREA))
data = merge(data, FULLVAL_BLDAREA_BORO, all.x = T)
data$FULLVAL_BLDAREA_BORO = data$FULLVAL_BLDAREA/data$FULLVAL_BLDAREA_BORO


#FULLVAL_BLDVOL
FULLVAL_BLDVOL_BORO = data %>%
  group_by(BORO) %>%
  summarise(FULLVAL_BLDVOL_BORO = mean(FULLVAL_BLDVOL))
data = merge(data, FULLVAL_BLDVOL_BORO, all.x = T)
data$FULLVAL_BLDVOL_BORO = data$FULLVAL_BLDVOL/data$FULLVAL_BLDVOL_BORO


##AVTOT

#AVTOT_LOTAREA
AVTOT_LOTAREA_BORO = data %>%
  group_by(BORO) %>%
  summarise(AVTOT_LOTAREA_BORO = mean(AVTOT_LOTAREA))
data = merge(data, AVTOT_LOTAREA_BORO, all.x = T)
data$AVTOT_LOTAREA_BORO = data$AVTOT_LOTAREA/data$AVTOT_LOTAREA_BORO


#AVTOT_BLDAREA
AVTOT_BLDAREA_BORO = data %>%
  group_by(BORO) %>%
  summarise(AVTOT_BLDAREA_BORO = mean(AVTOT_BLDAREA))
data = merge(data, AVTOT_BLDAREA_BORO, all.x = T)
data$AVTOT_BLDAREA_BORO = data$AVTOT_BLDAREA/data$AVTOT_BLDAREA_BORO


#AVTOT_BLDVOL
AVTOT_BLDVOL_BORO = data %>%
  group_by(BORO) %>%
  summarise(AVTOT_BLDVOL_BORO = mean(AVTOT_BLDVOL))
data = merge(data, AVTOT_BLDVOL_BORO, all.x = T)
data$AVTOT_BLDVOL_BORO = data$AVTOT_BLDVOL/data$AVTOT_BLDVOL_BORO


##AVLAND

#AVLAND_LOTAREA
AVLAND_LOTAREA_BORO = data %>%
  group_by(BORO) %>%
  summarise(AVLAND_LOTAREA_BORO = mean(AVLAND_LOTAREA))
data = merge(data, AVLAND_LOTAREA_BORO, all.x = T)
data$AVLAND_LOTAREA_BORO = data$AVLAND_LOTAREA/data$AVLAND_LOTAREA_BORO


#AVLAND_BLDAREA
AVLAND_BLDAREA_BORO = data %>%
  group_by(BORO) %>%
  summarise(AVLAND_BLDAREA_BORO = mean(AVLAND_BLDAREA))
data = merge(data, AVLAND_BLDAREA_BORO, all.x = T)
data$AVLAND_BLDAREA_BORO = data$AVLAND_BLDAREA/data$AVLAND_BLDAREA_BORO


#AVLAND_BLDVOL
AVLAND_BLDVOL_BORO = data %>%
  group_by(BORO) %>%
  summarise(AVLAND_BLDVOL_BORO = mean(AVLAND_BLDVOL))
data = merge(data, AVLAND_BLDVOL_BORO, all.x = T)
data$AVLAND_BLDVOL_BORO = data$AVLAND_BLDVOL/data$AVLAND_BLDVOL_BORO



###ALL
##FULLVAL  
#FULLVAL_LOTAREA
data$FULLVAL_LOTAREA_ALL=data$FULLVAL_LOTAREA/mean(data$FULLVAL_LOTAREA)

#FULLVAL_BLDAREA
data$FULLVAL_BLDAREA_ALL=data$FULLVAL_BLDAREA/mean(data$FULLVAL_BLDAREA)

#FULLVAL_BLDVOL
data$FULLVAL_BLDVOL_ALL=data$FULLVAL_BLDVOL/mean(data$FULLVAL_BLDVOL)


##AVTOT
#AVTOT_LOTAREA
data$AVTOT_LOTAREA_ALL=data$AVTOT_LOTAREA/mean(data$AVTOT_LOTAREA)

#AVTOT_BLDAREA
data$AVTOT_BLDAREA_ALL=data$AVTOT_BLDAREA/mean(data$AVTOT_BLDAREA)

#AVTOT_BLDVOL
data$AVTOT_BLDVOL_ALL=data$AVTOT_BLDVOL/mean(data$AVTOT_BLDVOL)


##AVLAND
#AVLAND_LOTAREA
data$AVLAND_LOTAREA_ALL=data$AVLAND_LOTAREA/mean(data$AVLAND_LOTAREA)

#AVLAND_BLDAREA
data$AVLAND_BLDAREA_ALL=data$AVLAND_BLDAREA/mean(data$AVLAND_BLDAREA)

#AVLAND_BLDVOL
data$AVLAND_BLDVOL_ALL=data$AVLAND_BLDVOL/mean(data$AVLAND_BLDVOL)

write.csv(data, file = "Final_Data.csv",row.names=FALSE)










