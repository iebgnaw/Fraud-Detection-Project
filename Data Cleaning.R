library(dplyr)

y = read.csv("NY_property_data.csv")

## LTFRONT, LTDEPTH, BLDFRONT,BLDDEPTH
## mising fields explore

L = y %>%
  select (LTFRONT, LTDEPTH) %>%
  filter(LTFRONT ==0 | LTDEPTH ==0)

SL = L %>%
  select (LTFRONT, LTDEPTH) %>%
  filter(LTFRONT !=0 | LTDEPTH !=0)

B = y%>%
  select(BLDFRONT,BLDDEPTH) %>%
  filter(BLDFRONT ==0 | BLDDEPTH ==0)

B %>%
  select(BLDFRONT,BLDDEPTH) %>%
  filter(BLDFRONT !=0 | BLDDEPTH !=0)


## Select LTFRONT, LTDEPTH from origonal table

Lindex = y%>%
  select(RECORD, LTFRONT, LTDEPTH)

colnames(Lindex) = c("RECORD", "ltfront", "ltdepth")

## filling in vlalues when LTFRONT and LTDEPTH both ==0
## Filling in (25,100)

for (i in 1:1070994){
  if (Lindex[i,2] ==0 & Lindex[i,3]==0){
    Lindex[i,2] = 25
    Lindex[i,3] = 100
  }
}


## group by LTDEPTH to get average LTFRONT

Ltdepth_avg= Lindex %>%
  filter(ltfront!=0 & ltdepth!=0)%>%
  group_by(ltfront) %>%
  summarise(round(mean(ltdepth),0))

## group by LTFRONT to get average LTDEPTH

Ltfront_avg= Lindex %>%
  filter(ltfront!=0 & ltdepth!=0)%>%
  group_by(ltdepth) %>%
  summarise(round(mean(ltfront),0))

colnames(Ltfront_avg) = c("ltdepth", "mean_ltfront")
colnames(Ltdepth_avg) = c("ltfront", "mean_ltdepth")


####
## filling in avg vlalues when only LTDEPTH ==0
Lindex = left_join(Lindex, Ltfront_avg,
                   by = c("LTdepth" = "ltdepth"))

Lindex$LTdepth = ifelse(Lindex$LTdepth==0, Lindex$mean_ltdepth.y,
                        Lindex$LTdepth)
  

## filling in avg vlalues when only LTFRONT ==0
Lindex = left_join(Lindex, Ltdepth_avg,
                     by = c("LTfront" = "ltfront"))

Lindex$LTfront = ifelse(Lindex$LTfront==0, Lindex$mean_ltfront.y,
                        Lindex$LTfront)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## select BLDFRONT, BLDDEPTH from originl data
Bindex = y%>%
  select(RECORD,BLDFRONT, BLDDEPTH)

colnames(Bindex) = c("RECORD", "BLDfront", "BLDdepth")

## filling in vlalues when BLDFRONT, BLDDEPTH both ==0
## Filling in (20,39)
  
for (i in 1:1070994){
  if (Bindex[i,2] ==0 & Bindex[i,3]==0){
    Bindex[i,2] = 20
    Bindex[i,3] = 39
  }
}

## group by BLDFRONT to get average BLDDEPTH

BLdepth_avg= y %>%
  select(RECORD,BLDFRONT, BLDDEPTH)%>%
  filter(BLDFRONT!=0 & BLDDEPTH!=0)%>%
  group_by(BLDFRONT) %>%
  summarise(round(mean(BLDDEPTH),0))

## group by BLDDEPTH to get average BLDFRONT

BLfront_avg= y %>%
  select(RECORD,BLDFRONT, BLDDEPTH)%>%
  filter(BLDFRONT!=0 & BLDDEPTH!=0)%>%
  group_by(BLDDEPTH) %>%
  summarise(round(mean(BLDFRONT),0))

colnames(BLfront_avg) = c("blddepth", "mean_bldfront")
colnames(BLdepth_avg) = c("bldfront", "mean_blddepth")


## filling in avg vlalues when only LTDEPTH ==0
Bindex = left_join(Bindex, BLfront_avg,
                   by = c("BLDdepth" = "blddepth"))

Bindex$BLDdepth = ifelse(Bindex$BLDdepth==0, Bindex$mean_blddepth,
                        Bindex$BLDdepth)


## filling in avg vlalues when only LTFRONT ==0
Bindex = left_join(Bindex, BLdepth_avg,
                   by = c("BLDfront" = "bldfront"))

Bindex$BLDfront = ifelse(Bindex$BLDfront==0, Bindex$mean_bldfront,
                        Bindex$BLDfront)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Fill in NA and Combine two dataframes and save to CSV file

Lindex = Lindex[,c(-4,-5)]
Bindex = Bindex[,c(-4,-5)]

## check NA location
LNA = Lindex %>%
  filter(is.na(LTfront)== 1|is.na(LTdepth) == 1) %>%
  select(RECORD,LTfront,LTdepth)

BNA = Bindex %>%
  filter(is.na(BLDfront)== 1|is.na(BLDdepth) == 1) %>%
  select(RECORD,BLDfront,BLDdepth)

## filling in NA values
for (i in c(153050,642972,917942)){
  if (is.na(Lindex[i,2])== 1|is.na(Lindex[i,3]) == 1){
    Lindex[i,2] = 25
    Lindex[i,3] = 100
  }
}

for (i in c(14508,95090,105503,105504,105507,105535,216581,1001845)){
  if (is.na(Bindex[i,2])== 1|is.na(Bindex[i,3]) == 1){
    Bindex[i,2] = 20
    Bindex[i,3] = 39
  }
}


final = merge.data.frame(Lindex, Bindex,
                         by = c("RECORD" = "RECORD"))

write.csv(final, file = "Front_Depth.csv",row.names=FALSE)

check = read.csv("Front_Depth.csv")
