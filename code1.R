## Set working directory

setwd("/home/jandas/R/courseraR/RepResearch/Project2/data")

## Set libraries
library(dplyr)
library(ggplot2)

## read compressed file

## Read only 100,000 rows
### Treat missing values as NA
StormData2 <- read.csv("repdata-data-StormData.csv.bz2")


## Verify most common type of events.
summary(StormData$EVTYPE)

## Clean data only for most common events
## Verify Health fields
summary(StormData$FATALITIES)
summary(StormData$INJURIES)

#StormData$EVTYPE[StormData$EVTYPE == "TSTM WIND"] <- "THUNDERSTORM WIND"
#StormData$EVTYPE[StormData$EVTYPE == "T*M WIND"] <- "THUNDERSTORM WINDS"



df <-group_by(StormData, EVTYPE)
df_sum <- summarize(df, HealthHarm=sum(FATALITIES,INJURIES))
adf_sum<-arrange(df_sum, desc(HealthHarm))

MostHarmful<-head(adf_sum, n=5)

sum(MostHarmful$HealthHarm)/sum(df_sum$HealthHarm)

MostHarmful$EVTYPE<-factor(MostHarmful$EVTYPE)

ggplot(MostHarmful, aes(x = EVTYPE, y = HealthHarm)) + geom_bar(stat = "identity", fill="Skyblue", colour="Skyblue")+ labs(x= "Event") + labs(y= "Economic Damage(Dollars)")

## Verify the values of
levels(StormData$PROPDMGEXP)
summary(StormData$CROPDMG)
levels(StormData$CROPDMGEXP)


StormData$exp <- ifelse(StormData$PROPDMGEXP %in% c("k","K"), 
                    1000, 
                        ifelse(df$PROPDMGEXP %in% c("m","M"), 
                            1000000,
                            ifelse(df$PROPDMGEXP %in% c("b","B"), 
                                1000000000,
                                ifelse(df$PROPDMGEXP %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8"), 
                                    as.numeric(df$PROPDMGEXP),
                                        NA          
                                )
                            )
                        )
                    )

StormData$PROPERTY_DAMAGE <- ifelse(is.na(StormData$exp),StormData$PROPDMG, StormData$PROPDMG*StormData$exp)


levels(StormData$CROPDMGEXP)

StormData$expC <- ifelse(StormData$CROPDMGEXP %in% c("k","K"), 
                        1000, 
                        ifelse(StormData$CROPDMGEXP %in% c("m","M"), 
                               1000000,
                               ifelse(StormData$CROPDMGEXP %in% c("b","B"), 
                                      1000000000,
                                      ifelse(StormData$CROPDMGEXP %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8"), 
                                             as.numeric(StormData$CROPDMGEXP),
                                             NA          
                                      )
                               )
                        )
)

StormData$CROP_DAMAGE <- ifelse(is.na(StormData$expC),StormData$CROPDMG, StormData$CROPDMG*StormData$expC)


sumar los costos prop y crop damages

df <-group_by(StormData, EVTYPE)
df_cost <- summarize(df, DAMAGE=sum(PROPERTY_DAMAGE,CROP_DAMAGE))
adf_cost<-arrange(df_cost, desc(DAMAGE))

MostCostly<-head(adf_cost, n=5)

sum(MostCostly$DAMAGE)/sum(df_cost$DAMAGE)

MostHarmful$EVTYPE<-factor(MostHarmful$EVTYPE)

MostCostly<-head(adf_cost, n=8)

sum(MostCostly$DAMAGE)/sum(df_cost$DAMAGE)


ggplot(MostCostly, aes(x = EVTYPE, y = DAMAGE)) + geom_bar(stat = "identity", fill="Skyblue", colour="Skyblue")


plot
resumen
list


## Add plot


##   Across the United States, which types of events 
##     (as indicated in the EVTYPE variable) are most harmful
##     with respect to population health?

##   Across the United States, which types of events 
##    have the greatest economic consequences