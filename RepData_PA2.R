########################################################################################################
##
##      Project:        Reproducabiity Assignment #2
##      Student:        Cary Correia
##      Date:           July 20, 2014
##
##      Purpose:        Prep for Markdown steps
########################################################################################################
##
##      Set up the working drive
setwd('/Users/carycorreia/Documents/Exploratory_Data_Project1/RepData_PeerAssessment2')
##      Read in the dataset
data<-read.table('repdata-data-StormData.csv.bz2', header=TRUE, sep=",")
nrow(data)

##      Data Processing
## EVTYPE has a lot of data issues:

## let's make our lives easier and get rid of noise-> subset all rows that have 0 for injuring, deaths, and property damage
data<-data[!(data$FATALITIES==0 & data$INJURIES==0 & data$PROPDMG==0 & data$CROPDMG==0),]
nrows(data)
##              
##  Get unique list of EVTYPEs then duplicate the header--these steps are necessary because i dont want to alter the orig col
evt<-as.data.frame(unique(data$EVTYPE))
evt$recode<-unique(data$EVTYPE)
names(evt)<-c("raw_EVTYPE", "original_EVTYPE")
#
# transform the EVTYPE to lower case
evt$raw_EVTYPE<-tolower(evt$raw_EVTYPE)
# Build storm data event matrix--> this code creates a new column and scans for a match to my keywords...
# if a keyword matches a 1 goes into the field:  note the order of the fields is important in case i get multiple hit

evt$AstronomicalLowTide<-as.integer(grepl(c("astronomical low tide"), evt$raw_EVTYPE))
evt$LakeshoreFlood<-as.integer(grepl(c("lakeshore flood"), evt$raw_EVTYPE))
evt$CoastalFlood<-as.integer(grepl(c("coastal flood"), evt$raw_EVTYPE))
evt$FlashFlood<-as.integer(grepl(c("flash flood|flash floooding"), evt$raw_EVTYPE))
evt$Flood<-as.integer(grepl(c("flood|fld|urban|small stream"), evt$raw_EVTYPE))
evt$Lightning<-as.integer(grepl(c("lightning|lighting|ligntning"), evt$raw_EVTYPE))
evt$Tornado<-as.integer(grepl(c("torn|whirl"), evt$raw_EVTYPE))
evt$MarineThunderstormWind<-as.integer(grepl(c("marine thunderstorm wind"), evt$raw_EVTYPE))
evt$ThunderstormWind<-as.integer(grepl(c("thunder|tstm|tunder|thundes|thudder|thundeer|thuner|thuder"), evt$raw_EVTYPE))
evt$HurricaneTyphoon<-as.integer(grepl(c("hurricane|typhoon|floyd"), evt$raw_EVTYPE))
evt$Avalanche<-as.integer(grepl(c("aval"), evt$raw_EVTYPE))
evt$Blizzard<-as.integer(grepl(c("blizz"), evt$raw_EVTYPE))
evt$ExtremeCold_WindChill<-as.integer(grepl(c("extreme cold|wind chill|record low|low temp|windchill|cool|hypo|hyper|record cold"), evt$raw_EVTYPE))
evt$FunnelCloud<-as.integer(grepl(c("funnel cloud|funnel|wall cloud"), evt$raw_EVTYPE))
evt$DenseFog<-as.integer(grepl(c("dense fog|fog|vog"), evt$raw_EVTYPE))
evt$HeavySnow<-as.integer(grepl(c("heavy snow|record snow|excessive snow|record winter snow|snow accumulation|snowstorm|heavy wet snow|record may snow|snow squall"), evt$raw_EVTYPE))
evt$IceStorm<-as.integer(grepl(c("ice storm|ice|glaze"), evt$raw_EVTYPE))
evt$Sleet<-as.integer(grepl(c("sleet"), evt$raw_EVTYPE))
evt$FrostFreeze<-as.integer(grepl(c("frost|freeze|freezing|icy roads"), evt$raw_EVTYPE))
evt$Cold_WindChill<-as.integer(grepl(c("cold"), evt$raw_EVTYPE))
evt$DebrisFlow<-as.integer(grepl(c("land|mud|rock|landslump"), evt$raw_EVTYPE))
evt$DenseSmoke<-as.integer(grepl(c("dense smoke|smoke"), evt$raw_EVTYPE))
evt$DustDevil<-as.integer(grepl(c("dust devil|devel"), evt$raw_EVTYPE))
evt$DustStorm<-as.integer(grepl(c("dust"), evt$raw_EVTYPE))
evt$Waterspout<-as.integer(grepl(c("waterspout|water spout|wayterspout"), evt$raw_EVTYPE))
evt$Drought<-as.integer(grepl(c("drought|dry|driest"), evt$raw_EVTYPE))
evt$ExcessiveHeat<-as.integer(grepl(c("excessive heat|record high|high temp|record warmth"), evt$raw_EVTYPE))
evt$Heat<-as.integer(grepl(c("heat|warm|warmth|hot spell|hot"), evt$raw_EVTYPE))
evt$FreezingFog<-as.integer(grepl(c("freezing fog"), evt$raw_EVTYPE))
evt$MarineHail<-as.integer(grepl(c("marine hail"), evt$raw_EVTYPE))
evt$Hail<-as.integer(grepl(c("hail"), evt$raw_EVTYPE))
evt$StormSurge_Tide<-as.integer(grepl(c("surge|high tide|high sea|surf|high waves|high water|swells|rogue|rough sea|heavy sea"), evt$raw_EVTYPE))
evt$HeavyRain<-as.integer(grepl(c("heavy rain|record rainfall|heavy precip|rainstorm|burst|torrential|hvy rain|excessive rain|shower|rain (heavy) |wet microburst"), evt$raw_EVTYPE))
evt$HighSurf<-as.integer(grepl(c("high surf"), evt$raw_EVTYPE))
evt$LakeEffectSnow<-as.integer(grepl(c("effect snow|heavy lake snow"), evt$raw_EVTYPE))
evt$RipCurrent<-as.integer(grepl(c("rip current"), evt$raw_EVTYPE))
evt$Seiche<-as.integer(grepl(c("seiche"), evt$raw_EVTYPE))
evt$MarineStrongWind<-as.integer(grepl(c("marine strong wind|coastal storm|coastalstorm"), evt$raw_EVTYPE))
evt$StrongWind<-as.integer(grepl(c("strong wind|turbulence|storm force wind|dry microburst|microburst"), evt$raw_EVTYPE))
evt$TropicalDepression<-as.integer(grepl(c("tropical depression"), evt$raw_EVTYPE))
evt$TropicalStorm<-as.integer(grepl(c("tropical storm"), evt$raw_EVTYPE))
evt$Tsunami<-as.integer(grepl(c("tsunami"), evt$raw_EVTYPE))
evt$VolcanicAsh<-as.integer(grepl(c("volcan"), evt$raw_EVTYPE))
evt$Wildfire<-as.integer(grepl(c("wildfire|wild fires|wild/forest fire|grass|forest fire|brush|red flag"), evt$raw_EVTYPE))
evt$WinterStorm<-as.integer(grepl(c("winter storm"), evt$raw_EVTYPE))
evt$WinterWeather<-as.integer(grepl(c("winter weather|wintry|winter mix|wintery"), evt$raw_EVTYPE))
evt$MarineHIghWind<-as.integer(grepl(c("marine high wind"), evt$raw_EVTYPE))
evt$HighWind<-as.integer(grepl(c("high wind|high winds|high  winds|gusty|gustnado|gradient|wind|non tstm|non-tstm"), evt$raw_EVTYPE))
#run count seq to determine 1's and multi's:
evt$count<-rowSums(Filter(is.integer, evt))

#split off the matrix of 1's= easy, 2-4 = orderRule, 0=misc
easy<-evt[evt$count==1,]
order<-evt[evt$count>1,]
misc<-evt[evt$count==0,]

#determine row where we have an EVALTYPE match
columns<-c(colnames(evt))
for(i in 1:nrow(evt)){
        evt$check[i]<-match("1", evt[i,3:50])
}
#pop in the corrected name
#note NA's are the misc dataframe--> these are EVTYPEs that we choose to ignore see codebook for definitions
evt$recode<-c(columns[evt$check[1:nrow(evt)]+2])

# merge/vlookup the recoded variable back into the main databrick
data<-merge(x=data, y=evt, by.x="EVTYPE", by.y="original_EVTYPE")

## Let's put in a count into dataset so we can count up during the aggregation process
data$count2<-rep(1,nrow(data))
## Let's create a column to isolate the 4 time periods of interest

## First-let's format the date columns
data$BGN_DATE<-as.Date(strptime(data$BGN_DATE, "%m/%d/%Y %H:%M:%S"))
data$period<-as.numeric(format(data$BGN_DATE, "%Y"))

data$period2<- ifelse(data$period<1955, '1950-1954',
                        ifelse(data$period<1993,'1955-1992',
                                ifelse(data$period<1996,'1993-1995',
                                        ifelse(data$period<2014,'1996-Present','unknown'))))

## Need to calculate CROPDMG and PROPDMG
data$crop.cost<-ifelse(!is.na(data$CROPDMG), 
                  ifelse(data$CROPDMGEXP=='K', data$CROPDMG*1000,
                         ifelse(data$CROPDMGEXP=='M', data$CROPDMG*1000000,
                                ifelse(data$CROPDMG=='B', data$CROPDMG*1000000000, 0))))
data$prop.cost<-ifelse(!is.na(data$PROPDMG), 
                       ifelse(data$PROPDMGEXP=='K', data$PROPDMG*1000,
                              ifelse(data$PROPDMGEXP=='M', data$PROPDMG*1000000,
                                     ifelse(data$PROPDMG=='B', data$PROPDMG*1000000000, 0))))

## Lets' build the aggregated data so we can take a look at the Y measures by the EVTYPE and Period 
## Fatalities summarized
        fatal.agg<-as.data.frame(tapply(data$FATALITIES, list(evtype=data$recode, period=data$period2), sum))  
        EF.kill<-rownames(fatal.agg); fatal.agg$EVTYPE<-EF.kill
## Injuries   summarized
        injuries.agg<-as.data.frame(tapply(data$INJURIES, list(evtype=data$recode, period=data$period2), sum))
        EF.injuries<-rownames(injuries.agg); injuries.agg$EVTYPE<-EF.injuries
## crop costs summarized
        crop.agg<-as.data.frame(tapply(data$crop.cost, list(evtype=data$recode, period=data$period2), sum))   
        EF.crops<-rownames(crop.agg); crop.agg$EVTYPE<-EF.crops
## prop.costs summarized
        prop.agg<-as.data.frame(tapply(data$prop.cost, list(evtype=data$recode, period=data$period2), sum))    
        EF.props<-rownames(prop.agg); prop.agg$EVTYPE<-EF.props

# let's reshape the data to fit this into 1 table
library(reshape)
fatal<-reshape(fatal.agg,
               varying=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
               v.names="Fatal.count",
               timevar="Period",
               times=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
               direction="long")
injure<-reshape(injuries.agg,
               varying=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
               v.names="injure.count",
               timevar="Period",
               times=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
               direction="long")
crops<-reshape(crop.agg,
                varying=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
                v.names="Crops.DMG",
                timevar="Period",
                times=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
                direction="long")
props<-reshape(prop.agg,
                varying=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
                v.names="Propety.DMG",
                timevar="Period",
                times=c('1950-1954', '1955-1992', '1993-1995', '1996-Present'),
                direction="long")
## Let's combine the data into 1 large table
Final.Table2<-fatal
Final.Table2$Injuries<-injure$injure.count
Final.Table2$CropDamage<-crops$Crops.DMG
Final.Table2$PropsDamage<-props$Propety.DMG

# sort the 4 facts for our pareto charts
fatalact <- subset(Final.Table2, select=c("EVTYPE", "Period", "Fatal.count"))
fatalact$Fatal.count<-as.numeric(fatalact$Fatal.count)
fatalsort <-fatalact[ with(fatalact, order(Period,-Fatal.count, EVTYPE), na.last=FALSE),]
fatalsort$top<-c(rep(1:46, 4))
## Lets Just Look at the Top 10 EVT's
fatTop<-subset(fatalsort, fatalsort$top<11)

injuriesact <- subset(Final.Table2, select=c("EVTYPE", "Period", "Injuries"))
injuriesact$Injuries<-as.numeric(injuriesact$Injuries)
injsort <-injuriesact[ with(injuriesact, order(Period,-Injuries, EVTYPE), na.last=FALSE),]
injsort$top<-c(rep(1:46, 4))
## Lets Just Look at the Top 10 EVT's
injTop<-subset(injsort, injsort$top<11)

cropsact <- subset(Final.Table2, select=c("EVTYPE", "Period", "CropDamage"))
cropsact$CropDamage<-as.numeric(cropsact$CropDamage)
cropssort <-cropsact[ with(cropsact, order(Period,-CropDamage, EVTYPE), na.last=FALSE),]
cropssort$top<-c(rep(1:46, 4))
## Lets Just Look at the Top 10 EVT's
cropTop<-subset(cropssort, cropssort$top<11)

propsact <- subset(Final.Table2, select=c("EVTYPE", "Period", "PropsDamage"))
propsact$PropsDamage<-as.numeric(propsact$PropsDamage)
propssort <-propsact[ with(propsact, order(Period,-PropsDamage, EVTYPE), na.last=FALSE),]
propssort$top<-c(rep(1:46, 4))
## Lets Just Look at the Top 10 EVT's
propTop<-subset(propssort, propssort$top<11)

## Question to answer:

##1 Across the United States, which types of events (as indicated in the EVTYPE variable) 
## are most harmful with respect to population health?
#Create Panel Plot 1: Fatalities and Injuries by EVTYPE
library(ggplot2)
cropssort$group<-as.factor(cropssort$Period)
require(gridExtra)

fatTop<-transform(fatTop, EVTYPE=reorder(EVTYPE, -Fatal.count))   ## reorder the data
plot1<-ggplot(fatTop, aes(EVTYPE, Fatal.count))+
        geom_bar(stat="identity", color="black", fill="blue")+
        facet_grid(Period ~.) +
        theme(legend.position = "none", axis.text.x=element_text(size=10, angle = 45, hjust = 1))+
        ylabs("Fatalities")+
        ggtitle("Fatalities by Environment Type:  Timeframe: 1950 to Present")

injTop<-transform(injTop, EVTYPE=reorder(EVTYPE, -Injuries))   ## reorder the data
plot2<-ggplot(injTop, aes(EVTYPE, Injuries))+
        geom_bar(stat="identity", color="black", fill="red")+
        facet_grid(Period ~.) +
        theme(legend.position = "none", axis.text.x=element_text(size=10, angle = 45, hjust = 1))+
        ggtitle("Injuries by Environment Type:  Timeframe: 1950 to Present")

grid.arrange(plot1, plot2, ncol=2)

##2 Across the United States, which types of events have the greatest economic consequences?
###############################################################################################
#Create Panel Plot 2: CropDamage and PropetyDamage by EVTYPE
cropTop<-transform(cropTop, EVTYPE=reorder(EVTYPE, -CropDamage))   ## reorder the data
plot1<-ggplot(cropTop, aes(EVTYPE, CropDamage))+
        geom_bar(stat="identity", color="black", fill="Green")+
        facet_grid(Period ~.) +
        theme(legend.position = "none", axis.text.x=element_text(size=10, angle = 45, hjust = 1))+
        ggtitle("Crop Damage by Environment Factors: Timeframe: 1950 to Present")

propTop<-transform(propTop, EVTYPE=reorder(EVTYPE, -PropsDamage))   ## reorder the data
plot2<-ggplot(propTop, aes(EVTYPE, PropsDamage))+
        geom_bar(stat="identity", color="black", fill="purple")+
        facet_grid(Period ~.) +
        theme(legend.position = "none", axis.text.x=element_text(size=10, angle = 45, hjust = 1))+
        ggtitle("Property Damage by Environment Factors:Timeframe: 1950 to Present")

grid.arrange(plot1, plot2, ncol=2)
####