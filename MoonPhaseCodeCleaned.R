##########################################################
########## MOON PHASE ANALYSIS ###########################
##########################################################

####Load Packages####
library(StreamMetabolism)
library(weatherr)
library(rwunderground)
library(raster)
library(geosphere)
library(SDMTools)
library(rgeos)
library(maptools)
library(lubridate)
library(rgdal)
library(lunar)
library(MuMIn)
library(lme4)
library(lmerTest)
library(adehabitatHR)
library(lattice)
library(tidyr)
library(amt)
library(MuMIn)
library(modelsummary)
library(terra)
library(dplyr)
library(tibble)
library(survival)
library(tidyr)
library(ggplot2)
library(landscapemetrics)
library(suncalc)


####Read in Rasters ####
Density_1m <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/Density1m_10m.tif")
Density_2m <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/Density2m_10m.tif")
Density_3m <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/Density3m_10m.tif")
Density_4m <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/Density4m_10m.tif")
Density_5m <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/Density5m_10m.tif")
Density_Over5m <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/DensityOver5m_10m.tif")

DistHeavy <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/DistHeavy_10m.tif")
DistLow <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/DistLow_10m.tif")
CanopyCover <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/CanCover_10m.tif")
patcharea <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/WoodyPatchArea_10m.tif")
patchshape <- raster("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/10m_Rasters/WoodyPatchShape_10m.tif")

landcover <- raster("C:/Users/KUMS2113/Desktop/GIS/LandCover10m.tif")


####Read in & check old telemetry data collar success####
e3m <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/E3M_GPS.csv")
e3m$ID <- "E3M"
e6m <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/E6M_GPS.csv")
e6m$ID <- "E6M"
e8f <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/E8F_GPS.csv")
e8f$ID <- "E8F"
e10f <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/E10F_GPS.csv")
e10f$ID <- "E10F"
e12f <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/E12F_GPS.csv")
e12f$ID <- "E12F"
e14f <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/E14F_GPS.csv")
e14f$ID <- "E14F"
e17f <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/E17F_GPSb.csv")
e17f$ID <- "E17F"
y12m <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/Ocelot GPS Data (for analysis)/Y12M_GPS.csv")
y12m$ID <- "Y12M"


cord.dec2 <- SpatialPoints(cbind(e3m$E, e3m$N),
                           proj4string = CRS("+proj=utm +zone=14N +datum=WGS84"))
cord.UTM2 <- spTransform(cord.dec2, CRS("+proj=longlat +datum=WGS84"))
cord.UTM2 <- as.data.frame(cord.UTM2)
#Add UTM Columns
e3m$Latitude <- cord.UTM2$coords.x2
e3m$Longitude <- cord.UTM2$coords.x1
head(e3m)
e3m <- e3m[, c(1:3, 7, 8, 6)]
colnames(e3m)[4] <- "Lat"
colnames(e3m)[5] <- "Long"

teldat <- rbind(e3m, e6m, e8f, e10f, e12f, e14f, e17f, y12m)
head(teldat)
colnames(teldat)[2] <- "Date"
colnames(teldat)[3] <- "Time"

teldat <- teldat[!is.na(teldat$Lat),]

teldat$Date <- as.Date(teldat$Date, "%m.%d.%Y")
teldat$Time <- format(strptime(teldat$Time, "%H:%M:%S"), "%H:%M:%S")
#teldat <- teldat[order(teldat$ID,teldat$Date_numeric),]


#Combine Date/Time into one column
date_info <- with(teldat, paste((teldat$Date), teldat$Time))
date_time <- strptime(date_info, "%Y-%m-%d %H:%M:%S")
#as.data.frame(date_time)
teldat$Date1 <- date_time
#teldat$Date2 <- as.POSIXct(teldat$Date1)
teldat$Date_numeric <- as.numeric(teldat$Date1)



head(teldat)

#Convert GMT to LMT
lmttdateimes <- with_tz(teldat$Date1, tzone = "UTC")
lmttimes <- format(strptime(lmttdateimes, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

teldat$Date <- as.Date(lmttdateimes$LMT.date, "%y-%m-%d")

teldat$Time <- lmttimes

#Combine Date/Time into one column
date_info <- with(teldat, paste((teldat$Date), teldat$Time))
date_time <- strptime(date_info, "%Y-%m-%d %H:%M:%S")
#as.data.frame(date_time)
teldat$Date1 <- date_time
#teldat$Date2 <- as.POSIXct(teldat$Date1)
teldat$Date_numeric <- as.numeric(teldat$Date1)


#Populate Time lag column
teldat$Time.lag=(ave(teldat$Date_numeric, teldat$ID, FUN=function(x) c(0, diff(x)))/60)
teldat$DT <-as.POSIXct(teldat$Date1, tz = "UTC")
teldat <- teldat[order(teldat$ID, teldat$DT),]








teldat <- teldat[teldat$Time.lag < 65 & teldat$Time.lag > 0,]
table(teldat$ID)
teldat <- teldat[!(is.na(teldat$Lat)),]

teldat$DayNo <- round(as.numeric(teldat$Date_numeric/86400))

e3m <- teldat[teldat$ID == "E3M",]
head(e3m)
e3m$Day <- as.numeric(format(strptime(e3m$Date, "%Y-%m-%d"), "%d" ))
e3m_freq <- as.data.frame(table(e3m$DayNo))
plot(e3m_freq$Var1, e3m_freq$Freq)


e6m <- teldat[teldat$ID == "E6M",]
head(e6m)
e6m$Day <- as.numeric(format(strptime(e6m$Date, "%Y-%m-%d"), "%d" ))
e6m_freq <- as.data.frame(table(e6m$DayNo))
plot(e6m_freq$Var1, e6m_freq$Freq)

e8f <- teldat[teldat$ID == "E8F",]
head(e8f)
e8f$Day <- as.numeric(format(strptime(e8f$Date, "%Y-%m-%d"), "%d" ))
e8f_freq <- as.data.frame(table(e8f$DayNo))
plot(e8f_freq$Var1, e8f_freq$Freq)

e10f <- teldat[teldat$ID == "E10F",]
head(e10f)
e10f$Day <- as.numeric(format(strptime(e10f$Date, "%Y-%m-%d"), "%d" ))
e10f_freq <- as.data.frame(table(e10f$DayNo))
plot(e10f_freq$Var1, e10f_freq$Freq)

e12f <- teldat[teldat$ID == "E12F",]
head(e12f)
e12f$Day <- as.numeric(format(strptime(e12f$Date, "%Y-%m-%d"), "%d" ))
e12f_freq <- as.data.frame(table(e12f$DayNo))
plot(e12f_freq$Var1, e12f_freq$Freq)

e14f <- teldat[teldat$ID == "E14F",]
head(e14f)
e14f$Day <- as.numeric(format(strptime(e14f$Date, "%Y-%m-%d"), "%d" ))
e14f_freq <- as.data.frame(table(e14f$DayNo))
plot(e14f_freq$Var1, e14f_freq$Freq)
mean(e14f_freq$Freq)

e17f <- teldat[teldat$ID == "E17F",]
head(e17f)
e17f$Day <- as.numeric(format(strptime(e17f$Date, "%Y-%m-%d"), "%d" ))
e17f_freq <- as.data.frame(table(e17f$DayNo))
plot(e17f_freq$Var1, e17f_freq$Freq)
View(e17f)


y12m <- teldat[teldat$ID == "Y12M",]
head(y12m)
y12m$Day <- as.numeric(format(strptime(y12m$Date, "%Y-%m-%d"), "%d" ))
y12m_freq <- as.data.frame(table(y12m$DayNo))
plot(y12m_freq$Var1, y12m_freq$Freq)
View(y12m)





####Read in and prep data ####
ocelots <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/OcelotsAnalysisData.csv")

head(ocelots)

ocelots <- ocelots[!is.na(ocelots$Latitude),]

ocelots$Date <- as.Date(ocelots$Date, "%m/%d/%Y")
ocelots$Time <- format(strptime(ocelots$Time, "%H:%M:%S"), "%H:%M:%S")
ocelots <- ocelots[order(ocelots$ID,ocelots$Date_numeric),]


#Combine Date/Time into one column
date_info <- with(ocelots, paste((ocelots$Date), ocelots$Time))
date_time <- strptime(date_info, "%Y-%m-%d %H:%M:%S")
#as.data.frame(date_time)
ocelots$Date1 <- date_time
#ocelots$Date2 <- as.POSIXct(ocelots$Date1)
ocelots$Date_numeric <- as.numeric(ocelots$Date1)


#Populate Time lag column
ocelots$Time.lag=(ave(ocelots$Date_numeric, ocelots$ID, FUN=function(x) c(0, diff(x)))/60)
ocelots$DT <-as.POSIXct(ocelots$Date1, tz = "America/Chicago")
ocelots <- ocelots[order(ocelots$ID, ocelots$DT),]


#Convert from Lat/Long to UTM
cord.dec2 <- SpatialPoints(cbind(ocelots$Longitude, ocelots$Latitude),
                           proj4string = CRS("+proj=longlat +datum=WGS84"))
cord.UTM2 <- spTransform(cord.dec2, CRS("+proj=utm +zone=14N +datum=WGS84"))
cord.UTM2 <- as.data.frame(cord.UTM2)
#Add UTM Columns
ocelots$Easting <- cord.UTM2$coords.x1
ocelots$Northing <- cord.UTM2$coords.x2
head(ocelots)

ocelots <- ocelots[!is.na(ocelots$DT),]



####Make into tibble####
ocelot.tib <- as_tibble(ocelots) %>%
  nest(-ID, .key = "ind.data") %>%
  mutate(tracks = map(ind.data, function(x){ # Adds a list column to the nested tibble
    x %>%
      make_track(.x = Easting,
                 .y = Northing,
                 .t = DT,
                 crs = CRS("+proj=utm +zone=14N +datum=WGS84"),
                 all_cols = T) %>%
      track_resample(rate = minutes(30), tolerance = minutes(5))
  })) %>%
  mutate(steps = map(tracks, function(x){ # Adds a list column for individual specific "steps" objects
    x %>%
      steps()
  })) %>%
  mutate(ssfData = map(steps, function(x){ # Adds a list column for individual specific "random_steps" objects
    x %>%
      random_steps(10, ta_distr = fit_distr(.$ta_, "unif"))}))



# For each row
for(i in 1:nrow(ocelot.tib)){
  # If it's row one
  if(i == 1){
    # Make individual ssfdata into a data frame and add catID column from ocelot.tib
    ssfData <- ocelot.tib$ssfData[[i]]
    ssfData$ID <- ocelot.tib$ID[i]
  } else {
    # If it's not row 1, make a new dataframe out of the new individuals data and add a column for its ID
    addData <- ocelot.tib$ssfData[[i]]
    addData$ID <- ocelot.tib$ID[i]
    # Now, add the rows from addData to ssfData
    ssfData <- rbind(ssfData, addData)
  }
}

ocssf <- as.data.frame(ssfData)
head(ocssf)

ocssf <- ocssf[, c(12, 2, 4, 8, 5, 6, 9, 10, 11)]

colnames(ocssf)[2] <- "Easting"
colnames(ocssf)[3] <- "Northing"
colnames(ocssf)[4] <- "Date"
colnames(ocssf)[5] <- "StepLength"
colnames(ocssf)[6] <- "TurningAngle"
colnames(ocssf)[7] <- "DT"



##Add Day/Night

cord.dec2 <- SpatialPoints(cbind(ocssf$Easting, ocssf$Northing),
                           proj4string = CRS("+proj=utm +zone=14N +datum=WGS84"))
cord.UTM2 <- spTransform(cord.dec2, CRS("+proj=longlat +datum=WGS84"))
cord.UTM2 <- as.data.frame(cord.UTM2)
#Add UTM Columns
ocssf$Latitude <- cord.UTM2$coords.x2
ocssf$Longitude <- cord.UTM2$coords.x1
head(ocssf)


sundat <- ocssf[, c(1, 4, 10, 11)]
head(sundat)

sundat$Time <- format(strptime(sundat$Date, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
sundat$Date <- as.Date(sundat$Date, "%Y-%m-%d")
sundat <- sundat[, c(1, 2, 5, 3, 4)]

colnames(sundat) <- c("ID", "date", "Time", "lat", "lon")


sun <- getSunlightTimes(data = sundat, keep = c("sunrise", "sunset"), tz = "America/Chicago")

ocssf$sunrise <- sun$sunrise
ocssf$sunset <- sun$sunset

ocssf$TOD <- ifelse(ocssf$Date > ocssf$sunrise & ocssf$Date < ocssf$sunset, 'Day', 'Night')




#########################
moonssf <- ocssf[ocssf$TOD == "Night",]
moonssf <- moonssf[moonssf$StepLength < 1500,]
head(moonssf)
moontrue <- moonssf[moonssf$case_ == "TRUE",]
  
  
hist(moonssf$StepLength)
tail(moonssf, 15)


moonssf$Illumination <- lunar.illumination(moonssf$Date)
moonssf$MoonPhase <- lunar.phase(moonssf$Date, name = T)

#Add daylength 
moonssf$Date1 <- format(strptime(moonssf$Date, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")

moonssf$DayLength <- daylength(moonssf$Latitude, moonssf$Date1)
moonssf$NightLength <- (24 - moonssf$DayLength)




summary(aov(moontrue$StepLength ~ moontrue$MoonPhase, data = moonssf))
summary(aov(moontrue$StepLength ~ moontrue$Illumination, data = moonssf))

full_new_moon <- moonssf[moonssf$MoonPhase == "New" | moonssf$MoonPhase == "Full", ]
summary(aov(full_new_moon$StepLength ~ full_new_moon$MoonPhase, data = full_new_moon))

boxplot(StepLength ~ MoonPhase, data=moontrue)

summary(glm(StepLength ~ Illumination, data = moontrue))




####Add Landscape Variables####

moonssf <-SpatialPointsDataFrame(moonssf[,c('Easting','Northing')],
                               proj4string=CRS("+proj=utm +zone=14"),
                               data=moonssf)


moonssf$DistHeavyCover <- terra::extract(DistHeavy, moonssf)
moonssf$DistLowCover <- terra::extract(DistLow, moonssf)
moonssf$PatchArea <- terra::extract(patcharea, moonssf)
moonssf$CanopyCover <- terra::extract(CanopyCover, moonssf)
moonssf$Density_1m <- terra::extract(Density_1m, moonssf)
moonssf$Density_2m <- terra::extract(Density_2m, moonssf)
moonssf$Density_3m <- terra::extract(Density_3m, moonssf)
moonssf$Density_4m <- terra::extract(Density_4m, moonssf)
moonssf$Density_5m <- terra::extract(Density_5m, moonssf)
moonssf$Density_Over5m <- terra::extract(Density_Over5m, moonssf)

moonssf$Density_1m <- ifelse(is.na(moonssf$Density_1m)==T, 0, moonssf$Density_1m)
moonssf$Density_2m <- ifelse(is.na(moonssf$Density_2m)==T, 0, moonssf$Density_2m)
moonssf$Density_3m <- ifelse(is.na(moonssf$Density_3m)==T, 0, moonssf$Density_3m)
moonssf$Density_4m <- ifelse(is.na(moonssf$Density_4m)==T, 0, moonssf$Density_4m)
moonssf$Density_5m <- ifelse(is.na(moonssf$Density_5m)==T, 0, moonssf$Density_5m)
moonssf$Density_Over5m <- ifelse(is.na(moonssf$Density_Over5m)==T, 0, moonssf$Density_Over5m)





moonssf$VegDensity <- moonssf$Density_1m + moonssf$Density_2m + moonssf$Density_3m + moonssf$Density_4m + moonssf$Density_5m + moonssf$Density_Over5m

moonssf$DistHeavyCover <- ifelse(is.na(moonssf$DistHeavyCover)==T, 0, moonssf$DistHeavyCover)
moonssf$DistLowCover <- ifelse(is.na(moonssf$DistLowCover)==T, 0, moonssf$DistLowCover)
moonssf$PatchArea <- ifelse(is.na(moonssf$PatchArea)==T, 0, moonssf$PatchArea)
moonssf$CanopyCover <- ifelse(is.na(moonssf$CanopyCover)==T, 0, moonssf$CanopyCover)
moonssf$VegDensity <- ifelse(is.na(moonssf$VegDensity)==T, 0, moonssf$VegDensity)
moonssf$Density_1m <- ifelse(is.na(moonssf$Density_1m)==T, 0, moonssf$Density_1m)





moonssf$DistHeavyCover <- scale(moonssf$DistHeavyCover)
moonssf$DistLowCover <- scale(moonssf$DistLowCover)
moonssf$PatchArea <- scale(moonssf$PatchArea)
moonssf$CanopyCover <- scale(moonssf$CanopyCover)
moonssf$VegDensity <- scale(moonssf$VegDensity)
moonssf$Density_1m <- scale(moonssf$Density_1m)


moonssf$LandCover <- terra::extract(landcover, moonssf)

table(moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==2207, "Herb", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover > 5000 & moonssf$LandCover < 5700, "Herb", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==6307, "Herb", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==6200, "Bare", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==6402, "Forest", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==6403, "Forest", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==6405, "Forest", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==6507, "Herb", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==6600, "Bare", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover > 7000 & moonssf$LandCover < 7400, "Shrub", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==7700, "Bare", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==9000, "Bare", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==9007, "Herb", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==9187, "Herb", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==9204, "Shrub", moonssf$LandCover)

moonssf$LandCover <- ifelse(moonssf$LandCover==9307, "Shrub", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==9411, "Bare", moonssf$LandCover)
moonssf$LandCover <- ifelse(moonssf$LandCover==9600, "Bare", moonssf$LandCover)


##
moonssf$ID_Step <- paste(moonssf$ID, moonssf$step_id_, sep = "-")



full_new_moon <- moonssf[moonssf$MoonPhase == "New" | moonssf$MoonPhase == "Full", ]
full_new_moon <- as.data.frame(full_new_moon)

full_new_moon <- full_new_moon[full_new_moon$case_ == "TRUE",]

summary(aov(full_new_moon$StepLength ~ full_new_moon$MoonPhase, data = full_new_moon))



oc_new_moon <- moonssf[moonssf$MoonPhase == "New" , ]
oc_full_moon <- moonssf[moonssf$MoonPhase == "Full", ]



#### Run SSF Models ####
oc_moon_intmod <- clogit(case_ ~ MoonPhase*DistLowCover + MoonPhase*DistHeavyCover + MoonPhase*PatchArea +  MoonPhase*CanopyCover + MoonPhase*Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = full_new_moon, na.action ="na.pass")
summary(oc_moon_intmod)




oc_newmoon_mod <- clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = oc_new_moon, na.action ="na.pass")
summary(oc_newmoon_mod)

oc_fullmoon_mod <- clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = oc_full_moon, na.action ="na.pass")
summary(oc_fullmoon_mod)




oc_moon_models <- list(
  "New Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = oc_new_moon, na.action ="na.pass"),
  "Full Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = oc_full_moon, na.action ="na.pass")
)
modelplot(oc_moon_models)



##############################
#### Modeling Step Length ####

head(full_new_moon)

summary(lmer(StepLength ~ (1|ID) + NightLength + Illumination, data = full_new_moon))


################################################################
###############         BOBCATS        #########################
################################################################

bobcats <- read.csv("C:/Users/KUMS2113/Desktop/A&M Stuff/Research/Ocelot & Bobcat Data/BobcatsAnalysisData.csv")

head(bobcats)

bobcats <- bobcats[!is.na(bobcats$Latitude),]

bobcats$Date <- as.Date(bobcats$Date, "%m/%d/%Y")
bobcats$Time <- format(strptime(bobcats$Time, "%H:%M:%S"), "%H:%M:%S")
bobcats <- bobcats[order(bobcats$ID,bobcats$Date_numeric),]


#Combine Date/Time into one column
date_info <- with(bobcats, paste((bobcats$Date), bobcats$Time))
date_time <- strptime(date_info, "%Y-%m-%d %H:%M:%S")
#as.data.frame(date_time)
bobcats$Date1 <- date_time
#bobcats$Date2 <- as.POSIXct(bobcats$Date1)
bobcats$Date_numeric <- as.numeric(bobcats$Date1)


#Populate Time lag column
bobcats$Time.lag=(ave(bobcats$Date_numeric, bobcats$ID, FUN=function(x) c(0, diff(x)))/60)
bobcats$DT <-as.POSIXct(bobcats$Date1, tz = "America/Chicago")
bobcats <- bobcats[order(bobcats$ID, bobcats$DT),]


#Convert from Lat/Long to UTM
cord.dec2 <- SpatialPoints(cbind(bobcats$Longitude, bobcats$Latitude),
                           proj4string = CRS("+proj=longlat +datum=WGS84"))
cord.UTM2 <- spTransform(cord.dec2, CRS("+proj=utm +zone=14N +datum=WGS84"))
cord.UTM2 <- as.data.frame(cord.UTM2)
#Add UTM Columns
bobcats$Easting <- cord.UTM2$coords.x1
bobcats$Northing <- cord.UTM2$coords.x2
head(bobcats)

bobcats <- bobcats[!is.na(bobcats$DT),]



####Make into tibble####
bobcat.tib <- as_tibble(bobcats) %>%
  nest(-ID, .key = "ind.data") %>%
  mutate(tracks = map(ind.data, function(x){ # Adds a list column to the nested tibble
    x %>%
      make_track(.x = Easting,
                 .y = Northing,
                 .t = DT,
                 crs = CRS("+proj=utm +zone=14N +datum=WGS84"),
                 all_cols = T) %>%
      track_resample(rate = minutes(30), tolerance = minutes(5))
  })) %>%
  mutate(steps = map(tracks, function(x){ # Adds a list column for individual specific "steps" objects
    x %>%
      steps()
  })) %>%
  mutate(ssfData = map(steps, function(x){ # Adds a list column for individual specific "random_steps" objects
    x %>%
      random_steps(10, ta_distr = fit_distr(.$ta_, "unif"))}))



# For each row
for(i in 1:nrow(bobcat.tib)){
  # If it's row one
  if(i == 1){
    # Make individual ssfdata into a data frame and add catID column from bobcat.tib
    ssfData <- bobcat.tib$ssfData[[i]]
    ssfData$ID <- bobcat.tib$ID[i]
  } else {
    # If it's not row 1, make a new dataframe out of the new individuals data and add a column for its ID
    addData <- bobcat.tib$ssfData[[i]]
    addData$ID <- bobcat.tib$ID[i]
    # Now, add the rows from addData to ssfData
    ssfData <- rbind(ssfData, addData)
  }
}

bobssf <- as.data.frame(ssfData)
head(bobssf)

bobssf <- bobssf[, c(12, 2, 4, 8, 5, 6, 9, 10, 11)]

colnames(bobssf)[2] <- "Easting"
colnames(bobssf)[3] <- "Northing"
colnames(bobssf)[4] <- "Date"
colnames(bobssf)[5] <- "StepLength"
colnames(bobssf)[6] <- "TurningAngle"
colnames(bobssf)[7] <- "DT"



##Add Day/Night

cord.dec2 <- SpatialPoints(cbind(bobssf$Easting, bobssf$Northing),
                           proj4string = CRS("+proj=utm +zone=14N +datum=WGS84"))
cord.UTM2 <- spTransform(cord.dec2, CRS("+proj=longlat +datum=WGS84"))
cord.UTM2 <- as.data.frame(cord.UTM2)
#Add UTM Columns
bobssf$Latitude <- cord.UTM2$coords.x2
bobssf$Longitude <- cord.UTM2$coords.x1
head(bobssf)


sundat <- bobssf[, c(1, 4, 10, 11)]
head(sundat)

sundat$Time <- format(strptime(sundat$Date, "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
sundat$Date <- as.Date(sundat$Date, "%Y-%m-%d")
sundat <- sundat[, c(1, 2, 5, 3, 4)]

colnames(sundat) <- c("ID", "date", "Time", "lat", "lon")


sun <- getSunlightTimes(data = sundat, keep = c("sunrise", "sunset"), tz = "America/Chicago")

bobssf$sunrise <- sun$sunrise
bobssf$sunset <- sun$sunset

bobssf$TOD <- ifelse(bobssf$Date > bobssf$sunrise & bobssf$Date < bobssf$sunset, 'Day', 'Night')




#########################
bob_moonssf <- bobssf[bobssf$TOD == "Night",]
bob_moonssf <- bob_moonssf[bob_moonssf$StepLength < 1500,]
head(bob_moonssf)
bobmoontrue <- bob_moonssf[bob_moonssf$case_ == "TRUE",]


hist(bob_moonssf$StepLength)
tail(bob_moonssf, 15)


bob_moonssf$Illumination <- lunar.illumination(bob_moonssf$Date)
bob_moonssf$MoonPhase <- lunar.phase(bob_moonssf$Date, name = T)

#Add daylength 
bob_moonssf$Date1 <- format(strptime(bob_moonssf$Date, "%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")

bob_moonssf$DayLength <- daylength(bob_moonssf$Latitude, bob_moonssf$Date1)
bob_moonssf$NightLength <- (24 - bob_moonssf$DayLength)




summary(aov(moontrue$StepLength ~ moontrue$MoonPhase, data = bob_moonssf))
summary(aov(moontrue$StepLength ~ moontrue$Illumination, data = bob_moonssf))

bob_full_new_moon <- bob_moonssf[bob_moonssf$MoonPhase == "New" | bob_moonssf$MoonPhase == "Full", ]
summary(aov(bob_full_new_moon$StepLength ~ bob_full_new_moon$MoonPhase, data = bob_full_new_moon))

boxplot(StepLength ~ MoonPhase, data=bobmoontrue)

summary(glm(StepLength ~ Illumination, data = bobmoontrue))




####Add Landscape Variables####

bob_moonssf <-SpatialPointsDataFrame(bob_moonssf[,c('Easting','Northing')],
                                     proj4string=CRS("+proj=utm +zone=14"),
                                     data=bob_moonssf)


bob_moonssf$DistHeavyCover <- terra::extract(DistHeavy, bob_moonssf)
bob_moonssf$DistLowCover <- terra::extract(DistLow, bob_moonssf)
bob_moonssf$PatchArea <- terra::extract(patcharea, bob_moonssf)
bob_moonssf$CanopyCover <- terra::extract(CanopyCover, bob_moonssf)
bob_moonssf$Density_1m <- terra::extract(Density_1m, bob_moonssf)
bob_moonssf$Density_2m <- terra::extract(Density_2m, bob_moonssf)
bob_moonssf$Density_3m <- terra::extract(Density_3m, bob_moonssf)
bob_moonssf$Density_4m <- terra::extract(Density_4m, bob_moonssf)
bob_moonssf$Density_5m <- terra::extract(Density_5m, bob_moonssf)
bob_moonssf$Density_Over5m <- terra::extract(Density_Over5m, bob_moonssf)

bob_moonssf$Density_1m <- ifelse(is.na(bob_moonssf$Density_1m)==T, 0, bob_moonssf$Density_1m)
bob_moonssf$Density_2m <- ifelse(is.na(bob_moonssf$Density_2m)==T, 0, bob_moonssf$Density_2m)
bob_moonssf$Density_3m <- ifelse(is.na(bob_moonssf$Density_3m)==T, 0, bob_moonssf$Density_3m)
bob_moonssf$Density_4m <- ifelse(is.na(bob_moonssf$Density_4m)==T, 0, bob_moonssf$Density_4m)
bob_moonssf$Density_5m <- ifelse(is.na(bob_moonssf$Density_5m)==T, 0, bob_moonssf$Density_5m)
bob_moonssf$Density_Over5m <- ifelse(is.na(bob_moonssf$Density_Over5m)==T, 0, bob_moonssf$Density_Over5m)





bob_moonssf$VegDensity <- bob_moonssf$Density_1m + bob_moonssf$Density_2m + bob_moonssf$Density_3m + bob_moonssf$Density_4m + bob_moonssf$Density_5m + bob_moonssf$Density_Over5m

bob_moonssf$DistHeavyCover <- ifelse(is.na(bob_moonssf$DistHeavyCover)==T, 0, bob_moonssf$DistHeavyCover)
bob_moonssf$DistLowCover <- ifelse(is.na(bob_moonssf$DistLowCover)==T, 0, bob_moonssf$DistLowCover)
bob_moonssf$PatchArea <- ifelse(is.na(bob_moonssf$PatchArea)==T, 0, bob_moonssf$PatchArea)
bob_moonssf$CanopyCover <- ifelse(is.na(bob_moonssf$CanopyCover)==T, 0, bob_moonssf$CanopyCover)
bob_moonssf$VegDensity <- ifelse(is.na(bob_moonssf$VegDensity)==T, 0, bob_moonssf$VegDensity)
bob_moonssf$Density_1m <- ifelse(is.na(bob_moonssf$Density_1m)==T, 0, bob_moonssf$Density_1m)





bob_moonssf$DistHeavyCover <- scale(bob_moonssf$DistHeavyCover)
bob_moonssf$DistLowCover <- scale(bob_moonssf$DistLowCover)
bob_moonssf$PatchArea <- scale(bob_moonssf$PatchArea)
bob_moonssf$CanopyCover <- scale(bob_moonssf$CanopyCover)
bob_moonssf$VegDensity <- scale(bob_moonssf$VegDensity)
bob_moonssf$Density_1m <- scale(bob_moonssf$Density_1m)


bob_moonssf$LandCover <- terra::extract(landcover, bob_moonssf)

table(bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==2207, "Herb", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover > 5000 & bob_moonssf$LandCover < 5700, "Herb", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==6307, "Herb", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==6200, "Bare", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==6402, "Forest", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==6403, "Forest", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==6405, "Forest", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==6507, "Herb", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==6600, "Bare", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover > 7000 & bob_moonssf$LandCover < 7400, "Shrub", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==7700, "Bare", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==9000, "Bare", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==9007, "Herb", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==9187, "Herb", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==9204, "Shrub", bob_moonssf$LandCover)

bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==9307, "Shrub", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==9411, "Bare", bob_moonssf$LandCover)
bob_moonssf$LandCover <- ifelse(bob_moonssf$LandCover==9600, "Bare", bob_moonssf$LandCover)


##
bob_moonssf$ID_Step <- paste(bob_moonssf$ID, bob_moonssf$step_id_, sep = "-")



bob_full_new_moon <- bob_moonssf[bob_moonssf$MoonPhase == "New" | bob_moonssf$MoonPhase == "Full", ]
bob_full_new_moon <- as.data.frame(bob_full_new_moon)

bob_full_new_moon <- bob_full_new_moon[bob_full_new_moon$case_ == "TRUE",]

summary(aov(bob_full_new_moon$StepLength ~ bob_full_new_moon$MoonPhase, data = bob_full_new_moon))


bob_new_moon <- bob_moonssf[bob_moonssf$MoonPhase == "New" , ]
bob_full_moon <- bob_moonssf[bob_moonssf$MoonPhase == "Full", ]



#### Run SSF Models ####
bob_moon_intmod <- clogit(case_ ~ MoonPhase*DistLowCover + MoonPhase*DistHeavyCover + MoonPhase*PatchArea +  MoonPhase*CanopyCover + MoonPhase*Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = bob_full_new_moon, na.action ="na.pass")
summary(bob_moon_intmod)




bob_newmoon_mod <- clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = bob_new_moon, na.action ="na.pass")
summary(bob_newmoon_mod)

bob_fullmoon_mod <- clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = bob_full_moon, na.action ="na.pass")
summary(bob_fullmoon_mod)




bob_moon_models <- list(
  "New Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = bob_new_moon, na.action ="na.pass"),
  "Full Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = bob_full_moon, na.action ="na.pass")
)
modelplot(bob_moon_models)



##############################
#### Modeling Step Length ####

head(bob_full_new_moon)

summary(lmer(StepLength ~ (1|ID) + NightLength + Illumination, data = bob_full_new_moon))


########### Combined Plot
cats_moon_models <- list(
  "Ocelot New Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = oc_new_moon, na.action ="na.pass"),
  "Ocelot Full Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = oc_full_moon, na.action ="na.pass"),
  "Bobcat New Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = bob_new_moon, na.action ="na.pass"),
  "Bobcat Full Moon" = clogit(case_ ~ DistLowCover + DistHeavyCover + PatchArea +  CanopyCover + Density_1m + strata(ID_Step), cluster = ID, method = "breslow", data = bob_full_moon, na.action ="na.pass")
)
modelplot(cats_moon_models)


#####Step Length Box Plot
##Remake with only true points
full_new_moon$Species_Phase <- paste("Ocelot", full_new_moon$MoonPhase, sep = " - ")

bob_full_new_moon$Species_Phase <- paste("Bobcat", bob_full_new_moon$MoonPhase, sep = " - ")

boxplotdat <- boxplotdat[boxplotdat$case_ == "TRUE",]
boxplotdat <- rbind(full_new_moon, bob_full_new_moon)

boxplot(StepLength ~ Species_Phase, data=boxplotdat, outline = F, xlab = "   ", ylab = "Step length (m)")
