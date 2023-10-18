
library(RCurl)
library(tidyverse)
library(ctmm)


#----------------------------------------------------------------------
# Reading in and formatting telemtry data for movement modeling
#----------------------------------------------------------------------
woth.locs <- read.csv(text = getURL("https://raw.githubusercontent.com/ValenteJJ/OccupancyClosureMS/main/woth_locs20.csv"), header=T, as.is=T)

#Remove transition locations where territory = NA
woth.locs <- subset(woth.locs, !is.na(woth.locs$territory))

# partition locations into individual territories:
woth.locs$bird_t <- paste0(woth.locs$bird_id, "_t", woth.locs$territory)

# format time
woth.locs$timestamp <- as.POSIXct(strptime(woth.locs$dt, "%d-%m-%y %H:%M"))

# translating variable names for ctmm
help("as.telemetry")
names(woth.locs)[names(woth.locs)=="bird_t"] <- "individual.local.identifier"
names(woth.locs)[names(woth.locs)=="longitude"] <- "location.long"
names(woth.locs)[names(woth.locs)=="latitude"] <- "location.lat"

# convert to ctmm w/ projection
woth.ctmm <- as.telemetry(woth.locs, projection = "+proj=utm +zone=18 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


#----------------------------------------------------------------------
# Fitting CTMM models and choosing the best one for each of the 34
# unique territories for which we could fit models
#----------------------------------------------------------------------

#Create a function for selecting the best movement model for each territory
selectCtmmModel = function(telemData){
  guess = ctmm.guess(telemData, interactive=F)
  fits = ctmm.select(telemData, guess, verbose=T, cores=0, trace=1)
  return(fits[[1]])
}

#Extract the best model for each of the 34 territories
model1 = selectCtmmModel(woth.ctmm$CNPS11_t1)
model2 = selectCtmmModel(woth.ctmm$CNPS13_t1)
model3 = selectCtmmModel(woth.ctmm$CNPS24_t3)
model4 = selectCtmmModel(woth.ctmm$CNPS29_t2)
model5 = selectCtmmModel(woth.ctmm$CNPS39_t2)
model6 = selectCtmmModel(woth.ctmm$CNPS4_t2)
model7 = selectCtmmModel(woth.ctmm$CNPS7_t1)
model8 = selectCtmmModel(woth.ctmm$CNPS8_t1)
model9 = selectCtmmModel(woth.ctmm$COLW1_t1)
model10 = selectCtmmModel(woth.ctmm$COLW2_t1)
model11 = selectCtmmModel(woth.ctmm$COLW3_t1)
model12 = selectCtmmModel(woth.ctmm$COLW4_t2)
model13 = selectCtmmModel(woth.ctmm$COLW5_t1)
model14 = selectCtmmModel(woth.ctmm$DAMB_t1)
model15 = selectCtmmModel(woth.ctmm$GT5_t1)
model16 = selectCtmmModel(woth.ctmm$NNP1_t1)
model17 = selectCtmmModel(woth.ctmm$NNP18_t1)
model18 = selectCtmmModel(woth.ctmm$NNP29_t2)
model19 = selectCtmmModel(woth.ctmm$NQP1_t1)
model20 = selectCtmmModel(woth.ctmm$NQP2_t2)
model21 = selectCtmmModel(woth.ctmm$WAM1_t1)
model22 = selectCtmmModel(woth.ctmm$WAM1_t2)
model23 = selectCtmmModel(woth.ctmm$WAM10_t1)
model24 = selectCtmmModel(woth.ctmm$WAM12_t1)
model25 = selectCtmmModel(woth.ctmm$WAM13_t1)
model26 = selectCtmmModel(woth.ctmm$WAM15_t2)
model27 = selectCtmmModel(woth.ctmm$WAM2_t1)
model28 = selectCtmmModel(woth.ctmm$WAM3_t1)
model29 = selectCtmmModel(woth.ctmm$WAM3_t2)
model30 = selectCtmmModel(woth.ctmm$WAM5_t2)
model31 = selectCtmmModel(woth.ctmm$WAM9_t1)
model32 = selectCtmmModel(woth.ctmm$WM1_t1)
model33 = selectCtmmModel(woth.ctmm$WM5_t1)
model34 = selectCtmmModel(woth.ctmm$WM6_t1)

modelList = list(model1, model2, model3, model4, model5, model6, model7, model8, model9, model10,
                 model11, model12, model13, model14, model15, model16, model17, model18, model19, model20,
                 model21, model22, model23, model24, model25, model26, model27, model28, model29, model30,
                 model31, model32, model33, model34)

#We also need to store the X and Y values for the territory center for each bird because we will
#need to move them later.
centerX = c(mean(woth.ctmm$CNPS11_t1@.Data[[5]]),
            mean(woth.ctmm$CNPS13_t1@.Data[[5]]),
            mean(woth.ctmm$CNPS24_t3@.Data[[5]]),
            mean(woth.ctmm$CNPS29_t2@.Data[[5]]),
            mean(woth.ctmm$CNPS39_t2@.Data[[5]]),
            mean(woth.ctmm$CNPS4_t2@.Data[[5]]),
            mean(woth.ctmm$CNPS7_t1@.Data[[5]]),
            mean(woth.ctmm$CNPS8_t1@.Data[[5]]),
            mean(woth.ctmm$COLW1_t1@.Data[[5]]),
            mean(woth.ctmm$COLW2_t1@.Data[[5]]),
            mean(woth.ctmm$COLW3_t1@.Data[[5]]),
            mean(woth.ctmm$COLW4_t2@.Data[[5]]),
            mean(woth.ctmm$COLW5_t1@.Data[[5]]),
            mean(woth.ctmm$DAMB_t1@.Data[[5]]),
            mean(woth.ctmm$GT5_t1@.Data[[5]]),
            mean(woth.ctmm$NNP1_t1@.Data[[5]]),
            mean(woth.ctmm$NNP18_t1@.Data[[5]]),
            mean(woth.ctmm$NNP29_t2@.Data[[5]]),
            mean(woth.ctmm$NQP1_t1@.Data[[5]]),
            mean(woth.ctmm$NQP2_t2@.Data[[5]]),
            mean(woth.ctmm$WAM1_t1@.Data[[5]]),
            mean(woth.ctmm$WAM1_t2@.Data[[5]]),
            mean(woth.ctmm$WAM10_t1@.Data[[5]]),
            mean(woth.ctmm$WAM12_t1@.Data[[5]]),
            mean(woth.ctmm$WAM13_t1@.Data[[5]]),
            mean(woth.ctmm$WAM15_t2@.Data[[5]]),
            mean(woth.ctmm$WAM2_t1@.Data[[5]]),
            mean(woth.ctmm$WAM3_t1@.Data[[5]]),
            mean(woth.ctmm$WAM3_t2@.Data[[5]]),
            mean(woth.ctmm$WAM5_t2@.Data[[5]]),
            mean(woth.ctmm$WAM9_t1@.Data[[5]]),
            mean(woth.ctmm$WM1_t1@.Data[[5]]),
            mean(woth.ctmm$WM5_t1@.Data[[5]]),
            mean(woth.ctmm$WM6_t1@.Data[[5]]))

centerY = c(mean(woth.ctmm$CNPS11_t1@.Data[[6]]),
            mean(woth.ctmm$CNPS13_t1@.Data[[6]]),
            mean(woth.ctmm$CNPS24_t3@.Data[[6]]),
            mean(woth.ctmm$CNPS29_t2@.Data[[6]]),
            mean(woth.ctmm$CNPS39_t2@.Data[[6]]),
            mean(woth.ctmm$CNPS4_t2@.Data[[6]]),
            mean(woth.ctmm$CNPS7_t1@.Data[[6]]),
            mean(woth.ctmm$CNPS8_t1@.Data[[6]]),
            mean(woth.ctmm$COLW1_t1@.Data[[6]]),
            mean(woth.ctmm$COLW2_t1@.Data[[6]]),
            mean(woth.ctmm$COLW3_t1@.Data[[6]]),
            mean(woth.ctmm$COLW4_t2@.Data[[6]]),
            mean(woth.ctmm$COLW5_t1@.Data[[6]]),
            mean(woth.ctmm$DAMB_t1@.Data[[6]]),
            mean(woth.ctmm$GT5_t1@.Data[[6]]),
            mean(woth.ctmm$NNP1_t1@.Data[[6]]),
            mean(woth.ctmm$NNP18_t1@.Data[[6]]),
            mean(woth.ctmm$NNP29_t2@.Data[[6]]),
            mean(woth.ctmm$NQP1_t1@.Data[[6]]),
            mean(woth.ctmm$NQP2_t2@.Data[[6]]),
            mean(woth.ctmm$WAM1_t1@.Data[[6]]),
            mean(woth.ctmm$WAM1_t2@.Data[[6]]),
            mean(woth.ctmm$WAM10_t1@.Data[[6]]),
            mean(woth.ctmm$WAM12_t1@.Data[[6]]),
            mean(woth.ctmm$WAM13_t1@.Data[[6]]),
            mean(woth.ctmm$WAM15_t2@.Data[[6]]),
            mean(woth.ctmm$WAM2_t1@.Data[[6]]),
            mean(woth.ctmm$WAM3_t1@.Data[[6]]),
            mean(woth.ctmm$WAM3_t2@.Data[[6]]),
            mean(woth.ctmm$WAM5_t2@.Data[[6]]),
            mean(woth.ctmm$WAM9_t1@.Data[[6]]),
            mean(woth.ctmm$WM1_t1@.Data[[6]]),
            mean(woth.ctmm$WM5_t1@.Data[[6]]),
            mean(woth.ctmm$WM6_t1@.Data[[6]]))

keep(modelList, woth.ctmm, woth.locs, centerX, centerY, sure=T)

save(list=ls(), file="SimulationStart.RData")

