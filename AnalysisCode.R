
library(RCurl)
library(tidyverse)
library(ctmm)
library(gdata)
library(sp)
library(openxlsx)
library(maptools)
library(unmarked)
library(marcher)



rm(list=ls())

#-------------------------------------------------------------------------------
#Importing initial data
#-------------------------------------------------------------------------------


#Capture dates and locations for all birds
capLocs = structure(list(bird_id = c("GT5", "WAM2", "WAM15", "NQP1", "WAM14", 
                                     "WAM4", "WM1", "WM5", "WM6", "COLW3", "COLW5", "COLW2", "COLW4", 
                                     "WAM9", "WAM12", "WAM10", "WAM5", "COLW1", "WAM3", "YRSP5", "CNPS29", 
                                     "CNPS24", "NNP18", "NNP29", "CNPS37", "NQP2", "CNPS4", "CNPS7", 
                                     "CNPS8", "CNPS11", "CNPS12", "CNPS13", "WAM1", "CNPS14", "CNPS23", 
                                     "CNPS27", "CNPS39", "CNPS41", "NNP1", "WAM13", "DAMB"), capDate = structure(c(15840, 
                                                                                                                   15841, 15842, 15843, 15844, 15844, 15846, 15846, 15846, 15847, 
                                                                                                                   15847, 15847, 15847, 15848, 15849, 15849, 15850, 15850, 15851, 
                                                                                                                   15851, 16207, 16208, 16210, 16210, 16211, 16211, 16212, 16212, 
                                                                                                                   16212, 16213, 16213, 16213, 16214, 16215, 16216, 16219, 16219, 
                                                                                                                   16219, 16220, 16220, 16224), class = "Date"), capX = c(340445.863, 
                                                                                                                                                                          347295.317507, 347286.188135, 353609.303718, 346840.044261, 347320.079009, 
                                                                                                                                                                          347367.328552, 347850.022516, 349147.997998, 349605.661954, 349758.150295, 
                                                                                                                                                                          350749.112292, 350551.237861, 346208.727566, 346285.213056, 346920.1479, 
                                                                                                                                                                          346717.506721, 349270.165159, 347389.261409, 347689.355364, 363975.664201, 
                                                                                                                                                                          364347.190166, 361848.923287, 362429.165192, 363993.242551, 354071.748066, 
                                                                                                                                                                          349541.979181, 349274.139412, 349072.262009, 350311.259472, 351546.882458, 
                                                                                                                                                                          352927.847831, 347479.057845, 353515.727316, 365296.59773, 365004.339534, 
                                                                                                                                                                          365838.021778, 366940.028113, 361946.136523, 346411.550005, 348986.156273
                                                                                                                   ), capY = c(4124109.085463, 4126420.96218, 4126932.675154, 4128262.692502, 
                                                                                                                               4125523.263301, 4125432.602676, 4131132.576443, 4131918.029633, 
                                                                                                                               4131899.865245, 4127985.305605, 4128586.452907, 4129374.952838, 
                                                                                                                               4129649.732751, 4127289.777334, 4126014.328275, 4126354.327796, 
                                                                                                                               4126836.724951, 4128492.10866, 4125719.691599, 4140807.599892, 
                                                                                                                               4121610.354915, 4121647.626975, 4119654.669997, 4119677.557153, 
                                                                                                                               4119061.368495, 4128151.078764, 4121736.778373, 4124350.989394, 
                                                                                                                               4124748.178232, 4126995.324151, 4127180.454493, 4127217.045587, 
                                                                                                                               4126077.216697, 4127439.266078, 4121786.603331, 4122451.164845, 
                                                                                                                               4119744.852849, 4119470.867925, 4120115.186111, 4125414.892595, 
                                                                                                                               4124819.205566)), row.names = c(NA, -41L), class = c("tbl_df", 
                                                                                                                                                                                    "tbl", "data.frame"))

#Estimated dates of range shifts based on eyeball test
estDepDates = structure(list(bird_id = c("GT5", "WAM2", "WAM15", "NQP1", "WAM14", 
                                         "WAM4", "WM1", "WM5", "WM6", "COLW3", "COLW5", "COLW2", "COLW4", 
                                         "WAM9", "WAM12", "WAM10", "WAM5", "COLW1", "WAM3", "YRSP5", "CNPS29", 
                                         "CNPS24", "NNP18", "NNP29", "CNPS37", "NQP2", "CNPS4", "CNPS7", 
                                         "CNPS8", "CNPS11", "CNPS12", "CNPS13", "WAM1", "CNPS14", "CNPS23", 
                                         "CNPS27", "CNPS39", "CNPS41", "NNP1", "WAM13", "DAMB"), departure_dt = structure(c(NA, 
                                                                                                                            NA, 15869, NA, 15864, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                            NA, 15867, 15867, 15867, NA, NA, NA, 16239, NA, 16221, 
                                                                                                                            16234, 16217, 16217, NA, NA, NA, NA, NA, 16250, 16221, NA, 16246, 
                                                                                                                            16262, NA, NA, NA, NA), class = "Date")), row.names = c(NA, 41L
                                                                                                                            ), class = "data.frame")




#Read in telemetry locations
wothLocs = read.csv(text = getURL("https://raw.githubusercontent.com/ValenteJJ/OccupancyClosureMS/main/woth_locs20.csv")) %>% 
  mutate(dt = dmy_hm(dt)) %>%
  mutate_if(is.factor, as.character) %>% 
  mutate(territory = NA)

#There are 41 unique birds
length(unique(wothLocs$bird_id))


#Arrange birds from those with the most telemetry points to those with the fewest
birdOrder = wothLocs %>% 
  group_by(bird_id) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))


#-------------------------------------------------------------------------------
#Functions necessary to conduct MRSA
#-------------------------------------------------------------------------------
getLikelihood.res <- function(p.s=NULL, T, Z.res, model = c("wn", "ou", "ouf")[1]){
  
  if(model == "wn") p <- 0 else
    if(model == "ou" | model == "ouf"){
      p <- c(tau.z = as.numeric(exp(p.s['logtau.z'])), 
             tau.v = as.numeric(p.s['kappa']*exp(p.s['logtau.z'])))
    }
  
  n <- length(T)
  S <-  outer(T, T, getCov, model=model, p=p)
  S[is.na(S)] <- 0
  S.inv <- Matrix::solve(S)
  Xt <- Re(Z.res)
  Yt <- Im(Z.res)
  logS.det <- as.numeric(Matrix::determinant(S, logarithm=TRUE)$mod)
  s.xx <- as.numeric(t(Xt) %*% S.inv %*% Xt)/n
  s.yy <- as.numeric(t(Yt) %*% S.inv %*% Yt)/n
  s.hat <- (s.xx + s.yy)/2
  ll <- -logS.det - n*log(s.hat) - n*log(2*pi) - n
  return(ll)
}

test_rangeshift2 = function (FIT, verbose = TRUE) {
  model <- FIT$model
  method <- FIT$method
  p.hat <- FIT$p.hat
  ll <- FIT$ll
  k <- FIT$df
  aic <- FIT$aic
  test.table <- data.frame(ll, k, aic)
  Z.res.null <- (FIT$X + (0+1i) * FIT$Y)
  Z.res.null <- Z.res.null - mean(Z.res.null)
  p.s.null <- NULL
  if (model != "wn") {
    tau.hat.null <- getTau(Z.res.null, T = FIT$T, model = model, 
                           method = method)$tau.hat
    if (model %in% c("ou", "ouf")) 
      p.s.null <- c(logtau.z = log(t(tau.hat.null["tau.z"])))
    if (model == "ouf") 
      p.s.null["kappa"] <- tau.hat.null["tau.v"]/tau.hat.null["tau.z"]
  }
  ll.null <- getLikelihood.res(p.s = p.s.null, T = FIT$T, Z.res = Z.res.null, 
                               model = model)
  k.null <- k - 4
  aic.null <- -2 * ll.null + 2 * k.null
  test.table <- data.frame(ll = c(ll.null, ll), k = c(k.null, 
                                                      k), aic = c(aic.null, aic))
  #row.names(test.table) <- c("with shift", "without shift")
  row.names(test.table) <- c('without shift', 'with shift')
  #lrt <- with(test.table, 2 * (ll[1] - ll[2]))
  lrt <- with(test.table, -2* (ll[1] - ll[2]))
  p.value <- 1 - pchisq(lrt, 4)
  if (verbose) {
    print(test.table)
    cat(paste0("\nl.r.t.: ", signif(lrt, 3), " with ", 
               4, " degrees of freedom, ", "p-value: ", 
               signif(p.value, 4), "\n"))
    if (p.value < 0.05 & with(test.table, aic[2] < aic[1])) 
      cat("There is almost certainly a significant range shift.\n")
    else if (p.value > 0.05 & with(test.table, aic[2] > aic[1])) 
      cat("There probably wasn't a range shift.\n")
    else cat("Tough call - there's disagreement in the criteria.\n")
  }
  invisible(list(aic.table = test.table, lrt = lrt, df = 4, 
                 p.value = p.value))
}



#-------------------------------------------------------------------------------
#Bird 1

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 1

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 2

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points. There is some statistical
#evidence for a range shift based on a LRT, but the two distinct estimated territory
#centers are < 300 m apart. Based on expert opinion, we decided this more likely
#represented a change in the center of activity with an individual's territory
#rather than a shift in the territory's location.
#-------------------------------------------------------------------------------

bird = 2

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#However, when we look at the centers of the two territories, they are only 159 m
#apart.
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 3

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 3

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 4

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 4

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 5

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 5

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 6

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, we were unable to fit a
#model with multiple range centers
#-------------------------------------------------------------------------------

bird = 6

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Attempt to fit an estimated range shift model, but model convergence fails
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)


#-------------------------------------------------------------------------------
#Bird 7

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 7

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 8

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 8

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 9

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 9

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 10

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 10

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 11

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 11

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 12

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 12

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 13

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 13

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 14

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 14

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 15

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points. The LRT does suggest statistical
#evidence for a range shift. However, when we look closely, the range shift model
#created nonsensical parameter estimates that calculate the territory shift is
#completed after tracking concluded. We thus treated all points as having come from 
#the same territory.
#-------------------------------------------------------------------------------

bird = 15

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is some evidence
output = test_rangeshift2(test, verbose=F)
output

#However, the estimated range shift concludes AFTER our last telemetry point was recorded.
test$p.hat[3] + test$p.hat[4]
max(locs$T)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 16

#Summary - We concluded there are 2 territories here. Initial inspection of the
#data indicated strong evidence for a range shift, although only a small number
#of points were recorded on the first territory. Thus, there were insufficient
#data to statistically test whether this constituted a range shift. Given the
#initial points were over 1500 m from the remaining points and that personnel who
#were present in the field noted territorial behaviors in both unique locations,
#we concluded this constituted a range shift. After eliminating the small number
#of initial points, we re-inspected the data for evidence of a second range shift
#both visually and statistically and did not find any.
#-------------------------------------------------------------------------------

bird = 16

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#The points indicate strong evidence for a range shift, although only a small handful
#have been recorded prior to the shift.
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

#The capature site, along with the first 2 points recorded, are over 1500 m away from the other points. Based
#on this and field observations of territoriality, we concluded this likely constituted a range shift.
sqrt((mean(c(locs$X[1:2], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[3:51]))^2 + (mean(c(locs$Y[1:2], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[3:51]))^2)

#We then subsetted the remainder of the points and tested for evidence of a second range shift
locs = locs[3:51,]

#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <= 2, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 2, 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 17

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, we were unable to fit a
#model with multiple range centers
#-------------------------------------------------------------------------------

bird = 17

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Attempt to fit an estimated range shift model, but model convergence fails
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)


#-------------------------------------------------------------------------------
#Bird 18

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 18

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 19

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 19

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 20

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, we were unable to fit a
#model with multiple range centers
#-------------------------------------------------------------------------------

bird = 20

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Attempt to fit an estimated range shift model, but model convergence fails
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)



#-------------------------------------------------------------------------------
#Bird 21

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points. There is some statistical
#evidence for a range shift based on a LRT, but the two distinct estimated territory
#centers are < 300 m apart. Based on expert opinion, we decided this more likely
#represented a change in the center of activity with an individual's territory
#rather than a shift in the territory's location.
#-------------------------------------------------------------------------------

bird = 21

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#However, when we look at the centers of the two territories, they are only 62 m
#apart.
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 22

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 22

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 23

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points. There is some statistical
#evidence for a range shift based on a LRT, but the two distinct estimated territory
#centers are < 300 m apart. Based on expert opinion, we decided this more likely
#represented a change in the center of activity with an individual's territory
#rather than a shift in the territory's location.
#-------------------------------------------------------------------------------

bird = 23

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#However, when we look at the centers of the two territories, they are only 268 m
#apart.
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)





#-------------------------------------------------------------------------------
#Bird 24

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 24

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 25

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 25

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)




#-------------------------------------------------------------------------------
#Bird 26

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 26

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <= 26, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt >= 27, 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 27

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points. There is some statistical
#evidence for a range shift based on a LRT, but the two distinct estimated territory
#centers are < 300 m apart. Based on expert opinion, we decided this more likely
#represented a change in the center of activity with an individual's territory
#rather than a shift in the territory's location.
#-------------------------------------------------------------------------------

bird = 27

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#However, when we look at the centers of the two territories, they are only 223 m
#apart.
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 28

#Summary - We concluded there are 2 territories here. Initial inspection of the
#data indicated strong evidence for a range shift, although only a small number
#of points were recorded on the first territory. Thus, there were insufficient
#data to statistically test whether this constituted a range shift. Given the
#initial points were over 1500 m from the remaining points and that personnel who
#were present in the field noted territorial behaviors in both unique locations,
#we concluded this constituted a range shift. After eliminating the small number
#of initial points, we re-inspected the data for evidence of a second range shift
#both visually and statistically and did not find any.
#-------------------------------------------------------------------------------

bird = 28

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#The points indicate strong evidence for a range shift, although only a small handful
#have been recorded prior to the shift.
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

#The capature site, along with the first point recorded, are over 1500 m away from the other points. Based
#on this and field observations of territoriality, we concluded this likely constituted a range shift.
sqrt((mean(c(locs$X[1], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[2:50]))^2 + (mean(c(locs$Y[1], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[2:50]))^2)

#We then subsetted the remainder of the points and tested for evidence of a second range shift
locs = locs[2:50,]

#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <= 1, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 1, 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 29

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 29

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt<=13, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt >= 14, 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 30

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 30

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 31

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 31

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 32

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 32

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.))) %>% 
  mutate(territory = ifelse(order <= 6, 1, NA)) %>% 
  mutate(territory = ifelse(order >= 15, 2, territory)) %>% 
  select(-order)

tmp = wothLocs %>% 
  filter(bird_id!=birdOrder$bird_id[bird]) %>% 
  rbind(locs)

wothLocs = tmp

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 33

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points. There is some statistical
#evidence for a range shift based on a LRT, but the two distinct estimated territory
#centers are < 300 m apart. Based on expert opinion, we decided this more likely
#represented a change in the center of activity with an individual's territory
#rather than a shift in the territory's location.
#-------------------------------------------------------------------------------

bird = 33

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#However, when we look at the centers of the two territories, they are only 280 m
#apart.
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 34

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 34

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 35

#Summary - We concluded there are 3 total territories here. Initial inspection of the
#data indicated strong evidence for 2 potential range shifts, although only a small
#number of points were recorded on the first territory. Thus, there were insufficient
#data to statistically test whether this constituted a range shift. Given the
#initial points were over 600 m from the second point cluster, and that personnel who
#were present in the field noted territorial behaviors in both unique locations,
#we concluded this constituted a range shift. After eliminating the small number
#of initial points, we re-inspected the data for evidence of a second range shift
#both visually and statistically and did find evidence. Thus, we concluded there
#are 2 range shifts here and 3 total territories.
#-------------------------------------------------------------------------------

bird = 35

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#The points indicate strong evidence for 2 potential range shifts, although only
#a small handful of points have been recorded prior to the first shift.
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

#The capature site, along with the first 2 points recorded, are over 600 m away from the second cluster of points.
#Based on this and field observations of territoriality, we concluded this likely constituted a range shift.
sqrt((mean(c(locs$X[1:2], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[3:13]))^2 + (mean(c(locs$Y[1:2], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[3:13]))^2)

#We then subsetted the remainder of the points and tested for evidence of a second range shift
locs = locs[3:47,]

#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is, so there are indeed 2 range shifts
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <=2, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 2 &
                              telem_pt <= 17, 2, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 17, 3, territory))

#Remove values
rm(locs)
rm(test)
rm(output)


#-------------------------------------------------------------------------------
#Bird 36

#Summary - We concluded there are 2 territories here. Initial inspection of the
#data indicated strong evidence for a range shift, although only a small number
#of points were recorded on the first territory. Thus, there were insufficient
#data to statistically test whether this constituted a range shift. Given the
#initial points were over 800 m from the remaining points and that personnel who
#were present in the field noted territorial behaviors in both unique locations,
#we concluded this constituted a range shift. After eliminating the small number
#of initial points, we re-inspected the data for evidence of a second range shift
#both visually and statistically and did not find any.
#-------------------------------------------------------------------------------

bird = 36

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#The points indicate strong evidence for a range shift, although only a small handful
#have been recorded prior to the shift.
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

#The capature site, along with the first point recorded, are over 800 m away from the other points. Based
#on this and field observations of territoriality, we concluded this likely constituted a range shift.
sqrt((mean(c(locs$X[1], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[2:47]))^2 + (mean(c(locs$Y[1], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[2:47]))^2)

#We then subsetted the remainder of the points and tested for evidence of a second range shift
locs = locs[2:47,]

#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <= 1, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 1, 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 37

#Summary - There are 2 territories here. There was evidence of a range shift
#based on visual inspection of the telemetry points. There is also statistical
#evidence for a range shift based on a LRT. Further, the two distinct estimated
#territory centers are >  300 m apart.
#-------------------------------------------------------------------------------

bird = 37

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates some evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates there is evidence for a range shift
output = test_rangeshift2(test, verbose=F)
output

#And the territory centers are > 300 m apart
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt < as_date('2014-06-25'), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt > as_date('2014-06-25'), 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)

#-------------------------------------------------------------------------------
#Bird 38

#Summary - We concluded there are 2 territories here. Initial inspection of the
#data indicated strong evidence for a range shift, although only a small number
#of points were recorded on the first territory. Thus, there were insufficient
#data to statistically test whether this constituted a range shift. Given the
#initial points were over 600 m from the remaining points and that personnel who
#were present in the field noted territorial behaviors in both unique locations,
#we concluded this constituted a range shift. After eliminating the small number
#of initial points, we re-inspected the data for evidence of a second range shift
#both visually and statistically. Results from the LRT indicated some evidence
#for a second range shift, but the two distinct estimated territory
#centers are < 300 m apart. Based on expert opinion, we decided this more likely
#represented a change in the center of activity with an individual's territory
#rather than a second shift in the territory's location.
#-------------------------------------------------------------------------------

bird = 38

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#The points indicate strong evidence for a range shift, although only a small handful
#have been recorded prior to the shift.
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

#The capature site, along with the first point recorded, are over 600 m away from the other points. Based
#on this and field observations of territoriality, we concluded this likely constituted a range shift.
sqrt((mean(c(locs$X[1], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[2:23]))^2 + (mean(c(locs$Y[1], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[2:23]))^2)

#We then subsetted the remainder of the points and tested for evidence of a second range shift
locs = locs[2:23,]

#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates iit is
output = test_rangeshift2(test, verbose=F)
output

#However, the range centers of the two territories are only ~200 m apart.
sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <= 1, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 1, 2, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 39

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, and the LRT also showed no
#statistical evidence for a range shift
#-------------------------------------------------------------------------------

bird = 39

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Fit an estimated range shift model
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))

#Conduct a likelihood ratio test to see if the range shift model is better than a static model
#p-value indicates it is not
output = test_rangeshift2(test, verbose=F)
output

#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)
rm(test)
rm(output)



#-------------------------------------------------------------------------------
#Bird 40

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, we were unable to fit a
#model with multiple range centers
#-------------------------------------------------------------------------------

bird = 40

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Attempt to fit an estimated range shift model, but model convergence fails
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)


#-------------------------------------------------------------------------------
#Bird 41

#Summary - Only 1 territory because there is no strong evidence for a range shift
#based on visual inspection of the telemetry points, we were unable to fit a
#model with multiple range centers
#-------------------------------------------------------------------------------

bird = 41

#Subset points specific to this bird
locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

#Visual inspection of telemetry points indicates no strong evidence for a range shift
ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


#We need a reasonable starting value for estimating when a territory shift occurs
#If our field personnel indicated some suspicion for a range shift, we will use the
#timestamp they identified. If they did not note such suspicion, we will use the midpoint
#of all timestamps recorded on the bird.
shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

#Generate initial parameter guesses for the model fit
p.m0 = with(locs, quickfit(T, X, Y, dt=shift, plotme=F))

#Attempt to fit an estimated range shift model, but model convergence fails
test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))


#Update territory numbers for each recorded point on the bird
wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

#Remove values
rm(locs)







#--------------------------------------------------------------------------------
#Bird by fucking bird
#--------------------------------------------------------------------------------







####
bird = 2
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)

try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)



####
bird = 3
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)

try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y, color=as.factor(territory)))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 4
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 5
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 6
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 7
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)



wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y, color=as.factor(territory)))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 8
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 9
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 10
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))




try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 11
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 12
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))




try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 13
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 14
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))




try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 15
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

summary(test)

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y, color=as.factor(territory)))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

#The shift concludes after the tracking is complete which is stupid

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 16
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

sqrt((mean(c(locs$X[1:2], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[3:51]))^2 + (mean(c(locs$Y[1:2], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[3:51]))^2)


locs = locs[3:51,]

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <= 2, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 2, 2, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 17
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 18
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))


locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y, color=as.factor(territory)))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 19
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]

if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 20
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

locs = locs[2:51,]

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 21
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 22
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt <= min(locs$dt)+dhours(test$p.hat[3]), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt >= min(locs$dt)+dhours(test$p.hat[3])+dhours(test$p.hat[4]), 2, territory))




try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 23
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 24
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 25
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 26
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <= 26, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt >= 27, 2, territory))

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y, color=as.factor(territory)))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 27
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 28
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

sqrt((mean(c(locs$X[1], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[2:50]))^2 + (mean(c(locs$Y[1], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[2:50]))^2)


locs = locs[2:50,]

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
shift = NA
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt==1, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 1, 2, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 29
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt<=13, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt >= 14, 2, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 30
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)

####
bird = 31
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 32
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs[1:50,], aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.))) %>% 
  mutate(territory = ifelse(order <= 6, 1, NA)) %>% 
  mutate(territory = ifelse(order >= 15, 2, territory)) %>% 
  select(-order)

tmp = wothLocs %>% 
  filter(bird_id!=birdOrder$bird_id[bird]) %>% 
  rbind(locs)

wothLocs = tmp


try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 33
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

sqrt((test$p.hat[5]-test$p.hat[7])^2 + (test$p.hat[6]-test$p.hat[8])^2)


wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 34
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 35
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

sqrt((mean(c(locs$X[1:2], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[3:13]))^2 + (mean(c(locs$Y[1:2], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[3:13]))^2)


summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}


wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt <=2, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 2 &
                              telem_pt <= 17, 2, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 17, 3, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 36
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

sqrt((mean(c(locs$X[1], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[2:47]))^2 + (mean(c(locs$Y[1], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[2:47]))^2)


summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt==2, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 2, 2, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 37
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}


wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt < as_date('2014-06-25'), 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              dt > as_date('2014-06-25'), 2, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 38
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

sqrt((mean(c(locs$X[1], capLocs$capX[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$X[2:23]))^2 + (mean(c(locs$Y[1], capLocs$capY[which(capLocs$bird_id==birdOrder$bird_id[bird])])) - mean(locs$Y[2:23]))^2)

summary(locs$distFromCap)

# locs = locs[2:23,]


shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}


wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt==1, 1, territory)) %>% 
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird] &
                              telem_pt > 1, 2, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)



####
bird = 39
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}


wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 40
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')

summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}


wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


####
bird = 41
####

locs = wothLocs %>% 
  filter(bird_id==birdOrder$bird_id[bird]) %>% 
  arrange(dt)%>% 
  mutate(order = seq(1,nrow(.)))

ggplot(locs, aes(x=X, y=Y))+
  geom_path()+
  geom_point()+
  theme_bw()+
  geom_point(data=capLocs[which(capLocs$bird_id==birdOrder$bird_id[bird]),], aes(x=capX, y=capY), color='red')


summary(locs$distFromCap)

shift = estDepDates$departure_dt[which(estDepDates$bird_id==birdOrder$bird_id[bird])]
if(!is.na(shift)){
  tmp1 = max(locs$dt[which(locs$dt < shift)])
  tmp2 = min(locs$dt[which(locs$dt > shift)])
  tmp3 = difftime(tmp2, tmp1)
  tmp4 = tmp1 + tmp3/2
  shift = as.numeric(difftime(tmp4, min(locs$dt), units='hours'))
} else {
  shift = (as.numeric(difftime(max(locs$dt), min(locs$dt), units='hours')))/2
}

try({p.m0 = with(locs, quickfit(T, X, Y, dt=shift))})
try({test = with(locs, estimate_shift(T=T, X=X, Y=Y, n.clust=2, p.m0=p.m0, method='like', model='MOU'))}, silent=T)
try({output = test_rangeshift2(test, verbose=F)}, silent=T)
try({results1 = rbind(results1, output$aic.table %>% mutate(bird_id = birdOrder$bird_id[bird]))}, silent=T)
try({results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                           'lrt' = output$lrt,
                                           'df' = output$df,
                                           'pValue' = output$p.value))}, silent=T)
try({results3 = rbind(results3, test$p.CI %>% 
                        mutate(variable = row.names(.),
                               bird_id = birdOrder$bird_id[bird]))}, silent=T)
if(!exists('output')){
  results2 = rbind(results2, data.frame('bird_id' = birdOrder$bird_id[bird],
                                        'lrt' = NA,
                                        'df' = NA,
                                        'pValue' = NA))
}

wothLocs = wothLocs %>%
  mutate(territory = ifelse(bird_id==birdOrder$bird_id[bird], 1, territory))



try({rm(locs)}, silent=T)
try({rm(test)}, silent=T)
try({rm(output)}, silent=T)


tmp = results2 %>% 
  arrange(pValue)

tmp1 = wothLocs %>% 
  select(bird_id, territory) %>% 
  unique() %>% 
  filter(territory > 1) %>% 
  arrange(bird_id)




#----------------------------------------------------------------------
# Reading in and formatting telemtry data for movement modeling
#----------------------------------------------------------------------
woth.locs <- read.csv(text = getURL("https://raw.githubusercontent.com/ValenteJJ/OccupancyClosureMS/main/wothLocations.csv"), header=T, as.is=T)

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


#Saving the output of the modeling analysis so we do not have to re-run it before
#every simulation
save(list=ls(), file="SimulationStart.RData")



#--------------------------------------------------------------------------
#Wood Thrush simulations and sampling
#--------------------------------------------------------------------------

rm(list=ls())

load(url("https://github.com/ValenteJJ/OccupancyClosureMS/blob/main/SimulationStart.RData?raw=true"))

#This function establishes territories of the birds and
#simulates their positions minute-by-minute

simLocations <- function(regionWidth = 6000, #m - creates a default of 36 km2 sampling region
                         regionHeight = 6000, #m
                         sampleWidth = regionWidth - 1000, #But we're only going to sample from the 25 km2 heart of the region
                         sampleHeight = regionHeight - 1000,
                         nDays = 42, #6 weeks of sampling
                         hours = 5, #Number of hours during which to simulate minute-by-minute bird locations
                         density = 0.2, #Males/ha
                         gridCellSize = 100){ #How far apart territory centers must be
  
  #Some housekeeping
  nBirds = round((regionWidth * regionHeight * density)/10000)
  times = seq(0, hours*60*60, 60)
  regionXmin = -regionWidth/2
  regionXmax = regionWidth/2
  regionYmin = -regionHeight/2
  regionYmax = regionHeight/2
  sampleXmin = -sampleWidth/2
  sampleXmax = sampleWidth/2
  sampleYmin = -sampleHeight/2
  sampleYmax = sampleHeight/2
  region = SpatialPolygons(list(Polygons(list(Polygon(cbind(c(regionXmin, regionXmax, regionXmax, regionXmin), c(regionYmax, regionYmax, regionYmin, regionYmin)))), 1)))
  sampleArea = SpatialPolygons(list(Polygons(list(Polygon(cbind(c(sampleXmin, sampleXmax, sampleXmax, sampleXmin), c(sampleYmax, sampleYmax, sampleYmin, sampleYmin)))), 1)))
  grid = SpatialPoints(makegrid(x=sampleArea, cellsize=gridCellSize))
  shiftProb = plogis(rnorm(1, mean = qlogis(0.009920322), sd = 0.23)) #This is based on estimates from our logistic exposure model
  
  for(d in 1:nDays){
    
    #On day 1...
    if(d==1){
      
      #Establish centers of initial territories
      tmp = sample(1:length(grid), nBirds)
      terrLocations = grid[tmp]
      availableLocations = grid[-tmp]
      
      #deltaX and deltaY represent matrices that have nBirds rows and nDays columns, and the values represent
      #the center of the X or Y coordinate for the center of the bird's territory on any particular day.
      #On day 1, we set these values to be all the same, but they will change later if the bird changes it's
      #territory location.
      deltaX = matrix(terrLocations@coords[,1], nrow=nBirds, ncol=nDays)
      deltaY = matrix(terrLocations@coords[,2], nrow=nBirds, ncol=nDays)
      
      #Select a movement model for each individual bird
      moveModels = sample(1:length(modelList), size=nBirds, replace=T)
      
      #When we simulate locations of birds, those simulated points will be located in the geographical space where
      #the data were originally collected. Here we are recording the centers of those territories so we can move
      #the center of the bird's territory to where we want it to be located in our simulated region.
      adjustX = centerX[moveModels]
      adjustY = centerY[moveModels]
      
      
      for(b in 1:nBirds){
        
        #Simulate bird locations for each 1 minute interval from the movement model
        tmpPts = data.frame(simulate(modelList[[moveModels[b]]], t=times)) %>% 
          select(t, x, y) %>% 
          mutate(day = d,
                 bird = b,
                 x = (x - adjustX[b]) + deltaX[b,d],
                 y = (y - adjustY[b]) + deltaY[b,d])
        
        #Store those locations
        if(b==1){
          tmpBirdLocs = tmpPts
        } else{
          tmpBirdLocs = rbind(tmpBirdLocs, tmpPts)
        }
      }
      birdLocs = tmpBirdLocs
    }
    
    #On all other days...
    if(d > 1){
      for(b in 1:nBirds){
        
        #If it does abandon, delete its territory, select a new territory
        if(rbinom(1, 1, shiftProb)==1){
          oldTerr = terrLocations[b]
          newTerrNum = sample(1:length(availableLocations), 1)
          tmp = spRbind(terrLocations[1:(b-1)], availableLocations[newTerrNum])
          if(b < nBirds){
            tmp = spRbind(tmp, terrLocations[(b+1):length(terrLocations)])
            terrLocations = tmp
          } else{
            terrLocations = tmp
          }
          availableLocations = spRbind(availableLocations[-newTerrNum], oldTerr)
          deltaX[b,(d:nDays)] = terrLocations@coords[b,1]
          deltaY[b,(d:nDays)] = terrLocations@coords[b,2]
        }
        
        #Simulate bird locations for each 1 minute interval from the movement model for the other days
        tmpPts = data.frame(simulate(modelList[[moveModels[b]]], t=times)) %>% 
          select(t, x, y) %>% 
          mutate(day = d,
                 bird = b,
                 x = (x - adjustX[b]) + deltaX[b,d],
                 y = (y - adjustY[b]) + deltaY[b,d])
        
        #Store those locations
        if(b==1){
          tmpBirdLocs = tmpPts
        } else{
          tmpBirdLocs = rbind(tmpBirdLocs, tmpPts)
        }
      }
      birdLocs = rbind(birdLocs, tmpBirdLocs)
    }
  }
  return(birdLocs)
}


#This function establishes the sampling protocol, samples birds using that protocol,
#fits an occupancy model to those data, then compares the occupancy estimate with
#the true occupancy status of the sampled sites based on 3 definitions of occupancy:
#instantaneous occupancy, daily occupancy, and seasonal occupancy.
options(dplyr.summarise.inform=F)

occAnalyses = function(circleData = NULL, radius = NULL, placement = NULL, occData=NULL, nMins=NULL, intervalLength=NULL, nSurveys=NULL, density=NULL){
  yData = data.frame(sampleTimes) %>% 
    arrange(sampleOrder) %>% 
    left_join(data.frame(circleData), by='sampleOrder') %>% 
    rename('samplePoint' = 'pointNum') %>% 
    left_join(occData, by=c('samplePoint', 'day')) %>% 
    mutate(t=t/60) %>% 
    filter(t > startTime & t < endTime) %>% 
    group_by(surveyLength, interval, numSurveys, x, y, samplePoint, sampleOrder, survey, day, startTime, endTime) %>% 
    summarise(occupiedMins = sum(occupied),
              occupied = max(occupied)) %>% 
    ungroup() %>% 
    mutate(pD = 1-(0.7)^occupiedMins) %>% # probability of detecting in any minute is 0.3
    mutate(unif = runif(nrow(.))) %>% 
    mutate(yData = ifelse(unif < pD, 1, 0)) %>% 
    filter(surveyLength==nMins & interval==intervalLength & numSurveys==nSurveys) %>% 
    pivot_wider(id_cols=samplePoint, names_from=survey, values_from=yData) %>% 
    arrange(samplePoint) %>% 
    select(-samplePoint)
  
  res = occu(~1~1, unmarkedFrameOccu(yData))
  
  #Summarizing "occupancy" at different time scales
  instOcc = occData %>% 
    group_by(day, t) %>% 
    summarise(points = n(), occupied = mean(occupied))
  dailyOcc = occData %>% 
    group_by(day, samplePoint) %>% 
    summarise(occupied = max(occupied)) %>% 
    ungroup() %>% 
    group_by(day) %>% 
    summarise(points = n(), occupied = mean(occupied))
  seasonOcc = occData %>% 
    group_by(samplePoint) %>% 
    summarise(occupied = max(occupied)) %>% 
    ungroup() %>% 
    summarise(points = n(), occupied = mean(occupied))
  
  tmp1 = data.frame(t(coef(res))) %>%
    setNames(c('psi', 'p')) %>% 
    mutate(surveyLength = nMins,
           intervalLength = intervalLength,
           nSurveys = nSurveys,
           radius = radius,
           placement=placement) %>% 
    mutate(instOcc = mean(instOcc$occupied),
           dailyOcc = mean(dailyOcc$occupied),
           seasonOcc = seasonOcc$occupied,
           density = density,
           iteration = i)
  
  return(tmp1)
  
}


#Creating a standardized sampling grid with points located 500 m apart
#Note that the points for the random sampling will be generated inside
#the for-loop
tmp = expand.grid("x" = seq(-2250, 2250, 500),
                  "y" = seq(-2250, 2250, 500)) %>% 
  arrange(y, x) %>% 
  mutate(pointNum = 1:nrow(.))
systPoints = SpatialPointsDataFrame(tmp, data=tmp)
systCircles100 = rgeos::gBuffer(systPoints, byid=T, width=100)
systCircles75 = rgeos::gBuffer(systPoints, byid=T, width=75)
systCircles50 = rgeos::gBuffer(systPoints, byid=T, width=50)

#Reading in the sampling times for the point count stations
sampleTimes = rbind(read.xlsx('https://github.com/ValenteJJ/OccupancyClosureMS/blob/main/Appendix%203%20-%20Simulated%20sampling%20schedule.xlsx?raw=true', sheet='Interval0'),
                    read.xlsx('https://github.com/ValenteJJ/OccupancyClosureMS/blob/main/Appendix%203%20-%20Simulated%20sampling%20schedule.xlsx?raw=true', sheet='Interval24Hours'),
                    read.xlsx('https://github.com/ValenteJJ/OccupancyClosureMS/blob/main/Appendix%203%20-%20Simulated%20sampling%20schedule.xlsx?raw=true', sheet='Interval10Days'))


#The 3 simulation densities
dens1 = 0.2
dens2 = 0.1
dens3 = 0.05


### CONDUCTING THE ITERATIVE SIMULATIONS ###


for(i in 1:2){
  
  print(i)
  birdPositions = simLocations(density = dens1)
  birdPositions2 = data.frame('bird' = sample(unique(birdPositions$bird), size = round((dens2/dens1)*length(unique(birdPositions$bird))))) %>%
    left_join(birdPositions, by='bird')
  birdPositions3 = data.frame('bird' = sample(unique(birdPositions$bird), size = round((dens3/dens1)*length(unique(birdPositions$bird))))) %>%
    left_join(birdPositions, by='bird')
  
  tmp = data.frame('x' = runif(100, min=-2250, max=2250),
                   'y' = runif(100, min=-2250, max=2250)) %>% 
    mutate(xQuadrat = factor(cut_interval(x, 10), labels=1:10)) %>% 
    mutate(yQuadrat = factor(cut_interval(y, 10), labels=1:10)) %>% 
    mutate(quadrat = as.integer(paste(as.character(yQuadrat), as.character(xQuadrat), sep=''))) %>% 
    mutate(quadrat = factor(as.character(quadrat), levels=as.character(c(11:19, 110, 210, 29:21, 31:39, 310, 410, 49:41, 51:59, 510, 610, 69:61, 71:79, 710, 810, 89:81, 91:99, 910, 1010, 109:101)))) %>% 
    arrange(quadrat, x, y) %>%
    mutate(xQuadrat = as.integer(as.character(xQuadrat)),
           yQuadrat = as.integer(as.character(yQuadrat)),
           quadrat = as.integer(as.character(quadrat))) %>% 
    mutate(pointNum = 1:nrow(.))
  randPoints = SpatialPointsDataFrame(tmp, data=tmp)
  randCircles100 = rgeos::gBuffer(randPoints, byid=T, width=100)
  randCircles75 = rgeos::gBuffer(randPoints, byid=T, width=75)
  randCircles50 = rgeos::gBuffer(randPoints, byid=T, width=50)
  
  
  birdPositionsSp = SpatialPointsDataFrame(birdPositions[,c('x', 'y')], data=birdPositions)
  birdPositions2Sp = SpatialPointsDataFrame(birdPositions2[,c('x', 'y')], data=birdPositions2)
  birdPositions3Sp = SpatialPointsDataFrame(birdPositions3[,c('x', 'y')], data=birdPositions3)
  
  
  
  
  #Simulations for density 1
  occSitesSyst100 = over(birdPositionsSp, systCircles100)
  occSitesSyst75 = over(birdPositionsSp, systCircles75)
  occSitesSyst50 = over(birdPositionsSp, systCircles50)
  occSitesRand100 = over(birdPositionsSp, randCircles100)
  occSitesRand75 = over(birdPositionsSp, randCircles75)
  occSitesRand50 = over(birdPositionsSp, randCircles50)
  
  occSitesSyst100 = birdPositions %>% 
    mutate(samplePoint = occSitesSyst100$pointNum) %>% 
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>% 
    unique() %>% 
    mutate(occupied = 1) %>% 
    full_join(expand.grid('t' = unique(birdPositions$t),
                          'day' = unique(birdPositions$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>% 
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>% 
    arrange(samplePoint, day, t)
  
  occSitesSyst75 = birdPositions %>% 
    mutate(samplePoint = occSitesSyst75$pointNum) %>% 
    filter(!is.na(samplePoint)) %>% 
    select(t, day, samplePoint) %>% 
    unique() %>% 
    mutate(occupied = 1) %>% 
    full_join(expand.grid('t' = unique(birdPositions$t),
                          'day' = unique(birdPositions$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>% 
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>% 
    arrange(samplePoint, day, t)
  
  occSitesSyst50 = birdPositions %>% 
    mutate(samplePoint = occSitesSyst50$pointNum) %>% 
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>% 
    unique() %>% 
    mutate(occupied = 1) %>% 
    full_join(expand.grid('t' = unique(birdPositions$t),
                          'day' = unique(birdPositions$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>% 
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>% 
    arrange(samplePoint, day, t)
  
  occSitesRand100 = birdPositions %>% 
    mutate(samplePoint = occSitesRand100$pointNum) %>% 
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>% 
    unique() %>% 
    mutate(occupied = 1) %>% 
    full_join(expand.grid('t' = unique(birdPositions$t),
                          'day' = unique(birdPositions$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>% 
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>% 
    arrange(samplePoint, day, t)
  
  occSitesRand75 = birdPositions %>% 
    mutate(samplePoint = occSitesRand75$pointNum) %>% 
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>% 
    unique() %>% 
    mutate(occupied = 1) %>% 
    full_join(expand.grid('t' = unique(birdPositions$t),
                          'day' = unique(birdPositions$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>% 
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>% 
    arrange(samplePoint, day, t)
  
  occSitesRand50 = birdPositions %>% 
    mutate(samplePoint = occSitesRand50$pointNum) %>% 
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>% 
    unique() %>% 
    mutate(occupied = 1) %>% 
    full_join(expand.grid('t' = unique(birdPositions$t),
                          'day' = unique(birdPositions$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>% 
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>% 
    arrange(samplePoint, day, t)
  
  # #Setting the sampling order for systematically placed points
  systCircles100$sampleOrder = 
    systCircles75$sampleOrder =
    systCircles50$sampleOrder = c(1:10, 20:11, 21:30, 40:31, 41:50, 41:50, 40:31, 21:30, 20:11, 1:10)
  randCircles100$sampleOrder =
    randCircles75$sampleOrder =
    randCircles50$sampleOrder = c(1:50, 50:1)
  
  
  tmpRes1 = rbind(occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 2, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 4, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 2, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 4, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 2, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 4, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 2, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 4, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 2, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 4, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 2, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 4, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 3, dens1),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 3, dens1),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 3, dens1),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 3, dens1),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 3, dens1),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 3, dens1),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 3, dens1))
  
  #Simulations for density 2
  occSitesSyst100 = over(birdPositions2Sp, systCircles100)
  occSitesSyst75 = over(birdPositions2Sp, systCircles75)
  occSitesSyst50 = over(birdPositions2Sp, systCircles50)
  occSitesRand100 = over(birdPositions2Sp, randCircles100)
  occSitesRand75 = over(birdPositions2Sp, randCircles75)
  occSitesRand50 = over(birdPositions2Sp, randCircles50)

  occSitesSyst100 = birdPositions2 %>%
    mutate(samplePoint = occSitesSyst100$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions2$t),
                          'day' = unique(birdPositions2$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesSyst75 = birdPositions2 %>%
    mutate(samplePoint = occSitesSyst75$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions2$t),
                          'day' = unique(birdPositions2$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesSyst50 = birdPositions2 %>%
    mutate(samplePoint = occSitesSyst50$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions2$t),
                          'day' = unique(birdPositions2$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesRand100 = birdPositions2 %>%
    mutate(samplePoint = occSitesRand100$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions2$t),
                          'day' = unique(birdPositions2$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesRand75 = birdPositions2 %>%
    mutate(samplePoint = occSitesRand75$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions2$t),
                          'day' = unique(birdPositions2$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesRand50 = birdPositions2 %>%
    mutate(samplePoint = occSitesRand50$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions2$t),
                          'day' = unique(birdPositions2$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  tmpRes2 = rbind(occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 2, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 4, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 2, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 4, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 2, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 4, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 2, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 4, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 2, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 4, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 2, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 4, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 3, dens2),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 3, dens2),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 3, dens2),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 3, dens2),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 3, dens2),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 3, dens2),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 3, dens2))
  
  
  
#Simulations for density 3
  occSitesSyst100 = over(birdPositions3Sp, systCircles100)
  occSitesSyst75 = over(birdPositions3Sp, systCircles75)
  occSitesSyst50 = over(birdPositions3Sp, systCircles50)
  occSitesRand100 = over(birdPositions3Sp, randCircles100)
  occSitesRand75 = over(birdPositions3Sp, randCircles75)
  occSitesRand50 = over(birdPositions3Sp, randCircles50)

  occSitesSyst100 = birdPositions3 %>%
    mutate(samplePoint = occSitesSyst100$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions3$t),
                          'day' = unique(birdPositions3$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesSyst75 = birdPositions3 %>%
    mutate(samplePoint = occSitesSyst75$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions3$t),
                          'day' = unique(birdPositions3$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesSyst50 = birdPositions3 %>%
    mutate(samplePoint = occSitesSyst50$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions3$t),
                          'day' = unique(birdPositions3$day),
                          'samplePoint' = unique(systPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesRand100 = birdPositions3 %>%
    mutate(samplePoint = occSitesRand100$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions3$t),
                          'day' = unique(birdPositions3$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesRand75 = birdPositions3 %>%
    mutate(samplePoint = occSitesRand75$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions3$t),
                          'day' = unique(birdPositions3$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  occSitesRand50 = birdPositions3 %>%
    mutate(samplePoint = occSitesRand50$pointNum) %>%
    filter(!is.na(samplePoint)) %>%
    select(t, day, samplePoint) %>%
    unique() %>%
    mutate(occupied = 1) %>%
    full_join(expand.grid('t' = unique(birdPositions3$t),
                          'day' = unique(birdPositions3$day),
                          'samplePoint' = unique(randPoints@data$pointNum)),
              by=c('t', 'day', 'samplePoint')) %>%
    mutate(occupied = ifelse(is.na(occupied), 0, occupied)) %>%
    arrange(samplePoint, day, t)

  tmpRes3 = rbind(occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 2, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 4, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 2, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 4, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 2, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 4, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 2, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 4, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 2, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 4, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 2, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 4, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, 'none', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '24Hours', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 30, '10Days', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, 'none', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '24Hours', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 10, '10Days', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, 'none', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '24Hours', 3, dens3),
                  occAnalyses(randCircles100, 100, 'random', occSitesRand100, 3, '10Days', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, 'none', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '24Hours', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 30, '10Days', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, 'none', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '24Hours', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 10, '10Days', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, 'none', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '24Hours', 3, dens3),
                  occAnalyses(randCircles75, 75, 'random', occSitesRand75, 3, '10Days', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, 'none', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '24Hours', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 30, '10Days', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, 'none', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '24Hours', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 10, '10Days', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, 'none', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '24Hours', 3, dens3),
                  occAnalyses(randCircles50, 50, 'random', occSitesRand50, 3, '10Days', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, 'none', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '24Hours', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 30, '10Days', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, 'none', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '24Hours', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 10, '10Days', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, 'none', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '24Hours', 3, dens3),
                  occAnalyses(systCircles100, 100, 'systematic', occSitesSyst100, 3, '10Days', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, 'none', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '24Hours', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 30, '10Days', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, 'none', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '24Hours', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 10, '10Days', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, 'none', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '24Hours', 3, dens3),
                  occAnalyses(systCircles75, 75, 'systematic', occSitesSyst75, 3, '10Days', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, 'none', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '24Hours', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 30, '10Days', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, 'none', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '24Hours', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 10, '10Days', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, 'none', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '24Hours', 3, dens3),
                  occAnalyses(systCircles50, 50, 'systematic', occSitesSyst50, 3, '10Days', 3, dens3))
  
  results = tmpRes1
  filename = paste(paste('results', i, sep=''), '.csv', sep='')
  write.csv(results, file=filename)
}

#----------------------------------------------------------------------