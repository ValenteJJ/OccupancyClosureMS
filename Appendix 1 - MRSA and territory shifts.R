
library(groundhog)
groundhog.library('tidyverse', '2023-11-02')
groundhog.library('marcher', '2023-11-02')
groundhog.library('RCurl', '2023-11-02')


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
wothLocs = read.csv(text = getURL("https://raw.githubusercontent.com/ValenteJJ/OccupancyClosureMS/main/wothLocations.csv")) %>% 
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