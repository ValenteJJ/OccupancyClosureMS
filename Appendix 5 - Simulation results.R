
library(groundhog)
groundhog.library('RCurl', '2023-11-02')
groundhog.library('tidyverse', '2023-11-02')
groundhog.library('cowplot', '2023-11-02')

rm(list=ls())

#----------------------------------------------------------------------
#Load the results from the simulation
#----------------------------------------------------------------------

load(url("https://github.com/ValenteJJ/OccupancyClosureMS/blob/main/simulationResults.RData?raw=true"))

#Metadata
#psi - model-estimated occupancy
#p - model-estimated detection probability
#label - a unique label associated with the sampling protocol
#sureyLength - length of each survey in the protocol
#intervalLength - time between surveys in the protocol
#nSurveys - number of surveys in the protocol
#radius - radius of sampling sites in the protocol
#placement - how point count stations were placed (systematically or randomly)
#instOcc - true value of instantantaneous occupancy from the simulation
#dailyOcc - true value of daily occupancy from the simulation
#seasonOcc - true value of seasonal occupancy from the simulation
#density - density of male Wood Thrush (birds/ha) in the simulation
#iteration - Unique number distinguishing each of the 1000 unique simulations for each protocol
#instOccBias - the difference between the model-estimated psi and instOcc
#dailyOccBias - the difference between the model-estimated psi and dailyOcc
#seasonOccBias - the difference between the model-estimated psi and seasonOcc


#----------------------------------------------------------------------
# Factors affecting occupancy estimates (density = 0.05)
#----------------------------------------------------------------------

#RADIUS
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, placement)) %>% 
  mutate(radius = recode(radius, '50m' = '50 m', '75m' = '75 m', '100m' = '100 m'))



a = ggplot(tmp, aes(x=radius, y=psiEst, fill=radius))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0, 1)


#PLACEMENT
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, radius)) %>% 
  mutate(placement = recode(placement, 'rand'='Random', 'syst'='Systematic'))

b = ggplot(tmp, aes(x=placement, y=psiEst, fill=placement))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)


#nSURVEYS
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, placement, radius)) %>% 
  mutate(nSurveys = recode(nSurveys, '2survs'='2 survs', '3survs' = '3 survs', '4survs' = '4 survs'))

c = ggplot(tmp, aes(x=nSurveys, y=psiEst, fill=nSurveys))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

#SURVEYLENGTH
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, intervalLength, placement, radius)) %>% 
  mutate(surveyLength = recode(surveyLength, '3min' = '3 min', '10min' = '10 min', '30min' = '30 min'))

d = ggplot(tmp, aes(x=surveyLength, y=psiEst, fill=surveyLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

#INTERVALLENGTH
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, surveyLength, placement, radius)) %>% 
  mutate(intervalLength = factor(as.character(intervalLength), levels=c('none', '24Hours', '10Days'))) %>% 
  mutate(intervalLength = recode(intervalLength, 'none' = 'None', '24Hours' = '24 hours', '10Days' = '10 days'))


e = ggplot(tmp, aes(x=intervalLength, y=psiEst, fill=intervalLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

f = plot_grid(c, d, e, a, b, ncol=1)


#RADIUS Variance
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, placement)) %>% 
  mutate(radius = recode(radius, '50m' = '50 m', '75m' = '75 m', '100m' = '100 m'))


aa = ggplot(tmp, aes(x=radius, y=varEst, fill=radius))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


#PLACEMENT
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, radius)) %>% 
  mutate(placement = recode(placement, 'rand'='Random', 'syst'='Systematic'))

bb = ggplot(tmp, aes(x=placement, y=varEst, fill=placement))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


#nSURVEYS
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, placement, radius)) %>% 
  mutate(nSurveys = recode(nSurveys, '2survs'='2 survs', '3survs' = '3 survs', '4survs' = '4 survs'))

cc = ggplot(tmp, aes(x=nSurveys, y=varEst, fill=nSurveys))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())

#SURVEYLENGTH
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, intervalLength, placement, radius)) %>% 
  mutate(surveyLength = recode(surveyLength, '3min' = '3 min', '10min' = '10 min', '30min' = '30 min'))

dd = ggplot(tmp, aes(x=surveyLength, y=varEst, fill=surveyLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())

#INTERVALLENGTH
tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, surveyLength, placement, radius)) %>% 
  mutate(intervalLength = factor(as.character(intervalLength), levels=c('none', '24Hours', '10Days'))) %>% 
  mutate(intervalLength = recode(intervalLength, 'none' = 'None', '24Hours' = '24 hours', '10Days' = '10 days'))

ee = ggplot(tmp, aes(x=intervalLength, y=varEst, fill=intervalLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


ff = plot_grid(cc, dd, ee, aa, bb, ncol=1)

fff = plot_grid(ggdraw()+draw_label('Surveys per site', angle=90, size=8),
                ggdraw()+draw_label('Survey length', angle=90, size=8),
                ggdraw()+draw_label('Between-survey interval', angle=90, size=8),
                ggdraw()+draw_label('Survey site radius', angle=90, size=8),
                ggdraw()+draw_label('Site placement', angle=90, size=8), ncol=1)

bottom = plot_grid(fff, f, ff, rel_widths=c(0.05, 0.95, 0.95), nrow=1)

top = plot_grid(NULL,
                ggdraw()+draw_label('(a) Mean occupancy estimates', size=11),
                ggdraw()+draw_label('(b) Variance in occupancy estimates', size=11),
                rel_widths=c(0.05, 0.95, 0.95), nrow=1)

plot_grid(top, bottom, rel_heights=c(0.05, 0.95), ncol=1)

#----------------------------------------------------------------------
# Factors affecting occupancy estimates (density = 0.1)
#----------------------------------------------------------------------


#RADIUS
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, placement)) %>% 
  mutate(radius = recode(radius, '50m' = '50 m', '75m' = '75 m', '100m' = '100 m'))



a = ggplot(tmp, aes(x=radius, y=psiEst, fill=radius))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0, 1)


#PLACEMENT
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, radius)) %>% 
  mutate(placement = recode(placement, 'rand'='Random', 'syst'='Systematic'))

b = ggplot(tmp, aes(x=placement, y=psiEst, fill=placement))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)


#nSURVEYS
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, placement, radius)) %>% 
  mutate(nSurveys = recode(nSurveys, '2survs'='2 survs', '3survs' = '3 survs', '4survs' = '4 survs'))

c = ggplot(tmp, aes(x=nSurveys, y=psiEst, fill=nSurveys))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

#SURVEYLENGTH
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, intervalLength, placement, radius)) %>% 
  mutate(surveyLength = recode(surveyLength, '3min' = '3 min', '10min' = '10 min', '30min' = '30 min'))


d = ggplot(tmp, aes(x=surveyLength, y=psiEst, fill=surveyLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

#INTERVALLENGTH
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, surveyLength, placement, radius)) %>% 
  mutate(intervalLength = factor(as.character(intervalLength), levels=c('none', '24Hours', '10Days'))) %>% 
  mutate(intervalLength = recode(intervalLength, 'none' = 'None', '24Hours' = '24 hours', '10Days' = '10 days'))


e = ggplot(tmp, aes(x=intervalLength, y=psiEst, fill=intervalLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

f = plot_grid(c, d, e, a, b, ncol=1)



#RADIUS Variance
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, placement)) %>% 
  mutate(radius = recode(radius, '50m' = '50 m', '75m' = '75 m', '100m' = '100 m'))


aa = ggplot(tmp, aes(x=radius, y=varEst, fill=radius))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


#PLACEMENT
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, radius)) %>% 
  mutate(placement = recode(placement, 'rand'='Random', 'syst'='Systematic'))

bb = ggplot(tmp, aes(x=placement, y=varEst, fill=placement))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


#nSURVEYS
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, placement, radius)) %>% 
  mutate(nSurveys = recode(nSurveys, '2survs'='2 survs', '3survs' = '3 survs', '4survs' = '4 survs'))

cc = ggplot(tmp, aes(x=nSurveys, y=varEst, fill=nSurveys))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())

#SURVEYLENGTH
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, intervalLength, placement, radius)) %>% 
  mutate(surveyLength = recode(surveyLength, '3min' = '3 min', '10min' = '10 min', '30min' = '30 min'))


dd = ggplot(tmp, aes(x=surveyLength, y=varEst, fill=surveyLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())

#INTERVALLENGTH
tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, surveyLength, placement, radius)) %>% 
  mutate(intervalLength = factor(as.character(intervalLength), levels=c('none', '24Hours', '10Days'))) %>% 
  mutate(intervalLength = recode(intervalLength, 'none' = 'None', '24Hours' = '24 hours', '10Days' = '10 days'))

ee = ggplot(tmp, aes(x=intervalLength, y=varEst, fill=intervalLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


ff = plot_grid(cc, dd, ee, aa, bb, ncol=1)

fff = plot_grid(ggdraw()+draw_label('Surveys per site', angle=90, size=8),
                ggdraw()+draw_label('Survey length', angle=90, size=8),
                ggdraw()+draw_label('Between-survey interval', angle=90, size=8),
                ggdraw()+draw_label('Survey site radius', angle=90, size=8),
                ggdraw()+draw_label('Site placement', angle=90, size=8), ncol=1)

bottom = plot_grid(fff, f, ff, rel_widths=c(0.05, 0.95, 0.95), nrow=1)

top = plot_grid(NULL,
                ggdraw()+draw_label('(a) Mean occupancy estimates', size=11),
                ggdraw()+draw_label('(b) Variance in occupancy estimates', size=11),
                rel_widths=c(0.05, 0.95, 0.95), nrow=1)

plot_grid(top, bottom, rel_heights=c(0.05, 0.95), ncol=1)

#----------------------------------------------------------------------
# Factors affecting occupancy estimates (density = 0.2)
#----------------------------------------------------------------------

#RADIUS
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, placement)) %>% 
  mutate(radius = recode(radius, '50m' = '50 m', '75m' = '75 m', '100m' = '100 m'))



a = ggplot(tmp, aes(x=radius, y=psiEst, fill=radius))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0, 1)


#PLACEMENT
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, radius)) %>% 
  mutate(placement = recode(placement, 'rand'='Random', 'syst'='Systematic'))

b = ggplot(tmp, aes(x=placement, y=psiEst, fill=placement))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)


#nSURVEYS
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, placement, radius)) %>% 
  mutate(nSurveys = recode(nSurveys, '2survs'='2 survs', '3survs' = '3 survs', '4survs' = '4 survs'))

c = ggplot(tmp, aes(x=nSurveys, y=psiEst, fill=nSurveys))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

#SURVEYLENGTH
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, intervalLength, placement, radius)) %>% 
  mutate(surveyLength = recode(surveyLength, '3min' = '3 min', '10min' = '10 min', '30min' = '30 min'))

d = ggplot(tmp, aes(x=surveyLength, y=psiEst, fill=surveyLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)

#INTERVALLENGTH
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            psiEst = mean(psi),
            psiLcl = quantile(psi, probs=0.025),
            psiUcl = quantile(psi, probs = 0.975),
            pEst = mean(p),
            pLcl = quantile(p, probs=0.025),
            pUcl = quantile(p, probs=0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, surveyLength, placement, radius)) %>% 
  mutate(intervalLength = factor(as.character(intervalLength), levels=c('none', '24Hours', '10Days'))) %>% 
  mutate(intervalLength = recode(intervalLength, 'none' = 'None', '24Hours' = '24 hours', '10Days' = '10 days'))

e = ggplot(tmp, aes(x=intervalLength, y=psiEst, fill=intervalLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())+
  ylim(0,1)


f = plot_grid(c, d, e, a, b, ncol=1)


#RADIUS Variance
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, placement)) %>% 
  mutate(radius = recode(radius, '50m' = '50 m', '75m' = '75 m', '100m' = '100 m'))


aa = ggplot(tmp, aes(x=radius, y=varEst, fill=radius))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


#PLACEMENT
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys, radius)) %>% 
  mutate(placement = recode(placement, 'rand'='Random', 'syst'='Systematic'))

bb = ggplot(tmp, aes(x=placement, y=varEst, fill=placement))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


#nSURVEYS
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, placement, radius)) %>% 
  mutate(nSurveys = recode(nSurveys, '2survs'='2 survs', '3survs' = '3 survs', '4survs' = '4 survs'))

cc = ggplot(tmp, aes(x=nSurveys, y=varEst, fill=nSurveys))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())

#SURVEYLENGTH
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, intervalLength, placement, radius)) %>% 
  mutate(surveyLength = recode(surveyLength, '3min' = '3 min', '10min' = '10 min', '30min' = '30 min'))


dd = ggplot(tmp, aes(x=surveyLength, y=varEst, fill=surveyLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())

#INTERVALLENGTH
tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement) %>% 
  summarise(n = n(),
            varEst = var(psi)) %>% 
  ungroup() %>% 
  unite(col='label', c(nSurveys, surveyLength, placement, radius)) %>% 
  mutate(intervalLength = factor(as.character(intervalLength), levels=c('none', '24Hours', '10Days'))) %>% 
  mutate(intervalLength = recode(intervalLength, 'none' = 'None', '24Hours' = '24 hours', '10Days' = '10 days'))

ee = ggplot(tmp, aes(x=intervalLength, y=varEst, fill=intervalLength))+
  geom_violin()+
  geom_point(alpha=0.2, size=0.5)+
  geom_line(aes(group=label), alpha=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  ylab('')+
  scale_fill_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  xlab('')+
  theme(legend.position='none')+
  theme(title = element_text(size=8))+
  theme(axis.title = element_blank())


ff = plot_grid(cc, dd, ee, aa, bb, ncol=1)

fff = plot_grid(ggdraw()+draw_label('Surveys per site', angle=90, size=8),
                ggdraw()+draw_label('Survey length', angle=90, size=8),
                ggdraw()+draw_label('Between-survey interval', angle=90, size=8),
                ggdraw()+draw_label('Survey site radius', angle=90, size=8),
                ggdraw()+draw_label('Site placement', angle=90, size=8), ncol=1)

bottom = plot_grid(fff, f, ff, rel_widths=c(0.05, 0.95, 0.95), nrow=1)

top = plot_grid(NULL,
                ggdraw()+draw_label('(a) Mean occupancy estimates', size=11),
                ggdraw()+draw_label('(b) Variance in occupancy estimates', size=11),
                rel_widths=c(0.05, 0.95, 0.95), nrow=1)

plot_grid(top, bottom, rel_heights=c(0.05, 0.95), ncol=1)

#----------------------------------------------------------------------
#Bias by protocol (density = 0.05)
#----------------------------------------------------------------------

tmp = simResults %>% 
  filter(density=='0.05birds/ha') %>% 
  select(surveyLength, intervalLength, nSurveys, radius, placement, instOccBias, dailyOccBias, seasonOccBias) %>% 
  pivot_longer(cols=c(instOccBias, dailyOccBias, seasonOccBias), names_to='occDef', values_to='bias') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement, occDef) %>% 
  summarise(n = n(),
            biasEst = mean(bias),
            biasLcl = quantile(bias, probs=0.025),
            biasUcl = quantile(bias, probs = 0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys)) %>% 
  mutate(occDef = factor(occDef, levels=c('instOccBias', 'dailyOccBias', 'seasonOccBias'))) %>% 
  filter(placement=='syst')

tmp1 = tmp %>% 
  filter(occDef=='instOccBias') %>% 
  filter(radius=='100m') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder))

tmp2 = tmp %>% 
  filter(occDef=='instOccBias') %>% 
  mutate(label = factor(label, levels=unique(tmp1$label)))

a = ggplot(tmp2, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Instantaneous occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))


tmp3 = tmp %>% 
  filter(occDef=='dailyOccBias') %>% 
  filter(radius=='100m') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder))

tmp4 = tmp %>% 
  filter(occDef=='dailyOccBias') %>% 
  mutate(label = factor(label, levels=unique(tmp3$label)))

b = ggplot(tmp4, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Daily occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))


tmp5 = tmp %>% 
  filter(occDef=='seasonOccBias') %>% 
  filter(radius=='100m') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder))

tmp6 = tmp %>% 
  filter(occDef=='seasonOccBias') %>% 
  mutate(label = factor(label, levels=unique(tmp5$label)))

c = ggplot(tmp6, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Seasonal occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))

lgnd = ggplot(tmp3, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(legend.position = 'top')

a = a+theme(plot.title=element_text(hjust=0.95))
b = b+theme(plot.title=element_text(hjust=1.9))
c = c+theme(plot.title=element_text(hjust=1.3))


d = plot_grid(a, b, c, nrow=1)

d

e = plot_grid(d, ggdraw()+draw_label('Bias in occupancy estimate (95% CI)', size=11),
              rel_heights=c(0.95, 0.05), ncol=1)

f = plot_grid(ggdraw()+draw_label('Protocol (survey length _ between survey interval _ number of surveys)', angle=90, size=11), e, rel_widths=c(0.05, 0.95))

plot_grid(get_legend(lgnd), f, ncol=1, rel_heights=c(0.05, 0.95))



#----------------------------------------------------------------------
#Bias by protocol (density = 0.1)
#----------------------------------------------------------------------

tmp = simResults %>% 
  filter(density=='0.1birds/ha') %>% 
  select(surveyLength, intervalLength, nSurveys, radius, placement, instOccBias, dailyOccBias, seasonOccBias) %>% 
  pivot_longer(cols=c(instOccBias, dailyOccBias, seasonOccBias), names_to='occDef', values_to='bias') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement, occDef) %>% 
  summarise(n = n(),
            biasEst = mean(bias),
            biasLcl = quantile(bias, probs=0.025),
            biasUcl = quantile(bias, probs = 0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys)) %>% 
  mutate(occDef = factor(occDef, levels=c('instOccBias', 'dailyOccBias', 'seasonOccBias'))) %>% 
  filter(placement=='syst')

tmp1 = tmp %>% 
  filter(occDef=='instOccBias') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder)) %>% 
  mutate(label = factor(label, levels=unique(label)))

a = ggplot(tmp1, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Instantaneous occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))


tmp2 = tmp %>% 
  filter(occDef=='dailyOccBias') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder)) %>% 
  mutate(label = factor(label, levels=unique(label)))

b = ggplot(tmp2, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Daily occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))


tmp3 = tmp %>% 
  filter(occDef=='seasonOccBias') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder)) %>%
  mutate(label = factor(label, levels=unique(label)))

c = ggplot(tmp3, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Seasonal occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))

lgnd = ggplot(tmp3, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(legend.position = 'top')

a = a+theme(plot.title=element_text(hjust=0.95))
b = b+theme(plot.title=element_text(hjust=1.9))
c = c+theme(plot.title=element_text(hjust=1.3))


d = plot_grid(a, b, c, nrow=1)

d

e = plot_grid(d, ggdraw()+draw_label('Bias in occupancy estimate (95% CI)', size=11),
              rel_heights=c(0.95, 0.05), ncol=1)

f = plot_grid(ggdraw()+draw_label('Protocol (survey length _ between surveys _ number of surveys)', angle=90, size=11), e, rel_widths=c(0.05, 0.95))

plot_grid(get_legend(lgnd), f, ncol=1, rel_heights=c(0.05, 0.95))


#----------------------------------------------------------------------
#Bias by protocol (density = 0.2)
#----------------------------------------------------------------------

tmp = simResults %>% 
  filter(density=='0.2birds/ha') %>% 
  select(surveyLength, intervalLength, nSurveys, radius, placement, instOccBias, dailyOccBias, seasonOccBias) %>% 
  pivot_longer(cols=c(instOccBias, dailyOccBias, seasonOccBias), names_to='occDef', values_to='bias') %>% 
  group_by(surveyLength, intervalLength, nSurveys, radius, placement, occDef) %>% 
  summarise(n = n(),
            biasEst = mean(bias),
            biasLcl = quantile(bias, probs=0.025),
            biasUcl = quantile(bias, probs = 0.975)) %>% 
  ungroup() %>% 
  unite(col='label', c(surveyLength, intervalLength, nSurveys)) %>% 
  mutate(occDef = factor(occDef, levels=c('instOccBias', 'dailyOccBias', 'seasonOccBias'))) %>% 
  filter(placement=='syst')

tmp1 = tmp %>% 
  filter(occDef=='instOccBias') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder)) %>% 
  mutate(label = factor(label, levels=unique(label)))

a = ggplot(tmp1, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Instantaneous occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))


tmp2 = tmp %>% 
  filter(occDef=='dailyOccBias') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder)) %>% 
  mutate(label = factor(label, levels=unique(label)))

b = ggplot(tmp2, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Daily occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))


tmp3 = tmp %>% 
  filter(occDef=='seasonOccBias') %>% 
  mutate(biasOrder = abs(biasEst)) %>% 
  arrange(desc(biasOrder)) %>%
  mutate(label = factor(label, levels=unique(label)))

c = ggplot(tmp3, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  geom_vline(xintercept=0, linetype='dashed')+
  ylab('Protocol (survey length _ between surveys _ number of surveys)')+
  xlab('Bias in occupancy estimate (95% CI)')+
  theme(legend.position='none')+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(axis.text = element_text(size=8))+
  ylab('')+
  theme(axis.title.x = element_blank())+
  ggtitle('Seasonal occupancy')+
  theme(axis.text.x = element_text(angle=45, hjust=1))

lgnd = ggplot(tmp3, aes(x=biasEst, y=label, color=radius))+
  geom_errorbarh(aes(xmin=biasLcl, xmax=biasUcl), position=position_dodge(width=0.5), height=0)+
  geom_point(position=position_dodge(width=0.5), size=0.5)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  guides(color=guide_legend(title="Site radius"))+
  scale_color_manual(values=c('#E69F00', '#56B4E9', '#009E73'))+
  theme(legend.position = 'top')

a = a+theme(plot.title=element_text(hjust=0.95))
b = b+theme(plot.title=element_text(hjust=1.9))
c = c+theme(plot.title=element_text(hjust=1.3))


d = plot_grid(a, b, c, nrow=1)

d

e = plot_grid(d, ggdraw()+draw_label('Bias in occupancy estimate (95% CI)', size=11),
              rel_heights=c(0.95, 0.05), ncol=1)

f = plot_grid(ggdraw()+draw_label('Protocol (survey length _ between surveys _ number of surveys)', angle=90, size=11), e, rel_widths=c(0.05, 0.95))

plot_grid(get_legend(lgnd), f, ncol=1, rel_heights=c(0.05, 0.95))


#----------------------------------------------------------------------

