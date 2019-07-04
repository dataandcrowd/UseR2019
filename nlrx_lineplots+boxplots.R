library(tidyverse)
files  <- list.files(pattern= "Gangnam_.*.csv")
tables <- lapply(files, read.csv, header = TRUE)#, fileEncoding="CP949", encoding="UTF-8") # MacOSX
gn <- do.call(rbind, tables)

files  <- list.files(pattern= "Gwanak_.*.csv")
tables <- lapply(files, read.csv, header = TRUE)#, fileEncoding="CP949", encoding="UTF-8") # MacOSX
gw <- do.call(rbind, tables)
gn <- gn[,-1]
gw <- gw[,-1]

gn.col.names <- c("iteration", "scenario","AC","percent",	"ticks",
                  "riskpop", "d_sinsa", "d_nonhyun1", "d_nonhyun2",
                  "d_samsung1", "d_samsung2","d_daechi1","d_daechi4","d_yeoksam1",
                  "d_yeoksam2","d_dogok1","d_dogok2","d_gaepo1","d_gaepo4",
                  "d_ilwon","d_ilwon1","d_ilwon2","d_suseo", "d_ap","d_chungdam",
                  "d_daechi2","d_gaepo2","d_segok",
                  "age_u15","age_btw1564","age_ov65","edu_high","edu_low")

gw.col.names <- c("iteration", "scenario","AC",	"percent","ticks",
                  "riskpop", "d_boramae", "d_chnim", "d_hengun", "d_inheon", "d_nak", 
                  "d_jung-ang", "d_namhyeon", "d_seowon", "d_sinwon", "d_seorim", "d_sinsa", 
                  "d_sillim", "d_nanhyang", "d_jowon", "d_daehak", "d_euncheon", "d_sunghyun", 
                  "d_chungryong", "d_nangok", "d_samsung", "d_miseong", 
                  "age_u15","age_btw1564","age_ov65","edu_high","edu_low")
colnames(gn) <- gn.col.names
colnames(gw) <- gw.col.names

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

#bau<- bind_rows(gn %>% filter(scenario == "BAU") %>%
#                  select(ticks, riskpop, AC, scenario, age_u15,age_btw1564,age_ov65,edu_high,edu_low) %>% 
#                  mutate(District= "Gangnam"),
#                gw %>% filter(scenario == "BAU") %>% 
#                  select(ticks, riskpop, AC, scenario, age_u15,age_btw1564,age_ov65,edu_high,edu_low) %>% 
#                  mutate(District= "Gwanak")) %>% 
#        group_by(District, scenario, AC, ticks) %>%
#        summarise_all(funs(mean, lo = lb, hi = ub))

#bau$AC[bau$AC == 100] <- "AC100"
#bau$AC[bau$AC == 150] <- "AC150"
#bau$AC[bau$AC == 200] <- "AC200"


#bau.risk <- bau %>%
#    reshape2::melt(id = c("ticks", "District", "scenario","AC"), variable.name = "type", value.name = "percent") 

#bau.risk %>% 
#  ggplot(aes(ticks, percent, group = District, colour = District)) + 
#  geom_line(size = 1) +
#  ylim(0,60) +
#  xlab("ticks") +
#  ylab("Percent of risk population(%)")  +
#  facet_grid(District ~ scenario + AC ) +
#  theme_bw() +
#  theme(legend.position = "bottom",
#        legend.title=element_text(size=12),
#        legend.text=element_text(size=12),
#        axis.text=element_text(size=12),
#        axis.title=element_text(size=12),
#        strip.text = element_text(size = 15))

#######
bau <- bind_rows(gn %>% filter(scenario == "BAU") %>%
                       select(ticks, riskpop, AC, scenario, age_u15,age_btw1564,age_ov65,edu_high,edu_low) %>% 
                       mutate(District= "Gangnam"),
                     gw %>% filter(scenario == "BAU") %>% 
                       select(ticks, riskpop, AC, scenario, age_u15,age_btw1564,age_ov65,edu_high,edu_low) %>% 
                       mutate(District= "Gwanak")) %>% 
  group_by(District, scenario, AC, ticks) %>%
  summarise_all(funs(mean, lo = lb, hi = ub)) %>% as.data.frame()


bau$AC[bau$AC == 100] <- "AC100"
bau$AC[bau$AC == 150] <- "AC150"
bau$AC[bau$AC == 200] <- "AC200"


risk0 <- bau %>% select(District:ticks, matches('riskpop')) %>% mutate(type = "%Risk.Total") %>% 
                 rename(mean = matches('mean'), low = matches('lo'), high = matches('hi'))
risk1 <- bau %>% select(District:ticks, matches('u15')) %>% mutate(type = "Age.u15") %>% 
                      rename(mean = matches('mean'), low = matches('lo'), high = matches('hi'))
risk2 <- bau %>% select(District:ticks, matches('btw1564')) %>% mutate(type = "Age.btw1564") %>% 
                      rename(mean = matches('mean'), low = matches('lo'), high = matches('hi'))
risk3 <- bau %>% select(District:ticks, matches('ov65')) %>% mutate(type = "Age.ov65") %>% 
                      rename(mean = matches('mean'), low = matches('lo'), high = matches('hi'))
risk4 <- bau %>% select(District:ticks, matches('edu_high')) %>% mutate(type = "Edu.hi")
risk5 <- bau %>% select(District:ticks, matches('edu_low'))  %>% mutate(type = "Edu.lo")
colnames(risk4)[5:7] <- c("mean", "low","high")
colnames(risk5)[5:7] <- c("mean", "low","high")
risk <- rbind(risk0, risk1, risk2, risk3, risk4, risk5)

risk %>% 
  ggplot(aes(ticks, mean)) + 
  geom_ribbon(aes(group = District, ymin = low, ymax = high), fill = "grey70", alpha = 0.5) +
  geom_line(aes(colour = District, group = District),size = 1) +
  facet_grid(type ~ scenario + AC ) +
  #ylim(0,30) +
  xlab("ticks") +
  ylab("Percent of risk population(%)")  +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 15))


######################
#Subdistrict analysis#
######################
#gn.dong <- gn %>% select(-c(riskpop, iteration, percent, age_u15, age_btw1564, age_ov65, edu_high, edu_low)) %>% 
#            group_by(scenario, AC, ticks) %>%
#            summarise_all(funs(mean))
#gw.dong <- gw %>% select(-c(riskpop, iteration, percent, age_u15, age_btw1564, age_ov65, edu_high, edu_low)) %>% 
#            group_by(scenario, AC, ticks) %>%
#            summarise_all(funs(mean))
#write.csv(gn.dong, "gn_dong.csv")
#write.csv(gw.dong, "gw_dong.csv")

####################
## Start from here 
## If you want to load the subdistricts
####################
library(gridExtra)
library(gghighlight)
library(tidyverse)
library(ggpubr)
library(directlabels)
library(tidyquant)
library(cowplot)

###########################
## Box plot: Read and clean
###########################

gn.dong <- gn %>% select(-c(riskpop, iteration, percent, age_u15, age_btw1564, age_ov65, edu_high, edu_low)) %>% 
  group_by(scenario, AC, ticks) 

gn.dong$AC[gn.dong$AC == 100] <- "AC100"
gn.dong$AC[gn.dong$AC == 150] <- "AC150"
gn.dong$AC[gn.dong$AC == 200] <- "AC200"


gn.box <- gn.dong  %>% filter(ticks == 8763) %>% 
  reshape2::melt(id = c("ticks","scenario","AC") , variable.name = "dongs", value.name = "percent") %>% select(-ticks)
gn.box$dongs <- gsub("d_", "", gn.box$dongs) # delete d_
gn.box <- gn.box %>%   
  mutate(abb = str_replace_all(dongs, c("sinsa" = "SI", "nonhyun1" ="NH1", "nonhyun2"="NH2", "samsung1" = "SA1",
                                        "samsung2" = "SA2", "daechi1"="DC1", "daechi4" = "DC4", "yeoksam1" = "YS1",
                                        "yeoksam2" = "YS2", "dogok1" = "DG1", "dogok2" = "DG2", "gaepo1" = "GP1", "gaepo4" = "GP4",
                                        "ilwon" = "IL", "ilwon1" = "IL1", "ilwon2" = "IL2", 
                                        "suseo" = "SS", "ap" = "AP", "chungdam" = "CD", "daechi2" = "DC2", "gaepo2" = "GP2",
                                        "segok" = "SG")))

gw.dong <- gw %>% select(-c(riskpop, iteration, percent, age_u15, age_btw1564, age_ov65, edu_high, edu_low)) %>% 
  group_by(scenario, AC, ticks) 

gw.dong$AC[gw.dong$AC == 100] <- "AC100"
gw.dong$AC[gw.dong$AC == 150] <- "AC150"
gw.dong$AC[gw.dong$AC == 200] <- "AC200"


gw.box <- gw.dong  %>% filter(ticks == 8763) %>% 
  reshape2::melt(id = c("ticks","scenario","AC") , variable.name = "dongs", value.name = "percent") %>% select(-ticks)
gw.box$dongs <- gsub("d_", "", gw.box$dongs) # delete d_
gw.box <- gw.box %>%   
  mutate(abb = str_replace_all(dongs,c("jung.ang" = "JA", "sunghyun" ="SH", "samsung"="SA", "euncheon" = "EC",
                                       "inheon" = "IH", "jowon"="JW", "miseong" = "DC4", "sinwon" = "SW",
                                       "seorim" = "SR", "sinsa" = "SI", "chungryong" = "CR", "sillim" = "SL", "seowon" = "SE",
                                       "nak" = "NK", "nanhyang" = "NH", "boramae" = "BO", "nangok"="NG",
                                       "daehak" = "DH", "namhyeon" = "NH", "chnim" = "CN", "hengun" = "HG")))

############################
## Line plot: read and clean
############################
gn.l <- read.csv("gn_dong.csv") %>% select(-X)
gw.l <- read.csv("gw_dong.csv") %>% select(-X)

gn.l$AC[gn.l$AC == 100] <- "AC100"
gn.l$AC[gn.l$AC == 150] <- "AC150"
gn.l$AC[gn.l$AC == 200] <- "AC200"

gw.l$AC[gw.l$AC == 100] <- "AC100"
gw.l$AC[gw.l$AC == 150] <- "AC150"
gw.l$AC[gw.l$AC == 200] <- "AC200"


gn.line <- gn.l  %>% 
  reshape2::melt(id = c("ticks","scenario","AC") , variable.name = "dongs", value.name = "percent")
gn.line$dongs <- gsub("d_", "", gn.line$dongs) # delete d_
gn.line <- gn.line %>%   
  mutate(abb = str_replace_all(dongs, c("sinsa" = "SI", "nonhyun1" ="NH1", "nonhyun2"="NH2", "samsung1" = "SA1",
                                           "samsung2" = "SA2", "daechi1"="DC1", "daechi4" = "DC4", "yeoksam1" = "YS1",
                                           "yeoksam2" = "YS2", "dogok1" = "DG1", "dogok2" = "DG2", "gaepo1" = "GP1", "gaepo4" = "GP4",
                                           "ilwon" = "IL", "ilwon1" = "IL1", "ilwon2" = "IL2", 
                                           "suseo" = "SS", "ap" = "AP", "chungdam" = "CD", "daechi2" = "DC2", "gaepo2" = "GP2",
                                           "segok" = "SG")))

gw.line <- gw.l  %>% 
  reshape2::melt(id = c("ticks","scenario","AC") , variable.name = "dongs", value.name = "percent")
gw.line$dongs <- gsub("d_", "", gw.line$dongs) # delete d_

gw.line <- gw.line %>%   
  mutate(abb = str_replace_all(dongs, c("jung.ang" = "JA", "sunghyun" ="SH", "samsung"="SA", "euncheon" = "EC",
                                        "inheon" = "IH", "jowon"="JW", "miseong" = "DC4", "sinwon" = "SW",
                                        "seorim" = "SR", "sinsa" = "SI", "chungryong" = "CR", "silim" = "SL", "seowon" = "SE",
                                        "nak" = "NK", "nanhyang" = "NH", "boramae" = "BO", 
                                        "daehak" = "DH", "namhyeon" = "NH", "chnim" = "CN", "hengun" = "HG")))

##############
## Draw Plot
##############

plotbb100 <- gn.box %>% 
  filter(scenario == "BAU" & AC == "AC100") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))


bl100 <- gn.line %>% filter(scenario == "BAU" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl100 <- direct.label(bl100,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

plotbb150 <- gn.box %>% 
  filter(scenario == "BAU" & AC == "AC150") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

bl150 <- gn.line %>% filter(scenario == "BAU" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,2,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl150 <- direct.label(bl150,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


plotbb200 <- gn.box %>% 
  filter(scenario == "BAU" & AC == "AC200") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))


bl200 <- gn.line %>% filter(scenario == "BAU" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,2,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl200 <- direct.label(bl200,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

############
##INC
############

plotib100 <- gn.box %>% 
  filter(scenario == "INC" & AC == "AC100") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + 
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

il100 <- gn.line %>% filter(scenario == "INC" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,60) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil100 <- direct.label(il100,list(dl.trans(x=x+0.1), "last.qp",cex=.9))

plotib150 <- gn.box %>% 
  filter(scenario == "INC" & AC == "AC150") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

il150 <- gn.line %>% filter(scenario == "INC" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,60) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil150 <- direct.label(il150,list(dl.trans(x=x+0.1), "last.qp",cex=.9))


plotib200 <- gn.box %>% 
  filter(scenario == "INC" & AC == "AC200") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

il200 <- gn.line %>% 
  filter(scenario == "INC" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,60) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil200 <- direct.label(il200,list(dl.trans(x=x+0.1), "last.qp",cex=.9))

############
##DEC
#########
########

plotdb100 <- gn.box %>% 
  filter(scenario == "DEC" & AC == "AC100") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

dl100 <- gn.line %>% filter(scenario == "DEC" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl100 <- direct.label(dl100,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

plotdb150 <- gn.box %>% 
  filter(scenario == "DEC" & AC == "AC150") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

dl150 <- gn.line %>% filter(scenario == "DEC" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
 # geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl150 <- direct.label(dl150,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


plotdb200 <- gn.box %>% 
  filter(scenario == "DEC" & AC == "AC200") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

dl200 <- gn.line %>% filter(scenario == "DEC" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl200 <- direct.label(dl200,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


b100 <- plotbl100 + annotation_custom(grob = ggplotGrob(plotbb100), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
b150 <- plotbl150 + annotation_custom(grob = ggplotGrob(plotbb150), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
b200 <- plotbl200 + annotation_custom(grob = ggplotGrob(plotbb200), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)

i100 <- plotil100 + annotation_custom(grob = ggplotGrob(plotib100), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
i150 <- plotil150 + annotation_custom(grob = ggplotGrob(plotib150), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
i200 <- plotil200 + annotation_custom(grob = ggplotGrob(plotib200), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)

d100 <- plotdl100 + annotation_custom(grob = ggplotGrob(plotdb100), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
d150 <- plotdl150 + annotation_custom(grob = ggplotGrob(plotdb150), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
d200 <- plotdl200 + annotation_custom(grob = ggplotGrob(plotdb200), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)


plot_grid(b100, b150, b200, labels = c("A", "B", "C"), ncol = 3, align = "h")
plot_grid(i100, i150, i200, labels = c("D", "E", "F"), ncol = 3, align = "h")
plot_grid(d100, d150, d200, labels = c("G", "H", "I"), ncol = 3, align = "h")

#plot_grid(b100, b150, b200, i100, i150, i200, d100, d150, d200, ncol = 3, 
#          labels = c("A", "B", "C", "D", "E", "F","G", "H", "I"), align = "h")

###

plotbb100 <- gw.box %>%
  filter(scenario == "BAU" & AC == "AC100") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))


bl100 <- gw.line %>% filter(scenario == "BAU" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl100 <- direct.label(bl100,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

plotbb150 <- gw.box %>% 
  filter(scenario == "BAU" & AC == "AC150") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

bl150 <- gw.line %>% filter(scenario == "BAU" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,2,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl150 <- direct.label(bl150,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


plotbb200 <- gw.box %>% 
  filter(scenario == "BAU" & AC == "AC200") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))


bl200 <- gw.line %>% filter(scenario == "BAU" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,2,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotbl200 <- direct.label(bl200,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

############
##INC
############

plotib100 <- gw.box %>% 
  filter(scenario == "INC" & AC == "AC100") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) + 
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

il100 <- gw.line %>% filter(scenario == "INC" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,60) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil100 <- direct.label(il100,list(dl.trans(x=x+0.1), "last.qp",cex=.9))

plotib150 <- gw.box %>% 
  filter(scenario == "INC" & AC == "AC150") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

il150 <- gw.line %>% filter(scenario == "INC" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,60) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil150 <- direct.label(il150,list(dl.trans(x=x+0.1), "last.qp",cex=.9))


plotib200 <- gw.box %>% 
  filter(scenario == "INC" & AC == "AC200") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

il200 <- gw.line %>% 
  filter(scenario == "INC" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  ylim(0,60) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotil200 <- direct.label(il200,list(dl.trans(x=x+0.1), "last.qp",cex=.9))

############
##DEC
#########
########

plotdb100 <- gw.box %>% 
  filter(scenario == "DEC" & AC == "AC200") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

dl100 <- gw.line %>% filter(scenario == "DEC" & AC == "AC100") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl100 <- direct.label(dl100,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))

plotdb150 <- gw.box %>% 
  filter(scenario == "DEC" & AC == "AC150") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

dl150 <- gw.line %>% filter(scenario == "DEC" & AC == "AC150") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  # geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl150 <- direct.label(dl150,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


plotdb200 <- gw.box %>% 
  filter(scenario == "DEC" & AC == "AC200") %>% 
  ggplot(aes(x = reorder(abb, percent, FUN = mean), y = percent, fill = abb)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=16, size=2) +
  guides(fill=FALSE) + 
  coord_flip() +
  xlab("") +
  ylab("%")  +
  theme_bw()+
  theme(legend.position = "bottom",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size = 4))

dl200 <- gw.line %>% filter(scenario == "DEC" & AC == "AC200") %>% 
  ggplot(aes(x=ticks, y=percent, group = dongs, color = dongs)) +
  geom_line(size = 0.8) +
  #geom_ma(ma_fun = SMA, n = 500, linetype = "solid", size = 1) +
  xlab("ticks") +
  ylab("Risk population (%)")  +
  scale_y_continuous(breaks = c(0,5,10,20,40,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0,2500,5000,7500,8763), limits = c(4000,10000)) +
  theme_bw() +
  theme(legend.position = "",
        legend.title=element_text(size=12),
        legend.text=element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        strip.text = element_text(size =12)) 
plotdl200 <- direct.label(dl200,list(dl.trans(x=x+0.1), "last.qp",cex=.9, vjust=-1))


b100 <- plotbl100 + annotation_custom(grob = ggplotGrob(plotbb100), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
b150 <- plotbl150 + annotation_custom(grob = ggplotGrob(plotbb150), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
b200 <- plotbl200 + annotation_custom(grob = ggplotGrob(plotbb200), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)

i100 <- plotil100 + annotation_custom(grob = ggplotGrob(plotib100), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
i150 <- plotil150 + annotation_custom(grob = ggplotGrob(plotib150), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
i200 <- plotil200 + annotation_custom(grob = ggplotGrob(plotib200), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)

d100 <- plotdl100 + annotation_custom(grob = ggplotGrob(plotdb100), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
d150 <- plotdl150 + annotation_custom(grob = ggplotGrob(plotdb150), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)
d200 <- plotdl200 + annotation_custom(grob = ggplotGrob(plotdb200), xmin = 4000, xmax = 7500, ymin = 20, ymax = 60)


plot_grid(b100, b150, b200, labels = c("A", "B", "C"), ncol = 3, align = "h")
plot_grid(i100, i150, i200, labels = c("D", "E", "F"), ncol = 3, align = "h")
plot_grid(d100, d150, d200, labels = c("G", "H", "I"), ncol = 3, align = "h")




