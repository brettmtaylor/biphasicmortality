### Surgeonfish biphasic mortality patterns - Taylor et al.

library(readxl)
library(brms)
library(bayesplot)
library(lme4)
library(segmented)
library(strucchange)
library(ggplot2)
library(gridExtra)
library(ggpubr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

datmort <- read_excel("surgeonfish_age_dat.xlsx", 
                  col_types = c("text", "numeric"))

dat <- datmort

mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

##################################
## LOOP TO GENERATE DATA TABLES ##
##################################

above_mode <- NULL #Placeholder to create data table in loop
max_age <- NULL #Placeholder to create data table in loop

length(unique(dat$Species_site)) #How many species/site combinations?

par(mfrow=c(3,3))
for(i in unique(dat$Species_site)){
  
  sub_location <- subset(dat, dat$Species_site==i) #Subset to one location
  max_age <- rbind(max_age, data.frame(Species_site=i, max_age=max(sub_location$Age))) #Extract observed max age
  
  #hist(sub_location$AGE, nclass = 20) #Histogram for Age
  #abline(v=  mode(sub_location$AGE), lty=2,col=2,lwd=3) #Vertical line for the mode
  
  sub_location <- subset(sub_location, sub_location$Age >=  mode(sub_location$Age)) #Subset to >= mode
  
  freq_count <- table(sub_location$Age) #Extract frequencies
  ln_freq_count <- as.numeric(log(freq_count)) #Natural log transform frequencies
  
  above_mode <- rbind(above_mode, data.frame(Species_site=rep(unique(sub_location$Species_site)), 
                                             freq_count=as.numeric(freq_count),
                                             ln_freq_count,
                                             Age=sort(unique(sub_location$Age)))) #Generate data table during loop
  
}

length(unique(above_mode$Species_site)) 


par(mfrow=c(1,1))
##############################################################################################
###############Iterative species assessments##################################################
##############################################################################################

## Acanthurus bahianus Ascension
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_Ascension")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=6)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABascensionmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(5, 20) + ylim(-0, 4)+ggtitle("Acanthurus bahianus Ascension")+
  stat_smooth(method = "lm", col = "black", linewidth = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75) 

anova(lin.mod, segmented) 


## Acanthurus tractus Bahamas (Lee Stocking)
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_Bahamas")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=6)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABbahamasmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 4)+ggtitle("Acanthurus tractus Bahamas")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented) 


## Acanthurus tractus Barbados
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_Barbados")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=6)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABbarbadosmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 4)+ggtitle("Acanthurus tractus Barbados")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented) 



## Acanthurus tractus Belize
#dat <- subset(above_mode, above_mode$Species_site =="Abahianus_Belize")

#lin.mod <- lm(ln_freq_count ~ Age, data = dat)
#segmented <- segmented(lin.mod, seg.Z = ~Age, psi=6)

#plot(dat$Age,dat$ln_freq_count, pch=16)
#plot(segmented, conf.level=0.95, shade = TRUE, add=T)
#abline(lin.mod)

#seg <- as.data.frame(segmented[["fitted.values"]])
#seg$x <- dat$Age
#colnames(seg) <- c("fit", "x")

#ABbelizemort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
#  geom_point(col = "black") + xlim(0, 10) + ylim(-0, 4)+ggtitle("Acanthurus tractus Belize")+
#  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
#  geom_hline(yintercept = 0, color = "black", size = .5) +
#  geom_line(data=seg, aes(x = x, y = fit), size = .75)

#anova(lin.mod, segmented) 



## Acanthurus tractus Bermuda
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_Bermuda")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=6)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABbermudamort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 31) + ylim(-0, 3.5)+ggtitle("Acanthurus tractus Bermuda")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Acanthurus bahianus Cabo Frio
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_CaboFrio")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=6)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABcabomort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 21) + ylim(-0, 4)+ggtitle("Acanthurus bahianus Cabo Frio")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus tractus Las Aves
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_LasAves")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=4)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABlasavesmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 4)+ggtitle("Acanthurus tractus Las Aves")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus tractus Los Roques
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_LosRoques")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=4)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABlosroqmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0.5, 4)+ggtitle("Acanthurus tractus Los Roques")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus bahianus Santa Helena
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_SantaHelena")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABsantahelmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 31) + ylim(-0, 3)+ggtitle("Acanthurus bahianus Santa Helena")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus tractus SanBlas
dat <- subset(above_mode, above_mode$Species_site =="Abahianus_SanBlas")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABsanblasmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 10) + ylim(-0.5, 4)+ggtitle("Acanthurus tractus San Blas")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Acanthurus chirurgus Bahamas
#dat <- subset(above_mode, above_mode$Species_site =="Achirurgus_Bahamas")

#lin.mod <- lm(ln_freq_count ~ Age, data = dat)
#segmented <- segmented(lin.mod, seg.Z = ~Age, psi=22)

#plot(dat$Age,dat$ln_freq_count, pch=16)
#plot(segmented, conf.level=0.95, shade = TRUE, add=T)
#abline(lin.mod)

#seg <- as.data.frame(segmented[["fitted.values"]])
#seg$x <- dat$Age
#colnames(seg) <- c("fit", "x")

#ACbahamasmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
#  geom_point(col = "black") + xlim(0, 40) + ylim(-0.2, 1.5)+ggtitle("Acanthurus chirurgus Bahamas")+
#  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
#  geom_hline(yintercept = 0, color = "black", size = .5) +
#  geom_line(data=seg, aes(x = x, y = fit), size = .75)

#anova(lin.mod, segmented)   



## Acanthurus chirurgus Belize
dat <- subset(above_mode, above_mode$Species_site =="Achirurgus_Belize")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACbelizemort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 12) + ylim(-0, 3)+ggtitle("Acanthurus chirurgus Belize")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Acanthurus chirurgus Bermuda
dat <- subset(above_mode, above_mode$Species_site =="Achirurgus_Bermuda")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACbermudamort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.4, 3.2)+ggtitle("Acanthurus chirurgus Bermuda")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus chirurgus Brazil
dat <- subset(above_mode, above_mode$Species_site =="Achirurgus_Brazil")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=9)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACbrazilmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 3)+ggtitle("Acanthurus chirurgus Brazil")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   

#residual plots
plot(lin.mod$residuals~dat$Age)
scatter.smooth(dat$Age, lin.mod$residuals, span = 4.5/5)
plot(segmented$residuals ~ dat$Age)
scatter.smooth(dat$Age, segmented$residuals, span = 4.5/5)

ACbrazil <- ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 3)+ggtitle("Acanthurus chirurgus Cabo Frio")+
  stat_smooth(method = "lm", col = "black", size = 1, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75, color = "grey")

ACbrazilLINres <- ggplot(lin.mod, aes(x = Age, y = lin.mod$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Linear model") + 
  geom_point(col = "black") + xlim(0, 15) + ylim(-1, 1) + stat_smooth(method = "loess", col = "black", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

ACbrazilSEGres <- ggplot(segmented, aes(x = Age, y = segmented$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Segmented model") + 
  geom_point(col = "black") + xlim(0, 15) + ylim(-1, 1) + stat_smooth(method = "loess", col = "grey", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

ggarrange(ACbrazil,
          ggarrange(ACbrazilLINres, ACbrazilSEGres, ncol = 2, labels = c("B", "C")),
          nrow = 2,
          labels = "A")
ggarrange(ACbrazil, ggarrange(ACbrazilLINres,ACbrazilSEGres,nrow = 2, labels = c("B", "C")), nrow = 1, labels = "A")




## Acanthurus chirurgus Los Roques
dat <- subset(above_mode, above_mode$Species_site =="Achirurgus_LosRoques")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AClosroqmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 2.5)+ggtitle("Acanthurus chirurgus Los Roques")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus chirurgus Margarita
dat <- subset(above_mode, above_mode$Species_site =="Achirurgus_Margarita")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACmargmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.2, 4)+ggtitle("Acanthurus chirurgus Margarita")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus chirurgus San Blas
dat <- subset(above_mode, above_mode$Species_site =="Achirurgus_SanBlas")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACsanblasmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0.2, 3)+ggtitle("Acanthurus chirurgus San Blas")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus coeruleus Ascension
#dat <- subset(above_mode, above_mode$Species_site =="Acoeruleus_Ascension")

#lin.mod <- lm(ln_freq_count ~ Age, data = dat)
#segmented <- segmented(lin.mod, seg.Z = ~Age, psi=30)

#plot(dat$Age,dat$ln_freq_count, pch=16)
#plot(segmented, conf.level=0.95, shade = TRUE, add=T)
#abline(lin.mod)

#seg <- as.data.frame(segmented[["fitted.values"]])
#seg$x <- dat$Age
#colnames(seg) <- c("fit", "x")

#ACoascensionmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
#  geom_point(col = "black") + xlim(0, 40) + ylim(-0.4, 2.5)+ggtitle("Acanthurus coeruleus Ascension")+
#  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
#  geom_hline(yintercept = 0, color = "black", size = .5) +
#  geom_line(data=seg, aes(x = x, y = fit), size = .75)

#anova(lin.mod, segmented)   


## Acanthurus coeruleus Bahamas
dat <- subset(above_mode, above_mode$Species_site =="Acoeruleus_Bahamas")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACoBahamasmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0, 3.5)+ggtitle("Acanthurus coeruleus Bahamas")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus coeruleus Belize
dat <- subset(above_mode, above_mode$Species_site =="Acoeruleus_Belize")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACoBelizemort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 3.5)+ggtitle("Acanthurus coeruleus Belize")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus coeruleus Bermuda
dat <- subset(above_mode, above_mode$Species_site =="Acoeruleus_Bermuda")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACobermudamort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 50) + ylim(-0, 2.5)+ggtitle("Acanthurus coeruleus Bermuda")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Acanthurus coeruleus Los Roques
dat <- subset(above_mode, above_mode$Species_site =="Acoeruleus_LosRoques")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AColosroqmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0, 3)+ggtitle("Acanthurus coeruleus Los Roques")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus coeruleus Margarita
dat <- subset(above_mode, above_mode$Species_site =="Acoeruleus_Margarita")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AComargmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0, 3.5)+ggtitle("Acanthurus coeruleus Margarita")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus coeruleus San Blas
dat <- subset(above_mode, above_mode$Species_site =="Acoeruleus_SanBlas")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ACosanblasmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0, 3)+ggtitle("Acanthurus coeruleus San Blas")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus blochii GBR
dat <- subset(above_mode, above_mode$Species_site =="Ablochii_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 35) + ylim(-0, 1.3)+ggtitle("Acanthurus blochii GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus blochii Guam
#dat <- subset(above_mode, above_mode$Species_site =="Ablochii_Guam")

#lin.mod <- lm(ln_freq_count ~ Age, data = dat)
#segmented <- segmented(lin.mod, seg.Z = ~Age, psi=5)

#plot(dat$Age,dat$ln_freq_count, pch=16)
#plot(segmented, conf.level=0.95, shade = TRUE, add=T)
#abline(lin.mod)

#seg <- as.data.frame(segmented[["fitted.values"]])
#seg$x <- dat$Age
#colnames(seg) <- c("fit", "x")

#ABguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
#  geom_point(col = "black") + xlim(0, 10) + ylim(-0, 2)+ggtitle("Acanthurus blochii Guam")+
#  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
#  geom_hline(yintercept = 0, color = "black", size = .5) +
#  geom_line(data=seg, aes(x = x, y = fit), size = .75)

#anova(lin.mod, segmented)   


## Acanthurus blochii Hawaii
dat <- subset(above_mode, above_mode$Species_site =="Ablochii_Hawaii")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=5)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ABhawaiimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 3.5)+ggtitle("Acanthurus blochii Oahu")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus dussumieri Hawaii
dat <- subset(above_mode, above_mode$Species_site =="Adussumieri_Hawaii")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ADhawaiimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 4)+ggtitle("Acanthurus dussumieri Oahu")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


#Residuals plot for paper
plot(lin.mod$residuals~dat$Age)
scatter.smooth(dat$Age, lin.mod$residuals, span = 4.5/5)
plot(segmented$residuals ~ dat$Age)
scatter.smooth(dat$Age, segmented$residuals, span = 4.5/5)

ADhawaii <- ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 4)+ggtitle("Acanthurus dussumieri Oahu")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75, color = "grey")

ADhawaiiLINres <- ggplot(lin.mod, aes(x = Age, y = lin.mod$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Linear model") + 
  geom_point(col = "black") + xlim(0, 30) + ylim(-1.2, 1) + stat_smooth(method = "loess", col = "black", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

ADhawaiiSEGres <- ggplot(segmented, aes(x = Age, y = segmented$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Segmented model") + 
  geom_point(col = "black") + xlim(0, 30) + ylim(-1.2, 1) + stat_smooth(method = "loess", col = "grey", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")






## Acanthurus guttatus Guam
dat <- subset(above_mode, above_mode$Species_site =="Aguttatus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AGguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 22) + ylim(-0.1, 3.5)+ggtitle("Acanthurus guttatus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   




## Acanthurus lineatus GBR
dat <- subset(above_mode, above_mode$Species_site =="Alineatus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=20)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ALgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 42) + ylim(-0, 2.5)+ggtitle("Acanthurus lineatus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus lineatus Guam
dat <- subset(above_mode, above_mode$Species_site =="Alineatus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ALguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.2, 3.5)+ggtitle("Acanthurus lineatus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus lineatus Northern Mariana Islands
dat <- subset(above_mode, above_mode$Species_site =="Alineatus_MTMNM")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ALmtmnmmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 3.5)+ggtitle("Acanthurus lineatus N. Mariana Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus lineatus Central Mariana Islands
dat <- subset(above_mode, above_mode$Species_site =="Alineatus_NMI")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ALnmimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-1, 4)+ggtitle("Acanthurus lineatus Cent. Mariana Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus nigricans GBR
dat <- subset(above_mode, above_mode$Species_site =="Anigricans_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ANigrigbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 40) + ylim(-0.2, 2.5)+ggtitle("Acanthurus nigricans GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Acanthurus nigricauda Southern Mariana Islands
dat <- subset(above_mode, above_mode$Species_site =="Anigricauda_SMI")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ANsmimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.5, 6)+ggtitle("Acanthurus nigricauda S. Mariana Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Acanthurus nigrofuscus GBR
dat <- subset(above_mode, above_mode$Species_site =="Anigrofuscus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=18)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ANigbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.3, 3)+ggtitle("Acanthurus nigrofuscus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus olivaceus GBR
dat <- subset(above_mode, above_mode$Species_site =="Aolivaceus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AOgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 35) + ylim(-0.2, 3.5)+ggtitle("Acanthurus olivaceus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus olivaceus Guam
dat <- subset(above_mode, above_mode$Species_site =="Aolivaceus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AOguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 3.5)+ggtitle("Acanthurus olivaceus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus olivaceus Hawaii
dat <- subset(above_mode, above_mode$Species_site =="Aolivaceus_Hawaii")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AOhawaiimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0.5, 4)+ggtitle("Acanthurus olivaceus Oahu")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus triostegus Guam
dat <- subset(above_mode, above_mode$Species_site =="Atriostegus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ATguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0.2, 4)+ggtitle("Acanthurus triostegus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus xanthopterus GBR
dat <- subset(above_mode, above_mode$Species_site =="Axanthopterus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AXgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 35) + ylim(-0.2, 2)+ggtitle("Acanthurus xanthopterus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus xanthopterus Guam
dat <- subset(above_mode, above_mode$Species_site =="Axanthopterus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AXguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 12) + ylim(-0.2, 3)+ggtitle("Acanthurus xanthopterus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Acanthurus xanthopterus Hawaii
dat <- subset(above_mode, above_mode$Species_site =="Axanthopterus_Hawaii")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=7)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

AXhawaiimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 4.5)+ggtitle("Acanthurus xanthopterus Oahu")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


#Residuals plot for paper


plot(lin.mod$residuals~dat$Age)
scatter.smooth(dat$Age, lin.mod$residuals, span = 4.5/5)
plot(segmented$residuals ~ dat$Age)
scatter.smooth(dat$Age, segmented$residuals, span = 4.5/5)

AXhawaii <- ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 4.5)+ggtitle("Acanthurus xanthopterus Oahu")+
  stat_smooth(method = "lm", col = "black", size = 1, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75, color = "grey")

AXhawaiiLINres <- ggplot(lin.mod, aes(x = Age, y = lin.mod$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Linear model") + 
  geom_point(col = "black") + xlim(0, 30) + ylim(-1.5, 2.2) + stat_smooth(method = "loess", col = "black", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

AXhawaiiSEGres <- ggplot(segmented, aes(x = Age, y = segmented$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Segmented model") + 
  geom_point(col = "black") + xlim(0, 30) + ylim(-1.5, 2.2) + stat_smooth(method = "loess", col = "grey", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

ggarrange(AXhawaii,
          ggarrange(AXhawaiiLINres, AXhawaiiSEGres, ncol = 2, labels = c("B", "C")),
          nrow = 2,
          labels = "A")
ggarrange(AXhawaii, ggarrange(AXhawaiiLINres,AXhawaiiSEGres,nrow = 2, labels = c("B", "C")), nrow = 1, labels = "A")





## Ctenochaetus striatus GBR
dat <- subset(above_mode, above_mode$Species_site =="Cstriatus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

CSgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 40) + ylim(-0.2, 3)+ggtitle("Ctenochaetus striatus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Ctenochaetus striatus Guam
dat <- subset(above_mode, above_mode$Species_site =="Cstriatus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

CSguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.1, 3.5)+ggtitle("Ctenochaetus striatus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Ctenochaetus strigosus Johnston Atoll
dat <- subset(above_mode, above_mode$Species_site =="Cstrigosus_Johnston")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=4)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

CSjohnstonmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.2, 3)+ggtitle("Ctenochaetus strigosus Johnston Atoll")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Ctenochaetus strigosus Main Hawaiian Islands
dat <- subset(above_mode, above_mode$Species_site =="Cstrigosus_MHI")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=4)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

CSmhimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 40) + ylim(-0.6, 4)+ggtitle("Ctenochaetus strigosus Main Hawaiian Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Ctenochaetus strigosus Northwest Hawaiian Islands
dat <- subset(above_mode, above_mode$Species_site =="Cstrigosus_NWHI")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=4)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

CSnwhimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.4, 3.5)+ggtitle("Ctenochaetus strigosus NW Hawaiian Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   





## Naso annulatus GBR
dat <- subset(above_mode, above_mode$Species_site =="Nannulatus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NAgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.2, 2)+ggtitle("Naso annulatus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso brevirostris GBR
dat <- subset(above_mode, above_mode$Species_site =="Nbrevirostris_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NBgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.2, 4)+ggtitle("Naso brevirostris GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso hexacanthus GBR
dat <- subset(above_mode, above_mode$Species_site =="Nhexacanthus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NHgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 45) + ylim(-0.2, 2)+ggtitle("Naso hexacanthus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso lituratus GBR
dat <- subset(above_mode, above_mode$Species_site =="Nlituratus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=11)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NLgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 40) + ylim(-0.5, 3)+ggtitle("Naso lituratus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Naso lituratus Guam
dat <- subset(above_mode, above_mode$Species_site =="Nlituratus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NLguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 14) + ylim(-0.2, 4)+ggtitle("Naso lituratus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Naso lituratus Northern Mariana Islands
#dat <- subset(above_mode, above_mode$Species_site =="Nlituratus_MTMNM")

#lin.mod <- lm(ln_freq_count ~ Age, data = dat)
#segmented <- segmented(lin.mod, seg.Z = ~Age, psi=20)

#plot(dat$Age,dat$ln_freq_count, pch=16)
#plot(segmented, conf.level=0.95, shade = TRUE, add=T)
#abline(lin.mod)

#seg <- as.data.frame(segmented[["fitted.values"]])
#seg$x <- dat$Age
#colnames(seg) <- c("fit", "x")

#NLmtmnmmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
#  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 3)+ggtitle("Naso lituratus N. Mariana Is.")+
#  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
#  geom_hline(yintercept = 0, color = "black", size = .5) +
#  geom_line(data=seg, aes(x = x, y = fit), size = .75)

#anova(lin.mod, segmented)   


## Naso lituratus Central Mariana Islands
dat <- subset(above_mode, above_mode$Species_site =="Nlituratus_NMI")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NLnmimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.2, 3.5)+ggtitle("Naso lituratus Cent. Mariana Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso lituratus Oahu
dat <- subset(above_mode, above_mode$Species_site =="Nlituratus_Oahu")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NLoahumort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 40) + ylim(-0.5, 3.5)+ggtitle("Naso lituratus Oahu")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso lituratus Pohnpei
dat <- subset(above_mode, above_mode$Species_site =="Nlituratus_Pohnpei")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NLpohnpeimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 15) + ylim(-0, 4.5)+ggtitle("Naso lituratus Pohnpei")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso lituratus Tutuila
dat <- subset(above_mode, above_mode$Species_site =="Nlituratus_Tutuila")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NLtutuilamort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.2, 5)+ggtitle("Naso lituratus Tutuila")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   

#Residuals plot for paper
plot(lin.mod$residuals~dat$Age)
scatter.smooth(dat$Age, lin.mod$residuals, span = 4.5/5)
plot(segmented$residuals ~ dat$Age)
scatter.smooth(dat$Age, segmented$residuals, span = 4.5/5)

NLtutuila <- ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.2, 5)+ggtitle("Naso lituratus Tutuila")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75, color = "grey")

NLtutuilaLINres <- ggplot(lin.mod, aes(x = Age, y = lin.mod$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Linear model") + 
  geom_point(col = "black") + xlim(0, 25) + ylim(-1.5, 2) + stat_smooth(method = "loess", col = "black", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

NLtutuilaSEGres <- ggplot(segmented, aes(x = Age, y = segmented$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Segmented model") + 
  geom_point(col = "black") + xlim(0, 25) + ylim(-1.5, 2) + stat_smooth(method = "loess", col = "grey", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")





## Naso tonganus GBR
dat <- subset(above_mode, above_mode$Species_site =="Ntonganus_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NTgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0, 3.5)+ggtitle("Naso tonganus GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso tonganus Guam
dat <- subset(above_mode, above_mode$Species_site =="Ntonganus_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NTguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 22) + ylim(-0.2, 3.5)+ggtitle("Naso tonganus Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Chagos
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_CHAGOS")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUchagosmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 2.5)+ggtitle("Naso unicornis Chagos")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Guam
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_GUAM")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.2, 5)+ggtitle("Naso unicornis Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Lizard
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_LIZARD")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUlizardmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 4)+ggtitle("Naso unicornis GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Northern Mariana Islands
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_MTMNM2018")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=20)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUmtmnmmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 40) + ylim(-0.2, 2)+ggtitle("Naso unicornis N. Mariana Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Central Mariana Islands
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_NMI2014")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUnmimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.2, 2.5)+ggtitle("Naso unicornis Cent. Mariana Is.")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Oahu
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_OAHU")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=15)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUoahumort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 55) + ylim(-0.3, 4.5)+ggtitle("Naso unicornis Oahu")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Pohnpei
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_POHNPEI")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUpohnpeimort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.2, 4.5)+ggtitle("Naso unicornis Pohnpei")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Saipan
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_SAIPAN")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUsaipanmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.6, 5.2)+ggtitle("Naso unicornis Saipan")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   

#Residuals plot for paper


plot(lin.mod$residuals~dat$Age)
scatter.smooth(dat$Age, lin.mod$residuals, span = 4.5/5)
plot(segmented$residuals ~ dat$Age)
scatter.smooth(dat$Age, segmented$residuals, span = 4.5/5)

NUsaipan <- ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 22) + ylim(-0.6, 5.2)+ggtitle("Naso unicornis Saipan")+
  stat_smooth(method = "lm", col = "black", size = 1, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75, color = "grey")

NUsaipanLINres <- ggplot(lin.mod, aes(x = Age, y = lin.mod$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Linear model") + 
  geom_point(col = "black") + xlim(0, 22) + ylim(-1, 1.5) + stat_smooth(method = "loess", col = "black", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

NUsaipanSEGres <- ggplot(segmented, aes(x = Age, y = segmented$residuals)) + xlab("Age class (yrs)") + ylab("Residuals of Segmented model") + 
  geom_point(col = "black") + xlim(0, 22) + ylim(-1, 1.5) + stat_smooth(method = "loess", col = "grey", level = 0.01, span = 5) + theme_classic() + geom_hline(yintercept = 0, color = "grey", size = .5, linetype = "dotted")

ggarrange(NUsaipan,
          ggarrange(NUsaipanLINres, NUsaipanSEGres, ncol = 2, labels = c("B", "C")),
          nrow = 2,
          labels = "A")
ggarrange(NUsaipan, ggarrange(NUsaipanLINres,NUsaipanSEGres,nrow = 2, labels = c("B", "C")), nrow = 1, labels = "A")



## Naso unicornis Tinian
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_TINIAN")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUtinianmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.6, 4)+ggtitle("Naso unicornis Tinian")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Tutuila
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_TUTUILA")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUtutuilamort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.4, 5)+ggtitle("Naso unicornis Tutuila")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Naso unicornis Yap
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_YAP")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUyapmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 20) + ylim(-0.2, 3)+ggtitle("Naso unicornis Yap")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Naso vlamingii GBR
dat <- subset(above_mode, above_mode$Species_site =="Nvlamingi_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NVgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 45) + ylim(-0.2, 3.5)+ggtitle("Naso vlamingii GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Naso vlamingii Guam
#dat <- subset(above_mode, above_mode$Species_site =="Nvlamingi_Guam")

#lin.mod <- lm(ln_freq_count ~ Age, data = dat)
#segmented <- segmented(lin.mod, seg.Z = ~Age, psi=5)

#plot(dat$Age,dat$ln_freq_count, pch=16)
#plot(segmented, conf.level=0.95, shade = TRUE, add=T)
#abline(lin.mod)

#seg <- as.data.frame(segmented[["fitted.values"]])
#seg$x <- dat$Age
#colnames(seg) <- c("fit", "x")

#NVguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
#  geom_point(col = "black") + xlim(0, 10) + ylim(-0, 3.5)+ggtitle("Naso vlamingii Guam")+
#  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
#  geom_hline(yintercept = 0, color = "black", size = .5) +
#  geom_line(data=seg, aes(x = x, y = fit), size = .75)

#anova(lin.mod, segmented) 



## Zebrasoma scopas GBR
#dat <- subset(above_mode, above_mode$Species_site =="Zscopas_GBR")

#lin.mod <- lm(ln_freq_count ~ Age, data = dat)
#segmented <- segmented(lin.mod, seg.Z = ~Age, psi=20)

#plot(dat$Age,dat$ln_freq_count, pch=16)
#plot(segmented, conf.level=0.95, shade = TRUE, add=T)
#abline(lin.mod)

#seg <- as.data.frame(segmented[["fitted.values"]])
#seg$x <- dat$Age
#colnames(seg) <- c("fit", "x")

#ZSgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
#  geom_point(col = "black") + xlim(0, 35) + ylim(-0.2, 2)+ggtitle("Zebrasoma scopus GBR")+
#  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
#  geom_hline(yintercept = 0, color = "black", size = .5) +
#  geom_line(data=seg, aes(x = x, y = fit), size = .75)

#anova(lin.mod, segmented)  


## Zebrasoma velifer GBR
dat <- subset(above_mode, above_mode$Species_site =="Zvelifer_GBR")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=20)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ZVgbrmort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.2, 2)+ggtitle("Zebrasoma velifer GBR")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Zebrasoma velifer Guam
dat <- subset(above_mode, above_mode$Species_site =="Zvelifer_Guam")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=10)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ZVguammort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.2, 2.5)+ggtitle("Zebrasoma velifer Guam")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   


## Naso unicornis Red Sea
dat <- subset(above_mode, above_mode$Species_site =="Nunicornis_RedSea")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=8)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

NUredseamort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 25) + ylim(-0.4, 2)+ggtitle("Naso unicornis Red Sea")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   



## Acanthurus sohal Red Sea
dat <- subset(above_mode, above_mode$Species_site =="Asohal_RedSea")

lin.mod <- lm(ln_freq_count ~ Age, data = dat)
segmented <- segmented(lin.mod, seg.Z = ~Age, psi=4)

plot(dat$Age,dat$ln_freq_count, pch=16)
plot(segmented, conf.level=0.95, shade = TRUE, add=T)
abline(lin.mod)

seg <- as.data.frame(segmented[["fitted.values"]])
seg$x <- dat$Age
colnames(seg) <- c("fit", "x")

ASredseamort <-ggplot(dat, aes(x = Age, y = ln_freq_count)) + xlab("Age (yrs)") + ylab("LN Frequency")  +
  geom_point(col = "black") + xlim(0, 30) + ylim(-0.1, 2.5)+ggtitle("Acanthurus sohal Red Sea")+
  stat_smooth(method = "lm", col = "black", size = .5, level = 0.01) + theme_classic() +
  geom_hline(yintercept = 0, color = "black", size = .5) +
  geom_line(data=seg, aes(x = x, y = fit), size = .75)

anova(lin.mod, segmented)   




######Plotting#########
grid.arrange(ABhawaiimort, ABgbrmort, ADhawaiimort, AGguammort,ALgbrmort, 
             ALguammort, ALmtmnmmort, ALnmimort, ANigrigbrmort, ANsmimort, 
             ANigbrmort, AOgbrmort, AOguammort, AOhawaiimort, ASredseamort,   
             ATguammort,AXgbrmort, AXguammort, AXhawaiimort, CSgbrmort, nrow = 4, ncol = 5)

grid.arrange(CSguammort, CSjohnstonmort, CSmhimort, CSnwhimort, NAgbrmort,
             NBgbrmort, NHgbrmort, NLgbrmort, NLguammort, NLnmimort,
             NLoahumort, NLpohnpeimort,NLtutuilamort, NTgbrmort, NTguammort,
             NUchagosmort,NUguammort, NUlizardmort, NUmtmnmmort, NUnmimort,
             nrow = 4, ncol = 5)

grid.arrange(NUoahumort, NUpohnpeimort, NUredseamort, NUsaipanmort, NUtinianmort,  
             NUtutuilamort, NUyapmort, NVgbrmort, ZVgbrmort, 
             ZVguammort,nrow = 4, ncol = 5)

grid.arrange(ABascensionmort, ABcabomort, ABsantahelmort, ABbahamasmort, ABbarbadosmort, 
             ABbermudamort, ABlasavesmort, ABlosroqmort, ABsanblasmort, ACbermudamort,
             ACbrazilmort, AClosroqmort, ACmargmort, ACsanblasmort, ACoBahamasmort, 
             ACoBelizemort, ACobermudamort, AColosroqmort, AComargmort, ACosanblasmort, nrow = 4, ncol = 5)



#Examples plot showing residuals
#######must go back and rerun each species/location first

NUsaipanPLOT <- ggarrange(NUsaipan, ggarrange(NUsaipanLINres,NUsaipanSEGres,nrow = 2, labels = c("B", "C")), nrow = 1, labels = "A")
AXhawaiiPLOT <- ggarrange(AXhawaii, ggarrange(AXhawaiiLINres,AXhawaiiSEGres,nrow = 2, labels = c("E", "F")), nrow = 1, labels = "D")
#ADhawaiiPLOT <- ggarrange(ADhawaii, ggarrange(ADhawaiiLINres,ADhawaiiSEGres,nrow = 2, labels = c("H", "I")), nrow = 1, labels = "G")
NLtutuilaPLOT <- ggarrange(NLtutuila, ggarrange(NLtutuilaLINres,NLtutuilaSEGres,nrow = 2, labels = c("H", "I")), nrow = 1, labels = "G")

ggarrange(NUsaipanPLOT, AXhawaiiPLOT, NLtutuilaPLOT, nrow = 3)
#save as portrait, 5.25"x7.25"


#simulated data for demographic demonstration

simulated <- subset(above_mode, above_mode$Species_site =="simulated")
simdat <- subset(datmort, datmort$Species_site == "simulated")
simulated2 <- subset(above_mode, above_mode$Species_site =="simulated2")
simdat2 <- subset(datmort, datmort$Species_site == "simulated2")


#combine the two simulated data sets vertically
simdata <- rbind(simdat, simdat2)

hist <- ggplot() + 
  geom_histogram(data = simdat, aes(x=Age), fill = "grey", color = "black", binwidth = 1) +
  geom_histogram(data = simdat2, aes(x=Age), fill = "red", color = "black", binwidth = 1, alpha = 0.5) + 
  theme_classic() + xlab("Age class (yrs)") + ylab("Frequency") + scale_x_continuous(breaks = seq(0,27,5))

#combine the two simulated data sets vertically
sim <- rbind(simulated, simulated2)

catchcurve <- ggplot(data = sim, aes(x=Age, y=ln_freq_count, color = Species_site)) +
  geom_point(show.legend = FALSE) + stat_smooth(method = "lm", level = 0.95, show.legend = FALSE) + theme_classic() + ylab("LN Frequency") + xlab("Age class (yrs)") +
  scale_color_manual(values = c("grey", "red")) + scale_x_continuous(breaks = seq(0,27,5)) +
  geom_point(aes(x = 1, y = 3.912), colour = "grey", fill = "white", shape = 21) + geom_point(aes(x = 2, y = 5.0106), colour = "grey", fill = "white", shape = 21) + geom_point(aes(x = 3, y = 5.768), colour = "grey", fill = "white", shape = 21) +
  geom_point(aes(x = 0, y = 4.8828), colour = "red", fill = "white", shape = 21) + geom_point(aes(x = 1, y = 5.7038), colour = "red", fill = "white", shape = 21)



ggarrange(hist, catchcurve, labels = c("A", "B"))



############################################################
##############Productivity plots############################
############################################################
#ypr <- read_excel("ypr_results.xlsx", col_types = c("text", 
#                                                    "numeric", "numeric", "numeric", "numeric", 
#                                                    "numeric"))
mk <- read_excel("mk_results.xlsx", col_types = c("text", 
                                                   "numeric", "numeric", "numeric", "numeric")) 
depletion <- read_excel("depletion_time.xlsx", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric"))
#ggplot(ypr, aes(x=seg_maxage)) + 
  #geom_histogram(aes(y=..density..), colour="black", fill="white")+
#  geom_density(alpha=.5, fill="darkgrey") + theme_classic() + xlim(0, 2) + xlab("Ratio of segmented / linear yield per recruit") +
 # geom_vline(xintercept = 1, color = "black", linetype = "dashed", size = .5)

ggplot(mk, aes(x=linear)) + 
#geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="darkgrey") + theme_classic() + xlim(0, 2) + xlab("M/K") +
 geom_vline(xintercept = 1, color = "black", linetype = "dashed", size = .5)

ggplot(depletion, aes(x=seg_maxage)) + 
  #geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.5, fill="darkgrey") + theme_classic() + xlim(0, 2) + xlab("Ratio of segmented / linear depletion time") +
  geom_vline(xintercept = 1, color = "black", linetype = "dashed", size = .5)


#relYPRplot <- ggplot() + 
#  geom_density(data = ypr, aes(x=seg_maxage), alpha=.5, fill="darkgrey") +
#  geom_density(data = ypr, aes(x=seg_linear), alpha=.5, fill="black") + theme_classic() + xlim(0, 2) + xlab("Ratio of yield per recruit") + 
#  geom_vline(xintercept = 1, color = "black", linetype = "dashed", size = .5)

relDEPLETION <- ggplot() + 
  geom_density(data = depletion, aes(x=seg_maxage), alpha=.8, fill="darkgrey") +
  geom_density(data = depletion, aes(x=seg_linear), alpha=.8, fill="black") + 
  geom_density(data = depletion, aes(x=seg_maxageCope), alpha=.4, fill="grey") + theme_classic() + xlim(0, 2) + xlab("Ratio of depletion time to 20% abundance") +
    geom_vline(xintercept = 1, color = "black", linetype = "dashed", size = .5)

relMK <- ggplot() + 
  geom_density(data = mk, aes(x=hoenig), alpha=.8, fill="darkgrey") +
  geom_density(data = mk, aes(x=linear), alpha=.8, fill="black") + 
  geom_density(data = mk, aes(x=hamelcope), alpha=.4, fill="grey") + 
  geom_density(data = mk, aes(x=segmented), alpha=.4, fill="blue") +theme_classic() + xlab("M/K ratio")
  

ggarrange(relDEPLETION, relMK, labels = c("A", "B"), nrow = 2)






below_mode <- NULL
for(i in unique(dat$Species_site)){
  
  sub_location <- subset(dat, dat$Species_site==i) #Subset to one location
  max_age <- rbind(max_age, data.frame(Species_site=i, max_age=max(sub_location$Age))) #Extract observed max age
  
  #hist(sub_location$AGE, nclass = 20) #Histogram for Age
  #abline(v=  mode(sub_location$AGE), lty=2,col=2,lwd=3) #Vertical line for the mode
  
  sub_location <- subset(sub_location, sub_location$Age <  mode(sub_location$Age)) #Subset to < mode
  
  freq_count <- table(sub_location$Age) #Extract frequencies
  ln_freq_count <- as.numeric(log(freq_count)) #Natural log transform frequencies
  
  below_mode <- rbind(below_mode, data.frame(Species_site=rep(unique(sub_location$Species_site)), 
                                             freq_count=as.numeric(freq_count),
                                             ln_freq_count,
                                             Age=sort(unique(sub_location$Age)))) #Generate data table during loop
  
}
