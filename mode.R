library(nnet)
library(margins)
library(mlogit)

###############
# DATA IMPORT #
###############

data("TravelMode", package = "AER")

##############
# TABLE 18.2 #
##############


# to calculate the mean - mean() function is used, for filtering - used subset() function

# so below are the calculations for each value of table

## air

# GC
mean(subset(TravelMode, TravelMode$mode == "air")$gcost)
mean(subset(TravelMode, TravelMode$mode == "air" & TravelMode$choice == "yes")$gcost)
# TIME
mean(subset(TravelMode, TravelMode$mode == "air")$wait)
mean(subset(TravelMode, TravelMode$mode == "air" & TravelMode$choice == "yes")$wait)
# INVC
mean(subset(TravelMode, TravelMode$mode == "air")$vcost)
mean(subset(TravelMode, TravelMode$mode == "air" & TravelMode$choice == "yes")$vcost)
# INVT
mean(subset(TravelMode, TravelMode$mode == "air")$travel)
mean(subset(TravelMode, TravelMode$mode == "air" & TravelMode$choice == "yes")$travel)
# HINC
mean(subset(TravelMode, TravelMode$mode == "air")$income)
mean(subset(TravelMode, TravelMode$mode == "air" & TravelMode$choice == "yes")$income)
# number choosing
air_NC <- nrow(subset(TravelMode, TravelMode$mode == "air" & TravelMode$choice == "yes"))
air_NC
# p
air_NC/nrow(subset(TravelMode, TravelMode$mode == "air"))

# train
# GC
mean(subset(TravelMode, TravelMode$mode == "train")$gcost)
mean(subset(TravelMode, TravelMode$mode == "train" & TravelMode$choice == "yes")$gcost)
# TIME
mean(subset(TravelMode, TravelMode$mode == "train")$wait)
mean(subset(TravelMode, TravelMode$mode == "train" & TravelMode$choice == "yes")$wait)
# INVC
mean(subset(TravelMode, TravelMode$mode == "train")$vcost)
mean(subset(TravelMode, TravelMode$mode == "train" & TravelMode$choice == "yes")$vcost)
# INVT
mean(subset(TravelMode, TravelMode$mode == "train")$travel)
mean(subset(TravelMode, TravelMode$mode == "train" & TravelMode$choice == "yes")$travel)
# HINC
mean(subset(TravelMode, TravelMode$mode == "train")$income)
mean(subset(TravelMode, TravelMode$mode == "train" & TravelMode$choice == "yes")$income)
# number choosing
train_NC <- nrow(subset(TravelMode, TravelMode$mode == "train" & TravelMode$choice == "yes"))
train_NC
# p
train_NC/nrow(subset(TravelMode, TravelMode$mode == "train"))

## bus

# GC
mean(subset(TravelMode, TravelMode$mode == "bus")$gcost)
mean(subset(TravelMode, TravelMode$mode == "bus" & TravelMode$choice == "yes")$gcost)
# TIME
mean(subset(TravelMode, TravelMode$mode == "bus")$wait)
mean(subset(TravelMode, TravelMode$mode == "bus" & TravelMode$choice == "yes")$wait)
# INVC
mean(subset(TravelMode, TravelMode$mode == "bus")$vcost)
mean(subset(TravelMode, TravelMode$mode == "bus" & TravelMode$choice == "yes")$vcost)
# INVT
mean(subset(TravelMode, TravelMode$mode == "bus")$travel)
mean(subset(TravelMode, TravelMode$mode == "bus" & TravelMode$choice == "yes")$travel)
# HINC
mean(subset(TravelMode, TravelMode$mode == "bus")$income)
mean(subset(TravelMode, TravelMode$mode == "bus" & TravelMode$choice == "yes")$income)
# number choosing
bus_NC <- nrow(subset(TravelMode, TravelMode$mode == "bus" & TravelMode$choice == "yes"))
bus_NC
# p
bus_NC/nrow(subset(TravelMode, TravelMode$mode == "bus"))

## car

# GC
mean(subset(TravelMode, TravelMode$mode == "car")$gcost)
mean(subset(TravelMode, TravelMode$mode == "car" & TravelMode$choice == "yes")$gcost)
# TIME
mean(subset(TravelMode, TravelMode$mode == "car")$wait)
mean(subset(TravelMode, TravelMode$mode == "car" & TravelMode$choice == "yes")$wait)
# INVC
mean(subset(TravelMode, TravelMode$mode == "car")$vcost)
mean(subset(TravelMode, TravelMode$mode == "car" & TravelMode$choice == "yes")$vcost)
# INVT
mean(subset(TravelMode, TravelMode$mode == "car")$travel)
mean(subset(TravelMode, TravelMode$mode == "car" & TravelMode$choice == "yes")$travel)
# HINC
mean(subset(TravelMode, TravelMode$mode == "car")$income)
mean(subset(TravelMode, TravelMode$mode == "car" & TravelMode$choice == "yes")$income)
# number choosing
car_NC <- nrow(subset(TravelMode, TravelMode$mode == "car" & TravelMode$choice == "yes"))
car_NC
# p
car_NC/nrow(subset(TravelMode, TravelMode$mode == "car"))

##############
# TABLE 18.3 #
##############

# boolean column of choice 
TravelMode$choice_accepted <- TravelMode$choice == "yes"
# income of those who choose air
TravelMode$incair <- with(TravelMode, income * (mode == "air"))

#Unweighted Sample Model
US_model<- mlogit(choice_accepted ~ gcost + wait + incair, data = TravelMode, shape = "long", reflevel = "car")


#weights calculation

#calculate in-sample probabilities
TravelMode$p<-NA
TravelMode[TravelMode$mode=="car","p"]<-4*nrow(TravelMode[(TravelMode$mode=="car" & TravelMode$choice=="yes"),])/nrow(TravelMode)
TravelMode[TravelMode$mode=="air","p"]<-4*nrow(TravelMode[(TravelMode$mode=="air" & TravelMode$choice=="yes"),])/nrow(TravelMode)
TravelMode[TravelMode$mode=="train","p"]<-4*nrow(TravelMode[(TravelMode$mode=="train" & TravelMode$choice=="yes"),])/nrow(TravelMode)
TravelMode[TravelMode$mode=="bus","p"]<-4*nrow(TravelMode[(TravelMode$mode=="bus" & TravelMode$choice=="yes"),])/nrow(TravelMode)

#add population statistics
TravelMode$tp<-NA
TravelMode[TravelMode$mode=="car","tp"]<-0.64
TravelMode[TravelMode$mode=="air","tp"]<-0.14
TravelMode[TravelMode$mode=="train","tp"]<-0.13
TravelMode[TravelMode$mode=="bus","tp"]<-0.09

#calculate weights for each transport mode
TravelMode$w<-NA
TravelMode$w<-TravelMode$tp/TravelMode$p

#store weights as numbers
wcar<- TravelMode[TravelMode$mode=="car","w"][1]
wair<- TravelMode[TravelMode$mode=="air","w"][1]
wtrain<- TravelMode[TravelMode$mode=="train","w"][1]
wbus<- TravelMode[TravelMode$mode=="bus","w"][1]

#assign choice-based transport weigts to indiwiduals
TravelMode$w<-NA
TravelMode[TravelMode$individual %in% TravelMode[TravelMode$mode=="air" & TravelMode$choice=="yes","individual"], "w"] <- wair
TravelMode[TravelMode$individual %in% TravelMode[TravelMode$mode=="car" & TravelMode$choice=="yes","individual"], "w"] <- wcar
TravelMode[TravelMode$individual %in% TravelMode[TravelMode$mode=="train" & TravelMode$choice=="yes","individual"], "w"] <- wtrain
TravelMode[TravelMode$individual %in% TravelMode[TravelMode$mode=="bus" & TravelMode$choice=="yes","individual"], "w"] <- wbus

#model with choice-bases corrections
CBW_model<- mlogit(choice_accepted ~ gcost + wait + incair, data = TravelMode, shape = "long", weights=TravelMode$w, alt.var = "mode", reflevel = "car")

#model summary
summary(CBW_model)
summary(US_model)

##############
# TABLE 18.4 #
##############

#unweighted
US_matrix<-round(rowsum(US_model[["probabilities"]],as.vector(TravelMode[TravelMode$choice=="yes","mode"])))[c("air","train","bus","car"),c("car","bus","train","air")]
#weighted
CBW_matrix<-round(rowsum(CBW_model[["probabilities"]],as.vector(TravelMode[TravelMode$choice=="yes","mode"])))[c("air","train","bus","car"),c("car","bus","train","air")]

library(reshape2)

ggplot(data = melt(US_matrix), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var1, Var2, label=value), colour = "white", check_overlap = TRUE)+
  labs(
    title = "Unrestricted sample",
    x = "Predicted",
    y = "Actual"
  )+
  theme_bw(base_size = 14, base_family = "Segoe UI Light")+
  theme(legend.position = "none")+
  coord_equal()

ggplot(data = melt(CBW_matrix), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var1, Var2, label=value), colour = "white", check_overlap = TRUE)+
  labs(
    title = "Choice-Based sample",
    x = "Predicted",
    y = "Actual"
  )+
  theme_bw(base_size = 14, base_family = "Segoe UI Light")+
  theme(legend.position = "none")+
  coord_equal()


##############
# TABLE 18.5 #
##############

#restricted choice set
FC_model <- mlogit(choice ~ gcost + wait, TravelMode, reflevel = "car",
                   alt.subset = c("car", "bus", "train"))
summary(FC_model)

#asymptotic covariance restricted
asm_FC<-vcov(FC_model)
asm_FC[upper.tri(asm_FC)]<-NA

#asymptotic covariance full
asm_US<-vcov(US_model)[2:5,2:5]
asm_US[upper.tri(asm_US)]<-NA

#Hausman test
iia<-hmftest(US_model,FC_model)
iia

p1<-ggplot(data = melt(asm_US, na.rm = T), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var1, Var2, label=round(value,4)), colour = "white", check_overlap = TRUE)+
  labs(
    title = "Unrestricted sample"
  )+
  theme_bw(base_size = 14, base_family = "Segoe UI Light")+
  theme(legend.position = "none",
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_blank(),
        axis.text.x =  element_text(angle = 30, vjust = 1, hjust = 1))+
  coord_equal()+
  scale_x_discrete(limits=rev)

p2<-ggplot(data = melt(asm_FC, na.rm = T), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(Var1, Var2, label=round(value,4)), colour = "white", check_overlap = TRUE)+
  labs(
    title = "Restricted-Choice Set"
  )+
  theme_bw(base_size = 14, base_family = "Segoe UI Light")+
  theme(legend.position = "none",
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title = element_blank(),
        axis.text.x =  element_text(angle = 30, vjust = 1, hjust = 1))+
  coord_equal()+
  scale_x_discrete(limits=rev)

grid.arrange(p1,p2, ncol=2)


##############
# TABLE 18.6 #
##############

# FIML estimate
FIML_model <- mlogit(choice_accepted ~ gcost + wait + incair, data = TravelMode, shape = "long", choice = "choice", alt.var = "mode", reflevel = "car", nests = list(fly = "air", ground = c("bus", "car", "train")), unscaled = TRUE)

#sigmas
sigma_fly.fiml = as.numeric(sqrt(pi^2 / (6*(FIML_model$coefficients["iv:fly"]^2))))
sigma_ground.fiml = as.numeric(sqrt(pi^2 / (6*(FIML_model$coefficients["iv:ground"]^2))))

sigma_fly.fiml
sigma_ground.fiml

# Unconditional model estimate
UNCOND_model <- mlogit(choice_accepted ~ gcost + wait + incair, data = TravelMode, shape = "long", choice = "choice", alt.var = "mode", reflevel = "car", unscaled = TRUE)

#sigmas
sigma_fly.un = sqrt(pi^2 / 6)
sigma_ground.un = sqrt(pi^2 / 6)

sigma_fly.un
sigma_ground.un

stargazer(FIML_model,UNCOND_model,type = "text",single.row = TRUE, column.labels = c("FIML","Unconditional"),dep.var.caption = "Accepted Choice", dep.var.labels.include = FALSE)

##############
# TABLE 18.7 #
##############

#HEV model 
HEV_model <- mlogit(choice_accepted ~ gcost + wait + incair, data = TravelMode,
                    shape = "long", choice = "choice", alt.var = "mode", 
                    reflevel = "car", heterosc = TRUE)

#heteroscedastic HEV 
#there is no capacity in mlogit() to control individual-based heteroscedasticity

#restricted HEV
HEV_RS_model <- mlogit(choice_accepted ~ gcost + wait + incair, data = TravelMode,
                       shape = "long", choice = "choice", alt.var = "mode",
                       reflevel = "car", heterosc = TRUE, alt.subset = c("car", "bus", "air", "train"),
                       constPar = c("sp.train" = 1, "sp.bus" = 1))

#Nested Logit
NL_model <- mlogit(
  choice ~ wait + gcost + incair, TravelMode, reflevel="car",
  nests = list(fly = "air", ground = c("train", "bus", "car")),
  unscaled = T
)


stargazer(HEV_model,HEV_RS_model,NL_model,type = "text",single.row = TRUE, column.labels = c("HEV","Restricted HEV", "Nested Logit"),dep.var.caption = "Accepted Choice", dep.var.labels.include = FALSE)


##############
# TABLE 18.8 #
##############

#multinomial logit ON MEAN
t(effects(US_model, covariate = "gcost", type = "rr"))

#Nested Logit

#HEV

#above could only be calculated by hand

##############
# TABLE 18.9 #
##############

#multinomial probit

#there isnt enough flexibility to do this part at all 


