#Author: Agnès Pérez-Millan
#Date last modification: 22/07/2022
#Longitudinal study with a Frequentist approach


library(lme4)
library(ggplot2)
library(dplyr)
library(lmerTest)
library(sjPlot)

##################################  STEP 0: DATA #################################

# Read the data prepared for the model
adnidata<- read.csv("adnidata.csv", stringsAsFactors = F)

#############################  STEP 1: RANDOM EFFECTS ############################

#RANDOM INTERCEPT MODEL
model1<-lmer(formula = Hippocampus2 ~ cHC*time + sMCI*time + cMCI*time + AD*time + 
               APOE4*time + PTGENDER + AGE + ICV2 + (1 | RID), data = adnidata, REML=FALSE)
summary(model1)

#RANDOM INTERCEPT + SLOPE MODEL
model2<-lmer(formula = Hippocampus2 ~ cHC*time + sMCI*time + cMCI*time + AD*time +  
               APOE4*time + PTGENDER + AGE + ICV2 + (time | RID), data = adnidata, REML = FALSE)
summary(model2)

#ANOVA
anova(model2,model1) 

#############################  STEP 2: THE MODEL ############################

#RANDOM INTERCEPT + SLOPE MODEL
model<-lmer(formula = Hippocampus2 ~ cHC*time + sMCI*time + cMCI*time + AD*time + 
              APOE4*time + PTGENDER + AGE + ICV2 + (time | RID), data = adnidata, REML = FALSE)
summary(model)


#Plot residuals vs fitted
plot(fitted(model), residuals(model), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col='red')
lines(smooth.spline(fitted(model), residuals(model)))


###########################  STEP 3: HYPOTHESIS TESTING ###########################

#Hypothesis testing
contest(model, c(0, 0, 0, 0,0,0,0,0,0,0,0,0,0,1,0)) #AD vs sHC
contest(model, c(0, 0, 0,0,0,0,0,0,0,0,0,1,0,0,0)) #sMCI vs sHC
contest(model, c(0, 0, 0,0,0,0,0,0,0,0,0,0,-1,1,0)) #AD vs cMCI
contest(model, c(0, 0, 0,0,0,0,0,0,0,0,0,-1,1,0,0)) #sMCI vs cMCI
contest(model, c(0, 0, 0,0,0,0,0,0,0,0,0,0,1,0,0)) #cMCI vs sHC
contest(model, c(0, 0, 0,0,0,0,0,0,0,0,1,0,0,0,0)) #cHC vs sHC
contest(model, diag(15)[11:14, ]) #sHC vs all

#### Plot CI ####

confint(model) #CI

my_theme <- theme_bw(base_size = 12)  +
  theme(
    plot.title = element_text(hjust = 0.5),# Center title position and size
  )

#plot
plot_model(model, type = "est",color = "bw", title="Confidence Intervals",
           ci_level = 0.5, outer_level = 0.95, dot.size=1.5, axis.lim=c(-2,1)) + my_theme 

