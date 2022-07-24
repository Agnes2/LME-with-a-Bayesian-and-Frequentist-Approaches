#Author: Agnès Pérez-Millan
#Date last modification: 22/07/2022
#ADNI Dataset analysis


library(ggplot2)
library(dplyr)

#Function to delete NA
delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

##################################  STEP 0: DATA #################################

#Read the data download from ADNI
adnioriginal<- read.csv("ADNIdata.csv", stringsAsFactors = F)

#Ral AGE subjects
RealAge<-adnioriginal$AGE+adnioriginal$Years_bl
adnioriginal<-cbind(adnioriginal,RealAge)

#Select the most important variables for this study
adni <- select(adnioriginal, RID, DX_bl, DX, RealAge,Years_bl, AGE, ICV,APOE4, 
               PTGENDER,sMCI, cMCI, sHC, cHC, AD,Ventricles,Hippocampus,
               WholeBrain,Entorhinal,Fusiform,MidTemp,Ventricles_bl,
               Hippocampus_bl,WholeBrain_bl,Entorhinal_bl,Fusiform_bl,MidTemp_bl,
               ICV_bl,Month_bl, Month, M) 
head(adni)


################################  STEP 1: VARIABLES ###############################

#Diagnostic labels as numeric variables
#Legend 0:CN, 1:AD i 2:MCI
adni$DX[adni$DX == "CN"] <- 0
adni$DX[adni$DX == "AD"] <- 1
adni$DX[adni$DX == "MCI"] <- 2

adni$DX_bl[adni$DX_bl == "CN"] <- 0
adni$DX_bl[adni$DX_bl == "AD"] <- 1
adni$DX_bl[adni$DX_bl == "MCI"] <- 2

adni$DX<-as.numeric(adni$DX)
adni$DX_bl<-as.numeric(adni$DX_bl)


#Sex as numeric variable
#Legend 0:M, 1:F
adni$PTGENDER[adni$PTGENDER == "Male"] <- 0
adni$PTGENDER[adni$PTGENDER == "Female"] <- 1

adni$PTGENDER<-as.numeric(adni$PTGENDER)

#APOE-E4 variable
#Legend carriers: 1+2 no carriers:0 --> carriers: 1 no carriers:0
adni$APOE4[adni$APOE4 == 2] <- 1


#New diagnostic labels with the five clinical groups
#Legend sHC: 1, cHC: 2, sMCI: 3, cMCI: 4, AD: 5
DX_final<-(adni$sHC)+2*(adni$cHC)+3*(adni$sMCI)+4*(adni$cMCI)+5*(adni$AD)
adni<-cbind(DX_final,adni)


##############################  STEP 2: HV ##############################

#Select variables needed
adnidata <- select(adni, RID, DX_final, DX_bl, DX, RealAge,Years_bl, AGE, ICV,APOE4, 
                   PTGENDER,sMCI, cMCI, sHC, cHC, AD,Hippocampus,Hippocampus_bl,
                   ICV_bl,Month_bl, Month, M) 
head(adnidata)

#To temove subjects that present NA
adnidata<-delete.na(adnidata) 

#To remove subjects that are no longer longitudinal due to the elimination of NA
x <- adnidata$RID[duplicated(adnidata$RID)]
unique(x)

long <- adnidata$RID %in% c(x) #RID TRUE-->duplicated, longitudinals

adnidata<-cbind(long,adnidata)
adnidata$long<-as.factor(adnidata$long)
adnidata<-subset(adnidata,adnidata$long==TRUE) #keep TRUE


############################  STEP 3: 4 TIME POINTS ############################

#Select the 4 time points
adnidata<-subset(adnidata,adnidata$M==0 | adnidata$M==6 | 
                   adnidata$M==12 | adnidata$M==24) 

#To remove subjects that are no longer longitudinal 
#due to the elimination of time points
x <- adnidata$RID[duplicated(adnidata$RID)]
unique(x)

long1 <- adnidata$RID %in% c(x) #RID TRUE-->duplicated, longitudinals

adnidata<-cbind(long1,adnidata)
adnidata$long1<-as.factor(adnidata$long1)
adnidata<-subset(adnidata,adnidata$long1==TRUE) #keep TRUE


##########################  STEP 4: DATA FOR THE MODEL ##########################

#Scale ICV and HV
adnidata$ICV2<-scale(adnidata$ICV) 
adnidata$Hippocampus2<-scale(adnidata$Hippocampus) 

#save data
write.csv(adnidata, file="adnidata.csv")