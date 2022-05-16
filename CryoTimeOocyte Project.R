#SCRIPT TO ANALYZE A DATABASE OF THE CRYOPRESERVED OOCYTE DONATION PROGRAM
#TO DETERMINE IF THE DURATION OF CRYOSTORAGE INFLUENCES THREE BINARY OUTCOMES:
# OOCYTE SURVIVAL, EMBRYO DEVELOPMENT AND PREGNANCY



library(readxl)
require(dplyr)
require(tidyr)
library(normtest)
library(nortest)
library(epiDisplay)



Base_Dados_Estatistica_Por_Ovulos_20210902 <- read_excel("Base de Dados por Ovocito.xlsx")

datos= as.data.frame(Base_Dados_Estatistica_Por_Ovulos_20210902)
summary(datos)

#SUBSET OF ONLY FROZEN OOCYTES
datos=subset(datos,datos$`N OOC Warmed`!=0)

length(datos$SURVIVAL)

summary(datos$`DON Age`)
summary(datos$`DON BMI`)
summary(datos$`DON 1stDonation`)


#VARIABLE SELECTION

datos1=data.frame(datos$`DON Age`,datos$`DON BMI`,datos$`DON 1stDonation`,
                  datos$`SPERM ORIGIN`, datos$`MALE AGE`, datos$`MALC SMOKING`, 
                  datos$`REC BMI`, datos$`REC SMOK`, datos$`MALE Severe Male Factor`,
                  datos$`REC AGE`, datos$`Oocyte FRESH=0, Cryo=1`, datos$`t CRYO`,
                  datos$`N OOC Warmed`, datos$SURVIVAL, datos$`NORMAL FERT`, datos$UBLAST,
                  datos$`0 BLASTOS`, datos$TR1, datos$`TR1 BLASTDAY`, datos$`TR1 NBLAST`, datos$`TR1 BLASTQUALITY`,
                  datos$`TR1 BHCG`, datos$`TR1 ECO`)


#cambio de variables
str(datos1)
datos1$datos.SURVIVAL=as.factor(datos1$datos.SURVIVAL)
datos1$datos..TR1.ECO.=as.factor(datos1$datos..TR1.ECO.)
datos1$datos.UBLAST=as.factor(datos1$datos.UBLAST)
datos1$datos..TR1.NBLAST.=as.factor(datos1$datos..TR1.NBLAST.)
datos1$datos..SPERM.ORIGIN.=as.factor(datos1$datos..SPERM.ORIGIN.)
datos1$datos..MALE.Severe.Male.Factor.=as.factor(datos1$datos..MALE.Severe.Male.Factor.)
datos1$datos..MALC.SMOKING.=as.factor(datos1$datos..MALC.SMOKING.)
datos1$datos..TR1.BLASTDAY.=as.factor(datos1$datos..TR1.BLASTDAY.)
datos1$datos..TR1.ECO.=as.factor(datos1$datos..TR1.ECO.)
datos1$datos..MALE.AGE.=as.numeric(datos1$datos..MALE.AGE.)
datos1$datos..DON.1stDonation.=as.factor(datos1$datos..DON.1stDonation.)
datos1$datos..REC.SMOK.=as.factor(datos1$datos..REC.SMOK.)
datos1$datos..REC.DIAGNOSIS.=as.factor(datos1$datos..REC.DIAGNOSIS.)
datos1$datos..NORMAL.FERT.=as.factor(datos1$datos..NORMAL.FERT.)
datos1$datos..TR1.BLASTQUALITY.=as.factor(datos1$datos..TR1.BLASTQUALITY.)
datos1$datos..TR1.BHCG.=as.factor(datos1$datos..TR1.BHCG.)
datos1$datos.TR1BIRTH=as.factor(datos1$datos.TR1BIRTH)
datos1$datos..TR1.NGESTSACS.=as.factor(datos1$datos..TR1.NGESTSACS.)
datos1$datos..TR1.ABORTION.=as.factor(datos1$datos..TR1.ABORTION.)
datos1$datos..TR1.BIRTHSEX.=as.factor(datos1$datos..TR1.BIRTHSEX.)
datos1$datos..TR1.BIRTHTYPE.=as.factor(datos1$datos..TR1.BIRTHTYPE.)
datos1$datos..DON.1stDonation.=as.factor(datos1$datos..DON.1stDonation.)
datos1$datos..TR1.BIRTHWEIGHT.=as.numeric(datos1$datos..TR1.BIRTHWEIGHT.)
datos1$datos..REC.BMI.=as.numeric(datos1$datos..REC.BMI.)
datos1$datos..TR2.BIRTH.SIZE.=as.numeric(datos1$datos..TR2.BIRTH.SIZE.)
datos1$datos..0.BLASTOS.=as.factor(datos1$datos..0.BLASTOS.)


#Subset of data in which at least 1 blastocyst was obtained.
datos1=subset(datos1,datos$`0 BLASTOS` !=0)
length(datos1$datos.SURVIVAL)

#Comparaci?n univariada de variables seg?n supervivencia SI/NO

modelSupervivencia=glm(datos1$datos.SURVIVAL ~ 
                         datos1$datos..DON.Age.+
                         datos1$datos..DON.BMI.+
                         datos1$datos..DON.1stDonation.+
                         datos1$datos..t.CRYO., 
                       family = binomial, data = datos1)
summary(modelSupervivencia)

logistic.display(modelSupervivencia)
summary(datos1$datos..DON.Age.)


### BLASTOCYST DEVELOPMENT

#Subset of data: oocytes with normal fertilization
datos1=subset(datos1,datos1$datos..NORMAL.FERT. !="NO")
summary(datos1$datos..NORMAL.FERT.)


model2=glm(datos1$datos.UBLAST~datos1$datos..DON.Age.+datos1$datos..DON.BMI.+datos1$datos..DON.1stDonation.+datos1$datos..t.CRYO.+datos1$datos..SPERM.ORIGIN.+datos1$datos..MALE.AGE.+datos1$datos..MALC.SMOKING.+datos1$datos..MALE.Severe.Male.Factor., family = binomial, data = datos1)
summary(model2)
logistic.display(model2)
summary(datos1$datos..DON.Age.)


## PREGNANCY BHCG

#Subset de datos con BHCG =/ NA
datos1=subset(datos1,datos1$datos..TR1.BHCG. !="NA")
summary(datos1$datos..TR1.BHCG.)
#Subset de datos SINGLE EMBRYO TRANSFER.
datos1=subset(datos1,datos1$datos..TR1.NBLAST. !="D")

head(datos1)
summary(datos1$datos..TR1.NBLAST.)

#Relevel FOR REFERENCE CATEGORIES
summary(datos1$datos..MALC.SMOKING.)
datos1$datos..MALC.SMOKING. <- relevel(datos1$datos..MALC.SMOKING., ref="N")
datos1$datos..SPERM.ORIGIN.<- relevel(datos1$datos..SPERM.ORIGIN., ref="F")
summary(datos1$datos..SPERM.ORIGIN.)
summary(datos1$datos..MALC.SMOKING.)

model3=glm(datos1$datos..TR1.ECO.~datos1$datos..DON.Age.+datos1$datos..DON.BMI.+datos1$datos..DON.1stDonation.+datos1$datos..t.CRYO.+datos1$datos..SPERM.ORIGIN.+datos1$datos..MALE.AGE.+datos1$datos..MALC.SMOKING.+datos1$datos..MALE.Severe.Male.Factor.+datos1$datos..REC.AGE.+datos1$datos..TR1.BLASTDAY., family = binomial, data = datos1)
summary(model3)

logistic.display(model3)
summary(datos1$datos..DON.Age.)
length(datos1$datos..DON.Age.)







