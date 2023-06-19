library(tidyverse)
library(haven)
library(data.table)

rm(list = ls())

#### Data Preparation ####

d2 <- readxl::read_xlsx("FINAL_DATA_v6.XLSX", col_names = T)
d.POP <- d2[,1:12]
d.G <- d2[,c(1,13:23)]
d.P <- d2[,c(1,24:34)]
d.Z <- d2[,c(1,35:45)]
d.Vt <- d2[,c(1,46:56)]
d.I <- d2[,c(1,57:67)]
d.DPER <- d2[,c(1,68:78)]
d.DUR <- d2[,c(1,79:89)]
d.POP <- melt.data.table(setDT(d.POP), id.vars = "State Name", variable.name = "Year")
names(d.POP)[3]<-"Population"
d.POP$Year <- str_replace_all(d.POP$Year, "POP_", "")
d.G <- melt.data.table(setDT(d.G), id.vars = "State Name", variable.name = "Year")
names(d.G)[3]<-"G"
d.G$Year <- str_replace_all(d.G$Year, "G_", "")
d.P <- melt.data.table(setDT(d.P), id.vars = "State Name", variable.name = "Year")
names(d.P)[3]<-"P"
d.P$Year <- str_replace_all(d.P$Year, "P_", "")
d.Z <- melt.data.table(setDT(d.Z), id.vars = "State Name", variable.name = "Year")
names(d.Z)[3]<-"Z"
d.Z$Year <- str_replace_all(d.Z$Year, "Z_", "")
d.Vt <- melt.data.table(setDT(d.Vt), id.vars = "State Name", variable.name = "Year")
names(d.Vt)[3]<-"Vt"
d.Vt$Year <- str_replace_all(d.Vt$Year, "Vt_", "")
d.I <- melt.data.table(setDT(d.I), id.vars = "State Name", variable.name = "Year")
names(d.I)[3]<-"I"
d.I$Year <- str_replace_all(d.I$Year, "I_", "")
d.DPER <- melt.data.table(setDT(d.DPER), id.vars = "State Name", variable.name = "Year")
names(d.DPER)[3]<-"DPER"
d.DPER$Year <- str_replace_all(d.DPER$Year, "DPER_", "")
d.DUR <- melt.data.table(setDT(d.DUR), id.vars = "State Name", variable.name = "Year")
names(d.DUR)[3]<-"DUR"
d.DUR$Year <- str_replace_all(d.DUR$Year, "DUR_", "")
Data_Model <- d.POP
Data_Model$G <- d.G$G
Data_Model$P <- d.P$P
Data_Model$Z <- d.Z$Z
Data_Model$Vt <- d.Vt$Vt
Data_Model$I <- d.I$I
Data_Model$DPER <- d.DPER$DPER
Data_Model$DUR <- d.DUR$DUR
Data_Model$Year[1:51]<-1980
Data_Model$Year[52:102]<-1984
Data_Model$Year[103:153]<-1988
Data_Model$Year[154:204]<-1992
Data_Model$Year[205:255]<-1996
Data_Model$Year[256:306]<-2000
Data_Model$Year[307:357]<-2004
Data_Model$Year[358:408]<-2008
Data_Model$Year[409:459]<-2012
Data_Model$Year[460:510]<-2016
Data_Model$Year[511:561]<-2020


d2020 <- Data_Model[512:561,]
#### Model ####

library(plm)
Data_Model$GI <- Data_Model$G*Data_Model$I
Data_Model$PI <- Data_Model$P*Data_Model$I
Data_Model$ZI <- Data_Model$Z*Data_Model$I

d3 <- Data_Model[1:510,]

# Model without 2020 Data
m1 <- plm(Vt~GI + PI + ZI + DPER + DUR + I, data = d3, model = "within")
summary(m1)
within_intercept(m1)

# Model including 2020 Data
m2 <- plm(Vt~GI + PI + ZI + DPER + DUR + I, data = Data_Model, model = "within")
summary(m2)
within_intercept(m2)

model_r <- plm(Vt~DPER + DUR + I, data = Data_Model, model = "within")
summary(model_r)
summary(model_r)

#### pool ability

znp <- pvcm(Vt~GI + PI + ZI + DPER + DUR + I, data = Data_Model, model = "within")
zplm <- plm(Vt~GI + PI + ZI + DPER + DUR + I, data = Data_Model, model = "within")
pooltest(zplm, znp)



# 2020 Comparison
d4 <- Data_Model[511:561,]
d4$Vt_hat <- 47.89135 + (0.634418)*d4$G*d4$I +
  (-0.040483)*d4$P*d4$I + (-0.168969)*d4$Z*d4$I +
  (-3.519073)*d4$DPER + (-6.554398)*d4$DUR + (5.446288)*d4$I
write.csv(d4,"E:/Econometrics Proj/Data/Final/VtHat_2020.csv")

#2012 Comparison
d5 <- Data_Model[409:459,]
d5$Vt_hat <- 47.89135 + (0.634418)*d5$G*d5$I +
  (-0.040483)*d5$P*d5$I + (-0.168969)*d5$Z*d5$I +
  (-3.519073)*d5$DPER + (-6.554398)*d5$DUR + (5.446288)*d5$I
write.csv(d5,"E:/Econometrics Proj/Data/Final/VtHat_2012.csv")

