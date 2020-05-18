install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")

library(tidyverse)
library(funModeling)
library(Hmisc)


glimpse(Lawsuits)
df_status(Lawsuits)
freq(Lawsuits)
profiling_num(Lawsuits)
plot_num(Lawsuits)
describe(Lawsuits)


boxplot(Lawsuits$Payment)

boxplot(Lawsuits$Age)

barchart(Lawsuits$Insurance,Lawsuits$Gender)

library(ggplot2)
library(scales)

hist(Lawsuits$Age)

qplot(Lawsuits$`Marital Status` ,Lawsuits$Payment, color = Lawsuits$`Marital Status`,geom = c("point","smooth"))

qplot(Lawsuits$Insurance, Lawsuits$Payment, color = Lawsuits$Gender, fill = Lawsuits$Insurance)

numData = data.frame(Lawsuits$Payment,Lawsuits$Severity, Lawsuits$Age, Lawsuits$`Private Attorney`, Lawsuits$`Marital Status`)
pairs(numData)
 
qplot(Lawsuits$Payment,Lawsuits$`Private Attorney`, data = Lawsuits, color = Lawsuits$Gender ,geom = c("point","smooth"), facets = .~ Lawsuits$Gender)

qplot( log(Lawsuits$Age), Lawsuits$Payment, data = Lawsuits,  geom = c("point","smooth"), color = Lawsuits$Gender)

pairs(log(numData))

cor(Lawsuits$Age, Lawsuits$Payment)












 

