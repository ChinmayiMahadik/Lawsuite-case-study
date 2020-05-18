#Case study 1 using lawsuits.xlsx

#packages used
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(DataExplorer)
library(ggplot2)
library(breakDown)
library(dplyr)
library(readxl)

#Read data from lawsuits.xlsx

setwd("G:/University of Denver Life!/C U Denver sem 2/BANA 6610 R programming/HW 2")
lawsuitData = read_excel("Lawsuits.xlsx", sheet = 1);lawsuitData
#(need to check which sheet in excel which yu du has provided)


#plotting box plot to check outliers
boxplot(lawsuitData$Payment)
boxplot(lawsuitData$Age)

#summary data

"add summary data code here"

#Univariate Graph Histogram of univariant(numeric) and ploting missing data

plot_missing(lawsuitData)
plot_histogram(lawsuitData)

library(RColorBrewer)
# frequency of gender
ggplot(lawsuitData, aes(lawsuitData$Gender)) + geom_bar(fill = "green") + labs(title = "Frequency of gender", x= "Gender", y = "Count")

#Relationship between specialty and frequency percentage

specData = lawsuitData %>%
  count(lawsuitData$Specialty) %>%
  mutate(per = n/sum(n), per_label = paste0(round(per*100),"%"))

ggplot(specData, aes(x= reorder(specData$`lawsuitData$Specialty`,per) , y= per )) + geom_bar(stat = "identity", fill = "pink", color = "black") + geom_text(aes(label = per_label ), vjust = -0.25)+ labs(title = "Relationship between specialty and frequency percentage", x= "Specialty", y = "Frequency %") + theme(axis.text.x = element_text(angle = 90))


# Percentage share of Insurance type

insuranceData = lawsuitData %>%
  count(lawsuitData$Insurance)
pielabels <- sprintf("%s = %3.1f%s", insuranceData$`lawsuitData$Insurance`,
                     100*insuranceData$n/sum(insuranceData$n), "%")
pie((100*insuranceData$n/sum(insuranceData$n)),
    labels=NA,
    clockwise=TRUE,
    col=brewer.pal(21,"Set1"),
    border="white",
    radius=0.5,
    cex=0.8,
    main="Percentage Share of Insurance type")
legend("bottomright",legend=pielabels,bty="n",
       fill=brewer.pal(21,"Set1"))

# Bivariate Graph

#Relationship between Insurance types and payment
ggplot(lawsuitData, aes(lawsuitData$Insurance, lawsuitData$Payment)) + geom_bar(stat = "identity", fill = "orange") + labs(title = "Relationship between Insurance and payments", x= "Insurance types", y = "Payment amount")+ theme(axis.text.x = element_text(angle = 90))

#Relationship between severity and payment
ggplot(lawsuitData, aes(factor(lawsuitData$Severity), lawsuitData$Payment)) + geom_bar(stat = "identity", fill = "cyan") + labs(title = "Relationship between severity and payments", x= "Insurance types", y = "Payment amount")+ theme(axis.text.x = element_text(angle = 90))

# Multivariate Graph
ggplot(lawsuitData, aes(x=lawsuitData$Insurance, y=lawsuitData$Payment)) + 
  geom_bar(aes(fill=lawsuitData$Gender),  
           stat="identity",
           position=position_dodge())+ labs(title = "Relationship between insurance types and payment with gender as factor", x= "Insurance Types", y = "Payment Amount", colour="Gender")+ theme(axis.text.x = element_text(angle = 90))


ggplot(lawsuitData, aes(x=lawsuitData$Insurance, y=lawsuitData$Payment)) + 
  geom_bar(aes(fill=factor(lawsuitData$`Private Attorney`)),  
           stat="identity",
           position=position_dodge())+ labs(title = "Relationship between insurance types and payment with attorney type as factor", x= "Insurance Types", y = "Payment Amount", colour="Private attorney")+ theme(axis.text.x = element_text(angle = 90))

"unable to change color will try to do that"

# correlation between numberic data

numericData = lawsuitData %>%
  count(lawsuitData$Payment,lawsuitData$Age,lawsuitData$Severity, lawsuitData$`Private Attorney`,lawsuitData$`Marital Status`)
pairs(numericData)

# to create an overall report
create_report(lawsuitData)

