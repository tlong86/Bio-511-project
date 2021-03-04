rm(list=ls())

library(readxl)
chagas <- read_excel("chagas table 1.xlsx")

library(ggplot2)
names(chagas)

#boxplot
boxplot(chagas$`Megacolon*`, xlab="Megacolon", ylab="")

#square root
chagas$Megacolon.sq=sqrt(chagas$`Megacolon*`)
chagas$indeterminate.sq=sqrt(chagas$`Indeterminate*`)
chagas$Cardiac.sq=sqrt(chagas$`Cardiac*`)

#boxplot of Megacolon.sq
boxplot(chagas$Megacolon.sq)

#factors of megacolon, phylum and genus
chagas$fMegacolon=factor(chagas$`Megacolon*`)

chagas$fphylum=factor(chagas$Phylum)
chagas$fgenus=factor(chagas$Genus)

#boxplots

ggplot(chagas ,aes(y=Megacolon.sq,x=fphylum))+geom_boxplot()+theme_classic()

ggplot(chagas ,aes(y=Megacolon.sq,x=fphylum,fgenus))+geom_boxplot()+theme_classic()

ggplot(chagas ,aes(y=indeterminate.sq,x=fphylum))+geom_boxplot()+theme_classic()

ggplot(chagas ,aes(y=indeterminate.sq,x=fphylum,fgenus))+geom_boxplot()+theme_classic()

ggplot(chagas ,aes(y=Cardiac.sq,x=fphylum))+geom_boxplot()+theme_classic()

ggplot(chagas ,aes(y=Cardiac.sq,x=fphylum,fgenus))+geom_boxplot()+theme_classic()

#dotcharts

dotchart(chagas$Megacolon.sq,pch = as.numeric(chagas$fphylum),main = "Megacolon")

dotchart(chagas$indeterminate.sq,pch = as.numeric(chagas$fphylum),main = "Indeterminate")

dotchart(chagas$Cardiac.sq,pch = as.numeric(chagas$fphylum),main = "Cardiac")

#histograms

hist(chagas$`Megacolon*`)

hist(chagas$`Indeterminate*`)

hist(chagas$`Cardiac*`)

#qqplots
qqnorm(chagas$`Megacolon*`)
qqline(chagas$`Megacolon*`)

qqnorm(chagas$Megacolon.sq)
qqline(chagas$Megacolon.sq)


qqnorm(chagas$`Indeterminate*`)
qqline(chagas$`Indeterminate*`)

qqnorm(chagas$indeterminate.sq)
qqline(chagas$indeterminate.sq)

qqnorm(chagas$`Cardiac*`)
qqline(chagas$`Cardiac*`)

qqnorm(chagas$Cardiac.sq)
qqline(chagas$Cardiac.sq)


#scaterplots
plot(chagas$`Megacolon*`,chagas$fphylum, xlab="Megacolon",ylab = "fPhylum")

plot(chagas$`Indeterminate*`,chagas$fphylum, xlab="Indeterminate",ylab = "fPhylum")

plot(chagas$`Cardiac*`,chagas$fphylum, xlab="Cardiac",ylab = "fPhylum")

#linear regresion models
model1=lm(chagas$`Megacolon*`~chagas$phylum, data=chagas)
summary(model1)
plot(model1)
AIC(model1)


model2=lm(chagas$`Indeterminate*`~chagas$Phylum,data = chagas)
plot(model2)
AIC(model2)


model3=lm(chagas$`Cardiac*`~chagas$Phylum,data=chagas)
plot(model3)
AIC(model3)


#pair plots
library(GGally)
ggpairs(chagas [,3:6])


