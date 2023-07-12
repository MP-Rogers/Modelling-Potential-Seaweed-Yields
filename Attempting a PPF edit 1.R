library(tidyverse)
library(plotly)
library(econocharts)


generate.harvest.mass<-function(avg.harvested.mass){
  cum.biomass<-c(0,avg.harvested.mass)
  for (i in 3:12){
    cum.biomass[i]<-avg.harvested.mass+cum.biomass[i-1]
  }
  return(cum.biomass)
}

generate.cultivation.mass<-function(avg.harvested.mass, assumed.growth.rate){
  cum.biomass<-c(0,avg.harvested.mass)
  for(i in 3:12){
    prev.mass<-cum.biomass[i-1]
    growth.on.prev<-prev.mass*(1+assumed.growth.rate)^7
    cum.biomass[i]<-avg.harvested.mass+growth.on.prev
  }
  return(cum.biomass)
}

  
avg.harvested.mass<-300
assumed.growth.rate<-0.0265

pure.harvest<-generate.harvest.mass(avg.harvested.mass) 
pure.cultivation<-generate.cultivation.mass(avg.harvested.mass, assumed.growth.rate)
weeks<-seq(1 ,12,1)

dataset<-data.frame(weeks, pure.harvest)
dataset<-data.frame(dataset, pure.cultivation)
d2<-data.frame(rev(pure.cultivation), pure.harvest)



#Begin Visualzing
g1<-dataset |> ggplot(mapping = aes(x = weeks))+
  geom_line(data = dataset, aes(y = pure.harvest), colour = "purple")+
  geom_point(data = dataset, aes(y = pure.harvest), colour = "purple")+
  geom_line(data = dataset, aes(y = pure.cultivation), colour = "green")+
  geom_point(data = dataset, aes(y = pure.cultivation), colour = "green")+
  scale_x_continuous(breaks = seq(1,12,1))+
  ylab("Wet Biomass to put to market(lbs)")+
  labs(title = "Pure Harvest and Cultivation Estimates for gathered seaweed")
g1<-ggplotly(g1)
print(g1)


ppf<-ggplot(dataset, mapping = aes(x = rev(pure.cultivation), y=pure.harvest))+
  geom_point(alpha = 0.8)+
  geom_line(size = 1.2)+
  xlab("Marketable Wet Biomass from Cultivation(lbs)")+
  ylab("Marketable Wet Biomass from Harvest(lbs)")+
  labs(title = "PPF Of Seaweed Production")
int.ppf<-ggplotly(ppf)
print(int.ppf)

#New Conditions

