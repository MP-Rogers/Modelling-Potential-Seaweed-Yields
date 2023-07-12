library(tidyverse)
library(plotly)
library(econocharts)


generate.harvest.mass<-function(avg.harvested.mass){
  cum.biomass<-avg.harvested.mass
  for (i in 2:12){
    cum.biomass[i]<-avg.harvested.mass+cum.biomass[i-1]
  }
  return(cum.biomass)
}

generate.cultivation.mass<-function(avg.harvested.mass, assumed.growth.rate){
  cum.biomass<-300
  for(i in 2:12){
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
weeks<-seq(1,12,1)
d1<-data.frame(weeks, pure.harvest)
d1<-data.frame(dataset, pure.cultivation)
d2<-data.frame(rev(pure.cultivation), pure.harvest)

d1.melt<-melt(d1, id.vars = "weeks")
dataset<-d1

g2<-dataset |> ggplot(mapping = aes(x = weeks, y = value, col = variable))+
  geom_point()+
  geom_line()
print(g2)

#Begin Visualzing
g1<-dataset |> ggplot(mapping = aes(x = weeks))+
  geom_line(data = dataset, aes(y = pure.harvest), colour = "purple")+
  geom_line(data = dataset, aes(y = pure.cultivation), colour = "green")+
  labs(title = "Pure Harvest and Cultivation Estimates for gathered seaweed")
g1<-ggplotly(g1)
print(g1)


ppf<-ggplot(dataset, mapping = aes(x = rev(pure.cultivation), y=pure.harvest))+
  geom_point(alpha = 0.8)+
  geom_line(size = 1.2)+
  xlab("Pure Cultivation")+
  ylab("Pure Harvest")+
  labs(title = "PPF Of Seaweed Production")
int.ppf<-ggplotly(ppf)
print(int.ppf)