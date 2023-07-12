library(reshape2)
avg.harvested.mass<-300
assumed.growth.rate<-0.0265
week<-seq(1,12,1)

Cultivated<-c(0, avg.harvested.mass)
#print(Cultivated)

for(i in 3:12){
  prev.mass<-Cultivated[i-1]
  #print(prev.mass)
  growth<-prev.mass*(1+assumed.growth.rate)^7
  #print(growth)
  if(i%%3 == 0){
    Cultivated[i]<-growth+avg.harvested.mass
  }else{
    Cultivated[i]<-growth
  }
}

Harvested<-c(0)
for(i in 2:12){
  if(i%%3 != 0){
    Harvested[i]<-Harvested[i-1]+avg.harvested.mass
  }
  else{
    Harvested[i]<-Harvested[i-1]
  }
}

cumulative<-Cultivated+Harvested
d1<-data.frame(week,Cultivated)
d1<-data.frame(d1, cumulative)
d2<-data.frame(week, Harvested)



d.merge<-merge(d1,d2, by = "week" )
d.melted<-melt(d.merge, id.vars = "week")

p<-ggplot(d.melted, mapping = aes(x = week, y = value, col = variable))+
  geom_point()+
  geom_line()+
  labs(title = "Potential Wet Production", subtitle = "Dive Trip Every 3rd Week Model")+
  scale_x_continuous(breaks = seq(1,12,1))+
  ylab("Potential Cultivated Mass(lbs)")+
  theme(plot.title = element_text(hjust = 0.5))
print(p)

p.int<-ggplotly(p)
print(p.int)