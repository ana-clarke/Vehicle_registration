# Packages
library(ggplot2)
library(dplyr)
library(googleVis)

# Bar charts #
ggplot(data1, aes(x = factor(year), y =vehReg, fill= region))+
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("First vehicle registration by Region") +
  xlab("Year") +
  ylab("Registrations")

ggplot(data1, aes(x = factor(year), y =vehReg, fill= type))+
  geom_bar(position = "dodge", stat = "identity")+
  ggtitle("First vehicle registration by Type") +
  xlab("Year") +
  ylab("Registrations")

# Time series plots #
ggplot(regions, aes(x = factor(year), y = vehReg, group = region, colour = region)) +
  geom_line() +
  geom_point() +
  ggtitle("First Vehicle Registration by Region") +
  xlab("Years") +
  ylab("Number of registrations") +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(types, aes(x = factor(Year), y = vehReg, colour = type, group = type)) +
  geom_line() +
  geom_point() +
  ggtitle("First Vehicle Registration by Type") +
  xlab("Years") +
  ylab("Number of registrations") +
  theme(plot.title = element_text(hjust = 0.5)) 



### Map of total first vehicle registration ###
q <- gvisGeoMap(total1, locationvar= 'latLong', hovervar='Region', numvar = 'TotalVehReg', 
                options=list(dataMode= 'markers', 
                             region = 'NZ', height=650, width = 700, colors="[0xF0FFFF, 0x6698FF, 0x1F45FC]"))

t <- gvisTable(total1[ , -c(3, 4, 5)],options=list(width=200, height=400))
qt <- gvisMerge(q, t, horizontal = T)

plot(qt)


# Animation per capita #
fulldata1 <- tbl_df(fulldata)

percap <- group_by(fulldata2, region)
sum <- percap %>%
  dplyr::summarise(max = max(percapita), min = min(percapita)) 
# %>%
#dplyr::arrange(desc(perc)) %>%
#print()

fulldata2 <- mutate(fulldata1, percapita = population/TotalVehReg)
fulldata2 <- arrange(fulldata2, percapita)


# Full plot Region #
library(googleVis)
p4 <- gvisMotionChart(fulldata2, idvar = "region", timevar = "year",
                      options=list(width=1000, height=700))

plot(p4)

write(p4$html$chart, file = "RegionAndPopulation_AnimatedPlot.html")

# Full plot Type # # No funcona aun!!#

# Merge population and types 
fulltype <- merge(population1, types)
str(fulltype)
head(fulltype)
fulltype$population <- as.numeric(fulltype$population)
fulltype$vehReg <- as.numeric(fulltype$vehReg)

p <- gvisMotionChart(types, idvar = "type", timevar = "year")

plot(p)

write(p$html$chart, file = "RegionAndPopulation_AnimatedPlot.html")