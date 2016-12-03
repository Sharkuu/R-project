options(prompt = "R: " )

library(lattice)

data.ch4 <- read.csv2("v4.2_CH4_tot_1970_2008.csv",skip = 2051,stringsAsFactors = FALSE, header = F)
data.ch4 <- data.ch4[0:879,]
data.ch4$V1 <-NULL
data.ch4$V2 <-NULL
data.ch4$V46 <- NULL
names(data.ch4) <- sapply(read.csv2("v4.2_CH4_tot_1970_2008.csv")[9,3:45], as.character)
#data.ch4[is.na(data.ch4)] <-0.0
na.omit(data.ch4)

plot(names(data.ch4)[5:43],data.ch4[1,5:43],type = "l")

public.electricity.heat<-subset(data.ch4, data.ch4[,4] == "Public electricity and heat production")

public.electricity.heat$mean <- rowMeans(public.electricity.heat[,5:43])


dotplot(mean~ Name, data = public.electricity.heat)

public.electricity.heat[which.min(public.electricity.heat$mean),]


#for(i in 1:nrow(public.electricity.heat)){
 # if(i==1){
  #plot(names(public.electricity.heat)[5:43],public.electricity.heat[1,5:43],type = "l", col = 1, ylim=c(0,13))
   # 
  #}else{
  
    
   # lines(names(public.electricity.heat)[5:43],public.electricity.heat[i,5:43],type = "l", col = i)
  #}
  
#}
country_names <- unique(data.ch4$Name)
iso_a3 <-unique(data.ch4$ISO_A3)

srednie.wszystkie.panstwa.wszystkie.zrodla = data.frame(ISO_A3 = (unique(data.ch4$ISO_A3)),
                                                        Name = country_names,
                                                        date1970.1980 = NA,
                                                        date1981.1990 = NA,
                                                        date1991.2000 =NA,
                                                        date2001.2008 = NA,
                                                        summary.mean = NA
                                                        )

for(i in country_names){
  x<-data.ch4[data.ch4$Name == i,]
  k<-x[,5:15]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,3] <-mean(s,na.rm = TRUE)
  
  k<-x[,12:25]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,4] <-mean(s,na.rm = TRUE)
  
  k<-x[,22:35]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,5] <-mean(s,na.rm = TRUE)
  
  k<-x[,32:43]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,6] <-mean(s,na.rm = TRUE)
  
  
  
}

#srednia calosciowa i segregowanie
srednie.wszystkie.panstwa.wszystkie.zrodla$summary.mean <-rowMeans(srednie.wszystkie.panstwa.wszystkie.zrodla[,3:6])
srednie.wszystkie.panstwa.wszystkie.zrodla<- srednie.wszystkie.panstwa.wszystkie.zrodla[with(srednie.wszystkie.panstwa.wszystkie.zrodla, order(-summary.mean)), ]
#barcharty 3 najwiekszych(w tym polska)   ####SKALA!!!!!!!!!
barchart(Name ~ date1970.1980, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,3)])
barchart(Name ~ date1981.1990, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,4)])
barchart(Name ~ date1991.2000, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,5)])
barchart(Name ~ date2001.2008, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,6)])


#######dla kazdego zrodla, wszystkie panstwa, dekadowo
source_names <- unique(data.ch4$IPCC_description)

srednie.wszystkie.zrodla = data.frame(zrodlo = unique(data.ch4$IPCC_description),
                                                        date1970.1980 = NA,
                                                        date1981.1990 = NA,
                                                        date1991.2000 =NA,
                                                        date2001.2008 = NA,
                                                        summary.mean = NA
)

for (i in source_names) {
  x<-data.ch4[data.ch4$IPCC_description == i,]
  k<-x[,5:15]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,2] <-mean(s,na.rm = TRUE)
  
  k<-x[,12:25]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,3] <-mean(s,na.rm = TRUE) 
  
  k<-x[,22:35]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,4] <-mean(s,na.rm = TRUE)
  
  k<-x[,32:43]
  s<-rowMeans(k,na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,5] <-mean(s,na.rm = TRUE) 
  
}
rm(k,s,x)

srednie.wszystkie.zrodla$summary.mean <-rowMeans(srednie.wszystkie.zrodla[,2:5])
srednie.wszystkie.zrodla<- srednie.wszystkie.zrodla[with(srednie.wszystkie.zrodla, order(-summary.mean)), ]

####wykresy zrodel wybranych(2 top 2 middle, 2 end) rocznie (nie moga byc na jednym bo skala jest pojebana)
j<-1
for(i in srednie.wszystkie.zrodla[c(1,2,11,12,20,21),1]){
  tmp <- subset(data.ch4, data.ch4[,4] == as.name(i))
  tmp <- colMeans(tmp[,5:43],na.rm = TRUE)
 if(j==1){
    plot(names(tmp),tmp,type = "l", col = j, xlab = "rok", ylab = as.name(i))
   j<-j+1
}else{
  print(tmp)
 plot(names(tmp),tmp,type = "l", col = j, xlab = "rok", ylab = as.name(i))
  j<- j+1
}}



# Mapka testy
# TODO or NOT TODO
# a. Plotować tylko nazwy panstw, ktore sa z europy i maja wartosci
# b. Rozwiazac problem z serbia i czarnogura
# c. Pomyslec czy mozna zrobic tak, zeby nazwy panstw sie nie zaslanialy
#    (Customowe nazwy panstw i kooordynaty, zakomentowana opcja)
# d. Plotowanie wartości przy panstwie?

library(rworldmap)


map.frame <- data.frame(
  country=srednie.wszystkie.panstwa.wszystkie.zrodla$Name,
  value=srednie.wszystkie.panstwa.wszystkie.zrodla$date1970.1980)

converted.map.frame <- joinCountryData2Map(map.frame, joinCode="NAME", nameJoinColumn="country")


pdf('Europa_rozklad.pdf')

# Poprzedni sposob zapisu do png, ale słaba jakosc niestety wychodzi 
#
# mapDevice(
#   device = 'png',filename='test.png',rows = 1,columns = 1,plotOrder="rows",
#   width = 1000,height = 1000,titleSpace = NULL,xaxs = "i",yaxs = "i")

mapParams <-mapCountryData(converted.map.frame, nameColumnToPlot="value", mapTitle="Europa 1970-1980",
                           mapRegion="Europe", colourPalette="heat",missingCountryCol = "dark grey",
                           aspect=1.5,borderCol = "gray20",oceanCol="lightcyan2",
                           catMethod =c(seq(0,400,by = 10)),addLegend = FALSE)
#catMethod ="pretty"

do.call( addMapLegend, c( mapParams, legendLabels="all", legendWidth=0.5 ))

labelCountries(dF = "",nameCountryColumn = "NAME",nameX = "LON",nameY = "LAT",
               nameColumnToPlot= "value",col = 'black',cex = 0.4)


#Stare nazywanie nazw
#
# Coordynaty
#country_coord<-data.frame(coordinates(n),stringsAsFactors=F)
# Nazwy panstw
#text(x=country_coord$X1,y=country_coord$X2,labels=row.names(country_coord),cex=0.5)
#
#Krotszy sposob nazw
#text(n, labels="NAME")
#

dev.off()
