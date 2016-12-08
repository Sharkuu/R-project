options(prompt = "R: " )

library(lattice)

data.ch4 <- read.csv2("v4.2_CH4_tot_1970_2008.csv",skip = 2051,stringsAsFactors = FALSE, header = F)
data.ch4 <- data.ch4[0:879,]
data.ch4$V1 <-NULL
data.ch4$V2 <-NULL
data.ch4$V46 <- NULL
names(data.ch4) <- sapply(read.csv2("v4.2_CH4_tot_1970_2008.csv")[9,3:45], as.character)
na.omit(data.ch4)



srednie.wszystkie.panstwa.wszystkie.zrodla = data.frame(ISO_A3 = (unique(data.ch4$ISO_A3)),
                                                        Name = unique(data.ch4$Name),
                                                        date1970.1980 = NA,
                                                        date1981.1990 = NA,
                                                        date1991.2000 =NA,
                                                        date2001.2008 = NA,
                                                        summary.mean = NA,stringsAsFactors=FALSE
                                                        )

for(i in unique(data.ch4$Name)){
  x<-data.ch4[data.ch4$Name == i,]
  s<-rowMeans(x[,5:15],na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,3] <-mean(s,na.rm = TRUE)
  
  s<-rowMeans(x[,12:25],na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,4] <-mean(s,na.rm = TRUE)
  
  s<-rowMeans(x[,22:35],na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,5] <-mean(s,na.rm = TRUE)
  
  s<-rowMeans(x[,32:43],na.rm = TRUE)
  srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name == i,6] <-mean(s,na.rm = TRUE)
  
  
  
}
srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$ISO_A3 == "MKD",2] <- "Macedonia"
srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$ISO_A3 == "SCG",2] <- "Serbia"
srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$ISO_A3 == "SCG",1] <- "SRB"
tmp <- srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$ISO_A3 == "SRB",]
tmp$ISO_A3 <- "MNE"
tmp$Name <- "Montenegro"
srednie.wszystkie.panstwa.wszystkie.zrodla <- rbind(srednie.wszystkie.panstwa.wszystkie.zrodla,tmp)
tmp$ISO_A3 <- "KVX"
tmp$Name <- "Kosovo"
srednie.wszystkie.panstwa.wszystkie.zrodla <- rbind(srednie.wszystkie.panstwa.wszystkie.zrodla,tmp)
#srednia calosciowa i segregowanie
srednie.wszystkie.panstwa.wszystkie.zrodla$summary.mean <-rowMeans(srednie.wszystkie.panstwa.wszystkie.zrodla[,3:6])
srednie.wszystkie.panstwa.wszystkie.zrodla<- srednie.wszystkie.panstwa.wszystkie.zrodla[with(srednie.wszystkie.panstwa.wszystkie.zrodla, order(-summary.mean)), ]
#barcharty 3 najwiekszych(w tym polska) 
a <- barchart(date1970.1980 ~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,3)], col = "orange", main = "Najwieksza emisja lata 1970-1980",ylim = c(0:340), ylab = "Srednia [Gg]")
b <- barchart(date1981.1990 ~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,4)], col = "orange", main = "Najwieksza emisja lata 1981-1990",ylim = c(0:340), ylab = "Srednia [Gg]")
c <- barchart(date1991.2000 ~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,5)], col = "orange", main = "Najwieksza emisja lata 1991-2000",ylim = c(0:340), ylab = "Srednia [Gg]")
d <- barchart(date2001.2008 ~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla[1:3,c(2,6)], col = "orange", main = "Najwieksza emisja lata 2001-2008",ylim = c(0:340), ylab = "Srednia [Gg]")
print(a, split = c(1,1,4,1), more = T)
print(b, split = c(2,1,4,1), more = T)
print(c, split = c(3,1,4,1), more = T)
print(d, split = c(4,1,4,1))


######dotplot wszystkie panstwa wszystkie zrodla

barchart(date1970.1980~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Srednie wartosci emisji lata 1970-1980",
         ylab = "Srednia [Gg]",
         ylim = c(0:340),
         panel=function(x,y,subscripts,...){
             panel.grid(h=15,v=0) 
             panel.barchart(x,y,...)
         })

barchart(date1981.1990~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Srednie wartosci emisji lata 1981-1990",
         ylab = "Srednia [Gg]",
         ylim = c(0:340),
         panel=function(x,y,subscripts,...){
           panel.grid(h=15,v=0) 
           panel.barchart(x,y,...)
         })
barchart(date1991.2000~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Srednie wartosci emisji lata 1991-2000",
         ylab = "Srednia [Gg]",
         ylim = c(0:340),
         panel=function(x,y,subscripts,...){
           panel.grid(h=15,v=0) 
           panel.barchart(x,y,...)
         })
barchart(date2001.2008~ Name, data = srednie.wszystkie.panstwa.wszystkie.zrodla,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Srednie wartosci emisji lata 2001-2008",
         ylab = "Srednia [Gg]",
         ylim = c(0:340),
         panel=function(x,y,subscripts,...){
           panel.grid(h=15,v=0) 
           panel.barchart(x,y,...)
         })
##########polska emisje
polska.srednie <-data.frame(zrodlo = unique(data.ch4$IPCC_description),
                            date1970.1980 = NA,
                            date1981.1990 = NA,
                            date1991.2000 =NA,
                            date2001.2008 = NA)
poland <- data.ch4[data.ch4$Name=='Poland',]
na.omit(poland)
for (i in unique(poland$IPCC_description)) {
  x<-data.ch4[data.ch4$IPCC_description == i,]
  
  s<-rowMeans(x[,5:15],na.rm = TRUE)
  polska.srednie[polska.srednie$zrodlo == i,2] <-mean(s,na.rm = TRUE)
  
  s<-rowMeans(x[,12:25],na.rm = TRUE)
  polska.srednie[polska.srednie$zrodlo == i,3] <-mean(s,na.rm = TRUE) 
  
  s<-rowMeans(x[,22:35],na.rm = TRUE)
  polska.srednie[polska.srednie$zrodlo == i,4] <-mean(s,na.rm = TRUE)
  
  s<-rowMeans(x[,32:43],na.rm = TRUE)
  polska.srednie[polska.srednie$zrodlo == i,5] <-mean(s,na.rm = TRUE) 
  
}
#sortowanie
#polska.srednie<- polska.srednie[with(polska.srednie, order(-date1970.1980)), ]
barchart(date1970.1980~ zrodlo, data = polska.srednie,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Polska lata 1970-1980",
         ylab = "Srednia [Gg]",
         ylim = c(0:380),
         panel=function(x,y,subscripts,...){
           panel.grid(h=15,v=0) 
           panel.barchart(x,y,...)
         })
barchart(date1981.1990~ zrodlo, data = polska.srednie,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Polska lata 1981-1990",
         ylab = "Srednia [Gg]",
         ylim = c(0:380),
         panel=function(x,y,subscripts,...){
           panel.grid(h=15,v=0) 
           panel.barchart(x,y,...)
         })
barchart(date1991.2000~ zrodlo, data = polska.srednie,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Polska lata 1991-2000",
         ylab = "Srednia [Gg]",
         ylim = c(0:380),
         panel=function(x,y,subscripts,...){
           panel.grid(h=15,v=0) 
           panel.barchart(x,y,...)
         })
barchart(date2001.2008~ zrodlo, data = polska.srednie,scales=list(x=list(rot=45)),
         col = "orange",
         main = "Polska lata 2001-2008",
         ylab = "Srednia [Gg]",
         ylim = c(0:380),
         panel=function(x,y,subscripts,...){
           panel.grid(h=15,v=0) 
           panel.barchart(x,y,...)
         })
#######polska po 88
polska88 <- data.ch4[data.ch4$Name=='Poland',]
polska88 <- colMeans(polska88[,23:43], na.rm = TRUE)
#######dla kazdego zrodla, wszystkie panstwa, dekadowo
srednie.wszystkie.zrodla = data.frame(zrodlo = unique(data.ch4$IPCC_description),
                                                        date1970.1980 = NA,
                                                        date1981.1990 = NA,
                                                        date1991.2000 =NA,
                                                        date2001.2008 = NA,
                                                        summary.mean = NA
)

for (i in unique(data.ch4$IPCC_description)) {
  x<-data.ch4[data.ch4$IPCC_description == i,]
  
  s<-rowMeans(x[,5:15],na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,2] <-mean(s,na.rm = TRUE)
  
  s<-rowMeans(x[,12:25],na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,3] <-mean(s,na.rm = TRUE) 
  
  s<-rowMeans(x[,22:35],na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,4] <-mean(s,na.rm = TRUE)
  
  s<-rowMeans(x[,32:43],na.rm = TRUE)
  srednie.wszystkie.zrodla[srednie.wszystkie.zrodla$zrodlo == i,5] <-mean(s,na.rm = TRUE) 
  
}
rm(s,x)

srednie.wszystkie.zrodla$summary.mean <-rowMeans(srednie.wszystkie.zrodla[,2:5])
srednie.wszystkie.zrodla<- srednie.wszystkie.zrodla[with(srednie.wszystkie.zrodla, order(-summary.mean)), ]

####wykresy zrodel wybranych(2 top 2 middle, 2 end) rocznie
j<-1
for(i in srednie.wszystkie.zrodla[c(1,2,11,12,20,21),1]){
  tmp <- subset(data.ch4, data.ch4[,4] == as.name(i))
  tmp <- colMeans(tmp[,5:43],na.rm = TRUE)
  if(j==1){ 
    plot(names(tmp),tmp,type = "l", col = j, xlab = "rok",lwd=4, ylab = paste(as.name(i)," [Gg]"))
    j<-j+1
  }else{
    print(tmp)
    plot(names(tmp),tmp,type = "l", col = j, xlab = "rok",lwd=4, ylab = paste(as.name(i)," [Gg]"))
    j<- j+1
  }}

##sasiedzi
sasiedzi <- srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Germany" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Ukraine" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Czech Republic" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Slovakia" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Belarus" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Lithuania" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Poland",]
j<-1
colors.plot <-c(1,2,"orange",4,5,6,"green")
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
for (i in 1:nrow(sasiedzi)) {
    if(j==1){
        plot(c(1970,1980,1990,2000),sasiedzi[i,3:6], type = "o",lwd=3,xaxt = "n", col = colors.plot[j],ylim = c(0,350), ylab = "Srednia [Gg]", xlab = "Dekada", main = "Polska na tle sasiadow")
        axis(1, at = c(1970,1980,1990,2000), labels = c("1970-1980","1981-1990","1991-2000","2001-2008") )
        j <- j+1
      }
    else{
        lines(c(1970,1980,1990,2000),sasiedzi[i,3:6], type = "o",lwd=3, col = colors.plot[j])
      axis(1, at = c(1970,1980,1990,2000), labels = c("1970-1980","1981-1990","1991-2000","2001-2008") )
      
        j <- j+1
        
    }
    
}
legend('topright','groups',inset=c(-0.2,-0.2), sasiedzi$Name, lty=c(1,1), lwd=c(2.5,2.5),col=colors.plot) 


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


pdf('Europa_rozklad70-80.pdf')

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


dev.off()

######

map.frame <- data.frame(
  country=srednie.wszystkie.panstwa.wszystkie.zrodla$Name,
  value=srednie.wszystkie.panstwa.wszystkie.zrodla$date1981.1990)

converted.map.frame <- joinCountryData2Map(map.frame, joinCode="NAME", nameJoinColumn="country")


pdf('Europa_rozklad81-90.pdf')

mapParams <-mapCountryData(converted.map.frame, nameColumnToPlot="value", mapTitle="Europa 1981-1990",
                           mapRegion="Europe", colourPalette="heat",missingCountryCol = "dark grey",
                           aspect=1.5,borderCol = "gray20",oceanCol="lightcyan2",
                           catMethod =c(seq(0,400,by = 10)),addLegend = FALSE)

do.call( addMapLegend, c( mapParams, legendLabels="all", legendWidth=0.5 ))

labelCountries(dF = "",nameCountryColumn = "NAME",nameX = "LON",nameY = "LAT",
               nameColumnToPlot= "value",col = 'black',cex = 0.4)

dev.off()
###

map.frame <- data.frame(
  country=srednie.wszystkie.panstwa.wszystkie.zrodla$Name,
  value=srednie.wszystkie.panstwa.wszystkie.zrodla$date1991.2000)

converted.map.frame <- joinCountryData2Map(map.frame, joinCode="NAME", nameJoinColumn="country")


pdf('Europa_rozklad91-00.pdf')

mapParams <-mapCountryData(converted.map.frame, nameColumnToPlot="value", mapTitle="Europa 1991-2000",
                           mapRegion="Europe", colourPalette="heat",missingCountryCol = "dark grey",
                           aspect=1.5,borderCol = "gray20",oceanCol="lightcyan2",
                           catMethod =c(seq(0,400,by = 10)),addLegend = FALSE)

do.call( addMapLegend, c( mapParams, legendLabels="all", legendWidth=0.5 ))

labelCountries(dF = "",nameCountryColumn = "NAME",nameX = "LON",nameY = "LAT",
               nameColumnToPlot= "value",col = 'black',cex = 0.4)

dev.off()
####

map.frame <- data.frame(
  country=srednie.wszystkie.panstwa.wszystkie.zrodla$Name,
  value=srednie.wszystkie.panstwa.wszystkie.zrodla$date2001.2008)

converted.map.frame <- joinCountryData2Map(map.frame, joinCode="NAME", nameJoinColumn="country")


pdf('Europa_rozklad01-08.pdf')

mapParams <-mapCountryData(converted.map.frame, nameColumnToPlot="value", mapTitle="Europa 2001-2008",
                           mapRegion="Europe", colourPalette="heat",missingCountryCol = "dark grey",
                           aspect=1.5,borderCol = "gray20",oceanCol="lightcyan2",
                           catMethod =c(seq(0,400,by = 10)),addLegend = FALSE)

do.call( addMapLegend, c( mapParams, legendLabels="all", legendWidth=0.5 ))

labelCountries(dF = "",nameCountryColumn = "NAME",nameX = "LON",nameY = "LAT",
               nameColumnToPlot= "value",col = 'black',cex = 0.4)

dev.off()



# Mapy drugiego typu, Heatmapa
# in progress, problemy są z legendą


data.heat <- read.csv2("v42_CH4_1970_TOT.txt",skip = 3,stringsAsFactors = FALSE, header = F)
num_data <- data.frame(data.matrix(data.heat))

library(maptools)
library(png)
library(grid)


map1 <- readShapePoly("CNTR_2014_03M_SH/Data/CNTR_RG_03M_2014.shp")
coordinates(num_data) <- ~V2+V1  
gridded(num_data) <- TRUE

# Poprzednia wersja
#
# png(file="Map3.png",width=35,height=30,unit="cm", res=200, type = "cairo")
# 
# spplot(num_data["V3"], xlim=c(-5,35), ylim=c(35,70),
#        sp.layout = list("sp.polygons",map1),
#        contour=F,at=at
#        #colorkey=list(at=seq(0, 400, 30))
# )

at <- c(0e+0, 1.5e-5, 1.0e-4, 1.0e-3, 1.0e-2, 1.0e-1, 1.0e+0, 2.0e+0,5.0e+0, 1.0e+1,5.0e+1, 1.0e+2, 2.0e+2,5.0e+2,1.0e+3,5.0e+3)
png(file="HeatMap999.png",width=37,height=28,unit="cm", res=100, type = "cairo")
num_data@data$cutV3 <- cut(num_data@data$V3, breaks = c(at,Inf)) # converted numeric to factor
 
spplot(num_data["cutV3"], xlim=c(-11, 38), ylim=c(34, 71), main=list(label="Europa Xyear",cex=1.5), 
        colorkey = list(height = 1, labels = list(at = seq(0.5, length(at) -0.5), labels = at)),col.regions=colorRampPalette(c("blue4","purple4", "yellow2", "red4")), #col.regions=rev(rainbow(24, start = 0, end = 12/24))
        sp.layout = list("sp.polygons", map1, first = F), contour = F)
grid.text("Roczna skala emisji metanu (tony)", x=unit(0.99, "npc"), y=unit(0.50, "npc"), rot=-90, gp=gpar(fontsize=13,fontface=2))

dev.off()


#Petla do heatmap
# for(i in 0:38) {
#   file.input <- paste0("v42_CH4_",1970+i,"_TOT.txt")
#   title <- paste0("Europa rok ",1970+i)
#   image.output <- paste0("Europa_Heatmap_",1970+i,".png") 
#   print(paste0(file.input," ",title," ",image.output))
# }
