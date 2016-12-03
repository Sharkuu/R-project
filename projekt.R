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

####################to trzeba zmienic bo to dotplot tylko dla elektrowni i cieplowni
public.electricity.heat<-subset(data.ch4, data.ch4[,4] == "Public electricity and heat production")

public.electricity.heat$mean <- rowMeans(public.electricity.heat[,5:43])


dotplot(mean~ Name, data = public.electricity.heat)

public.electricity.heat[which.min(public.electricity.heat$mean),]
##########################################################################3

srednie.wszystkie.panstwa.wszystkie.zrodla = data.frame(ISO_A3 = unique(data.ch4$ISO_A3),
                                                        Name = unique(data.ch4$Name),
                                                        date1970.1980 = NA,
                                                        date1981.1990 = NA,
                                                        date1991.2000 =NA,
                                                        date2001.2008 = NA,
                                                        summary.mean = NA
                                                        )

for(i in unique(data.ch4$Name)){
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
 

srednie.wszystkie.zrodla = data.frame(zrodlo = unique(data.ch4$IPCC_description),
                                                        date1970.1980 = NA,
                                                        date1981.1990 = NA,
                                                        date1991.2000 =NA,
                                                        date2001.2008 = NA,
                                                        summary.mean = NA
)

for (i in unique(data.ch4$IPCC_description)) {
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

####wykresy zrodel wybranych(2 top 2 middle, 2 end) rocznie (nie moga byc na jednym bo skala jest pojebana CHYBA?)
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


####problem jest taki ze to sa srednie z dekad, a to nie moze byc jako wartosc x'a dlatego jest c(1970,1980,1990,2000)
sasiedzi <- srednie.wszystkie.panstwa.wszystkie.zrodla[srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Germany" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Ukraine" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Czech Republic" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Slovakia" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Belarus" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Lithuania" | srednie.wszystkie.panstwa.wszystkie.zrodla$Name=="Poland",]
j<-1
for (i in 1:nrow(sasiedzi)) {
  if(j==1){
    plot(c(1970,1980,1990,2000),sasiedzi[i,3:6], type = "l", col = j,ylim = c(0,350), ylab = "srednia", xlab = "Dekada(nie do konca BO LE")
    j <- j+1
  }
  else{
    lines(c(1970,1980,1990,2000),sasiedzi[i,3:6], type = "l", col = j)
    j <- j+1
    
  }
  
}
