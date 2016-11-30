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


for(i in 1:nrow(public.electricity.heat)){
  if(i==1){
  plot(names(public.electricity.heat)[5:43],public.electricity.heat[1,5:43],type = "l", col = 1, ylim=c(0,13))
    
  }else{
  
    
    lines(names(public.electricity.heat)[5:43],public.electricity.heat[i,5:43],type = "l", col = i)
  }
  
}
