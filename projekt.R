options(prompt = "R: " )

library(lattice)

data.ch4 <- read.csv2("v4.2_CH4_tot_1970_2008.csv",skip = 2051,stringsAsFactors = FALSE, header = F)
data.ch4 <- data.ch4[0:879,]
data.ch4$V1 <-NULL
data.ch4$V2 <-NULL
data.ch4$V46 <- NULL
names(data.ch4) <- sapply(read.csv2("v4.2_CH4_tot_1970_2008.csv")[9,3:45], as.character)
data.ch4[is.na(data.ch4)] <-0.0

na.omit(data.ch4)

plot(names(data.ch4)[5:43],data.ch4[1,5:43],type = "l")

t<- (data.ch4[seq(from = 1, to = 879, by=23),5:43])
t <- colMeans(t)
