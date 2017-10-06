library(XLConnect)
library(dplyr)
library(ggfortify)

imp_xl <- function(x,y,z,ty,tm=""){
  x <- readWorksheet(x,sheet =y,region = c(z),header=F)
  x <- x[-indx,]
  x.ob <- x[-grep("OB",x)]
  x.nb <- x[-grep("NB",x)]
  x.ob.ts <- ts(x.ob,frequency=12,start = c(ty,tm))
  x.nb.ts <- ts(x.nb,frequency=12,start = c(ty,tm))
  x.ts <- list(ob=x.ob.ts,nb=x.nb.ts)
  #x.ts <- ts(x,frequency=12,start = c(ty,tm))
  return(x.ts)
}

imp_xl_2 <- function(x,y,z,ty,tm=""){
  #x <- wb_14
  #y <- "Table VIII"
  #z <- "B25:B63"
  #ty <- 2012
  #tm <- 7
  x <- readWorksheet(x,sheet =y,region = c(z),header=F)
  x <- x[-indx,]
  x.ob <- x[grep("OB",time2)]
  x.nb <- x[grep("NB",time2)]
  x.ob.ts <- ts(x.ob,frequency=12,start = c(ty,tm))
  x.nb.ts <- ts(x.nb,frequency=12,start = c(ty,tm))
  x.ts <- cbind(ob=x.ob.ts,nb=x.nb.ts)
  #x.ts <- ts(x,frequency=12,start = c(ty,tm))
  return(x.ts)
}

file_12 <- "./RAWDATA/BB_sep9_2012.xls"
file_14 <- "./RAWDATA/statisticaltable_2014.xls"
file_16 <- "./RAWDATA/statisticaltable_aug2016.xls"
wb_12 <- loadWorkbook(file_12)
wb_14 <- loadWorkbook(file_14)
wb_16 <- loadWorkbook(file_16)
#####  2010-12 #####
dat_12 <- readWorksheet(wb_12,sheet = "Table IB",startRow = 25,endRow=50,startCol =1)
dat_12 <- filter(dat_12, !X2010.11=="2011-12")
inf.pp.10.12.05.06 <- dat_12$X0.99
inf.pp.10.12.95.96 <- dat_12$X10.17
inf.12m.10.12.05.06 <- dat_12$X.
inf.12m.10.12.95.96 <- dat_12$X8.80
inf.pp.10.12.05.06.ts <- ts(inf.pp.10.12.05.06,frequency = 12,start=c(2010,7))
inf.pp.10.12.95.96.ts <- ts(inf.pp.10.12.95.96,frequency = 12,start=c(2010,7))
inf.12m.10.12.05.06.ts <- ts(inf.12m.10.12.05.06,frequency = 12,start=c(2010,7))
inf.12m.10.12.95.96.ts <- ts(inf.12m.10.12.95.96,frequency = 12,start=c(2010,7))

#####  2012-14 #####
dat_14 <- readWorksheet(wb_14,sheet = "Table IB",startRow = 27,endRow=52,startCol =1)
dat_14 <- filter(dat_14, !X2012.13=="2013-14" )
inf.pp.12.14.05.06 <- dat_14$X8.05
inf.pp.12.14.95.96 <- dat_14$X7.97
inf.12m.12.14.05.06 <- dat_14$X6.78
inf.12m.12.14.95.96 <- dat_14$X7.70
inf.pp.12.14.05.06.ts <- ts(inf.pp.12.14.05.06,frequency = 12,start=c(2012,7))
inf.pp.12.14.95.96.ts <- ts(inf.pp.12.14.95.96,frequency = 12,start=c(2012,7))
inf.12m.12.14.05.06.ts <- ts(inf.12m.12.14.05.06,frequency = 12,start=c(2012,7))
inf.12m.12.14.95.96.ts <- ts(inf.12m.12.14.95.96,frequency = 12,start=c(2012,7))


# Please note that from July of 201, inflation with base 95-96 has been suspended and 05-06 has been in full use3

#####  2014-16 #####

dat_16 <- readWorksheet(wb_16,sheet = "Table IB",startRow = 39,endRow=64,startCol =1)
dat_16 <- filter(dat_16, !X2014.15=="2015-16" )
inf.pp.14.16.05.06 <- dat_16$X6.25
inf.pp.14.16.95.96 <- dat_16$X.
inf.12m.14.16.05.06 <- dat_16$X6.40
inf.12m.14.16.95.96 <- dat_16$X..1
inf.pp.14.16.05.06.ts <- ts(inf.pp.14.16.05.06,frequency = 12,start=c(2014,7))
inf.pp.14.16.95.96.ts <- ts(inf.pp.14.16.95.96,frequency = 12,start=c(2014,7))
inf.12m.14.16.05.06.ts <- ts(inf.12m.14.16.05.06,frequency = 12,start=c(2014,7))
inf.12m.14.16.95.96.ts <- ts(inf.12m.14.16.95.96,frequency = 12,start=c(2014,7))



ts.list <- grep("ts$",ls(),value=T)

for(i in 1:length(ts.list)){
  x <- get(ts.list[i])
  #get(ts.list[i])
  save(x,file=paste0("./RDATA/",ts.list[i],".RData"))
}

# july 2010 - june 2016
inf.pp.10.16.95.96 <- c(inf.pp.10.12.95.96.ts,inf.pp.12.14.95.96,inf.pp.14.16.95.96)
inf.pp.10.16.95.96.ts <- ts(inf.pp.10.16.95.96,frequency = 12, start=c(2010,7))

inf.pp.10.16.05.06 <- c(inf.pp.10.12.05.06.ts,inf.pp.12.14.05.06,inf.pp.14.16.05.06)
inf.pp.10.16.05.06.ts <- ts(inf.pp.10.16.05.06,frequency = 12, start=c(2010,7))

# up to 2012 april we keep 95 base, then from 2012 may we maintain 05 base becase that's what 
# data says in the above. from may 2012 we have full data on 05 base
inf.pp.10.12apr.95.96.ts <- window(inf.pp.10.16.95.96.ts,end=c(2012,4))

inf.pp.12may.16.05.06.ts <- window(inf.pp.10.16.05.06.ts,c(2012,5),2016)

inf.pp.10.16.95.05.ts <- ts(c(inf.pp.10.12apr.95.96.ts,inf.pp.12may.16.05.06.ts),
                            frequency = 12,
                            start=c(2010,7))

# just noticed that the numbers are being stored as characters, therefore I have to change the mode

mode(inf.pp.10.16.95.05.ts) <- "numeric"


######### 12 month average ########
# july 2010 - june 2016
inf.12m.10.16.95.96 <- c(inf.12m.10.12.95.96.ts,inf.12m.12.14.95.96,inf.12m.14.16.95.96)
inf.12m.10.16.95.96.ts <- ts(inf.12m.10.16.95.96,frequency = 12, start=c(2010,7))

inf.12m.10.16.05.06 <- c(inf.12m.10.12.05.06.ts,inf.12m.12.14.05.06,inf.12m.14.16.05.06)
inf.12m.10.16.05.06.ts <- ts(inf.12m.10.16.05.06,frequency = 12, start=c(2010,7))
mode(inf.12m.10.16.05.06.ts) <- "numeric"
plot(window(inf.12m.10.16.05.06.ts,start=c(2014,7)))

#########################################
######### From actual inflation data ####
########################################

# I am using the following function to import all the data, I did not use at the beginning but from food inflation in 2012 I started implementign it

imp_xl <- function(x,y,z,ty,tm=""){
  x <- readWorksheet(x,sheet =y,region = c(z),header=F)
  x <- x[-which(time.12$Col1=="2011-12"),]
  x.ts <- ts(x,frequency=12,start = c(ty,tm))
  return(x.ts)
}

#####  2010-12 #####
# importing general cpi
#dat_12 <- readWorksheet(wb_12,sheet = "Table VIII",startRow = 25,endRow=50,startCol =1)

time.12 <- readWorksheet(wb_12,sheet = "Table VIII",region = c("A22:A46"),header=F)

cpi.10.12.ts <- imp_xl(wb_12,"Table VIII","B22:B46",ty=2010,tm=7)

# cpi.12 <- readWorksheet(wb_12,sheet = "Table VIII",region = c("B22:B46"),header=F)
# cpi.12 <- cpi.12[-which(time.12$Col1=="2011-12"),]
# cpi.10.12.ts <- ts(cpi.12,frequency=12,start = c(2010,7))

# Now we will create a data frame just in case I want to directly run ggplot on the data frame without using autoplot

cpi.10.12.df <- data.frame(time=yearmon(time(cpi.10.12.ts)),cpi=cpi.10.12.ts)

# Now import inflation pp

inf.pp.10.12.ts <-imp_xl(wb_12,"Table VIII","C22:C46",ty=2010,tm=7) 
#-------------------------
# import inflation 12m
inf.12m.10.12.ts <-imp_xl(wb_12,"Table VIII","D22:D46",ty=2010,tm=7) 
#--------------------------
# import food cpi
cpi.food.10.12.ts <-imp_xl(wb_12,"Table VIII","E22:E46",ty=2010,tm=7) 

#--------------------------
# import food inflation pp
inf.food.pp.10.12.ts <-imp_xl(wb_12,"Table VIII","F22:F46",ty=2010,tm=7) 
#--------------------------
# import food inflation 12m
inf.food.12m.10.12.ts <-imp_xl(wb_12,"Table VIII","G22:G46",ty=2010,tm=7) 

#--------------------------
# import non food CPI
cpi.nfood.10.12.ts <-imp_xl(wb_12,"Table VIII","H22:H46",ty=2010,tm=7) 

#--------------------------
# import non-food inflation pp
inf.nfood.pp.10.12.ts <-imp_xl(wb_12,"Table VIII","I22:I46",ty=2010,tm=7) 

#--------------------------
# import non-food inflation 12m
inf.nfood.12m.10.12.ts <-imp_xl(wb_12,"Table VIII","J22:J46",ty=2010,tm=7) 

#--------------------------
# import clothing and footwear CPI
inf.cpi.cloth.10.12.ts <-imp_xl(wb_12,"Table VIII","K22:K46",ty=2010,tm=7) 


#--------------------------
# import Gross rent, fuel and lighting CPI
inf.cpi.rent.10.12.ts <-imp_xl(wb_12,"Table VIII","L22:L46",ty=2010,tm=7) 

#--------------------------
# import furniture, furnishing and other
inf.cpi.furniture.10.12.ts <-imp_xl(wb_12,"Table VIII","M22:M46",ty=2010,tm=7) 


#--------------------------
# import medical and health care expenses
inf.cpi.med.10.12.ts <-imp_xl(wb_12,"Table VIII","N22:N46",ty=2010,tm=7) 


#--------------------------
# import transport and communications
inf.cpi.trans.10.12.ts <-imp_xl(wb_12,"Table VIII","O22:O46",ty=2010,tm=7) 


#--------------------------
# import recreation, entertainment, education and cultural services
inf.cpi.recreat.10.12.ts <-imp_xl(wb_12,"Table VIII","P22:P46",ty=2010,tm=7) 


#--------------------------
# import Misc goods and services
inf.cpi.misc.10.12.ts <-imp_xl(wb_12,"Table VIII","Q22:Q46",ty=2010,tm=7) 

###################################################
#==================================================

time<- readWorksheet(wb_14,sheet = "Table VIII",region = c("A25:A63"),header=F)
time <- as.character(time$Col1) 
indx <- grep("^2",time)
time2 <- time[-indx]
obs <- grep("OB",time$Col1)
nbs <- grep("NB",time$Col1)
vars <- c("cpi.12.14.ts","inf.pp.12.14.ts","inf.12m.12.14.ts",
"cpi.food.12.14.ts","inf.food.pp.12.14.ts","inf.food.12m.12.14.ts",
"cpi.nfood.12.14.ts","inf.nfood.pp.12.14.ts","inf.nfood.12m.12.14.ts",
"inf.cpi.cloth.12.14.ts", "inf.cpi.rent.12.14.ts", "inf.cpi.furniture.12.14.ts",
 "inf.cpi.med.12.14.ts","inf.cpi.trans.12.14.ts", "inf.cpi.recreat.12.14.ts",  
"inf.cpi.misc.12.14.ts") 
lts <- LETTERS[2:17]
reg <- c()
for(i in lts){
  reg <- c(reg,paste0(i,25,":",i,63))
}
k <- 0
for(i in vars){
  k <- k+1
  assign(i,imp_xl_2(wb_14,"Table VIII",reg[k],ty=2012,tm=7))
}
                  
 
