library(XLConnect)
#library(dplyr)
#library(ggfortify)

# imp_xl <- function(x,y,z,ty,tm=""){
#   x <- readWorksheet(x,sheet =y,region = c(z),header=F)
#   x <- x[-indx,]
#   x.ts <- ts(x,frequency=12,start = c(ty,tm))
#   return(x.ts)
# }

imp_xl_2 <- function(x,y,z,ty,tm=""){
   #x <- wb_12
   #y <- "Table VIII"
   #z <-  "B22:B46"
   #ty <- 2010
   #tm <- 7
  x <- readWorksheet(x,sheet =y,region = c(z),header=F)
  x <- x[-indx,]
  x.ob <- x[grep("OB",time2)]
  x.nb <- x[grep("NB",time2)]
  if(length(x.ob)==0 & length(x.nb)>0)
    x.ob <- rep(0,length(x.nb)) else if(length(x.nb)==0 & length(x.ob)>0)
      x.nb <- rep(0,length(x.ob)) else if(length(x.nb)==0 & length(x.ob)==0) {
        x.ob <- x
        x.nb <- rep(0,length(x)) } else {
          x.ob <- x.ob
          x.nb <- x.nb
        }
  
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


#########################################
######### From actual inflation data ####
########################################

#####  2010-12 #####

time<- readWorksheet(wb_12,sheet = "Table VIII",region = c("A22:A46"),header=F)
time <- as.character(time$Col1) 
indx <- grep("^2",time)
time2 <- time[-indx]
#obs <- grep("OB",time$Col1)
#nbs <- grep("NB",time$Col1)
vars <- c("cpi.10.12.ts","inf.pp.10.12.ts","inf.12m.10.12.ts",
"cpi.food.10.12.ts","inf.food.pp.10.12.ts","inf.food.12m.10.12.ts",
"cpi.nfood.10.12.ts","inf.nfood.pp.10.12.ts","inf.nfood.12m.10.12.ts",
"cpi.cloth.10.12.ts", "cpi.rent.10.12.ts", "cpi.furniture.10.12.ts",
 "cpi.med.10.12.ts","cpi.trans.10.12.ts", "cpi.recreat.10.12.ts",  
"cpi.misc.10.12.ts") 
lts <- LETTERS[2:17]
reg <- c()
for(i in lts){
  reg <- c(reg,paste0(i,22,":",i,46))
}

k <- 0
all_10_12 <- list()
for(i in vars){
  k <- k+1
  assign(i,imp_xl_2(wb_12,"Table VIII",reg[k],ty=2010,tm=7))
  x <- get(i)
  x[x==0] <- NA
  all_10_12 <- c(all_10_12,list(i=x))
}

names(all_10_12) <- vars
save(all_10_12,file="./RDATA/all_10_12.RData")
 
###################################################
#####  2012-14 #####
#==================================================

time<- readWorksheet(wb_14,sheet = "Table VIII",region = c("A25:A63"),header=F)
time <- as.character(time$Col1) 
indx <- grep("^2",time)
time2 <- time[-indx]
vars <- c("cpi.12.14.ts","inf.pp.12.14.ts","inf.12m.12.14.ts",
"cpi.food.12.14.ts","inf.food.pp.12.14.ts","inf.food.12m.12.14.ts",
"cpi.nfood.12.14.ts","inf.nfood.pp.12.14.ts","inf.nfood.12m.12.14.ts",
"cpi.cloth.12.14.ts", "cpi.rent.12.14.ts", "cpi.furniture.12.14.ts",
 "cpi.med.12.14.ts","cpi.trans.12.14.ts", "cpi.recreat.12.14.ts",  
"cpi.misc.12.14.ts") 
lts <- LETTERS[2:17]
reg <- c()
for(i in lts){
  reg <- c(reg,paste0(i,25,":",i,63))
}

all_12_14 <- list()
k <- 0
for(i in vars){
  k <- k+1
  assign(i,imp_xl_2(wb_14,"Table VIII",reg[k],ty=2012,tm=7))
  x <- get(i)
  x[x==0] <- NA
  all_12_14 <- c(all_12_14,list(i=x))
}

names(all_12_14) <- vars
save(all_12_14,file="./RDATA/all_12_14.RData")                  
 
 
###################################################
#####  2014-16 #####
#==================================================

time<- readWorksheet(wb_16,sheet = "Table VII",region = c("A38:A62"),header=F)
time <- as.character(time$Col1) 
indx <- grep("^2",time)
time2 <- time[-indx]
vars <- c("cpi.14.16.ts","inf.pp.14.16.ts","inf.12m.14.16.ts",
"cpi.food.14.16.ts","inf.food.pp.14.16.ts","inf.food.12m.14.16.ts",
"cpi.nfood.14.16.ts","inf.nfood.pp.14.16.ts","inf.nfood.12m.14.16.ts",
"cpi.cloth.14.16.ts", "cpi.rent.14.16.ts", "cpi.furniture.14.16.ts",
 "cpi.med.14.16.ts","cpi.trans.14.16.ts", "cpi.recreat.14.16.ts",  
"cpi.misc.14.16.ts") 
lts <- LETTERS[2:17]
reg <- c()
for(i in lts){
  reg <- c(reg,paste0(i,38,":",i,62))
}

k <- 0
all_14_16 <-list() 

for(i in vars){
  k <- k+1
  assign(i,imp_xl_2(wb_16,"Table VII",reg[k],ty=2014,tm=7))
  x <- get(i)
  x[x==0] <- NA
  all_14_16 <- c(all_14_16,list(i=x))
}

names(all_14_16) <- vars
save(all_14_16,file="./RDATA/all_14_16.RData")

#========= Combining all the files =====================

vars_all <- c("cpi.10.16.ts","inf.pp.10.16.ts","inf.12m.10.16.ts",
          "cpi.food.10.16.ts","inf.food.pp.10.16.ts","inf.food.12m.10.16.ts",
          "cpi.nfood.10.16.ts","inf.nfood.pp.10.16.ts","inf.nfood.12m.10.16.ts",
          "cpi.cloth.10.16.ts", "cpi.rent.10.16.ts", "cpi.furniture.10.16.ts",
          "cpi.med.10.16.ts","cpi.trans.10.16.ts", "cpi.recreat.10.16.ts",  
          "cpi.misc.10.16.ts")

all_10_16 <- list()
k <- 0

for(i in vars_all) {
  k <- k+1
  assign(i,
         ts(rbind(all_10_12[[k]],all_12_14[[k]],all_14_16[[k]]),frequency = 12, start=c(2010,7)))
x <- get(i)
all_10_16 <- c(all_10_16,list(i=x))
}

names(all_10_16) <- vars_all
save(all_10_16,file="./RDATA/all_10_16.RData")
                  
