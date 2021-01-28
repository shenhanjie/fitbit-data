# Fitbit Physical Activity Project
# Hanjie Shen, 01/14/2020

# Required Packages
require(fdapace)
require(fda.usc)
require(fda)
require(plyr)
require(readr)
require(MFPCA)

# Read Fitbit Data
setwd("~/Desktop/Fitbit Project/Fitbit Data")
fitbit.dat = list.files(pattern="*.csv")

# Generate dataset for each ppt.
c1.dat = ldply(as.list(fitbit.dat[1:12]), read.csv)
c2.dat = ldply(as.list(fitbit.dat[13:24]), read.csv)
c3.dat = ldply(as.list(fitbit.dat[25:39]), read.csv)
c4.dat = ldply(as.list(fitbit.dat[40:55]), read.csv)
c5.dat_part1 = ldply(as.list(fitbit.dat[56:65]), read.csv)
c5.dat_part2 = read.csv("C5_11.csv", header = F)
names(c5.dat_part2) = names(c5.dat_part1)
c5.dat_part3 = ldply(as.list(fitbit.dat[67]), read.csv)
c5.dat = Reduce(function(...) rbind(...), list(c5.dat_part1,c5.dat_part2,c5.dat_part3))


id_c1 = unique(c1.dat$ID)
id_c2 = unique(c2.dat$ID)
id_c3 = unique(c3.dat$ID)
id_c4 = unique(c4.dat$ID)
id_c5 = unique(c5.dat$ID)

# Prepare data for FPCA
setwd("~/Desktop/Fitbit Project")
ppt.dat = function(id, dat){
  ppt.alldat = dat[dat$ID == id,]
  ppt_date = unique(ppt.alldat$date)
  ppt_dat_daily = data.frame(ID = rep(id,length(ppt_date)))
  ppt_dat_daily$date = ppt_date
  for (i in 1:length(ppt_date)){
    ppt_dat_daily$steps[i] = sum(ppt.alldat$steps[ppt.alldat$date == ppt_date[i]])
    ppt_dat_daily$distance[i] = sum(ppt.alldat$distance[ppt.alldat$date == ppt_date[i]])
    ppt_dat_daily$intensity[i] = sum(ppt.alldat$intensity[ppt.alldat$date == ppt_date[i]])
    ppt_dat_daily$mets[i] = sum(ppt.alldat$mets[ppt.alldat$date == ppt_date[i]])
    ppt_dat_daily$mvpa[i] = length(ppt.alldat$mets[ppt.alldat$date == ppt_date[i] & ppt.alldat$mets >= 30])
  }
  ppt_dat_daily[1:280,]
}

c1.list = vector(mode = "list", length = length(id_c1))
for (i in 1:length(id_c1)){
  c1.list[[i]] = ppt.dat(id = id_c1[i], dat = c1.dat)
}
c1.final = Reduce(function(...) rbind(...), c1.list)

c2.list = vector(mode = "list", length = length(id_c2))
for (i in 1:length(id_c2)){
  c2.list[[i]] = ppt.dat(id = id_c2[i], dat = c2.dat)
}
c2.final = Reduce(function(...) rbind(...), c2.list)

c3.list = vector(mode = "list", length = length(id_c3))
for (i in 1:length(id_c3)){
  c3.list[[i]] = ppt.dat(id = id_c3[i], dat = c3.dat)
}
c3.final = Reduce(function(...) rbind(...), c3.list)

c4.list = vector(mode = "list", length = length(id_c4))
for (i in 1:length(id_c4)){
  c4.list[[i]] = ppt.dat(id = id_c4[i], dat = c4.dat)
}
c4.final = Reduce(function(...) rbind(...), c4.list)
c4.final = c4.final[!c4.final$ID %in% c("P0461-710","P0461-718","P0461-722","P0461-766","P0461-908"),]

c5.list = vector(mode = "list", length = length(id_c5))
for (i in 1:length(id_c5)){
  c5.list[[i]] = ppt.dat(id = id_c5[i], dat = c5.dat)
}
c5.final = Reduce(function(...) rbind(...), c5.list)
c5.final = c5.final[!c5.final$ID %in% c("P0461-1265"),]


#write.csv(c1.final[complete.cases(c1.final),],"c1_final.csv")
#write.csv(c2.final[complete.cases(c2.final),],"c2_final.csv")
#write.csv(c3.final[complete.cases(c3.final),],"c3_final.csv")
#write.csv(c4.final[complete.cases(c4.final),],"c4_final.csv")
#write.csv(c5.final[complete.cases(c5.final),],"c5_final.csv")

# Read final data
setwd("~/Desktop/Fitbit Project")
fitbit.dat = list.files(pattern="*.csv")
alldat0 = ldply(as.list(fitbit.dat[2:6]), read.csv)
alldat = alldat0[,-1]
id_abnormal = unique(alldat[alldat$steps > 100000,]$ID)
alldat = alldat[!alldat$ID %in% id_abnormal,]
id = unique(alldat$ID)

rando = read.csv("randomization_raw.csv")
rando$ID = NA
for (i in 1:133){
  temp = strsplit(rando$study_id[i], "-")
  rando$ID[i] = paste0(temp[[1]][2],"-",temp[[1]][3])
}
rando$ID[134:167] = rando$study_id[134:167]

alldat$arm = rando$randomization_group[match(alldat$ID,rando$ID)]
alldat$mets = alldat$mets/10

# Daily Functional Curves
N = length(id)
M = 280
steps.dat = matrix(NA,N,M)
distance.dat = matrix(NA,N,M)
intensity.dat = matrix(NA,N,M)
mets.dat = matrix(NA,N,M)
mvpa.dat = matrix(NA,N,M)
for (i in 1:N){
  steps.dat[i,] = alldat$steps[alldat$ID == id[i]]
  distance.dat[i,] = alldat$distance[alldat$ID == id[i]]
  intensity.dat[i,] = alldat$intensity[alldat$ID == id[i]]
  mets.dat[i,] = alldat$mets[alldat$ID == id[i]]
  mvpa.dat[i,] = alldat$mvpa[alldat$ID == id[i]]
}
row.names(steps.dat) = id
row.names(distance.dat) = id
row.names(intensity.dat) = id
row.names(mets.dat) = id

s = seq(1,280,length.out = 280)
arm = rep(NA,N)
for (i in 1:N){
  arm[i] = unique(alldat$arm[alldat$ID == id[i]])
}
arm = arm + 1

steps.fdata = fdata(steps.dat,s)
plot(steps.fdata, type="l",col=grey(arm/4), ylab = "Steps",xlab = "Days",main = "Daily Steps")
lines(func.mean(steps.fdata),col="red",lwd=2) 

matplot(s, t(steps.dat), 
        type='l', lty=1, col="light grey", ylab = "Steps",xlab = "Days", main = "Daily Steps")
matlines(s, t(func.mean(fdata(steps.dat[row.names(steps.dat) %in% unique(alldat$ID[alldat$arm == 0]),],s))$data),
         type='l', lty=1, col = "red")
matlines(s, t(func.mean(fdata(steps.dat[row.names(steps.dat) %in% unique(alldat$ID[alldat$arm == 1]),],s))$data),
         type='l', lty=1, col = "blue")
matlines(s, t(func.mean(fdata(steps.dat[row.names(steps.dat) %in% unique(alldat$ID[alldat$arm == 2]),],s))$data),
         type='l', lty=1, col = "green")
matlines(s, t(func.mean(fdata(steps.dat[row.names(steps.dat) %in% unique(alldat$ID[alldat$arm == 3]),],s))$data),
         type='l', lty=1, col = "brown")


distance.fdata = fdata(distance.dat,s)
plot(distance.fdata, type="l",col=gray(1:nrow(distance.dat)/nrow(distance.dat)), ylab = "Distance",xlab = "Days",main = "")
lines(func.mean(distance.fdata),col=3,lwd=2) 

intensity.fdata = fdata(intensity.dat,s)
plot(intensity.fdata, type="l",col=gray(1:nrow(intensity.dat)/nrow(intensity.dat)), ylab = "Intensity",xlab = "Days",main = "")
lines(func.mean(intensity.fdata),col=3,lwd=2) 

mets.fdata = fdata(mets.dat,s)
plot(mets.fdata, type="l",col=gray(1:nrow(mets.dat)/nrow(mets.dat)), ylab = "Mets",xlab = "Days",main = "")
lines(func.mean(mets.fdata),col=3,lwd=2) 


# Weekly Functional Curves
alldat2 = alldat
weekly.ind0 = rep(1:40,7)
weekly.ind0 = sort(weekly.ind0)
weekly.ind = rep(weekly.ind0,N)
alldat2$weekly.ind = weekly.ind

alldat.wk = data.frame(ID = rep(id,each = 40))
for (i in 1:N){
  for (j in 1:40){
    alldat.wk$steps[alldat.wk$ID == id[i]][j] = sum(alldat2$steps[alldat2$ID == id[i] & alldat2$weekly.ind == j])
    alldat.wk$distance[alldat.wk$ID == id[i]][j] = sum(alldat2$distance[alldat2$ID == id[i] & alldat2$weekly.ind == j])
    alldat.wk$intensity[alldat.wk$ID == id[i]][j] = sum(alldat2$intensity[alldat2$ID == id[i] & alldat2$weekly.ind == j])
    alldat.wk$mets[alldat.wk$ID == id[i]][j] = sum(alldat2$mets[alldat2$ID == id[i] & alldat2$weekly.ind == j])
    alldat.wk$mvpa[alldat.wk$ID == id[i]][j] = sum(alldat2$mvpa[alldat2$ID == id[i] & alldat2$weekly.ind == j])
  }
}
alldat.wk$mets = alldat.wk$mets/10
#write.csv(alldat.wk,"alldat_week.csv")
alldat.wk = read.csv("alldat_week.csv")
alldat.wk$arm = rando$randomization_group[match(alldat.wk$ID,rando$ID)]

N = length(id)
M = 40
steps.dat.wk = matrix(NA,N,M)
distance.dat.wk = matrix(NA,N,M)
intensity.dat.wk = matrix(NA,N,M)
mets.dat.wk = matrix(NA,N,M)
mvpa.dat.wk = matrix(NA,N,M)
arm.dat.wk = matrix(NA,N,M)

for (i in 1:N){
  steps.dat.wk[i,] = alldat.wk$steps[alldat.wk$ID == id[i]]
  distance.dat.wk[i,] = alldat.wk$distance[alldat.wk$ID == id[i]]
  intensity.dat.wk[i,] = alldat.wk$intensity[alldat.wk$ID == id[i]]
  mets.dat.wk[i,] = alldat.wk$mets[alldat.wk$ID == id[i]]
  mvpa.dat.wk[i,] = alldat.wk$mvpa[alldat.wk$ID == id[i]]
  arm.dat.wk[i,] = alldat.wk$arm[alldat.wk$ID == id[i]]
}
row.names(steps.dat.wk) = id
row.names(distance.dat.wk) = id
row.names(intensity.dat.wk) = id
row.names(mets.dat.wk) = id
row.names(mvpa.dat.wk) = id
row.names(arm.dat.wk) = id

s.wk = seq(1,40,length.out = 40)

matplot(s.wk, t(mvpa.dat.wk), 
        type='l', lty=1, col="light grey", ylab = "MVPA (minutes)",xlab = "Weeks", main = "Total Weekly MVPA")

matplot(s.wk, t(func.mean(fdata(mvpa.dat.wk[row.names(mvpa.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 0]),],s))$data),
         type='l', lty=1, col = "red", ylab = "MVPA (minutes)",xlab = "Weeks", main = "Total Weekly MVPA")
matlines(s.wk, t(func.mean(fdata(mvpa.dat.wk[row.names(mvpa.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 1]),],s))$data),
         type='l', lty=1, col = "blue")
matlines(s.wk, t(func.mean(fdata(mvpa.dat.wk[row.names(mvpa.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 2]),],s))$data),
         type='l', lty=1, col = "green")
matlines(s.wk, t(func.mean(fdata(mvpa.dat.wk[row.names(mvpa.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 3]),],s))$data),
         type='l', lty=1, col = "brown")
legend("topright", 
       col = c("red","blue","green","brown"), 
       lty = 1, legend = c("Arm A","Arm B","Arm C","Arm D"),
       cex = 0.55)


matplot(s.wk, t(steps.dat.wk), 
        type='l', lty=1, col="light grey", ylab = "Steps",xlab = "Weeks", main = "Total Weekly Steps")
matlines(s.wk, t(func.mean(fdata(steps.dat.wk[row.names(steps.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 0]),],s))$data),
         type='l', lty=1, col = "red")
matlines(s.wk, t(func.mean(fdata(steps.dat.wk[row.names(steps.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 1]),],s))$data),
         type='l', lty=1, col = "blue")
matlines(s.wk, t(func.mean(fdata(steps.dat.wk[row.names(steps.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 2]),],s))$data),
         type='l', lty=1, col = "green")
matlines(s.wk, t(func.mean(fdata(steps.dat.wk[row.names(steps.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 3]),],s))$data),
         type='l', lty=1, col = "brown")
legend("topright", 
       col = c("red","blue","green","brown"), 
       lty = 1, legend = c("Arm A","Arm B","Arm C","Arm D"),
       cex = 0.55)

matplot(s.wk, t(distance.dat.wk), 
        type='l', lty=1, col="light grey", ylab = "Distance",xlab = "Weeks", main = "Total Weekly Distance")
matlines(s.wk, t(func.mean(fdata(distance.dat.wk[row.names(distance.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 0]),],s))$data),
         type='l', lty=1, col = "red")
matlines(s.wk, t(func.mean(fdata(distance.dat.wk[row.names(distance.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 1]),],s))$data),
         type='l', lty=1, col = "blue")
matlines(s.wk, t(func.mean(fdata(distance.dat.wk[row.names(distance.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 2]),],s))$data),
         type='l', lty=1, col = "green")
matlines(s.wk, t(func.mean(fdata(distance.dat.wk[row.names(distance.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 3]),],s))$data),
         type='l', lty=1, col = "brown")
legend("topright", 
       col = c("red","blue","green","brown"), 
       lty = 1, legend = c("Arm A","Arm B","Arm C","Arm D"),
       cex = 0.55)

matplot(s.wk, t(intensity.dat.wk), 
        type='l', lty=1, col="light grey", ylab = "Intensity",xlab = "Weeks", main = "Total Weekly Intensity")
matlines(s.wk, t(func.mean(fdata(intensity.dat.wk[row.names(intensity.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 0]),],s))$data),
         type='l', lty=1, col = "red")
matlines(s.wk, t(func.mean(fdata(intensity.dat.wk[row.names(intensity.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 1]),],s))$data),
         type='l', lty=1, col = "blue")
matlines(s.wk, t(func.mean(fdata(intensity.dat.wk[row.names(intensity.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 2]),],s))$data),
         type='l', lty=1, col = "green")
matlines(s.wk, t(func.mean(fdata(intensity.dat.wk[row.names(intensity.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 3]),],s))$data),
         type='l', lty=1, col = "brown")
legend("topright", 
       col = c("red","blue","green","brown"), 
       lty = 1, legend = c("Arm A","Arm B","Arm C","Arm D"),
       cex = 0.55)

matplot(s.wk, t(mets.dat.wk), 
        type='l', lty=1, col="light grey", ylab = "Mets",xlab = "Weeks", main = "Total Weekly Mets")
matlines(s.wk, t(func.mean(fdata(mets.dat.wk[row.names(mets.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 0]),],s))$data),
         type='l', lty=1, col = "red")
matlines(s.wk, t(func.mean(fdata(mets.dat.wk[row.names(mets.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 1]),],s))$data),
         type='l', lty=1, col = "blue")
matlines(s.wk, t(func.mean(fdata(mets.dat.wk[row.names(mets.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 2]),],s))$data),
         type='l', lty=1, col = "green")
matlines(s.wk, t(func.mean(fdata(mets.dat.wk[row.names(mets.dat.wk) %in% unique(alldat.wk$ID[alldat.wk$arm == 3]),],s))$data),
         type='l', lty=1, col = "brown")
legend("topright", 
       col = c("red","blue","green","brown"), 
       lty = 1, legend = c("Arm A","Arm B","Arm C","Arm D"),
       cex = 0.55)






# FPCA Analysis
# Daily
L.steps = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s,N), t(steps.dat))
FPCAdense.steps = FPCA(L.steps$Ly, L.steps$Lt)
plot(FPCAdense.steps)
FPCAdense.steps$cumFVE


L.distance = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s,N), t(distance.dat))
FPCAdense.distance = FPCA(L.distance$Ly, L.distance$Lt)
plot(FPCAdense.distance)
FPCAdense.distance$cumFVE


L.intensity = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s,N), t(intensity.dat))
FPCAdense.intensity = FPCA(L.intensity$Ly, L.intensity$Lt)
plot(FPCAdense.intensity)
FPCAdense.intensity$cumFVE


L.mets = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s,N), t(mets.dat))
FPCAdense.mets = FPCA(L.mets$Ly, L.mets$Lt)
plot(FPCAdense.mets)
FPCAdense.mets$cumFVE

# Weekly

L.steps.wk = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s.wk,N), t(steps.dat.wk))
FPCAdense.steps.wk = FPCA(L.steps.wk$Ly, L.steps.wk$Lt)
plot(FPCAdense.steps.wk)
FPCAdense.steps.wk$cumFVE

CreatePathPlot(FPCAdense.steps.wk, K=3, pch = 4, showMean = T, showObs = F)

CreateOutliersPlot(FPCAdense.steps.wk, optns = list(K = 3, variant = 'KDE'))
CreateFuncBoxPlot(FPCAdense.steps.wk, xlab = 'Weeks', ylab = 'Steps', optns = list(K =3, variant='bagplot'))


L.distance.wk = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s.wk,N), t(distance.dat.wk))
FPCAdense.distance.wk = FPCA(L.distance.wk$Ly, L.distance.wk$Lt)
plot(FPCAdense.distance.wk)
FPCAdense.distance.wk$cumFVE

L.intensity.wk = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s.wk,N), t(intensity.dat.wk))
FPCAdense.intensity.wk = FPCA(L.intensity.wk$Ly, L.intensity.wk$Lt)
plot(FPCAdense.intensity.wk)
FPCAdense.intensity.wk$cumFVE

L.mets.wk = MakeFPCAInputs(IDs = rep(1:N, each=M), tVec=rep(s.wk,N), t(mets.dat.wk))
FPCAdense.mets.wk = FPCA(L.mets.wk$Ly, L.mets.wk$Lt)
plot(FPCAdense.mets.wk)
FPCAdense.mets.wk$cumFVE


























































