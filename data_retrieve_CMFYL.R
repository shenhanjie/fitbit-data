library(httr)
library(base64enc)
library(httpuv)
library(tidyverse)
library(Hmisc)


#setwd("C:/Users/shenh/Desktop/Greenlee Study/CMFYL Study")
setwd("~/Desktop/CMFYL Study")

# Read Fitbit Account data
fitbit_account = read.csv("CMFYL_Fitbit_Account.csv")

# Read randomiztion data
rando = read.csv("Randomization_data.csv")
rando$study_id = as.character(rando$study_id)
rando$randomization_date = as.Date(as.character(rando$randomization_date))

# Load current tokens and dataset
load(gzfile("oauth2.0_token_list_c1_backup.gz"))


closeAllConnections()

# define a function to find end of month

eom <- function(date) {
  # date character string containing POSIXct date
  date.lt <- as.POSIXlt(date) # add a month, then subtract a day:
  mon <- date.lt$mon + 2 
  year <- date.lt$year
  year <- year + as.integer(mon==13) # if month was December add a year
  mon[mon==13] <- 1
  iso = ISOdate(1900+year, mon, 1, hour=0)
  result = as.POSIXct(iso) - 86400 # subtract one day
  result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst)*3600
}



# define a Fitbit data download function
FitbitDownloadr = function(d, tk, id) {
  
  all.min.1d.list = list()
  #for (i in 1:length(tk){
  for (i in c(1:3, 5:10)){
    # device
    resp.device = GET(url = "https://api.fitbit.com/1/user/-/devices.json", config(token = tk[[i]]))
    
    # avtivities
    resp.step = GET(url = paste0("https://api.fitbit.com/1/user/-/activities/steps/date/",d,"/1d.json"), config(token = tk[[i]]))
    resp.distance = GET(url = paste0("https://api.fitbit.com/1/user/-/activities/distance/date/",d,"/1d.json"), config(token = tk[[i]]))
    resp.intensity = GET(url = paste0("https://api.fitbit.com/1/user/-/activities/log/calories/date/",d,"/1d.json"), config(token = tk[[i]]))
    
    #resp.weight= GET(url = paste0("https://api.fitbit.com/1/user/-/body/log/weight/date/",d,"/1d.json"), config(token = tk[[i]]))
    
    
    # select variables from returned data
    device.info = data.frame(do.call(cbind,lapply(content(resp.device)[[1]],unlist)))
    step.min.1d = data.frame(do.call(rbind,lapply(content(resp.step)[[2]]$dataset,unlist)))
    distance.min.1d = data.frame(do.call(rbind,lapply(content(resp.distance)[[2]]$dataset,unlist)))
    intensity.min.1d = data.frame(do.call(rbind,lapply(content(resp.intensity)[[2]]$dataset,unlist)))
    
    # assemble data
    all.min.1d.list[[i]] = data.frame(study_id = id[i], date = as.character(d), time = step.min.1d[,1], steps = step.min.1d[,2],
                                 distance_miles = distance.min.1d[,2], intensity.min.1d[,1:2], calories = intensity.min.1d[,4],
                                 last_synced = as.Date(strptime(substr(content(resp.device)[[1]]['lastSyncTime'],1,10),"%Y-%m-%d")))
  }
  
      
  #all.min.1d$date = d
  #all.min.1d$ID = token.deviceid$study_id[token.deviceid$device_id==t$credentials$user_id]
  #all.min.1d$study_id = id
  all.min.1d = Reduce(function(...) rbind(...), all.min.1d.list)
  all.min.1d
}

FitbitDownloadr_Aria = function(d, tk, id) {
  
  all.weight.1d.list = list()
  #for (i in 1:length(tk){
  for (i in c(1, 7, 10)){
    # device
    resp.device = GET(url = "https://api.fitbit.com/1/user/-/devices.json", config(token = tk[[i]]))
    
    resp.weight = GET(url = paste0("https://api.fitbit.com/1/user/-/body/log/weight/date/",d,".json"), config(token = tk[[i]]))
    
    
    
    # select variables from returned data
    device.info = data.frame(do.call(cbind,lapply(content(resp.device)[[2]],unlist)))
    weight = data.frame(do.call(rbind,lapply(content(resp.weight)$weight,unlist)))
    #step.min.1d = data.frame(do.call(rbind,lapply(content(resp.step)[[2]]$dataset,unlist)))
    
    # assemble data
    all.weight.1d.list[[i]] = data.frame(study_id = id[i], date = as.character(d), weight = weight$weight,
                                      last_synced = as.Date(strptime(substr(content(resp.device)[[2]]['lastSyncTime'],1,10),"%Y-%m-%d")))
  }
  
  
  all.weight.1d = Reduce(function(...) rbind(...), all.weight.1d.list)
  all.weight.1d
}


# Fitbit Report
FitbitReport = function(d, tk, id, cohort) {
  
  all.device.1d.list = list()
  for (i in c(1:3, 5:10)){
    # device
    resp.device = GET(url = "https://api.fitbit.com/1/user/-/devices.json", config(token = tk[[i]]))
    
    device.info = data.frame(do.call(cbind,lapply(content(resp.device)[[1]],unlist)))
    
    all.device.1d.list[[i]] = data.frame(Study_ID = id[i], Cohort = cohort, Device = as.character(device.info$deviceVersion),
                                         Battery = as.character(device.info$battery),
                                         Last_synced = as.Date(strptime(substr(content(resp.device)[[1]]['lastSyncTime'],1,10),"%Y-%m-%d")),
                                         Report_date = d)
    
    all.device.1d.list[[i]]$Status = ifelse(all.device.1d.list[[i]]$Report_date - all.device.1d.list[[i]]$Last_synced > 7, "sync overdue","")
    
    
  }
  
  all.device.1d = Reduce(function(...) rbind(...), all.device.1d.list)
  all.device.1d
}

# Aria Report
FitbitReport_Aria = function(d, tk, id, cohort) {
  
  all.device.1d.list = list()
  for (i in c(1:2, 7, 10)){
    # device
    resp.device = GET(url = "https://api.fitbit.com/1/user/-/devices.json", config(token = tk[[i]]))
    
    device.info = data.frame(do.call(cbind,lapply(content(resp.device)[[2]],unlist)))
    
    all.device.1d.list[[i]] = data.frame(Study_ID = id[i], Cohort = cohort, Device = as.character(device.info$deviceVersion),
                                         Battery = as.character(device.info$battery),
                                         Last_synced = as.Date(strptime(substr(content(resp.device)[[2]]['lastSyncTime'],1,10),"%Y-%m-%d")),
                                         Report_date = d)
    
    all.device.1d.list[[i]]$Status = ifelse(all.device.1d.list[[i]]$Report_date - all.device.1d.list[[i]]$Last_synced > 7, "sync overdue","")
    
    
  }
  
  all.device.1d = Reduce(function(...) rbind(...), all.device.1d.list)
  all.device.1d
}


################################## Collect Daily Cohort 1 Data
c1_studyid = as.character(fitbit_account$study_id[grep("C1", fitbit_account$study_id)])

day0 = max(rando$randomization_date)
day1 = seq(day0,Sys.Date(),by = 1)

file.locate = paste0("/Volumes/researcher/greenlee_h/Data Raw/CMFYL Fitbit data/Cohort 1")
setwd(file.locate)

for (i in 1:length(day1)){
  all.min = FitbitDownloadr(day1[i], oauth2.0_token.list_c1, id = c1_studyid)
  write.csv(all.min, paste0("G10335-C1_", format(day1[i],'%Y-%m-%d'),".csv"), row.names = F)
}

for (i in 240:length(day1)){
  all.min = FitbitDownloadr(day1[i], oauth2.0_token.list_c1, id = c1_studyid)
  write.csv(all.min, paste0("G10335-C1_", format(day1[i],'%Y-%m-%d'),".csv"), row.names = F)
}

day1 = Sys.Date()
all.min = FitbitDownloadr(day1, oauth2.0_token.list_c1, id = c1_studyid)
write.csv(all.min, paste0("G10335-C1_", format(day1,'%Y-%m-%d'),".csv"), row.names = F)

day1 = Sys.Date()
all.weight = FitbitDownloadr_Aria(day1, oauth2.0_token.list_c1, id = c1_studyid)
write.csv(all.weight, paste0("G10335-C1_Weight_", format(day1,'%Y-%m-%d'),".csv"), row.names = F)


# Generate Daily Report
file.locate = paste0("/Volumes/researcher/greenlee_h/Data Raw/CMFYL Fitbit data/Cohort 1 Report")
setwd(file.locate)


day1 = Sys.Date()
all.device = FitbitReport(day1, oauth2.0_token.list_c1, id = c1_studyid, cohort = "Cohort 1")
write.csv(all.device, paste0("Fitbit sync ",day1, ".csv"), row.names = F)


day1 = Sys.Date()
all.device.aria = FitbitReport_Aria(day1, oauth2.0_token.list_c1, id = c1_studyid, cohort = "Cohort 1")
write.csv(all.device.aria, paste0("Fitbit Aria sync ",day1, ".csv"), row.names = F)


################################## Collect Cohort 2 Data
c2_studyid = as.character(fitbit_account$study_id[grep("C2", fitbit_account$study_id)])

# create the first daily data
day1 = Sys.Date()




############################################################################3
# Collect Cohort 1 Data
n_c1 = length(c1_studyid)
for (i in 1:n_c1){
  file.locate = paste0("/Volumes/researcher/greenlee_h/Data Raw/CMFYL Fitbit data/",c1_studyid[i])
  setwd(file.locate)
  day0 = rando$randomization_date[rando$study_id == c1_studyid[i]]
  day1 = seq(day0,Sys.Date(),by = 1)
  all.min = FitbitDownloadr(day1, oauth2.0_token.list_c1[[i]], id = c1_studyid[i])
  write.csv(all.min, paste0(c1_studyid[i],"_", format(day1,'%Y-%m-%d'),".csv"))
}

cov.name = c("study_id","date","time","steps","distance_miles", "level","mets","calories",      
             "device_version","device_id","battery","lastSyncTime")

c1_studyid = fitbit_account$study_id[grep("C1", fitbit_account$study_id)]
#day1 = seq(as.Date('2020-09-01'),as.Date('2020-09-03'),by = 1)
for (i in 1:n_c1){
  file.locate = paste0("/Volumes/researcher/greenlee_h/Data Raw/CMFYL Fitbit data/",c1_studyid[i])
  setwd(file.locate)
  day1 = Sys.Date()
  all.min0= read.csv(paste0(c1_studyid[i],"_", format(day1-1,'%Y-%m-%d'),".csv"))
  all.min1 = FitbitDownloadr(day1, oauth2.0_token.list_c1[[i]], id = c1_studyid[i])
  all.min0 = all.min0[,names(all.min1)]
  all.min = rbind(all.min0, all.min1)
  write.csv(all.min, paste0(c1_studyid[i],"_", format(day1,'%Y-%m-%d'),".csv"))
}


cmfyl_data_list_c1 = list()
for (i in 1:n_c1){
  file.locate = paste0("/Volumes/researcher/greenlee_h/Data Raw/CMFYL Fitbit data/",c1_studyid[i])
  setwd(file.locate)
  day1 = Sys.Date()
  cmfyl_data_list_c1[[i]] = read.csv(paste0(c1_studyid[i],"_", format(day1,'%Y-%m-%d'),".csv"))
}

cmfyl_data_c1 = Reduce(function(...) rbind(...), cmfyl_data_list_c1)

setwd("/Volumes/researcher/greenlee_h/Data Raw/CMFYL Fitbit data")
day1 = Sys.Date()
write.csv(cmfyl_data_c1[,cov.name], paste0("CMFYL_C1_", format(day1,'%Y-%m-%d'),".csv"))



# day1 <- as.Date(max(all.min$date))+1
# 
# if (day1<Sys.Date()) {
#   
#   all.min <-  FitbitDownloadr(day1, oauth2.0_token.list)
# 
#   names(all.min) <- c('time','steps','distance','intensity','mets','date','ID')
#   
#   all.min[,c('steps','distance','intensity','mets')] <- 
#     lapply(all.min[,c('steps','distance','intensity','mets')], function(x) 
#       as.numeric(as.character(x))) 
#   
#   all.min$date.time <- strptime(paste(all.min$date,
#                                          as.character(all.min$time)),
#                                    "%Y-%m-%d %H:%M:%S")
#   
#   all.min <- all.min[,c('ID','date','time','date.time','steps','distance','intensity','mets')]
#   
#   
#   if (format(day1,'%d')=='01'){
#     save(list="all.min",
#          file=paste0("all.min",
#                      format(day1,'%Y-%m'),".gz"))
#     
#     write_csv(all.min,
#               paste0("all.min",
#                      format(day1,'%Y-%m'),".csv")) 
#   } else {
#   
#     write_csv(all.min,
#               paste0("all.min",
#                      format(day1,'%Y-%m'),".csv"),append=TRUE)
#   }
#   
#   
# }

  
  
  
  
  
  