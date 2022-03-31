GAS = read.csv("https://raw.githubusercontent.com/qwerty29544/RpracticeBook/master/2Data/01FlatTables/GAZ.csv", 
                header = T, 
                sep = "\t", 
                encoding = "UTF-8", 
                dec = ",")
GAS=na.omit(GAZ)
GAS$Temp_Kelvin = GAZ[,3] + 273
GAS=GAS[,c(1,2,10,4,5,6,7,8,9)]
GAS[,7]=as.factor(GAS[,7])
GAS[,8]=as.factor(GAS[,8])
GAS[,9]=as.factor(GAS[,9])
GAS$ze=GAS$газ.м3.сут/GAS$конд.т.м3.сут
GAS$xi=GAS$газ.м3.сут/GAS$вода.м3.сут
GAS$ce=GAS$вода.м3.сут/GAS$конд.т.м3.сут
GAS_date=as.Date(GAS$дата.замера, format = '%d/%m/%Y')
GAS$дата.замера=GAS_date
format(GAS$дата.замера, '%Y')
GAS_2018=GAS[format(GAS$дата.замера, '%Y')=='2018',]
GAS[format(GAS$дата.замера, '%Y')=='2018' & GAS$ID==111,]
unique(GAS$ID)[!unique(GAS$ID)%in%unique(GAS[GAS$вода.м3.сут > 2,]$ID)]
unique(GAS$ID)[!unique(GAS$ID)%in%unique(GAS[GAS$вода.м3.сут + GAS$конд.т.м3.сут + GAS$газ.м3.сут < 1000,]$ID)]
splt=split(GAS_2018, GAS_2018$Группа)
summs=c()
for (i in 1:length(splt))
  summs[i]=sum(splt[[i]]$газ.м3.сут)
names(splt)[which.max(summs)]

splt_2=split(GAS_2018, GAS_2018$Куст)
summs2=c()
for (j in 1:length(splt_2))
  summs[j]=sum(splt_2[[j]]$газ.м3.сут)  
names(splt_2)[which.max(summs2)]

splt_3=split(GAS_2018, GAS_2018$Куст)
summs3=c()
for (k in 1:length(splt_3)){
  t = splt_3[[k]]$gkv
  summs3[k]=mean(as.numeric(t[!is.infinite(t) & !is.nan(t)]))
}
names(splt_3)[which.max(summs3)]
