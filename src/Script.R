setwd("C:/Users/willy/OneDrive/Documents/test")
datauji <- read.csv("Data Vaksinasi Dosis Pertama.csv", TRUE, ";")
data = datauji$Jumlah
itv = round(1 +(3.3 *logb(length(data), base = 10)))
      
# Mencari data maksimum dan minimum
minimal = min(data)/100
maksimal = max(data)/100
      
# Mencari data minimum & maksimum baru untuk dijadikan sebagai batas atas dan batas bawah interval semesta U 
min.new = floor(minimal)*100
max.new = ceiling(maksimal)*100
      
# Panjang Interval
dataitv = data[c(1:length(data))]
dif = c()
for (i in 2:length(data)) {
  dif[i-1] = abs(dataitv[i] - dataitv[i-1])
}
ratadif = mean(dif,2)
Sratadif = ratadif/2
      
if (Sratadif > 100) {
  L = round(Sratadif/100) * 100
} else if (Sratadif > 10) {
  L = round(Sratadif/10) * 10
} else if (Sratadif > 1) {
  L = round(Sratadif)
} else {
  L = round(Sratadif*10)/10
}
n = floor((max.new-min.new)/L)
n = itv
      
# Batas-batas interval
intrv.1 = seq(min.new,max.new,len = n+1)
      
# Pembagian interval dan membentuk himpunan fuzzy
box1 = data.frame(NA,nrow=length(intrv.1)-1,ncol=3)
names(box1) = c("bawah","atas","kel")
      
for (i in 1:length(intrv.1)-1) {
  box1[i,1]=intrv.1[i]
  box1[i,2]=intrv.1[i+1]
  box1[i,3]=i
}
      
# Nilai tengah interval
n.tengah = data.frame(tengah=(box1[,1]+box1[,2])/2,kel=box1[,3])
      
# Fuzzyfikasi ke data aktual
fuzifikasi=c() 
for (i in 1:length(data)){
  for (j in 1:nrow(box1)){
    if (i!=which.max(data)){
      if (data[i]>=(box1[j,1])&data[i]<(box1[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
    else {
      if (data[i]>=(box1[j,1])&data[i]<=(box1[j,2])){
        fuzifikasi[i]=j
        break
      }
    }
  }
}
      
# Fuzzyfikasi ke data asal
fuzzyfy = cbind(data,fuzifikasi)
  
# FLR
FLR = data.frame(fuzzifikasi=0,left=NA,right =NA)
for (i in 1:length(fuzifikasi)) {
  FLR[i,1] = fuzifikasi[i]
  FLR[i+1,2] = fuzifikasi[i]
  FLR[i,3] = fuzifikasi[i]
}
FLR = FLR[-nrow(FLR),]
FLR = FLR[-1,]
      
# Membuat FLRG
FLRG = table(FLR[,2:3])
      
# Matriks transisi
# Pembulatan peluang matriks transisi 3 angka belakang koma
bobot = round(prop.table(table(FLR[,2:3]),1),5)
      
# PERAMALAN MARKOV CHAIN
# Mengambil nilai diagonal
diagonal = diag(bobot)
m.diagonal = diag(diagonal)
    
# Mengambil nilai selain diagonal
pinggir = bobot-m.diagonal
    
ramal=NULL
for (i in 1:(length(fuzifikasi))){
  for (j in 1:(nrow(bobot))){
    if (fuzifikasi[i]==j)
    {ramal[i+1]=(diagonal[j]*data[i])+sum(pinggir[j,]*n.tengah[,1]) }else
      if (fuzifikasi[i]==j)
      {ramal[i]=0}
  }
}
ramal = ramal[-length(ramal)]
      
# Adjusted forecasting value (peramalan tahap kedua)
      
adjusted = rep(0,nrow(FLR)) 
selisih = FLR[,3]-FLR[,2] 
for(i in 1:nrow(FLR))
{
  if (FLR[i,2]!=FLR[i,3] && diagonal[FLR[i,2]]==0)
  {adjusted[i]=selisih[i]*(L/2)} else   #Untuk tidak komunicate
    if (selisih[i]==1 && diagonal[FLR[i,2]]>0)
    {adjusted[i]=(L)} else #Untuk  komunicate
      if (FLR[i,2]!=FLR[i,3] && diagonal[FLR[i,2]]>0)
      {adjusted[i]=selisih[i]*L/2} #Untuk komunicate
}
      
# Adjusted forecasting value (peramalan tahap akhir)
ramal=ramal[c(2:length(ramal))]
adj.forecast=adjusted + ramal
    
# Tabel pembanding
datapakai = data[c(2:length(data))]
galat = abs(datapakai-adj.forecast)
tabel = cbind(datapakai,ramal,adjusted,adj.forecast,galat)
      
# Penyesuaian overestimate ramalan
adj.forecast2 = rep(0,length(datapakai)) 
for(i in 1:length(datapakai)){
  if((ramal[i]-datapakai[i])<(adj.forecast[i]-datapakai[i]))
  {adj.forecast2[i]=ramal[i]+(L/2)}
  else
  {adj.forecast2[i]=adj.forecast[i]}
}

Awal = data[c(2:length(data))]
Prediksi = adj.forecast2
tabel = cbind(Awal,Prediksi)
tabel
  
error = abs(Awal-Prediksi)
MAPE = mean(abs(error/Awal*100), na.rm=TRUE)
hasil = cbind(MAPE)
hasil