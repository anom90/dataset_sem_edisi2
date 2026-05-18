#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)
library(semptools)

#Panggil Data
data <- read.xlsx('Data_SEM_MED.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model
model_sem_med <- '
                  PTO =~ PTO1 + PTO2 + PTO3 + PTO4 + PTO5
                  PTM =~ PTM1 + PTM2 + PTM3 + PTM4 + PTM5
                  MMS =~ MMS1 + MMS2 + MMS3 + MMS4 + MMS5
                  PMB =~ PTT + PMI + PMR
                  PKS =~ PKK + PKP
                  PMB ~ a*PTO + b*PTM + c*MMS
                  PKS ~ d*PMB
                  
                  #Efek Tidak Langsung (ETL)
                  etlPTO.PKS := a*d
                  etlPTM.PKS := b*d
                  etlMMS.PKS := c*d
                  '

#Estimasi Model
uji_sem_med <- sem(model_sem_med, data=data)
summary(uji_sem_med, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Reliabilitas
model_rel_sem_med <- '
                  PTO =~ PTO1 + PTO2 + PTO3 + PTO4 + PTO5
                  PTM =~ PTM1 + PTM2 + PTM3 + PTM4 + PTM5
                  MMS =~ MMS1 + MMS2 + MMS3 + MMS4 + MMS5
                  PMB =~ PTT + PMI + PMR
                  PKS =~ PKK + PKP
                  '
uji_rel_sem_med <- sem(model_rel_sem_med, data=data)
compRelSEM(uji_rel_sem_med, simplify = TRUE)

#Validitas Konvergen
AVE(uji_sem_med)

#Validitas Diskriminan
htmt(model_sem_med, data)

#1. Visualisasi Model Hipotetik
semPaths(uji_sem_med,
         whatLabels = "path",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeLat    = 10,
         sizeMan    = 8,
         sizeMan2   = 3,
         nCharNodes = 0,
         edge.label.cex = 0.8,
         label.scale = FALSE,
         equalizeManifests = TRUE,
         mar = c(1, 3, 1, 3),
         
         # Simpan Plot
         filetype = "pdf",
         filename = "SEM Mediator-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#2. Visualisasi Hitungan Estimasi
p_est1 <- semPaths(uji_sem_med,
                   whatLabels = "est",
                   style      = "lisrel",
                   layout     = "tree",
                   rotation   = 2,
                   nDigits    = 2,
                   edge.color = "black",
                   sizeLat    = 10,
                   sizeMan    = 8,
                   sizeMan2   = 3,
                   nCharNodes = 0,
                   edge.label.cex = 0.8,
                   label.scale = FALSE,
                   equalizeManifests = TRUE,
                   mar = c(1, 3, 1, 3),
                   
                   # Simpan Plot
                   filetype = "pdf",
                   filename = "SEM Mediator-Hitungan Estimasi",
                   width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

p_est2 <- mark_sig(p_est1, uji_sem_med, 
                   alphas = c('*' = 0.05, ' ' = 1))
plot(p_est2)

#3. Visualisasi Hitungan Standardized
p_std1 <- semPaths(uji_sem_med,
                   whatLabels = "std",
                   style      = "lisrel",
                   layout     = "tree",
                   rotation   = 2,
                   nDigits    = 2,
                   edge.color = "black",
                   sizeLat    = 10,
                   sizeMan    = 8,
                   sizeMan2   = 3,
                   nCharNodes = 0,
                   edge.label.cex = 0.8,
                   label.scale = FALSE,
                   equalizeManifests = TRUE,
                   mar = c(1, 3, 1, 3),
                   
                   # Simpan Plot
                   filetype = "pdf",
                   filename = "SEM Mediator-Hitungan Standardized",
                   width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_std2 <- mark_sig(p_std1, uji_sem_med, 
                   alphas = c('*' = 0.05, ' ' = 1))
plot(p_std2)


#Simpan
sink('Hasil Analisis SEM Mediator.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil SEM Mediator***', '\n')
summary(uji_sem_med, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Estimasi Reliabilitas***', '\n')
compRelSEM(uji_rel_sem_med, simplify = TRUE)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_sem_med)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
htmt(model_sem_med, data)
cat('\n')
sink()

