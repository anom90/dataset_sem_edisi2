#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)
library(semptools)

#Panggil Data
data <- read.xlsx('Data_SEM.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model
model_sem_mean <- '
                  PEND_K =~ PEND_K1 + PEND_K2 + PEND_K3 + 
                            PEND_K4 + PEND_K5
                  PAND_K =~ PAND_K1 + PAND_K2 + PAND_K3 + 
                            PAND_K4 + PAND_K5
                  PENG_K =~ PENG_K1 + PENG_K2 + PENG_K3 + 
                            PENG_K4 + PENG_K5
                  PERS_K =~ PERS_K1 + PERS_K2 + PERS_K3 + 
                            PERS_K4 + PERS_K5
                  PERS_K ~ PEND_K + PAND_K + PENG_K
                  
                  #Atur Intecept Salah Satu Indikator
                  #Variabel Laten = 0
                  PEND_K1 ~ 0*1
                  PAND_K1 ~ 0*1
                  PENG_K1 ~ 0*1
                  PERS_K1 ~ 0*1
                  
                  #Atur Intecept Salah Satu 
                  #Variabel Laten = 0
                  PERS_K ~ 0*1
                  PEND_K ~ 1
                  PAND_K ~ 1
                  PENG_K ~ 1
                  '

#Estimasi Model
uji_sem_mean <- sem(model_sem_mean, data=data, 
                    meanstructure = TRUE)
summary(uji_sem_mean, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Reliabilitas
model_rel_sem_mean <- '
                  PEND_K =~ PEND_K1 + PEND_K2 + PEND_K3 + 
                            PEND_K4 + PEND_K5
                  PAND_K =~ PAND_K1 + PAND_K2 + PAND_K3 + 
                            PAND_K4 + PAND_K5
                  PENG_K =~ PENG_K1 + PENG_K2 + PENG_K3 + 
                            PENG_K4 + PENG_K5
                  PERS_K =~ PERS_K1 + PERS_K2 + PERS_K3 + 
                            PERS_K4 + PERS_K5
                  '
uji_rel_sem_mean <- sem(model_rel_sem_mean, data = data,
                        meanstructure = TRUE)
compRelSEM(uji_rel_sem_mean, simplify = TRUE)

#Validitas Konvergen
AVE(uji_sem_mean)

#Validitas Diskriminan
htmt(model_sem_mean, data)


#1. Visualisasi Model Hipotetik
semPaths(uji_sem_mean,
         whatLabels = "path",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeLat    = 7,
         sizeMan    = 8,
         sizeMan2   = 3,
         nCharNodes = 0,
         edge.label.cex = 0.6,
         label.scale = FALSE,
         equalizeManifests = TRUE,
         mar = c(1, 1, 1, 1),
         
         # Simpan Plot
         filetype = "pdf",
         filename = "SEM Mean-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )


#2. Visualisasi Hitungan Estimasi
p_est1 <- semPaths(uji_sem_mean,
                   whatLabels = "est",
                   style      = "lisrel",
                   layout     = "tree",
                   rotation   = 2,
                   nDigits    = 2,
                   edge.color = "black",
                   sizeLat    = 7,
                   sizeMan    = 8,
                   sizeMan2   = 3,
                   nCharNodes = 0,
                   edge.label.cex = 0.6,
                   label.scale = FALSE,
                   equalizeManifests = TRUE,
                   mar = c(1, 1, 1, 1),
                   
                   # Simpan Plot
                   filetype = "pdf",
                   filename = "SEM Mean-Hitungan Estimasi",
                   width = 11.69, height = 8.27 # Ukuran A4 Landscape
                   )

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_est2 <- mark_sig(p_est1, uji_sem_mean, 
                   alphas = c('*' = 0.05, ' ' = 1))
plot(p_est2)

#3. Visualisasi Hitungan Standardized
p_std1 <- semPaths(uji_sem_mean,
                   whatLabels = "std",
                   style      = "lisrel",
                   layout     = "tree",
                   rotation   = 2,
                   nDigits    = 2,
                   edge.color = "black",
                   sizeLat    = 7,
                   sizeMan    = 8,
                   sizeMan2   = 3,
                   nCharNodes = 0,
                   edge.label.cex = 0.6,
                   label.scale = FALSE,
                   equalizeManifests = TRUE,
                   mar = c(1, 1, 1, 1),
                   
                   # Simpan Plot
                   filetype = "pdf",
                   filename = "SEM Mean-Hitungan Standardized",
                   width = 11.69, height = 8.27 # Ukuran A4 Landscape
                   )

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_std2 <- mark_sig(p_std1, uji_sem_mean, 
                   alphas = c('*' = 0.05, ' ' = 1))
plot(p_std2)

#Simpan
sink('Hasil Analisis SEM Meanstructure.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil SEM Meanstructure***', '\n')
summary(uji_sem_mean, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Estimasi Reliabilitas***', '\n')
compRelSEM(uji_rel_sem_mean, simplify = TRUE)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_sem_mean)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
htmt(model_sem_mean, data)
cat('\n')
sink()

