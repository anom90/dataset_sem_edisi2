#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)
library(semptools)

#Panggil Data
data <- read.xlsx('Data_SEM_Modif.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model
model_sem_modif1 <- '
                  KSE =~ KSE1 + KSE2 + KSE3 + KSE4
                  KIE =~ KIE1 + KIE2 + KIE3 + KIE4
                  KPE =~ KPE1 + KPE2 + KPE3 + KPE4
                  KSTE =~ KSTE1 + KSTE2 + KSTE3
                  KSTE ~ KSE + KIE + KPE
                  '

#Estimasi Model
uji_sem_modif1 <- sem(model_sem_modif1, data = data, 
                     estimator = 'MLR')
summary(uji_sem_modif1, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Indeks Modifikasi
modificationindices(uji_sem_modif1, sort. = TRUE, 
                    maximum.number = 10)

#Spesifikasi Model Modifikasi
model_sem_modif2 <- '
                  KSE =~ KSE1 + KSE2 + KSE3 + KSE4
                  KIE =~ KIE1 + KIE2 + KIE3 + KIE4
                  KPE =~ KPE1 + KPE2 + KPE3 + KPE4
                  KSTE =~ KSTE1 + KSTE2 + KSTE3
                  KSTE ~ KSE + KIE + KPE
                  
                  #Tambahkan Korelasi Error Varians
                  KSE1 ~~  KSE2
                  KSE3 ~~  KSE4
                  KIE1 ~~  KIE2
                  '

#Estimasi Model Modifikasi
uji_sem_modif2 <- sem(model_sem_modif2, data = data, 
                     estimator = 'MLR')
summary(uji_sem_modif2, 
        fit.measure = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE
        )

#Reliabilitas
model_rel_sem_modif2 <- '
                  KSE =~ KSE1 + KSE2 + KSE3 + KSE4
                  KIE =~ KIE1 + KIE2 + KIE3 + KIE4
                  KPE =~ KPE1 + KPE2 + KPE3 + KPE4
                  KSTE =~ KSTE1 + KSTE2 + KSTE3
                  
                  #Tambahkan Korelasi Error Varians
                  KSE1 ~~  KSE2
                  KSE3 ~~  KSE4
                  KIE1 ~~  KIE2
                  '
uji_rel_sem_modif2 <- sem(model_rel_sem_modif2, data = data, 
                      estimator = 'MLR')
compRelSEM(uji_rel_sem_modif2, simplify = TRUE)

#Validitas Konvergen
AVE(uji_sem_modif2)

#Validitas Diskriminan
htmt(model_sem_modif2, data)

#1. Visualisasi Model Hipotetik
semPaths(uji_sem_modif1,
         whatLabels = "path",
         style      = "lisrel",
         layout     = "tree2",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeLat    = 10,
         sizeMan    = 8,
         sizeMan2   = 4,
         nCharNodes = 0,
         edge.label.cex = 0.6,
         label.scale = FALSE,
         mar = c(1, 3.5, 1, 3.5),
         
         # Simpan Plot
         filetype = "pdf",
         filename = "SEM Modifikasi-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#2. Visualisasi Hitungan Estimasi
p_est1 <- semPaths(uji_sem_modif2,
                   whatLabels = "est",
                   style      = "lisrel",
                   layout     = "tree2",
                   rotation   = 2,
                   nDigits    = 2,
                   edge.color = "black",
                   sizeLat    = 10,
                   sizeMan    = 8,
                   sizeMan2   = 4,
                   nCharNodes = 0,
                   edge.label.cex = 0.6,
                   label.scale = FALSE,
                   mar = c(1, 3.5, 1, 3.5),
                   
                   # Simpan Plot
                   filetype = "pdf",
                   filename = "SEM Modifikasi-Hitungan Estimasi",
                   width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_est2 <- mark_sig(p_est1, uji_sem_modif2, 
                   alphas = c('*' = 0.05, ' ' = 1))
plot(p_est2)


#3. Visualisasi Hitungan Standardized
p_std1 <- semPaths(uji_sem_modif2,
                   whatLabels = "std",
                   style      = "lisrel",
                   layout     = "tree2",
                   rotation   = 2,
                   nDigits    = 2,
                   edge.color = "black",
                   sizeLat    = 10,
                   sizeMan    = 8,
                   sizeMan2   = 4,
                   nCharNodes = 0,
                   edge.label.cex = 0.6,
                   label.scale = FALSE,
                   mar = c(1, 3.5, 1, 3.5),
                   
                   # Simpan Plot
                   filetype = "pdf",
                   filename = "SEM Modifikasi-Hitungan Standardized",
                   width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#Buat Tanda Signifikansi Jalur dan Tambahkan SE
p_std2 <- mark_sig(p_std1, uji_sem_modif2, 
                   alphas = c('*' = 0.05, ' ' = 1))
plot(p_std2)


#Simpan
sink('Hasil Analisis SEM Modifikasi.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil SEM Modifikasi***', '\n')
summary(uji_sem_modif1, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Estimasi Reliabilitas***', '\n')
compRelSEM(uji_rel_sem_modif2, simplify = TRUE)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_sem_modif2)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
htmt(model_sem_modif2, data)
cat('\n')
sink()
