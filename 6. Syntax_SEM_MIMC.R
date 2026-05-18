#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)

#Panggil Data
data <- read.xlsx('Data_SEM_MIMC.xlsx', sheet = 1)
head(data)

#Buat Variabel Dummy
# Ubah kolom 5: nilai 2 menjadi 1, selain itu menjadi 0
data[,5] <- ifelse(data[,5] == 2, 1, 0)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data[,-5])
uji_normal

#Spesifikasi Model
model_sem_mimc <- '
                  MI =~ MI1 + MI2 + MI3 + MI4
                  MI ~ JK
                  '

#Estimasi Model
uji_sem_mimc <- sem(model_sem_mimc, data = data, 
                   estimator = 'WLSMV')
summary(uji_sem_mimc, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Reliabilitas
model_rel_mimc <- '
                  MI =~ MI1 + MI2 + MI3 + MI4
                  '
uji_rel_mimc <- sem(model_rel_mimc, data = data, 
                    estimator = 'WLSMV')
compRelSEM(uji_rel_mimc, simplify = TRUE)

#Validitas Konvergen
AVE(uji_sem_mimc)

#1. Visualisasi Model Hipotetik
semPaths(uji_sem_mimc,
         whatLabels = "path",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeLat    = 10,
         sizeMan    = 8,
         sizeMan2   = 5,
         nCharNodes = 0,
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "MIMC-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#2. Visualisasi Hitungan Estimasi
semPaths(uji_sem_mimc,
         whatLabels = "est",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeLat    = 10,
         sizeMan    = 8,
         sizeMan2   = 5,
         nCharNodes = 0,
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "MIMC-Hitungan Estimasi",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#3. Visualisasi Hitungan Standardized
semPaths(uji_sem_mimc,
         whatLabels = "std",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeLat    = 10,
         sizeMan    = 8,
         sizeMan2   = 5,
         nCharNodes = 0,
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "MIMC-Hitungan Standardized",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#Simpan
sink('Hasil Analisis MIMC.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil MIMC***', '\n')
summary(uji_sem_mimc, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Estimasi Reliabilitas***', '\n')
compRelSEM(uji_rel_mimc, simplify = TRUE)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_sem_mimc)
cat('\n')
sink()
