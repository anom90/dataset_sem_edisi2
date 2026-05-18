#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)

#Panggil Data
data <- read.xlsx('Data_ESEM.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model
model_esem <- '
              #efa
              efa("KS")*F1 +
              efa("KS")*F2 +
              efa("KS")*F3 +
              efa("KS")*F4 =~ KS1 + KS2 + KS3 + KS4 +
                              KS5 + KS6 + KS7 + KS8 +
                              KS9 + KS10 + KS11 + KS12
                  
              #cfa
              KA =~ SIK + KOG + KET
                  
              #regresi
              KA ~ F1 + F2 + F3 + F4
              '

#Estimasi Model
uji_esem <- sem(model_esem, data=data)
summary(uji_esem, 
        fit.measure = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#1. Visualisasi Model Hipotetik
semPaths(uji_esem,
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
         edge.label.cex = 0.6,
         label.scale = FALSE,
         equalizeManifests = TRUE,
         curve = -2,
         mar = c(1,3,1,3),
         
         
         # Simpan Plot
         filetype = "pdf",
         filename = "ESEM-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#2. Visualisasi Hitungan Estimasi
semPaths(uji_esem,
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
         edge.label.cex = 0.6,
         label.scale = FALSE,
         equalizeManifests = TRUE,
         curve = -2,
         mar = c(1,3,1,3),
         
         
         # Simpan Plot
         filetype = "pdf",
         filename = "ESEM-Hitungan Estimasi",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#3. Visualisasi Hitungan Standardized
semPaths(uji_esem,
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
         edge.label.cex = 0.6,
         label.scale = FALSE,
         equalizeManifests = TRUE,
         curve = -2,
         mar = c(1,3,1,3),
         
         
         # Simpan Plot
         filetype = "pdf",
         filename = "ESEM-Hitungan Standardized",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )


#Simpan
sink('Hasil Analisis ESEM.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil ESEM***', '\n')
summary(uji_esem, 
        fit.measure = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
sink()



