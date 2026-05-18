#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)

#Panggil Data
data <- read.xlsx('Data_CFA_4.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model
model_cfa_modif1 <- '
                    K =~ K_1 + K_2 + K_3 + K_4 + K_5
                  '

#Estimasi Model
uji_cfa_modif1 <- sem(model_cfa_modif1, data = data)
summary(uji_cfa_modif, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Modifikasi Model
modificationindices(uji_cfa_modif1, sort. = T)

#Spesifikasi Model Modifikasi
model_cfa_modif2 <- '
                    K =~ K_1 + K_2 + K_3 + K_4 + K_5
                    
                    #Tambahkan Error Varians
                    K_1 ~~ K_2
                  '
#Estimasi Model Modifikasi
uji_cfa_modif2 <- sem(model_cfa_modif2, data=data)
summary(uji_cfa_modif2, 
        fit.measure = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Reliabilitas
compRelSEM(uji_cfa_modif2)

#Validitas Konvergen
AVE(uji_cfa_modif2)

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavPredict(uji_cfa_modif2)

#Menggabungkan Skor Faktor dengan Data Asli
data_gab <- round(cbind(data, s_faktor),2)
head(data_gab)

#1. Visualisasi Model Hipotetik
semPaths(uji_cfa_modif1,
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
         filename = "CFA Modifikasi-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#2. Visualisasi Hitungan Estimasi
semPaths(uji_cfa_modif2,
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
         filename = "CFA Modifikasi-Hitungan Estimasi",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#3. Visualisasi Hitungan Standardized
semPaths(uji_cfa_modif2,
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
         filename = "CFA Modifikasi-Hitungan Standardized",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#Simpan
sink('Hasil Analisis CFA Modifikasi.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil CFA Modifikasi***', '\n')
summary(uji_cfa_modif, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Estimasi Reliabilitas***', '\n')
compRelSEM(uji_cfa_modif2)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_cfa_modif2)
cat('\n')
cat('***Hasil Estimasi Skor Faktor***', '\n')
data_gab
cat('\n')
sink()

