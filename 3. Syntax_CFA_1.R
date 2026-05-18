#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)

#Panggil Data
data <- read.xlsx('Data_CFA_1.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model
model_cfa_uni <- '
                  MB =~ MB_1 + MB_2 + 
                        MB_3 + MB_4 + 
                        MB_5
                  '

#Estimasi Model
uji_cfa_uni <- sem(model_cfa_uni, data=data)
summary(uji_cfa_uni, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Validitas Konvergen
AVE(uji_cfa_uni)

#Reliabilitas
compRelSEM(uji_cfa_uni)

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavPredict(uji_cfa_uni)

#Menggabungkan Skor Faktor dengan Data Asli
data_gab <- round(cbind(data, s_faktor),2)
head(data_gab)

#1. Visualisasi Model Hipotetik
semPaths(uji_cfa_uni,
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
         filename = "CFA Unidimensi-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#2. Visualisasi Hitungan Estimasi
semPaths(uji_cfa_uni,
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
         filename = "CFA Unidimensi-Hitungan Estimasi",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#3. Visualisasi Hitungan Standardized
semPaths(uji_cfa_uni,
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
         filename = "CFA Unidimensi-Hitungan Standardized",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#Simpan
sink('Hasil Analisis CFA Unidimensi.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil CFA Unidimensi***', '\n')
summary(uji_cfa_uni, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_cfa_uni)
cat('\n')
cat('***Hasil Estimasi Reliabilitas Komposit***', '\n')
compRelSEM(uji_cfa_uni)
cat('\n')
cat('***Hasil Estimasi Skor Faktor***', '\n')
data_gab
sink()



