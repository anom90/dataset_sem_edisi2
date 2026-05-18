#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)

#Panggil Data
data <- read.xlsx('Data_CFA_2.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data[,-11])
uji_normal

#Spesifikasi Model
model_cfa_2orde <- '
                  AK =~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5
                  MK =~ MK_1 + MK_2 + MK_3 + MK_4 + MK_5
                  KP =~ AK + MK

                  #Atur Nilai Varians Variabel Laten
                  AK ~~ e*AK
                  MK ~~ e*MK
                  '

#Estimasi Model
uji_cfa_2orde <- sem(model_cfa_2orde, data=data)
summary(uji_cfa_2orde, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#Validitas Konvergen
AVE(uji_cfa_2orde)

#Reliabilitas
#Reliabilitas Orde Pertama
model_cfa_1orde <- '
                  AK =~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5
                  MK =~ MK_1 + MK_2 + MK_3 + MK_4 + MK_5
                  '
uji_cfa_1orde <- sem(model_cfa_1orde, data=data)
compRelSEM(uji_cfa_1orde, simplify = TRUE)

#Reliabilitas Orde Kedua
compRelSEM(uji_cfa_2orde, simplify = TRUE,
           W = "KP <~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5 + 
                      MK_1 + MK_2 + MK_3 + MK_4 + MK_5"
           )

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavPredict(uji_cfa_2orde)

#Menggabungkan Skor Faktor dengan Data Asli
data_gab <- round(cbind(data[,-11], s_faktor),2)
head(data_gab)

#1. Visualisasi Model Hipotetik
semPaths(uji_cfa_2orde,
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
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "CFA Second Orde-Model-Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#2. Visualisasi Hitungan Estimasi
semPaths(uji_cfa_2orde,
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
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "CFA Second Orde-Hitungan Estimasi",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#3. Visualisasi Hitungan Standardized
semPaths(uji_cfa_2orde,
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
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "CFA Second Orde-Hitungan Standardized",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#Simpan
sink('Hasil Analisis CFA Second Orde.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil CFA Second Orde***', '\n')
summary(uji_cfa_2orde, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_cfa_2orde)
cat('\n')
cat('***Hasil Estimasi Reliabilitas Orde Pertama***', '\n')
compRelSEM(uji_cfa_1orde, simplify = TRUE)
cat('***Hasil Estimasi Relibilitas Orde Kedua***', '\n')
compRelSEM(uji_cfa_2orde, simplify = TRUE,
           W = "KP <~ AK_1 + AK_2 + AK_3 + AK_4 + AK_5 + 
                      MK_1 + MK_2 + MK_3 + MK_4 + MK_5"
           )
cat('\n')
cat('***Hasil Estimasi Skor Faktor***', '\n')
data_gab
cat('\n')
sink()
