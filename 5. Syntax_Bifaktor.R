#Jalankan Paket Analisis

library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)
library(BifactorIndicesCalculator)

#Panggil Data
data <- read.xlsx("Data_Bifactor.xlsx", sheet = 1)
head(data)


#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model
model_bifactor <- '
                  # Faktor Umum: memuat semua item
                  G    =~ L1 + L2 + L3 + L4 + L5 +
                          R1 + R2 + R3 + R4 + R5 +
                          G1 + G2 + G3 + G4 + G5 +
                          V1 + V2 + V3 + V4 + V5
                
                  # Faktor Spesifik
                  LISTENING =~ L1 + L2 + L3 + L4 + L5
                  READING =~ R1 + R2 + R3 + R4 + R5
                  GRAMMAR =~ G1 + G2 + G3 + G4 + G5
                  VOCAB =~ V1 + V2 + V3 + V4 + V5
                  '

#Estimasi Model
uji_bifactor <- cfa(model_bifactor,
                    data = data,
                    orthogonal = TRUE,
                    ordered = TRUE)
summary(uji_bifactor, 
        fit.measures = TRUE,
        standardized = TRUE, 
        rsquare = TRUE)



#Evaluasi Model
eval_bifactor <- bifactorIndices(uji_bifactor)
eval_bifactor

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavPredict(uji_bifactor)
head(s_faktor)


#1. Visualisasi Model Hipotetik
semPaths(uji_bifactor, 
         what = "model",
         whatLabels = "path",
         bifactor = "G", 
         layout = "tree2",
         nDigits    = 2,
         edge.color = "black",
         sizeLat = 9,
         sizeMan    = 4,
         intercepts = FALSE, 
         residuals = FALSE, 
         exoCov = FALSE,
         thresholdSize = 0,
         nCharNodes = 0,
         label.scale = FALSE,
         mar = c(5, 1, 5, 1),
         filetype = "pdf",
         filename = "Bifactor-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
)

#2. Visualisasi Hitungan Estimasi
semPaths(uji_bifactor, 
         what = "model",
         whatLabels = "est", 
         bifactor = "G", 
         layout = "tree2",
         nDigits    = 2,
         edge.color = "black",
         sizeLat = 9,
         sizeMan    = 4,
         intercepts = FALSE, 
         residuals = FALSE, 
         exoCov = FALSE,
         thresholdSize = 0,
         nCharNodes = 0,
         label.scale = FALSE,
         mar = c(5, 1, 5, 1),
         filetype = "pdf",
         filename = "Bifactor-Hitungan Estimasi",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#3. Visualisasi Hitungan Standardized
semPaths(uji_bifactor, 
         what = "model",
         whatLabels = "std", 
         bifactor = "G", 
         layout = "tree2",
         nDigits    = 2,
         edge.color = "black",
         sizeLat = 9,
         sizeMan    = 4,
         intercepts = FALSE, 
         residuals = FALSE, 
         exoCov = FALSE,
         thresholdSize = 0,
         nCharNodes = 0,
         label.scale = FALSE,
         mar = c(5, 1, 5, 1),
         filetype = "pdf",
         filename = "Bifactor-Model Standardized",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#Simpan
sink('Hasil Analisis Bifactor.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil Estimasi Model Bifactor***', '\n')
summary(uji_bifactor, 
        fit.measures = TRUE,
        standardized = TRUE, 
        rsquare = TRUE)

cat('\n')
cat('***Hasil Evaluasi Model Bifactor***', '\n')
eval_bifactor
cat('\n')
cat('***Hasil Estimasi Skor Faktor***', '\n')
s_faktor
cat('\n')
sink()

