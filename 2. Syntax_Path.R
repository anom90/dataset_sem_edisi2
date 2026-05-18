#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)

#Panggil Data
data <- read.xlsx('Data_Path.xlsx', sheet = 1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data)
uji_normal

#Spesifikasi Model

mod_path <- '
            K_Pend ~ a*K_Dos + b*K_Mhs + d*K_Prog
            K_Prog ~ e*K_Dos + f*K_Mhs + g*K_Staf
            
            #Efek Tidak Langsung dan Total Efek
            etlAE := e*d
            totAE := a + e*d
            etlBE := f*d
            totBE := b + f*d
            etlCE := g*d
            '
#Estimasi Model
uji_path <- sem(mod_path, data=data)
summary(uji_path, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

#1. Visualisasi Model Hipotetik
semPaths(uji_path,
         whatLabels = "path",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeMan    = 6,
         sizeMan2   = 4,
         nCharNodes = 0,
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "Analisis Jalur-Model Hipotetik",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#2. Visualisasi Hitungan Estimasi
semPaths(uji_path,
         whatLabels = "est",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeMan    = 6,
         sizeMan2   = 4,
         nCharNodes = 0,
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "Analisis Jalur-Hitungan Estimasi",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#3. Visualisasi Hitungan Standardized
semPaths(uji_path,
         whatLabels = "std",
         style      = "lisrel",
         layout     = "tree",
         rotation   = 2,
         nDigits    = 2,
         edge.color = "black",
         sizeMan    = 6,
         sizeMan2   = 4,
         nCharNodes = 0,
         edge.label.cex = 0.8,
         label.scale = FALSE,
         
         # Simpan Plot
         filetype = "pdf",
         filename = "Analisis Jalur-Hitungan Standardized",
         width = 11.69, height = 8.27 # Ukuran A4 Landscape
         )

#Simpan
sink('Hasil Analisis Jalur.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil Analisis Jalur***', '\n')
summary(uji_path, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
sink()

