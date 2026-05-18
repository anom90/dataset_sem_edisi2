#Jalankan Paket Analisis
library(openxlsx)
library(lavaan)
library(semPlot)
library(semTools)

#Panggil Data
data <- read.xlsx('Data_MGCFA.xlsx',1)
head(data)

#Normalitas Multivariat
uji_normal <- mardiaKurtosis(data[,-c(1,2)])
uji_normal

# Spesifikasi model pengukuran
model_mgcfa <- '
                motivasi =~ M1 + M2 + M3 + M4 + M5
                efikasi  =~ E1 + E2 + E3 + E4
              '

# Estimasi Model
# Level 1: Configural Invariance
uji_config <- cfa(model_mgcfa,
                  data    = data,
                  group   = 'Jenjang',
                  estimator = 'ML')

# Level 2: Metric (Weak) Invariance
uji_metric <- cfa(model_mgcfa,
                  data        = data,
                  group       = 'Jenjang',
                  group.equal = 'loadings',
                  estimator   = 'ML')

# Level 3: Scalar (Strong) Invariance
uji_scalar <- cfa(model_mgcfa,
                  data        = data,
                  group       = 'Jenjang',
                  group.equal = c('loadings', 
                                  'intercepts'),
                  estimator   = 'ML')

# Level 4: Strict Invariance (opsional)
uji_strict <- cfa(model_mgcfa,
                  data        = data,
                  group       = 'Jenjang',
                  group.equal = c('loadings',
                                  'intercepts', 
                                  'residuals'),
                  estimator   = 'ML')

# Evaluasi Model Baseline
summary(uji_config, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)

# Perbandingan model menggunakan semTools
measEqOut <- compareFit(uji_config, 
                        uji_metric, 
                        uji_scalar,
                        uji_strict)
summary(round(measEqOut))

#Validitas Konvergen
AVE(uji_config)

#Validitas Diskriminan
htmt(model_mgcfa, data)

#Reliabilitas
compRelSEM(uji_config, simplify = TRUE)

#Skor Faktor
#Estimasi Skor Faktor
s_faktor <- lavPredict(uji_config)

#Menggabungkan Skor Faktor dengan Data Asli
# 1. Extract skor faktor SMP dan SMA
s_faktor_smp <- data.frame(
  ID = data$ID[data$Jenjang == 'SMP'],
  s_faktor$SMP)

s_faktor_sma <- data.frame(
  ID = data$ID[data$Jenjang == 'SMA'],
  s_faktor$SMA)

# 3. Gabungkan kedua grup
s_faktor_all <- rbind(s_faktor_smp, s_faktor_sma)

# 4. Gabungkan seluruh data
data_gab <- merge(data, 
                  s_faktor_all, 
                  by = "ID", 
                  all.x = TRUE)

# 5. Urutkan berdasarkan ID
data_gab <- data_gab[order(data_gab$ID), ]
head(data_gab)

#1. Visualisasi Model Hipotetik
pdf(file = "MGCFA-Model Hipotetik.pdf", width = 11.69, height = 8.27)
par(mfrow = c(1,2))
semPaths(uji_config,
         what = "models",
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
         intercepts = FALSE)
dev.off()

#2. Visualisasi Hitungan Estimasi
pdf(file = "MGCFA-Hitungan Estimasi.pdf", width = 11.69, height = 8.27)
par(mfrow = c(1,2))
semPaths(uji_config,
         what = "models",
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
         intercepts = FALSE)
dev.off()

#3. Visualisasi Hitungan Standardized
pdf(file = "MGCFA-Hitungan Standardized.pdf", width = 11.69, height = 8.27)
par(mfrow = c(1,2))
semPaths(uji_config,
         what = "models",
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
         intercepts = FALSE)
dev.off()


#Simpan
sink('Hasil Analisis MG-CFA.txt')
cat('***Uji Asumsi Normalitas Multivariat***', '\n')
uji_normal
cat('\n')
cat('***Ringkasan Hasil MG-CFA***', '\n')
summary(uji_config, 
        fit.measures = TRUE, 
        standardized = TRUE, 
        rsquare = TRUE)
cat('\n')
cat('***Hasil Perbandingan Model***', '\n')
summary(measEqOut)
cat('\n')
cat('***Hasil Validitas Konvergen***', '\n')
AVE(uji_config)
cat('\n')
cat('***Hasil Validitas Diskriminan***', '\n')
htmt(model_mgcfa, data)
cat('\n')
cat('***Hasil Estimasi Reliabilitas***', '\n')
compRelSEM(uji_config, simplify = TRUE)
cat('\n')
cat('***Hasil Estimasi Skor Faktor***', '\n')
data_gab
cat('\n')
sink()
