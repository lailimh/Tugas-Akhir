library(lmtest)
# Membaca data dari file CSV
IPM2 <- read.csv("D:/matkul/SEMESTER 7/TUGAS AKHIR/IPM2.csv", sep = ";")
IPM2
summary(IPM2)
# Plot boxplot untuk masing-masing variabel
par(mfrow = c(2, 3))  # Atur tata letak plot dalam 2 baris dan 3 kolom

boxplot(IPM2$IPM, main = "Boxplot IPM", ylab = "IPM", col = "lightblue")
boxplot(IPM2$Harapanlamasekolah, main = "Boxplot Harapanlamasekolah", ylab = "Harapanlamasekolah", col = "lightgreen")
boxplot(IPM2$Ratalamasekolah, main = "Boxplot Ratalamasekolah", ylab = "Ratalamasekolah", col = "lightpink")
boxplot(IPM2$Pertumbuhanekonomi, main = "Boxplot Pertumbuhanekonomi", ylab = "Pertumbuhanekonomi", col = "lightyellow")
boxplot(IPM2$Indekskesehatan, main = "Boxplot Indekskesehatan", ylab = "Indekskesehatan", col = "lightgray")
boxplot(IPM2$Pengangguran, main = "Boxplot Pengangguran", ylab = "Pengangguran", col = "lightgray")

# Membuat model regresi linear
model <- lm(IPM ~ Harapanlamasekolah + Ratalamasekolah + Pertumbuhanekonomi + Indekskesehatan + Pengangguran, data = IPM2)

# Menampilkan ringkasan model
summary(model)

# Membuat Q-Q plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Uji normalitas residual menggunakan Kolmogorof Smirnov
ks.test(residuals(model), "pnorm", mean = mean(residuals(model)), sd = sd(residuals(model)))

#Homokedastisitas
# Membuat plot residual terhadap nilai yang diprediksi
# 3. Hitung Residual
residuals <- residuals(model)

# 4. Hitung Absolut Residual
abs_residuals <- abs(residuals)

# 5. Lakukan Regresi Ulang untuk Glejser Test
glejser_model <- lm(abs_residuals ~ Harapanlamasekolah + Ratalamasekolah + Pertumbuhanekonomi + Indekskesehatan + Pengangguran, data = IPM2)

# 6. Lihat Hasil
summary(glejser_model)
plot(fitted(model), residuals(model), main = "Residual vs Fitted", 
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, col = "red")

library(lmtest)
bptest(model)

# Plot residual terhadap setiap variabel independen
plot(IPM2$Harapanlamasekolah, residuals(model), main = "Residual vs Harapanlamasekolah", 
     xlab = "Harapanlamasekolah", ylab = "Residuals")
plot(IPM2$Ratalamasekolah, residuals(model), main = "Residual vs Ratalamasekolah", 
     xlab = "Ratalamasekolah", ylab = "Residuals")

#Autoresidual
library(lmtest)
dwtest(model)

#Multikolinearitas
library(car)
vif(model)

#hitung DFBETAS
dfbetas_values <- dfbetas(model)
dfbetas_values

#Hitung DFFITS
dffits_values <- dffits(model)
dffits_values


# Hitung Cook's Distance
cooks_values <- cooks.distance(model)
cooks_values

#leverage 
leverage_values <- hatvalues(model)
leverage_values

#R-Student
rstudent_values <- rstudent(model)
rstudent_values

results <- data.frame(
  DFBETAS = apply(dfbetas_values, 1, max),  # Nilai DFBETAS maksimum untuk setiap observasi
  DFFITS = dffits_values,
  Cooks_Distance = cooks_values,
  Leverage = leverage_values,
  R_Student = rstudent_values
)
print(results)

# Identifikasi observasi yang berpotensi outlier
outliers <- results[abs(results$DFFITS) > (2 * sqrt((ncol(model$model) + 1) / nrow(model$model))) | 
                      results$Cooks_Distance > 1 |
                      abs(results$R_Student) > 2, ]
print(outliers)

library(MASS)
# Model dengan pembobot Andrew
rr.andrew <- rlm(IPM ~ Harapanlamasekolah + Ratalamasekolah + Pertumbuhanekonomi + Indekskesehatan + Pengangguran, data = IPM2, method = "MM")

# Menampilkan ringkasan model
summary(rr.andrew)

# Menghitung residuals dan total variasi
residuals <- rr.andrew$residuals
fitted_values <- rr.andrew$fitted.values
observed_values <- IPM2$IPM  # Ganti dengan nama variabel dependen Anda

# Menghitung RSS (Residual Sum of Squares)
RSS <- sum(residuals^2)

# Menghitung TSS (Total Sum of Squares)
mean_observed <- mean(observed_values)
TSS <- sum((observed_values - mean_observed)^2)

# Menghitung R^2
R2 <- 1 - (RSS / TSS)
R2


# Residual dan bobot
hweights <- data.frame(
  Kab = IPM$Kab,               # Nama kabupaten/kota
  resid = rr.andrew$resid,     # Residual
  weight = rr.andrew$w         # Bobot
)

# Mengurutkan berdasarkan bobot terkecil
hweights_sorted <- hweights[order(hweights$weight), ]

# Menampilkan 15 pengamatan dengan bobot terkecil
hweights_sorted[1:15, ]

library(robustbase)

# Regresi LTS
lts_model <- ltsReg(IPM ~ Harapanlamasekolah + Ratalamasekolah + Pertumbuhanekonomi + Indekskesehatan + Pengangguran, data = IPM2)

# Ringkasan model
summary(lts_model)

# Residual dan bobot dari LTS
lts_resid <- lts_model$residuals
lts_weights <- lts_model$lts.wt

# Gabungkan data residual dan bobot
lts_weights_df <- data.frame(
  Kab = IPM$Kab,
  resid = lts_resid,
  weight = lts_weights
)

# Urutkan berdasarkan bobot terkecil
lts_weights_sorted <- lts_weights_df[order(lts_weights_df$weight), ]

# Tampilkan pengamatan dengan bobot terkecil
lts_weights_sorted[1:15, ]

