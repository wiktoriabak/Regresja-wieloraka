# Załaduj wymagane biblioteki
library(openxlsx)
library(dplyr)
library(car)
library(MASS)
library(ggplot2)
library(lmtest)
library(gridExtra)

dane <- read.xlsx("dane.xlsx")

dane <- dane[, -ncol(dane)]

print(nrow(dane))

# Dopasuj początkowy model liniowy
model <- lm(bezrobocie ~ wynagrodzenie + wspolczynnik_feminizacji + wspolczynnik_urbanizacji + oferty_pracy_na_10000 + malzenstwa_na_10000, data = dane)

# Oblicz odległość Cooka
odl_cooka <- cooks.distance(model)
cutoff_cook <- 4 / nrow(dane)
influential_points_cook <- which(odl_cooka > cutoff_cook)

# Wykres odległości Cooka
cooks_data <- data.frame(Index = 1:length(odl_cooka), CookDistance = odl_cooka)

ggplot(cooks_data, aes(x = Index, y = CookDistance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = cutoff_cook, linetype = "dashed", color = "red") +
  labs(title = "Wykres Odległości Cooka",
       x = "Indeks",
       y = "Odległość Cooka") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Usuń dane wpływowe dla Cooka
dane <- dane[-influential_points_cook, ]


print(nrow(dane))

# Dopasuj model liniowy
model <- lm(bezrobocie ~ wynagrodzenie + wspolczynnik_feminizacji + wspolczynnik_urbanizacji + oferty_pracy_na_10000 + malzenstwa_na_10000, data = dane)


# Oblicz DFFITS
DFFITS <- dffits(model)
cutoff_dffits <- 2 * sqrt(ncol(dane) / nrow(dane))
influential_points_dffits <- which(abs(DFFITS) > cutoff_dffits)

# Wykres DFFITS
dffits_data <- data.frame(Index = 1:length(DFFITS), DFFITS = DFFITS)

ggplot(dffits_data, aes(x = Index, y = DFFITS)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  geom_hline(yintercept = cutoff_dffits, linetype = "dashed", color = "red") +
  geom_hline(yintercept = -cutoff_dffits, linetype = "dashed", color = "red") +
  labs(title = "Wykres DFFITS",
       x = "Indeks",
       y = "DFFITS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Usuń dane wpływowe dla DFFITS
dane <- dane[-influential_points_dffits, ]


print(nrow(dane))

# Dopasuj model liniowy
model <- lm(bezrobocie ~ wynagrodzenie + wspolczynnik_feminizacji + wspolczynnik_urbanizacji + oferty_pracy_na_10000 + malzenstwa_na_10000, data = dane)


# Oblicz DFBETA dla wszystkich zmiennych
DFBETA <- dfbeta(model)
n <- nrow(dane)
cutoff_dfbeta <- 2 / sqrt(n)

# Wykresy DFBETA dla wszystkich zmiennych
zmienne <- names(coef(model))[-1] # Nazwy zmiennych niezależnych
plots_dfbeta <- list()
influential_points_dfbeta <- list()
for (i in 1:length(zmienne)) {
  dfbeta_values <- DFBETA[, i + 1]
  influential_points_dfbeta[[i]] <- which(abs(dfbeta_values) > cutoff_dfbeta)
  
  dfbeta_data <- data.frame(Index = 1:nrow(DFBETA), DFBETA = dfbeta_values)
  plot_dfbeta <- ggplot(dfbeta_data, aes(x = Index, y = DFBETA)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    labs(title = paste("Wykres DFBETA (", zmienne[i], ")", sep = ""),
         x = "Indeks",
         y = "DFBETA") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  plots_dfbeta[[i]] <- plot_dfbeta
}


# Wyświetl wykresy DFBETA jeden pod drugim
grid.arrange(grobs = plots_dfbeta)


#usunięto 19 wierszy

# Oblicz VIF w celu sprawdzenia współliniowości
VIF <- vif(model)
print(VIF)

#VIF nie wykazuje współliniowości (nie ma żadnej zmiennej > 10)


# Wybór modelu za pomocą AIC i regresji krokowej
model_AIC <- stepAIC(model, direction = "both", trace = TRUE)
summary(model_AIC)

# Wybór modelu za pomocą regresji krokowej z testem F
model_F <- step(model, direction = "both", test = "F", trace = TRUE)
summary(model_F)

# odrzucamy współczynnik_urbanizacji

#Dopasowany model
model2 <- lm(bezrobocie ~ wynagrodzenie + wspolczynnik_feminizacji + oferty_pracy_na_10000 + malzenstwa_na_10000, data = dane)

# Test Breuscha-Pagana na heteroskedastyczność
test_bp <- bptest(model2)
print(test_bp$p.value)


# Test RESET na specyfikację modelu
reset <- resettest(model2, power = 3)
print(reset$p.value)

# Test Rainbowa na liniowość
rain_test <- raintest(model2)
print(rain_test$p.value)
# z obu testów wychodzi liniowość p-value 0.1 i 0.08


# Sprawdzenie reszt pod kątem normalności
reszty <- residuals(model2)
shapiro <- shapiro.test(reszty)
print(shapiro$p.value)

#nie ma normalności reszt
