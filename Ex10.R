# Valores dos dados
dados <- c(9.71, 8.97, 8.21, 10.72, 9.32, 12.37, 5.97, 10.12, 7.77, 7.90, 
           7.58, 6.46, 10.93, 10.22, 9.74, 8.32, 11.15, 11.67, 7.48, 13.42, 
           9.67, 8.70, 4.57, 8.19, 9.32, 5.70, 4.83, 6.03, 7.46, 6.91, 10.89, 
           9.87, 10.67, 8.15, 10.40, 9.47, 7.20, 11.86, 9.42, 8.14, 7.97, 9.27, 
           11.55, 9.91, 11.66, 11.17, 5.39, 13.11, 11.50, 11.15, 10.38, 7.99, 
           8.97, 7.62, 6.72, 9.76, 7.59, 12.63, 8.56, 10.87, 10.78, 9.69, 9.97, 
           6.99, 11.11, 8.16, 10.77, 9.23, 11.10, 5.88, 10.63, 8.82, 5.24, 9.90, 
           4.76, 11.30, 10.70, 11.49, 7.36, 10.85, 7.57, 4.24, 5.46, 8.24, 9.54, 
           9.65, 7.47, 9.11, 8.93, 9.43, 7.07, 10.46, 10.57, 6.02, 9.68, 10.09, 
           9.85, 6.29, 8.12, 9.37, 7.63, 8.80, 8.75, 6.47, 10.22, 8.70, 7.52, 
           13.12, 4.92, 6.37, 6.28, 8.21, 8.59, 5.09, 7.29, 8.24, 9.03, 9.78, 
           6.29, 7.12, 7.58, 10.27, 7.22, 11.82, 7.07, 6.88, 7.06, 7.20, 8.24, 
           11.20, 8.31, 12.78, 8.46, 5.96, 5.37, 8.55, 7.71, 9.61, 11.52, 10.50, 
           8.98, 10.53, 7.24, 5.58, 7.60, 7.91, 9.46, 7.77, 6.40, 6.40, 9.77, 
           7.71, 10.61, 7.15, 7.96, 6.50, 9.48, 9.46, 5.52, 11.76, 10.22, 9.69, 
           9.17, 8.45, 5.15, 8.80, 5.51, 9.70, 10.23, 6.52, 6.96, 10.98, 8.07, 
           9.84, 9.50, 7.55, 12.55, 11.84, 11.50, 7.41)

# Parâmetros da distribuição triangular
a <- 3.5
b <- 14

# Número de classes
k <- 8

# Dividindo o suporte em k classes com igual amplitude
amplitude <- (b - a) / k

# Limites das classes
limites_classes <- seq(a, b, by = amplitude)
limites_classes <- cbind(limites_classes[-length(limites_classes)], limites_classes[-1])

# Função de distribuição da distribuição triangular
dist_triangular <- function(x, a, b) {
  ifelse(x < a, 0,
         ifelse(x < (a + b) / 2, 2 * ((x - a) / (b - a))^2,
                ifelse(x < b, 1 - 2 * ((b - x) / (b - a))^2, 1)))
}

# Frequências observadas
frequencias_observadas <- numeric(k)
for (i in 1:k) {
  frequencias_observadas[i] <- sum(dados >= limites_classes[i,1] & dados < limites_classes[i,2])
}

# Frequências esperadas
frequencias_esperadas <- numeric(k)
for (i in 1:k) {
  # Calculando a probabilidade acumulada para o intervalo de classe
  prob_inicio <- ifelse(limites_classes[i,1] < a, 0, dist_triangular(limites_classes[i,1], a, b))
  prob_fim <- ifelse(limites_classes[i,2] >= b, 1, dist_triangular(limites_classes[i,2], a, b))
  # Frequência esperada = probabilidade acumulada * tamanho da amostra
  frequencias_esperadas[i] <- (prob_fim - prob_inicio) * length(dados)
}

# Teste de Qui-Quadrado
chi_sq <- sum((frequencias_observadas - frequencias_esperadas)^2 / frequencias_esperadas)

# Graus de liberdade
graus_liberdade <- k - 1

# Valor-p
valor_p <- 1 - pchisq(chi_sq, graus_liberdade)

# Saída
cat("Classes:\n")
print(limites_classes)
cat("\nFrequências Observadas:\n")
print(frequencias_observadas)
cat("\nFrequências Esperadas:\n")
print(frequencias_esperadas)
cat("\nQui-Quadrado:", chi_sq)
cat("\nGraus de Liberdade:", graus_liberdade)
cat("\nValor-p:", round(valor_p, 4))
