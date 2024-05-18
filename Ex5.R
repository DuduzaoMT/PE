set.seed(1950)  # Define a semente para reproduzibilidade

# 1. Gerar amostras de variáveis aleatórias normais padrão
r <- 300  # número de amostras
m <- 170  # número de valores de T por amostra
n <- 23   # tamanho da amostra


#matriz de amostras
T_sample <- matrix(0,nrow = r, ncol = m)

proportions <- numeric(r)

for (i in 1:r) {
  
  #calcular uma amostra de valores
  T_values <- matrix(rnorm(m * (n+1)), nrow = m, ncol = (n+1))
  for (j in 1:m){
    T_value <- sqrt(n) * (T_values[j, 1] / sqrt(sum(T_values[j, 2:(n+1)]^2)))
    
    # cada linha do T_sample é uma amostra
    T_sample[i,j] <- T_value
  }
  # para cada amostra calcular a proporção de valores <= 1.5
  linha <- T_sample[i, ]
  proportions[i] <- mean( linha <= 1.5 )
}

p <- mean(proportions)
probability <- pt(1.5, df = n)

difference <- abs(p - probability) * 100
result <- round(difference, 5)

result
