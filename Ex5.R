set.seed(1950)  # Define a semente para reproduzibilidade

# 1. Gerar amostras de variáveis aleatórias normais padrão
r <- 300  # número de amostras
m <- 170  # número de valores de T por amostra
n <- 23   # tamanho da amostra
T_sample <- matrix(rnorm(r * m), nrow = r, ncol = m)

proportions <- numeric(r)

for (i in 1:r) {
  T_values <- sqrt(n) * (T_sample[i, 1] / sqrt(sum(T_sample[i, -1]^2)))
  proportions[i] <- mean(T_values <= 1.5)
}

mean_proportion <- mean(proportions)
probability <- pt(1.5, df = n)

difference <- abs(mean_proportion - probability) * 100
result <- round(difference, 5)

result

