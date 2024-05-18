# Carregando o pacote 'VGAM' para usar a distribuição gamma
#install.packages("VGAM")
library(VGAM)

# Definindo parâmetros
set.seed(1973)
n_sample <- 1000
n <- 40
a <- 4

# Gerando amostras
samples <- matrix(rexp(n_sample * n , rate = 1/a), nrow = n_sample)

# Calculando Y para cada amostra
Y_simulated <- rowSums(samples)

# Calculando proporção de Y > 126
prop_Y_gt_126_simulated <- mean(Y_simulated > 126)

print(paste("Proporção de valores simulados de Y > 126:", prop_Y_gt_126_simulated))

# Parâmetros da distribuição gamma


# Calculando a probabilidade usando a função pgamma (cumulative distribution function) da distribuição gamma
prob_Y_gt_126_exact <- 1 - pgamma(126, shape = n, rate = 1/a)

print(paste("Probabilidade exata de Y > 126:", prob_Y_gt_126_exact))

# Calculando a diferença percentual
diff_percent <- abs(prop_Y_gt_126_simulated - prob_Y_gt_126_exact) * 100

print(paste("Diferença percentual entre as abordagens:", round(diff_percent, 4)))
