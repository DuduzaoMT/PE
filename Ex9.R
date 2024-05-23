# Fixando a semente
set.seed(2822)

# Parâmetros do problema
m <- 5000
n <- 100
lambda_0 <- 2.90
lambda_1 <- 3.15
k <- 3.234

# Inicializando contadores de erro
erro_tipo_I <- 0
erro_tipo_II <- 0

# Função para simular a amostra e calcular a média
simula_amostra <- function(lambda_val, n) {
  amostra <- rpois(n, lambda_val)
  return(mean(amostra))
}

# Simulando sob H0 e H1
for (i in 1:m) {
  # Amostras sob H0
  media_H0 <- simula_amostra(lambda_0, n)
  if (media_H0 > k) {
    erro_tipo_I <- erro_tipo_I + 1
  }
  
  # Amostras sob H1
  media_H1 <- simula_amostra(lambda_1, n)
  if (media_H1 <= k) {
    erro_tipo_II <- erro_tipo_II + 1
  }
}

# Frequências relativas
alpha <- erro_tipo_I / m
beta <- erro_tipo_II / m

# Calculando o quociente
quociente <- beta / alpha

# Mostrando os resultados
cat("Frequência relativa de erro do tipo I (alpha):", alpha, "\n")
cat("Frequência relativa de erro do tipo II (beta):", beta, "\n")
cat("Quociente (beta/alpha):", quociente, "\n")
