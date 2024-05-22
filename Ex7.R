# Carregando o pacote stats4
library(stats4)


# X - 'montante de indemnização (em milhares de euros) de determinada carteira de apólices'

# Função de densidade de probabilidade de X para X >= 4
fx <- function (theta){
  return (theta*(x^(-theta-1))*(a^theta))
}

# Dados observados
dados <- c(4.37, 4.3, 5.15, 5.11, 5.15, 4.66, 6.15, 5.72, 5.87, 5.64, 4.05)
a <- 4

# Definindo a função de log-verossimilhança negativa
neg_log_verossimilhança <- function(theta) {
  if (theta <= 0) {
    return(Inf)  # Para garantir que theta é positivo
  }
  n <- length(dados)
  log_likelihood <- n * log(theta) + n * theta * log(a) - (theta + 1) * sum(log(dados))
  return(-log_likelihood)  # Retorna o negativo da log-verossimilhança
}

# Usando a função mle para encontrar a EMV de theta
resultado_mle <- mle(neg_log_verossimilhança, start = list(theta = 3.6))

# Obtendo a estimativa de theta
estimativa_theta <- coef(resultado_mle)
print(estimativa_theta)

# Estimando o quantil de probabilidade p = 0.75
p <- 0.75
quantil_75 <- function(theta) {
  a * (1 - p)^(-1 / theta)
}

estimativa_quantil_75 <- quantil_75(estimativa_theta)
print(estimativa_quantil_75)

# Verdadeiro valor do quantil quando theta = 3.6
theta_verdadeiro <- 3.6
quantil_75_verdadeiro <- quantil_75(theta_verdadeiro)
print(quantil_75_verdadeiro)

# Calculando o desvio absoluto
desvio_absoluto <- abs(quantil_75_verdadeiro - estimativa_quantil_75)
print(sprintf("Desvio absoluto: %.4f", desvio_absoluto))
