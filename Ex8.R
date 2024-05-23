set.seed(1820)

amostra <- c(34.0,39.5,33.2,38.1,29.9,37.4,32.1,36.5,31.4,34.1,33.1,31.5,33.9,33.9)

amostra_sem_reposicao <- sample(amostra, 8, replace = FALSE)
n <- 8

# Calcular média amostral e variância amostral
media_amostral <- mean(amostra_sem_reposicao)
variancia_amostral <- var(amostra_sem_reposicao)

# Quantis da distribuição qui-quadrado
a <- qchisq(0.025, df = n - 1)
b <- qchisq(0.975, df = n - 1)

# Limites do intervalo de confiança para a variância
limite_inferior <- ((n - 1) * variancia_amostral) / b
limite_superior <- ((n - 1) * variancia_amostral) / a

# Exibir intervalo de confiança
cat("Intervalo de confiança para a variância (sigma^2):\n")
cat("(", limite_inferior, ",", limite_superior, ")\n")

# Instalar e carregar o pacote pracma
#install.packages("pracma")
library(pracma)

# Função para resolver o sistema de equações
equations <- function(x) {
  c <- x[1]
  d <- x[2]
  F_c <- pchisq(c, df = n - 1)
  F_d <- pchisq(d, df = n - 1)
  f_c <- dchisq(c, df = n + 3)
  f_d <- dchisq(d, df = n + 3)
  return(c(F_d - F_c - 0.95, f_d - f_c))
}

# Chutar valores iniciais para c e d (a e b)
a <- qchisq(0.025, df = n - 1)
b <- qchisq(0.975, df = n - 1)
initial_guess <- c(a, b)

# Resolver as equações
solucao <- fsolve(equations,initial_guess)
c_value <- solucao$x[1]
d_value <- solucao$x[2]

print(paste("c:", c_value))
print(paste("d:", d_value))


# Calcular intervalo de confiança para a variância com base em c e d
limite_inferior_new <- ((n - 1) * variancia_amostral) / d_value
limite_superior_new <- ((n - 1) * variancia_amostral) / c_value

# Exibir intervalo de confiança
cat("Novo intervalo de confiança para a variância (sigma^2):\n")
cat("(", limite_inferior_new, ",", limite_superior_new, ")\n")

# Calcular amplitudes dos intervalos de confiança do passo 2 e 3
amplitude_intervalo_2 <- limite_superior - limite_inferior
amplitude_intervalo_3 <- limite_superior_new - limite_inferior_new

# Calcular a diferença entre as amplitudes arredondada a três casas decimais
diferenca <- abs(round(amplitude_intervalo_3 - amplitude_intervalo_2, 4))

# Exibir a diferença
cat("Diferença entre as amplitudes dos intervalos de confiança:", diferenca, "\n")




