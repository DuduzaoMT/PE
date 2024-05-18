# Definindo a semente
set.seed(2255)

# Número de realizações
n_realizacoes <- 150

# Inicializando o contador de avisos sonoros sem desligar o sistema
avisos_sonoros <- 0

caminhos <- matrix(0, nrow = 1, ncol = 9)


# Simulação das realizações do sistema
for (i in 1:n_realizacoes) {
  # Gerando os sinais emitidos pelos circuitos
  for (k in 1:9){
    caminhos[,k] <- sample(1:10, size = 1, replace = TRUE, prob = 1:10 / 55)
  }
  
  # Verificando se pelo menos um circuito emitiu o sinal 2
  if (2 %in% caminhos) {
    # Incrementando o contador de avisos sonoros sem desligar o sistema
    avisos_sonoros <- avisos_sonoros + 1
    
    # Verificando se nenhum circuito emitiu o sinal 1
    if (!(1 %in% caminhos)) {
      # Desligando o sistema
      avisos_sonoros <- avisos_sonoros - 1
    }
  }
}
print(sinais)

# Calculando a proporção de vezes em que é produzido um aviso sonoro sem desligar o sistema
proporcao <- avisos_sonoros / n_realizacoes

# Exibindo a proporção arredondada a 2 casas decimais
print(round(proporcao, 2))