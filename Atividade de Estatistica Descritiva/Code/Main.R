dados <- read.table("DATA.txt",header=TRUE,sep=",")
library(DescTools)
calculaIMC <- function(a){
  a["IMC"] <- NULL
  for(i in 1:nrow(a)){
    a$IMC[i] <- formatC((a$peso[i]/a$altura[i]^2), digit=1, format="f")
  }
  return(a)
}

moda <- function(a){
  uniqa <- unique(a)
  uniqa[which.max(tabulate(match(a, uniqa)))]
}

Tabela_IMC_a <- calculaIMC(dados)
Tabela_IMC_a["Media"] <- NULL
Tabela_IMC_a$Media <- formatC(mean(as.numeric(Tabela_IMC_a$IMC)), digit=1, format = "f")
Tabela_IMC_a["Mediana"] <- NULL
Tabela_IMC_a$Mediana <- formatC(median(as.numeric(Tabela_IMC_a$IMC)), digit=1, format = "f")
Tabela_IMC_a["DecilD4"] <- NULL
Tabela_IMC_a$DecilD4[1] <- formatC(quantile(as.numeric(Tabela_IMC_a$IMC, prob = seq(0, 1, length = 11), type = 5)[5]), digits = 3)
Tabela_IMC_a["Desvio"] <- NULL
Tabela_IMC_a$Desvio <- formatC(sd(as.numeric(Tabela_IMC_a$IMC)), digit=3)
Tabela_IMC_a["Moda"] <- NULL
Tabela_IMC_a$Moda <- formatC(moda(as.numeric(Tabela_IMC_a$IMC)),digit=3)
for(i in 2:nrow(Tabela_IMC_a)) ## limpa os valores repetidos
{
  Tabela_IMC_a$Media[i] <- ''
  Tabela_IMC_a$Mediana[i] <- ''
  Tabela_IMC_a$DecilD4[i] <- ''
  Tabela_IMC_a$Desvio[i] <- ''
  Tabela_IMC_a$Moda[i] <- ''
}
print(Tabela_IMC_a)
Tabela_Freq <- data.frame("IMCs" = Freq(Tabela_IMC_a$IMC, ord = "des")$level, "Frequencia" = Freq(Tabela_IMC_a$IMC, ord = "des")$freq)
print(Tabela_Freq)
hist(as.numeric(Tabela_IMC_a$IMC), xlab = "IMC(kg/m2)", ylab = "Frequências", main = "Histograma de IMC",breaks=40, col="purple")
boxplot(as.numeric(Tabela_IMC_a$IMC), horizontal = TRUE, xlab="Altura(m)", col = "red")



