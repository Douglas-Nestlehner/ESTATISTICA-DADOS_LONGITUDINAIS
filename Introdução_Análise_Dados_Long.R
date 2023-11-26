setwd("")
set.seed(4)
library(tidyverse)
library(ggthemes)

# DADOS
df = read.table("PesoNascimento_tratados.txt", skipNul = F)
colnames(df) = c("ID_Mãe", "Ordem_Nascimento", "Peso_Nascimento", "Idade_Materna", "ID_Filho")

str(df)

df$ID_Mãe = as.factor(df$ID_Mãe)
df$ID_Filho = as.factor(df$ID_Filho)

summary(df)

library(xtable)
xtable(head(df))

desc = summary(df$Peso_Nascimento)
xtable(desc)

boxplot(df$Peso_Nascimento)


# Grafico espaguete
ggplot(df, aes(x = Ordem_Nascimento, y = Peso_Nascimento, group = ID_Mãe)) +
  geom_line(alpha=1, size = 0.8) + 
  labs(y = "Peso Nascimento (em gramas)", x = "Ordem de Nascimento",  title = 'Grafico de Espaguete para toda a amostra') +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Amostra aleatória
# Sortear as amostras
n=5
A1 = df$ID_Mãe %>% sample(n, replace = F) 
A = df %>% 
  filter(df$ID_Mãe == A1[1] | df$ID_Mãe == A1[2] | df$ID_Mãe == A1[3] | df$ID_Mãe == A1[4] | df$ID_Mãe == A1[5]) 
A

# Grafico espaguete para amostra
ggplot(A, aes(x = Ordem_Nascimento, y = Peso_Nascimento, group = ID_Mãe, color = ID_Mãe)) +
  geom_line(alpha=1, size = 0.8) + 
  labs(y = "Peso Nascimento (em gramas)", x = "Ordem de Nascimento") +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 


## Medidas descritivas
# Média
M = df %>% 
  group_by(Ordem_Nascimento) %>% 
  summarise(
    Média_Peso = mean(Peso_Nascimento),
    Mediana = median(Peso_Nascimento),
    Média_Idade = mean(Idade_Materna),
    Desvio = sd(Peso_Nascimento))
M$ID_Mãe = c(rep("Média",5))
M
z = data.frame(ID_Mãe = M$ID_Mãe, Ordem_Nascimento = c(1,2,3,4,5), Peso_Nascimento = M$Média_Peso, 
               Idade_Materna = M$Média_Idade, ID_Filho = M$ID_Mãe)

df2 = rbind(df, z)
df2$grupo = ifelse(df2$ID_Mãe == "Média", "Média", "Amostra")
tail(df2)

df3 = df2[4391:4395,]
df3
ggplot(df3, aes(x = Ordem_Nascimento, y = Peso_Nascimento, group = ID_Mãe, color = grupo)) +
  geom_line(alpha=1, size = 0.8) + 
  labs(y = "Peso Nascimento (em gramas)", x = "Ordem de Nascimento",  title = 'Grafico de Espaguete para toda a amostra (Média)') +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 




# Mediana
z2 = data.frame(ID_Mãe = M$ID_Mãe, Ordem_Nascimento = c(1,2,3,4,5), Peso_Nascimento = M$Mediana, 
                Idade_Materna = M$Média_Idade, ID_Filho = M$ID_Mãe)

df3 = rbind(df, z2)
df3$grupo = ifelse(df3$ID_Mãe == "Média", "Mediana", "Amostra")
tail(df3)

ggplot(df3, aes(x = Ordem_Nascimento, y = Peso_Nascimento, group = ID_Mãe, color = grupo)) +
  geom_line(alpha=1, size = 0.8) + 
  labs(y = "Peso Nascimento (em gramas)", x = "Ordem de Nascimento",  title = 'Grafico de Espaguete para toda a amostra (Mediana)') +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Espaguete Idade
ggplot(df, aes(x = Ordem_Nascimento, y = Idade_Materna, group = ID_Mãe)) +
  geom_line(alpha=1, size = 0.8) + 
  labs(y = "Idade", x = "Ordem de Nascimento",  title = 'Grafico de Espaguete Idade Materna') +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 

# Espaguete desvio
z3 = data.frame(ID_Mãe = M$ID_Mãe, Ordem_Nascimento = c(1,2,3,4,5), Peso_Nascimento = M$Desvio, 
                Idade_Materna = M$Média_Idade, ID_Filho = M$ID_Mãe)

df4 = rbind(df, z3)
df4$grupo = ifelse(df3$ID_Mãe == "Média", "Desvio", "Amostra")

df5 = rbind(df4[4395,], df4[4394,],df4[4393,],df4[4392,],df4[4391,])
df5
ggplot(df5, aes(x = Ordem_Nascimento, y = Peso_Nascimento, group = ID_Mãe, color = grupo)) +
  geom_line(alpha=1, size = 0.8) + 
  labs(y = "Peso Nascimento (em gramas)", x = "Ordem de Nascimento",  title = 'Grafico de Espaguete Desvio-Padrão') +
  cowplot::theme_cowplot() +
  theme(plot.title = element_text(hjust = 0.5)) 

#############
# ANÁLISE DA DEPENDÊNCIA TEMPORAL 
#############
# Para se verificar a dependência das medidas dos elementos, entre os
#instantes de tempo, usualmente se calcula a matriz de correlações de
#Pearson.

# Transofrmar/converter para o formato largo 
df$ID_Filho = NULL
df$Idade_Materna = NULL
dlargo = reshape(df, v.names= c("Peso_Nascimento"), timevar="Ordem_Nascimento", 
                 idvar=c("ID_Mãe"), direction="wide")
head(dlargo)

valores = bind_cols(dlargo$Peso_Nascimento.1, dlargo$Peso_Nascimento.2, dlargo$Peso_Nascimento.3, dlargo$Peso_Nascimento.4, dlargo$Peso_Nascimento.5)
corr = cor(valores)
xtable(corr)

library(corrplot)
corrplot(corr,
         method = 'color', 
         type = 'lower',
         tl.col = '#424242',
         tl.srt = 45,
         addCoef.col = 'black',
         col = colorRampPalette(c('red', 'white', 'blue'))(200),
         tl.cex = 0.8,
         number.cex = 0.9,
         number.font = 1,
         cl.cex = 0.9)

# 
setwd("")
set.seed(4)
library(tidyverse)
library(ggthemes)

# DADOS
df = read.table("PesoNascimento_tratados.txt", skipNul = F)
colnames(df) = c("ID_Mãe", "Ordem_Nascimento", "Peso_Nascimento", "Idade_Materna", "ID_Filho")

str(df)

df$ID_Mãe = as.factor(df$ID_Mãe)
df$ID_Filho = as.factor(df$ID_Filho)
df$Ordem_Nascimento = as.factor(df$Ordem_Nascimento)
summary(df)

# Ajuste modelo de regresão 
M1 = lm(Peso_Nascimento ~ Ordem_Nascimento + Idade_Materna, data = df)
summary(M1)

# Diagnostico
residuos = residuals(M1)
preditos = fitted.values(M1)

plot(preditos,residuos,xlab='Valores preditos',ylab='Residuos')

qqnorm(residuos,xlab='Quantis teoricos', ylab='Quantis amostrais')
qqline(residuos,col="red")


library(nortest)
shapiro.test(residuos)
lillie.test(residuos) 
ad.test(residuos)