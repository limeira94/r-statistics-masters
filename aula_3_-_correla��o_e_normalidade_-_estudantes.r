#######################################################################################################
#
#         MÉTODOS ESTATÍSTICOS APLICADOS A ENG. FLORESTAL - UDESC
#               
#                      Prof. Marcos Felipe Nicoletti 
#                  
#######################################################################################################
#
####        CORRELAÇÃO E NORMALIDADE
#
#---------------------------------------------------------------------------

# Leitura dos dados
dados = read.csv("tabela-a1.csv",header=T, dec=",", sep = ";")
head(dados)
summary(dados$dap)
summary(dados)

# Gráfico de dispesão 

plot(dados$dap, dados$h, xlim=c(10,50), ylim=c(5,15))
plot(dados$dap, dados$h, xlim=c(10,50), ylim=c(5,15), pch=4,
     cex=2, xlab="DAP (cm)", ylab= "H (m)")
plot(dados$dap, dados$vol, xlim=c(10,50), ylim=c(0,1.5), pch=20, 
     cex=2, xlab="DAP (cm)", ylab= "Volume (m³)")

# Gráfico box-plot
par(mfrow=c(1,3)) # cria uma janela com uma linha e três colunas
boxplot(dados$dap,       col="grey", ylab="dap (cm)")
boxplot(dados$h,         col="grey", ylab="h (m)")
boxplot(dados$vol, col="grey",  ylab="Volume (m³)")
dev.off()

# QQ-plots
par(mfrow=c(1,3))
qqnorm(dados$dap, main="QQ-plot DAP (cm)")
qqline(dados$dap, lwd=2)

qqnorm(dados$h, main="QQ-plot h (m)", xlab="Quantis teóricos", ylab="Quantis amostrais")
qqline(dados$h, lwd=2)

qqnorm(dados$vol, main="QQ-plot Volume (m³)")
qqline(dados$vol, lwd=2)
dev.off()

#---------------------------------------------------------------------------
# Construir um gráfico em 3 dimensões
#--------------------------------------------------------------------------
# Para fazer gráficos Tridimensionais, é necessário o uso de alguns pacotes
install.packages("lattice")
require(lattice)
require(scatterplot3d)

d = (dados[,2:4])

s3d = scatterplot3d(d, type = "h", color = "blue",
              angle = 55, scale.y = 0.7, pch = 16,
              main = "", xlab="dap (cm)",
              ylab="Altura (m)", zlab="Volume (m³)")

##############################################################################
#
#         CORRELAÇÃO LINEAR
#
##########################################################################

# Coeficientes de Correlação Linear

# Utiliza os dados da tabela a1
head(dados)
summary(dados)
length(dados$arv)

##### Correlação de Pearson
?cor
cor(dados$h, dados$dap, method = "pearson")
cor(dados$h, dados$dap)
cor(dados$vol, dados$dap, method = "pearson")
cor(dados$vol, dados$h, method = "pearson")

cov(dados$dap, dados$vol)
var(dados$dap)

##### Correlação de Spearman

cor(dados$h, dados$dap, method = "spearman")
cor(dados$vol, dados$dap, method = "spearman")
cor(dados$h, dados$vol, method = "spearman")

cor(dados$h, dados$vol, method = "kendall")


#### Matriz de Correlação
cor(d)

#-------------------------------------------------------------------------
##### Testes de Correlação

# Percentil da dist. t de Student - t tabelado
?qt
qt(1-0.05/2, 28)

# Testes de correlação para Pearson
cor.test(dados$h, dados$dap, method = "pearson")
cor.test(dados$vol, dados$h, method = "pearson")
cor.test(dados$vol, dados$dap, method = "pearson")

#testes de correlação para Spearman
# Correlação DAP x H
cor.test(dados$h, dados$dap, method = "spearman")
rs = cor(dados$h, dados$dap, method = "spearman")
rs
rs*sqrt((30-2)/(1-rs^2)) # valor da estatística t para comparação

# Correlação DAP x volume
cor.test(dados$vol, dados$dap, method = "spearman")
dv = cor(dados$vol, dados$dap, method = "spearman")
dv
dv*sqrt((30-2)/(1-dv^2)) # valor da estatística t para comparação

# Correlação Volume x H
cor.test(dados$h, dados$vol, method = "spearman")
vh = cor(dados$h, dados$vol, method = "spearman")
vh
vh*sqrt((30-2)/(1-vh^2)) # valor da estatística t para comparação

## Plotanto a correlação

install.packages("corrplot")
library(corrplot)

m = cor(d)

corrplot(m)
corrplot(m, method = "ellipse")
corrplot(m, method = "number")
corrplot(m, method = "ellipse", type="upper")

corrplot(m, type = "upper")
corrplot(m, type = "lower")

#########################################################################
#
#        TESTES DE NORMALIDADE
#
########################################################################

head(dados)

# Teste de Shapiro Willk
help("shapiro.test")
shapiro.test(dados$dap)
shapiro.test(dados$h)
shapiro.test(dados$vol)

# Transformação do volume buscando normalidade
dados$v_t = log(dados$vol)

head(dados)
shapiro.test(dados$v_t)

# Teste de Kolmogorov Smirnov
help(ks.test)
ks.test(dados$arv , dados$h)
ks.test(dados$dap, dados$vol)

# Pacote nortest
pearson.test() # teste qui quadrado para normalidade

#-------------------------------------------------------------------------
# Teste para comparação de variâncias

# Pacote stats - já instalado
require(stats)

# Teste de Levene
install.packages("car")
library(car)

#bartlett.test(dados$dap, dados$h)
#bartlett.test(dap ~ arv, data=dados)

leveneTest(dados$dap, dados$h)

############################################################################
###          REGRESSÃO LINEAR
#---------------------------------------------------------------------------
# Crescimento de uma planta Y após ser submetida a um tempo
# X de exposição solar em horas.
#             Y = a + bX

x <- c(0.1,0.2,0.3,0.5,0.8,1.0,1.5,2.0) # Variável independente - horas
y <- c(0.88,0.90,0.99,1.12,1.40,1.62,2.20,3.10) # Variável resposta - crescimento

plot(y~x)
scatter.smooth(y,x)

?lm

m1 = lm(y ~ x )
m2 = lm(dados$vol ~dados$dap) # Modelo dados dendrométricos
summary(m2)
coef(m1)
anova(m1)
plot(m1)

# Y = b0 + b1X + b2 X²
m3 = lm(y ~ x + I(x^2))
summary(m3)
coef(m3)
anova(m2)


#####   Normalidade dos Resíduos
#Teste de Shapiro-Wilk: tem como objetivo avaliar se uma
#distribuição é semelhante a uma distribuição normal.
# Quando o p-value for maior que 0,05 (p > 0.05) a hipótese 
# nula (dos dados seguirem uma distribuição normal) é aceita.
shapiro.test(m1$residuals)
shapiro.test(m2$residuals)

#####    Outliers nos Resíduos
# Obter os resíduos padronizados na função summary(), inserimos
#nela outra função chamada rstandart() e indicamos nosso modelo (m1).
summary(rstandard(m1))
# INTERPRETAÇÃO: Observando os valores Min e Max, percebe-se que os resíduos não estão fora do
# intervalo -3 e 3. 

# FIM...