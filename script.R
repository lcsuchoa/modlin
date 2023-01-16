### -----
### importacao dos dados

library(tidyverse)
library(DAAG)
library(nortest)
library(lawstat)
library(lmtest)
library(MASS)
library(corrplot)

### leitura dos dados
data <- read.csv('data.csv', stringsAsFactors = T, na.strings="")
data <- na.omit(data)

### -----
### analise exploratoria

summary(data)

# dataframe com as variaveis numericas
df_1 <- data[, c("Hours.per.day", "Age", "Anxiety",
                 "Depression", "Insomnia", "OCD")]

### matriz de correlação
cor(df_1)

## mapa de calor da matriz de correlação
corr <-  cor(df_1)
corrplot(corr) 
palette <-  colorRampPalette(c("lightblue", "white", "darkred")) (20) 
heatmap(x = corr, col = palette, symm = TRUE)

# a idade dos respondentes varia de 10 a 89, mediana é 21 anos
hist(data$Age)
boxplot(data$Age)

# a quantidade de horas de música por dia varia de 0 a 24, a mediana é 3
hist(data$Hours.per.day)
boxplot(data$Age)

# gráficos usados no artigo, boxplot da variável Age e boxplot da variável Hour.per.day
par(mfrow=c(1,2))
boxplot(data$Age, main="Boxplot de Idade")
boxplot(data$Hours.per.day, main="Boxplot de Horas por dia")


# remover os outliers
# outlier para idade é superior a 40 anos
data2 <- data[which(data$Age <= 40 & data$Hours.per.day <= 10),]

age = data2$Age
anxiety = data2$Anxiety
depression = data2$Depression
insomnia = data2$Insomnia
ocd = data2$OCD
while.working <- data2$While.working
instrumentalist <- data2$Instrumentalist
fav.genre <- data2$Fav.genre
composer <- data2$Composer

# stepwise forward
lm0<-lm(data2$Hours.per.day~1)
lmax<-lm(data2$Hours.per.day~age+anxiety+depression+insomnia+ocd+while.working+instrumentalist+fav.genre+composer)
step(lm0,scope=list(lower=lm0,upper=lmax),trace=TRUE,test="F")

# Ajustando modelo proposto pelo stepwise
lmm <- lm(formula = data2$Hours.per.day ~ while.working + insomnia + 
            instrumentalist + composer + age + ocd)
summary(lmm)


res = rstudent(lmm)
shapiro.test(res)
plot(lmm$fitted.values, res) # homoscedastidicidade, outlier e relacao linear
abline(h=0)

hist(res) # normalidade dos erros

# Fazendo transformação log
Y <- ifelse(data2$Hours.per.day == 0, 0.1, data2$Hours.per.day)
lmmln <- lm(formula = log(Y) ~ while.working + insomnia + 
              instrumentalist + age + ocd + composer)
summary(lmmln)

shapiro.test(lmmln$residuals)

# encontrando pontos influentes usando as medidas de influência
infmed=influence.measures(lmmln)
infmed
summary(infmed)

influence.rows <- row.names(summary(infmed)) 

# removendo pontos influentes
data3 <- data2[which(!(row.names(data2) %in% influence.rows)),]

age = data3$Age
anxiety = data3$Anxiety
depression = data3$Depression
insomnia = data3$Insomnia
ocd = data3$OCD
while.working <- data3$While.working
instrumentalist <- data3$Instrumentalist
fav.genre <- data3$Fav.genre
composer <- data3$Composer
Y <- ifelse(data3$Hours.per.day == 0, 0.1, data3$Hours.per.day)


# ajustando modelo sem os pontos influentes
lmmln3 <- lm(formula = log(Y) ~ while.working + insomnia + instrumentalist + age + composer)

summary(lmmln3)

# testes de normalidade e homocedasticidade
shapiro.test(lmmln3$residuals)
bptest(formula(lmmln3), studentize=T)

# restringindo a base de dados Idade entre 18 e 30 anos e Horas por dia menores ou iguais a 8.
data4 <- data3[which(data3$Age <= 30 & data3$Age >= 18 & data3$Hours.per.day <= 8),]

age = data4$Age
anxiety = data4$Anxiety
depression = data4$Depression
insomnia = data4$Insomnia
ocd = data4$OCD
while.working <- data4$While.working
instrumentalist <- data4$Instrumentalist
composer <- data4$Composer
bpm <- data4$BPM

Y <- data4$Hours.per.day

# ajsutando modelo com as restrições da base de dados
fit <- lm(sqrt(Y) ~ while.working + insomnia + instrumentalist + composer)
summary(fit)

# teste de normalidade
shapiro.test(fit$residuals)

# ajustando modelo sem instrumentalist (não significante)
fit.sqrt <- lm(formula = sqrt(Y) ~ while.working + insomnia + composer)

summary(fit.sqrt)

# estudentizando os resíduos e fazendos os teste de normalidade e homocedasticidade
res_t = rstudent(fit.sqrt)
shapiro.test(res_t)
bptest(formula(fit.sqrt), studentize=T)

par(mfrow=c(1,2))
# histograma dos resíduos
hist(res_t, main="Histograma dos resíduos", ylab= 'Frequência' , xlab= 'Resíduos estudentizados')

# qq plot
qqnorm(res_t, main= "Q-Q Plot", xlab= 'Quantis teóricos' , ylab= 'Quantis amostrais')
qqline(res_t)
# gráfico de dispersão
par(mfrow=c(1,1))
plot(fit.sqrt$fitted.values,res_t, main="Gráfico de dispersão dos resíduos", xlab= 'Valores ajustados' , ylab= 'Resíduos' ,pch=20 ,cex=1.2)
abline(h=0)

fit.sqrt$coefficients

# calculando todos os cenários para saber o impacto da insônia no número de horas
y.0.i <- fit.sqrt$coefficients[1] + 0*fit.sqrt$coefficients[3]
y.10.i <- fit.sqrt$coefficients[1] + 10*fit.sqrt$coefficients[3]

y.0.i.w <- fit.sqrt$coefficients[1] + fit.sqrt$coefficients[2] + 0*fit.sqrt$coefficients[3]
y.10.i.w <- fit.sqrt$coefficients[1] + fit.sqrt$coefficients[2] + 10*fit.sqrt$coefficients[3]

y.0.i.c <- fit.sqrt$coefficients[1] + 0*fit.sqrt$coefficients[3] + fit.sqrt$coefficients[4]
y.10.i.c <- fit.sqrt$coefficients[1] + 10*fit.sqrt$coefficients[3] + fit.sqrt$coefficients[4]

y.0.i.wc <- fit.sqrt$coefficients[1] + fit.sqrt$coefficients[2] + 0*fit.sqrt$coefficients[3] + fit.sqrt$coefficients[4]
y.10.i.wc <- fit.sqrt$coefficients[1] + fit.sqrt$coefficients[2] + 10*fit.sqrt$coefficients[3] + fit.sqrt$coefficients[4]

y.10.i^2 - y.0.i^2
y.10.i.w^2 - y.0.i.w^2
y.10.i.c^2 - y.0.i.c^2
y.10.i.wc^2 - y.0.i.wc^2

impactos <- c(y.10.i^2 - y.0.i^2, y.10.i.w^2 - y.0.i.w^2, y.10.i.c^2 - y.0.i.c^2, y.10.i.wc^2 - y.0.i.wc^2)
impactos
# impacto médio
mean(impactos)
# impacto médio em minutos
mean(impactos)*60

# beta0 em minutos
fit.sqrt$coefficients[1]*60
