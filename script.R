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

par(mfrow=c(1,2))
boxplot(data$Age, main="Boxplot de Idade")
boxplot(data$Hours.per.day, main="Boxplot de Horas por dia")


# Remover os outliers
# Outlier para idade é superior a 40 anos
data2 <- data[which(data$Age <= 40 & data$Hours.per.day <= 10),]

# stepwise
df <- data2[,c("Hours.per.day", "While.working", "Instrumentalist", "Composer",
               "Insomnia")]
age = data2$Age
anxiety = data2$Anxiety
depression = data2$Depression
insomnia = data2$Insomnia
ocd = data2$OCD
while.working <- data2$While.working
instrumentalist <- data2$Instrumentalist
fav.genre <- data2$Fav.genre
composer <- data2$Composer


lm0<-lm(data2$Hours.per.day~1)
lmax<-lm(data2$Hours.per.day~age+anxiety+depression+insomnia+ocd+while.working+instrumentalist+fav.genre+composer)
step(lm0,scope=list(lower=lm0,upper=lmax),trace=TRUE,test="F")

# Ajustando modelo proposto pelo stepwise
lmm <- lm(formula = data2$Hours.per.day ~ while.working + insomnia + 
            instrumentalist + composer + age + ocd)
summary(lmm)

shapiro.test(lmm$residuals)

res = rstudent(lmm)
plot(lmm$fitted.values, res) # homoscedastidicidade, outlier e relacao linear
abline(h=0)

hist(res) # normalidade dos erros
# Fazendo transformação log
Y <- ifelse(data2$Hours.per.day == 0, 0.1, data2$Hours.per.day)
lmmln <- lm(formula = log(Y) ~ while.working + insomnia + 
              instrumentalist + age + ocd + composer)
summary(lmmln)

shapiro.test(lmmln$residuals)

# pontos influentes
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
# ajustando o modelo


# ajustando modelo sem os pontos influentes
lmmln3 <- lm(formula = log(Y) ~ while.working + insomnia + instrumentalist + age + composer)

summary(lmmln3)

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

fit <- lm(sqrt(Y) ~ while.working + insomnia + instrumentalist + composer)
summary(fit)

shapiro.test(fit$residuals)

# removendo instrumentalist (não significante)
fit.sqrt <- lm(formula = sqrt(Y) ~ while.working + insomnia + composer)

summary(fit.sqrt)

res_t = rstudent(fit.sqrt)
shapiro.test(res_t)
bptest(formula(fit.sqrt), studentize=T)
# histograma
hist(res_t)
# qq plot
qqnorm(res_t)
qqline(res_t)
# sla
plot(fit.sqrt$fitted.values,res_t)
abline(h=0)
