### -----
### importacao dos dados

library(tidyverse)
library(DAAG)
library(nortest)
library(lawstat)
library(lmtest)
library(MASS)


data <- read.csv('data.csv', stringsAsFactors = T, na.strings="")
data <- na.omit(data)

### -----
### analise exploratoria

summary(data)

# dataframe com as variaveis numericas
df1 <- data[, c("Age", "Hours.per.day", "While.working", "Instrumentalist",
                "Composer", "Exploratory", "Foreign.languages", "BPM", "Anxiety",
                "Depression", "Insomnia", "OCD")]

df_1 <- data[, c("Hours.per.day", "Age", "Anxiety",
                 "Depression", "Insomnia", "OCD")]

### PRECISA TER TABELA DE CORRELACAO
cor(df_1)
# phi de cramer pra var nao continuas

# a idade dos respondentes varia de 10 a 89, mediana é 21 anos
hist(data$Age)
boxplot(data$Age)

# a quantidade de horas de música por dia varia de 0 a 24, a mediana é 3
hist(data$Hours.per.day)
boxplot(data$Hours.per.day)


# tirar os outliers
# Outlier para idade eh superior a 40 anos
data2 <- data[which(data$Age <= 40 & data$Hours.per.day <= 10),]


hist(data2$Age)
boxplot(data2$Age)
hist(data2$Hours.per.day)
boxplot(data2$Hours.per.day)


# analise de correlaçao
plot(df1)
cor(df1$Age, df1$Hours.per.day)
cor(df1$Anxiety, df1$Depression)



# dataframe com as variaveis categoricas
df2 <- data[, c("Primary.streaming.service", "Fav.genre", "Frequency..Classical.",
                "Frequency..Country.", "Frequency..EDM.", "Frequency..Folk.",
                "Frequency..Gospel.", "Frequency..Hip.hop.", "Frequency..Jazz.",
                "Frequency..K.pop.", "Frequency..Latin.", "Frequency..Lofi.",
                "Frequency..Metal.", "Frequency..Pop.", "Frequency..R.B.",
                "Frequency..Rap.", "Frequency..Rock.", "Frequency..Video.game.music.",
                "Music.effects")]




df3 <- data[,c("Hours.per.day", "While.working", "Instrumentalist", "Composer",
               "Insomnia")]





# foco nas relacoes entre os x


# teste de multiconiaridade


fit1 <- lm(Hours.per.day ~ ., data=df1)
summary(fit1)

fit3 <- lm(Hours.per.day ~ ., data = df3)
summary(fit3)


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


lm0<-lm(data2$Hours.per.day~1)
lmax<-lm(data2$Hours.per.day~age+anxiety+depression+insomnia+ocd+while.working+instrumentalist+fav.genre)
step(lm0,scope=list(lower=lm0,upper=lmax),trace=TRUE,test="F")

# Ajustando modelo proposto pelo stepwise
lmm <- lm(formula = data2$Hours.per.day ~ while.working + insomnia + 
     instrumentalist + age + ocd)
summary(lmm)

res = rstudent(lmm)
plot(lmm$fitted.values, res) # homoscedastidicidade, outlier e relacao linear
abline(h=0)

hist(res) # normalidade dos erros
# Fazendo transformação log
Y <- ifelse(data2$Hours.per.day == 0, 0.1, data2$Hours.per.day)
lmmln <- lm(formula = log(Y) ~ while.working + insomnia + 
              instrumentalist + age + ocd)
summary(lmmln)

shapiro.test(lmmln$residuals)

#pontos influentes
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

# segundo stepwise (sem pontos influentes)
lm0<-lm(data3$Hours.per.day~1)
lmax<-lm(data3$Hours.per.day~age+anxiety+depression+insomnia+ocd+while.working+instrumentalist+fav.genre)
step(lm0,scope=list(lower=lm0,upper=lmax),trace=TRUE,test="F")

# ajustando o modelo
Y <- ifelse(data3$Hours.per.day == 0, 0.1, data3$Hours.per.day)
lmmln2 <- lm(formula = log(Y) ~ while.working + insomnia + 
               instrumentalist + age + ocd)
summary(lmmln2)

shapiro.test(lmmln2$residuals)

# ajustando modelo sem ocd (não significante)
lmmln3 <- lm(formula = log(Y) ~ while.working + insomnia + instrumentalist + age)

summary(lmmln3)

shapiro.test(lmmln3$residuals)
lillie.test(lmmln3$residuals)
bptest(formula(lmmln3), studentize=T)

boxcox(Y~insomnia,lab=seq(-1,1,1/10))


lillie.test(age)

lillie.test(sqrt(age))

fit.log <- lm(formula = log(Y) ~ while.working + insomnia + instrumentalist + data3$Composer + age)

summary(fit.log)

shapiro.test(fit.log$residuals)
lillie.test(fit.log$residuals)
bptest(formula(fit.log), studentize=T)

hist(sqrt(insomnia))

data4 <- data3[which(data3$Age <= 30 & data3$Hours.per.day <= 8),]

age = data4$Age
anxiety = data4$Anxiety
depression = data4$Depression
insomnia = data4$Insomnia
ocd = data4$OCD
while.working <- data4$While.working
instrumentalist <- data4$Instrumentalist
composer <- data4$Composer
bpm <- data4$BPM

Y <- ifelse(data4$Hours.per.day == 0, 0.1, data4$Hours.per.day)

Y <- data4$Hours.per.day

boxplot(age)

fit.log <- lm(formula = sqrt(Y) ~ while.working + insomnia + instrumentalist + composer)

summary(fit.log)

shapiro.test(fit.log$residuals)
lillie.test(fit.log$residuals)
bptest(formula(fit.log), studentize=T)

hist(sqrt(Y))

infmed=influence.measures(fit.log)
infmed
summary(infmed)

influence.rows <- row.names(summary(infmed)) 

# removendo pontos influentes
data5 <- data4[which(!(row.names(data4) %in% influence.rows)),]

age = data5$Age
anxiety = data5$Anxiety
depression = data5$Depression
insomnia = data5$Insomnia
ocd = data5$OCD
while.working <- data5$While.working
instrumentalist <- data5$Instrumentalist
composer <- data5$Composer
bpm <- data5$BPM

# Y <- ifelse(data5$Hours.per.day == 0, 0.1, data5$Hours.per.day)

Y <- data5$Hours.per.day

boxplot(age)

fit.log <- lm(formula = sqrt(Y) ~ while.working + insomnia)

summary(fit.log)

shapiro.test(fit.log$residuals)
lillie.test(fit.log$residuals)
bptest(formula(fit.log), studentize=T)

### requisitos
# resumo, palavras-chave, introducao com motivacao, revisao bibliografica,
# descricao e tratamento da base de dados, metodologia, analise dos resultados,
# conclusao,referencias, apendice (opcional)

### passo a passo
# analise exploratoria
# testar transformacao de variavel
# analise de correlacao
# stepwise para definir variaveis

### perguntas
# como trabalhar com os generos?
# teria problema o modelo ficar com r2 ruim?