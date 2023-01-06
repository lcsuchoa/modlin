### -----
### importacao dos dados

library(tidyverse)
library(DAAG)


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
vif(fit1)



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
summary(fit)

fit2 <- lm(Hours.per.day ~ .-1, data=df2)
summary(fit2)

fit3 <- lm(Hours.per.day ~ ., data = df3)
summary(fit3)


res3 = rstudent(fit3)
plot(fit3$fitted.values, res3) # homoscedastidicidade, outlier e relacao linear
abline(h=0)

hist(res3) # normalidade dos erros


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