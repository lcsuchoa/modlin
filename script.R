### -----
### importacao dos dados

library(tidyverse)
library(DAAG)

setwd('/Users/lucasgomes/Documents/faculdade/modlin')
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

age = data2$Age
anxiety = data2$Anxiety
depression = data2$Depression
insomnia = data2$Insomnia
ocd = data2$OCD

lm0<-lm(data2$Hours.per.day~1)
lmax<-lm(data2$Hours.per.day~age+anxiety+depression+insomnia+ocd)
step(lm0,scope=list(lower=lm0,upper=lmax),trace=TRUE,test="F")

lmm = lm(formula = data2$Hours.per.day ~ insomnia + age + ocd)
summary(lmm)


#pontos influentes
infmed=influence.measures(fit3)
infmed
summary(infmed)






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