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

# a idade dos respondentes varia de 10 a 89, mediana é 21 anos
hist(data$Age)
boxplot(data$Age)

# a quantidade de horas de música por dia varia de 0 a 24, a mediana é 3
hist(data$Hours.per.day)
boxplot(data$Hours.per.day)

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

shapiro.test(fit3$residuals)

#pontos influentes
infmed=influence.measures(fit3)
infmed
summary(infmed)


### remover outliers?