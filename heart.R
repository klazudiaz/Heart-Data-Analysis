#install.packages("readxl")
library(readxl)
sciezka_do_pliku <- "C:\\sem.V\\analiza danych\\heart.xlsx"
heart <- read_xlsx(sciezka_do_pliku)
head(heart)

#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)

str(heart)
typy_danych <- sapply(heart, class)
print(typy_danych)

kolumny_do_usuniecia <- c("Status","Sex","Age","BMI","Time","Diabetes","GFR","ACE_ARB", "Na", "Ferritin", "Tsat", "sTfR", "Anemia", "Fe", "ACE_ARB", "Digoxin", "BetaBlocker", "Loop_diuretics")
df <- heart%>% select(-one_of(kolumny_do_usuniecia))
head(df)
colSums(is.na(df))

brakujace_dane <- colSums(is.na(df))

print(brakujace_dane)

df <- na.omit(df)
head(df)

boxplot(df$NYHA)
boxplot(df$LVEF)
boxplot(df$NT_proBNP)
boxplot(df$Hemoglobina)

df <- df[df$NT_proBNP <= 4000, ]
head(df)

#install.packages("ggplot2")
#install.packages("moments")
library(ggplot2)
library(moments)

shapiro.test(df$NYHA)
hist_NYHA <- ggplot(df, aes(x = NYHA)) +
  geom_histogram(aes(y=..density..), bins = 30, fill="lightblue", color="black") +
  stat_function(fun=dnorm, args=list(mean=mean(df$NYHA), sd=sd(df$NYHA)), color="darkblue", size=1) +
  labs(title="Histogram NYHA", x="Warto?ci", y="G?sto??") +
  theme_minimal()
print(hist_NYHA)

shapiro.test(df$LVEF)
hist_LVEF <- ggplot(df, aes(x = LVEF)) +
  geom_histogram(aes(y=..density..), bins = 30, fill="lightblue", color="black") +
  stat_function(fun=dnorm, args=list(mean=mean(df$LVEF), sd=sd(df$LVEF)), color="darkblue", size=1) +
  labs(title="Histogram LVEF", x="Wartosci", y="Gestosc") +
  theme_minimal()
print(hist_LVEF)

shapiro.test(df$NT_proBNP)
hist_NT_proBNP <- ggplot(df, aes(x = NT_proBNP)) +
  geom_histogram(aes(y=..density..), bins = 30, fill="lightblue", color="black") +
  stat_function(fun=dnorm, args=list(mean=mean(df$NT_proBNP), sd=sd(df$NT_proBNP)), color="darkblue", size=1) +
  labs(title="Histogram NT_proBNP", x="Wartosci", y="Gestosc") +
  theme_minimal()
print(hist_NT_proBNP)

shapiro.test(df$Hemoglobina)
hist_Hemoglobina<- ggplot(df, aes(x = Hemoglobina)) +
  geom_histogram(aes(y=..density..), bins = 30, fill="lightblue", color="black") +
  stat_function(fun=dnorm, args=list(mean=mean(df$Hemoglobina), sd=sd(df$Hemoglobina)), color="darkblue", size=1) +
  labs(title="Histogram Hemoglobina", x="Wartosci", y="Gestocs") +
  theme_minimal()
print(hist_Hemoglobina)


cor(df[,c(1,2,3,4,5)], use = "complete.obs", method="spearman")

cor(df[,c(1,2,3,4,5)], use = "complete.obs", method="pearson")



library(ggplot2)
library(ggcorrplot)
kor<- cor(df[, sapply(df, is.numeric)], use = "pairwise.complete.obs")
ggcorrplot(kor, method="square")


wilcox.test(df$Statin, df$NYHA, paired=F)
wilcox.test(df$Statin, df$LVEF, paired=F)
wilcox.test(df$Statin, df$NT_proBNP, paired=F)
wilcox.test(df$Statin, df$Hemoglobina, paired=F)

rl <- glm(Statin~NYHA+LVEF+NT_proBNP+Hemoglobina, data=df, family = "binomial")
summary(rl)

library(ggplot2)

df$NYHA <- as.numeric(df$NYHA)

df$Predictions <- predict(rl, type = "response")

ggplot(df, aes(x = LVEF, y = Predictions)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Wykres zaleznosci miedzy LVEF a Statin",
       x = "LVEF",
       y = "Statin") +
  theme_minimal()

ggplot(df, aes(x = NYHA, y = Predictions)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Wykres zaleznosci miedzy NYHA a Statin",
       x = "NYHA",
       y = "Statin") +
  theme_minimal()


ggplot(df, aes(x = NT_proBNP, y = Predictions)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Wykres zaleznosci miedzy NT_proBNP a Statin",
       x = "NT_proBNP",
       y = "Statin") +
  theme_minimal()

kruskal.test(NYHA~Statin, data=df)
kruskal.test(LVEF~Statin, data=df)
kruskal.test(NT_proBNP~Statin, data=df)
