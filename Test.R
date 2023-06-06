library(tidyverse)
library(dplyr)
library(ggplot2)


#Priprava podatkiv:

preberi.csvs <- \(filePath){
  df <- read.csv(file.path("./", filePath))
  return(df)
}


podatki <- "exams.csv" %>% preberi.csvs()

#Preimenovanje podatki v Slovenščino in spremenitev izobrazbe staršev v številčni zapis.

colnames(podatki) <- c(
  "Spol",
   "Rasa",
   "Izobrazba.staršev",
   "Kosilo",
   "Vkjučenost.v.priprave.predmeta",
   "Rezultati.matematike",
   "Rezultati.branja",
   "Rezultati.pisanja"
)

preslikava_izobrazbe <- c(
  "some high school" = 1,
  "high school" = 2,
  "some college" = 3,
  "associate's degree" = 4,
  "bachelor's degree" = 5,
  "master's degree" = 6
)
podatki$Izobrazba.staršev <- preslikava_izobrazbe[podatki$Izobrazba.staršev]



#Razlike pri spolih
povprečna_razlika_med_spoli <- podatki %>% group_by(Spol) %>% summarize(
  Rezultati.matematike = mean(Rezultati.matematike),
  Rezultati.branja = mean(Rezultati.branja),
  Rezultati.pisanja = mean(Rezultati.pisanja),
  Rezultati.povprečje = mean(c(Rezultati.matematike, Rezultati.branja, Rezultati.pisanja)))

head(povprečna_razlika_med_spoli)



ggplot(gather(povprečna_razlika_med_spoli, key = "Score", value = "Average", -Spol), aes(x = Spol, y = `Average`, fill = `Score`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Povprečje rezultatov glede na spol ", x = "Spol", y = "Povprečje") +
  scale_fill_manual(values = c("Red", "Blue", "Green", "Yellow")) +
  coord_cartesian(ylim=c(60,75))




#Razlika glede na starševsko izobrazbo
povprečna_razlika_glede_na_izobrazbo_staršev <- podatki %>% group_by(Izobrazba.staršev) %>% summarize(
  Rezultati.matematike = mean(Rezultati.matematike),
  Rezultati.branja = mean(Rezultati.branja),
  Rezultati.pisanja = mean(Rezultati.pisanja),
  Rezultati.povprečje = mean(c(Rezultati.matematike, Rezultati.branja, Rezultati.pisanja)))

povprečna_razlika_glede_na_izobrazbo_staršev


#Razlika glede pripravljanje
povprečje_izpitov_glede_na_priprave <- podatki %>% group_by(Vkjučenost.v.priprave.predmeta) %>% summarize(
  Rezultati.matematike = mean(Rezultati.matematike),
  Rezultati.branja = mean(Rezultati.branja),
  Rezultati.pisanja = mean(Rezultati.pisanja),
  Rezultati.povprečje = mean(c(Rezultati.matematike, Rezultati.branja, Rezultati.pisanja)))


#Ocene
filtrirani_podatki_glede_na_ocene <- podatki
filtrirani_podatki_glede_na_ocene$Povprečni.rezultati <- rowMeans(filtrirani_podatki_glede_na_ocene[, c("Rezultati.matematike", "Rezultati.branja", "Rezultati.pisanja")])

ggplot(filtrirani_podatki_glede_na_ocene, aes(x = Povprečni.rezultati)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "steelblue") +
  labs(x = "Povprečni.rezultati", y = "Število") +
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  theme_minimal()



filtrirani_podatki_glede_na_ocene <- filter(filtrirani_podatki_glede_na_ocene, Povprečni.rezultati > 99)
filtrirani_podatki_glede_na_ocene <- filtrirani_podatki_glede_na_ocene[order(filtrirani_podatki_glede_na_ocene$Povprečni.rezultati, decreasing = TRUE), ]
head(filtrirani_podatki_glede_na_ocene)



#Linearna regresija med rezultat branja in rezultat pisanja.
plot(podatki$Rezultati.branja, podatki$Rezultati.pisanja, main = "Linearna regresija", xlab = "Rezultat branja", ylab = "Rezultat pisanja")
abline(lm(Rezultati.pisanja ~ Rezultati.branja, data = podatki), col = "Blue")

#Linearna regresija med rezultat pisanja in rezultat matematike.
plot(podatki$Rezultati.pisanja, podatki$Rezultati.matematike, main = "Linearna regresija", xlab = "Rezultat pisanja", ylab = "Rezultat matematike")
abline(lm(Rezultati.matematike ~ Rezultati.pisanja, data = podatki), col = "Blue")

