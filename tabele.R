# Knjižnice

library(knitr)
library(dplyr)
library(readr)
library(rvest)
library(gsubfn)
library(ggplot2)
library(reshape2)
library(shiny)
library(devtools)
library(tidyr)
library(digest)
library(rgeos)
library(maptools)
library(rmarkdown)
library(DT)
library(extrafont)

drzave.slo <- c(
  "Austria" = "Avstrija",
  "Belgium" = "Belgija",
  "Bosnia and Herzegovina" = "Bosna in Hercegovina",
  "Denmark" = "Danska",
  "Estonia" = "Estonija",
  "Switzerland" = "Švica",
  "France" = "Francija",
  "Italy" = "Italija",
  "Liechtenstein" = "Lihtenštajn",
  "Sweden" = "Švedska",
  "Luxembourg" = "Luksemburg",
  "Norway" = "Norveška",
  "Croatia" = "Hrvaška",
  "Germany" = "Nemčija",
  "Slovenia" = "Slovenija",
  "Portugal" = "Portugalska",
  "Romania" = "Romunija",
  "United Kingdom" = "Združeno kraljestvo",
  "Montenegro" = "Črna Gora",
  "Turkey" = "Turčija",
  "Australia" = "Avstralija",
  "Bulgaria" = "Bolgarija",
  "Greece" = "Grčija",
  "Cyprus" = "Ciper",
  "Hungary" = "Madžarska",
  "Malta" = "Malta",
  "Poland" = "Poljska",
  "Czech Republic" = "Češka",
  "Spain" = "Španija",
  "Latvia" = "Latvija",
  "Lithuania" = "Litva",
  "Finland" = "Finska",
  "Ireland" = "Irska",
  "Iceland" = "Islandija",
  "Russia" = "Rusija",
  "Slovakia" = "Slovaška",
  "Netherlands" = "Nizozemska",
  "Czech Republic" = "Češka",
  "Former Yugoslav Republic of Macedonia" = "Makedonija",
  "Ukraine" = "Ukrajina",
  "Serbia" = "Srbija",
  "Albania" = "Albanija",
  "Andorra" = "Andora",
  "Armenia" = "Armenija",
  "Azerbaijan" = "Azerbajdžan",
  "Belarus" = "Belorusija",
  "Georgia" = "Gruzija",
  "Moldova" = "Moldavija",
  "Monaco" = "Monako",
  "Macedonia" = "Makedonija",
  "Kosovo" = "Kosovo"
)


# Uvozimo podatke za bolnišnice

bolnisnica <- read_csv("Podatki/bolnisnica.csv", 
                       col_types = cols(id = col_integer(), 
                                        zaloga = col_number()))

bolnisnica <- bolnisnica %>% mutate(drzava = drzave.slo[drzava])

# Uvozimo podatke za kri

kri1 <- read_csv("Podatki/kri1.csv", col_types = cols(datum_prejetja = col_date(format = "%d/%m/%Y"), 
                                                      stevilka_vrecke = col_integer()))

kri2 <- read_csv("Podatki/kri2.csv", col_types = cols(datum_prejetja = col_date(format = "%d/%m/%Y"), 
                                                      stevilka_vrecke = col_integer()))

kri <- rbind(kri1, kri2)

# Uvozimo podatke za donatorje

donor1 <- read_csv("Podatki/donor1.csv", 
                   col_types = cols(Hemoglobin = col_number(), 
                                    Starost = col_integer(), Telefon = col_character(), 
                                    Teza = col_number(), datum_donacije = col_date(format = "%d/%m/%Y"), 
                                    id = col_integer()))

donor1 <- donor1 %>% mutate(Drzava = drzave.slo[Drzava])


donor2 <- read_csv("Podatki/donor2.csv", 
                   col_types = cols(Hemoglobin = col_number(), 
                                    Starost = col_integer(), Telefon = col_character(), 
                                    Teza = col_number(), datum_donacije = col_date(format = "%d/%m/%Y"), 
                                    id = col_integer()))

donor2 <- donor2 %>% mutate(Drzava = drzave.slo[Drzava])

donator <- rbind(donor1, donor2)

# Uvozimo podatke za prejemnike

prejemnik1 <- read_csv("Podatki/prejemnik1.csv", 
                       col_types = cols(Starost = col_integer(), 
                                        Telefon = col_character(), datum_vloge = col_date(format = "%d/%m/%Y"), 
                                        id = col_integer()))

prejemnik1 <- prejemnik1 %>% mutate(Drzava = drzave.slo[Drzava])

prejemnik2 <- read_csv("Podatki/prejemnik2.csv", 
                       col_types = cols(Starost = col_integer(), 
                                        Telefon = col_character(), datum_vloge = col_date(format = "%d/%m/%Y"), 
                                        id = col_integer()))
prejemnik2 <- prejemnik2 %>% mutate(Drzava = drzave.slo[Drzava])

prejemnik <- rbind(prejemnik1, prejemnik2)

# Funkcija, ki zgenerira naključne krvne skupine za donatorje in prejemnike
# (sample upošteva pogostost krvnih skupin v Evropi)

x <- c("A+", "A-", "B+", "B-", "AB+", "AB-", "0+", "0-")
  
skupine_donator <- sample(x, prob = c(.34, .06, 0.09, 0.02, 0.03, 0.01, 0.38, 0.07), size = 2000, replace = TRUE) 

vektor_donator <- as.data.frame(skupine_donator)

skupine_prejemnik <- sample(x, prob = c(.34, .06, 0.09, 0.02, 0.03, 0.01, 0.38, 0.07), size = 2000, replace = TRUE) 

vektor_prejemnik <- as.data.frame(skupine_prejemnik)


# Krvne skupine dodamo donatorjem in prejemnikom

donator <- cbind(donator, vektor_donator)

prejemnik <- cbind(prejemnik, vektor_prejemnik)


# Stolpec datum_prejetja v tabeli kri se ujema s stolpcem datum_donacije iz tabele donator,
# dodamo pa še podatek o hemoglobinu in krvni skupini donatorja vrečke krvi

dodatek <- donator[, c(8, 10, 9)]

kri <- cbind(kri[, c(1)], dodatek)


# Popravimo id-je in stevilko_vrecke

vsi_id <- sample(100000:999999, size = 5000, replace = FALSE)
vektor_id <- as.data.frame(vsi_id)

don_id <- as.data.frame(vektor_id[c(1:2000),])
prej_id <- as.data.frame(vektor_id[c(2001:4000),])
bol_id <- as.data.frame(vektor_id[c(4001:5000),])

vrecka_id <- sample(10000000:99999999, size = 2000, replace = FALSE)
vrecka_id_tabela <- as.data.frame(vrecka_id)

donator <- cbind(donator[, c(2:10)], don_id)
donator <- donator[, c(10, 1:9)]
donator <- donator[, c(1:7, 9, 10)]
colnames(donator) <- c("id", "ime", "kraj", "drzava", "starost", "telefon",
                       "teza", "datum_vpisa_v_evidenco", "krvna_skupina")

prejemnik <- cbind(prejemnik[, c(2:8)], prej_id)
prejemnik <- prejemnik[, c(8, 1:7)]
colnames(prejemnik) <- c("id", "ime", "kraj", "drzava", "starost", "telefon",
                         "datum_vloge", "krvna_skupina")

bolnisnica <- cbind(bolnisnica[, c(2:6)], bol_id)
bolnisnica <- bolnisnica[, c(6, 1:5)]
colnames(bolnisnica) <- c("id", "ime", "kraj", "drzava", "direktor", "zaloga")

kri <- cbind(kri[, c(2:4)], vrecka_id_tabela)
kri <- kri[, c(4, 1:3)]
kri <- kri[, c(1, 2, 4)]
colnames(kri) <- c("stevilka_vrecke", "hemoglobin", "datum_prejetja")

# Zaloga bolnišnice: ena vrečka ima 400ml krvi

bolnisnica$zaloga <- bolnisnica$zaloga * 0.4

#tabele relacij
donira1 <- as.data.frame(donator[, c(1)])
donira2 <- as.data.frame(kri[, c(1)])

donira <-cbind(donira1, donira2)
colnames(donira) <- c("id", "stevilka_vrecke")

prejemnik_pomozna <- prejemnik[ , c(1, 3, 4)]
bolnisnica_pomozna <- bolnisnica[ , c(1:4)]

#če je bolnišnica v istem kraju kot prejemnik
bolnisnica_nahaja1 <- left_join(prejemnik_pomozna, bolnisnica_pomozna, by="kraj")

#funkcija, ki izbriše podatke NA
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}


nahaja1 <- delete.na(bolnisnica_nahaja1)
nahaja1 <- nahaja1 %>% group_by(id.x) %>% sample_n(1)
nahaja1 <- nahaja1[, c(1,4)]
colnames(nahaja1) <- c("id.x","id")

#če bolnišnica ni v istem kraju kot prejemnik
ostali <- bolnisnica_nahaja1[ !(bolnisnica_nahaja1$id.x %in% nahaja1$id.x), ]
colnames(ostali)[colnames(ostali) == "drzava.x"] <- "drzava"

nahaja2 <- left_join(ostali, bolnisnica_pomozna, by="drzava")
nahaja2 <- nahaja2[, c(1,7)]
nahaja2 <- nahaja2 %>% group_by(id.x) %>% sample_n(1)

nahaja <- rbind.data.frame(nahaja1, nahaja2)
#nahaja <- as.data.frame(mapply(c, nahaja1, nahaja2))
colnames(nahaja) <- c("id_prejemnika", "id_bolnisnice")
