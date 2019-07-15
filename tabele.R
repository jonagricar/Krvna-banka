# KNJIŽNICE

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



# SLOVAR DRŽAV

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



# GLAVNE TABELE



# Uvozimo podatke za bolnišnice

bolnisnica1 <- read_csv("Podatki/bolnisnica.csv", 
                       col_types = cols(id = col_integer(), 
                                        zaloga = col_number()))

bolnisnica2 <- read_csv("Podatki/bolnice_dodatno.csv", 
                        col_types = cols(id = col_integer(), 
                                         zaloga = col_number()))

bolnisnica <- rbind(bolnisnica1, bolnisnica2)
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

donor2 <- read_csv("Podatki/donor2.csv", 
                   col_types = cols(Hemoglobin = col_number(), 
                                    Starost = col_integer(), Telefon = col_character(), 
                                    Teza = col_number(), datum_donacije = col_date(format = "%d/%m/%Y"), 
                                    id = col_integer()))

donator <- rbind(donor1, donor2)

email_d1 <- read_csv("Podatki/email.csv")
email_d2 <- read_csv("Podatki/email2.csv")
email_d <- rbind(email_d1, email_d2)

donator <- cbind(donator, email_d)
donator <- donator[, c(1, 10, 3, 4, 5, 11, 7, 8, 9)]
donator <- donator %>% mutate(Drzava = drzave.slo[Drzava])



# Uvozimo podatke za prejemnike

prejemnik1 <- read_csv("Podatki/prejemnik1.csv", 
                       col_types = cols(Starost = col_integer(), 
                                        Telefon = col_character(), datum_vloge = col_date(format = "%d/%m/%Y"), 
                                        id = col_integer()))

prejemnik2 <- read_csv("Podatki/prejemnik2.csv", 
                       col_types = cols(Starost = col_integer(), 
                                        Telefon = col_character(), datum_vloge = col_date(format = "%d/%m/%Y"), 
                                        id = col_integer()))

prejemnik <- rbind(prejemnik1, prejemnik2)

email_p1 <- read_csv("Podatki/email3.csv")
email_p2 <- read_csv("Podatki/email4.csv")
email_p <- rbind(email_p1, email_p2)
teza_p1 <- read_csv("Podatki/teza_prej1.csv")
teza_p2 <- read_csv("Podatki/teza_prej2.csv")
teza_p <- rbind(teza_p1, teza_p2)

prejemnik <- cbind(prejemnik, email_p, teza_p)
prejemnik <- prejemnik[, c(1, 8, 3, 4, 5, 9, 10, 7)]
prejemnik <- prejemnik %>% mutate(Drzava = drzave.slo[Drzava])



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
# dodamo pa še podatek o hemoglobinu in skupini donatorja vrečke krvi

dodatek <- donator[, c(8, 10, 9)]

kri <- cbind(kri[, c(1)], dodatek)



# Popravimo id-je in stevilko_vrecke

vsi_id <- sample(100000:999999, size = 5265, replace = FALSE)
vektor_id <- as.data.frame(vsi_id)

don_id <- as.data.frame(vektor_id[c(1:2000),])
prej_id <- as.data.frame(vektor_id[c(2001:4000),])
bol_id <- as.data.frame(vektor_id[c(4001:5265),])

vrecka_id <- sample(10000000:99999999, size = 2000, replace = FALSE)
vrecka_id_tabela <- as.data.frame(vrecka_id)

donator <- cbind(donator[, c(2:10)], don_id)
donator <- donator[, c(10, 1:9)]
donator <- donator[, c(1:7, 9, 10)]
colnames(donator) <- c("id", "ime", "kraj", "drzava", "starost", "email",
                       "teza", "datum_vpisa_v_evidenco", "krvna_skupina")

prejemnik <- cbind(prejemnik[, c(2:9)], prej_id)
prejemnik <- prejemnik[, c(9, 1:8)]
colnames(prejemnik) <- c("id", "ime", "kraj", "drzava", "starost", "email",
                         "teza", "datum_vloge", "krvna_skupina")



#naredimo glavno tabelo z vsemi osebami, združimo po stolpcih donatorja, prejemnike bomo od donatorjev ločili tako, da bodo imeli
#prejemniki vrednost null pri datumu vpisa v evidenco
oseba <- merge(donator, prejemnik, all=TRUE)
oseba <- oseba[, 1:9]
prejemnikx <- prejemnik

bolnisnica <- cbind(bolnisnica[, c(2:6)], bol_id)
bolnisnica <- bolnisnica[, c(6, 1:4)]
colnames(bolnisnica) <- c("id", "ime", "kraj", "drzava", "direktor")

bolnisnica <- bolnisnica %>% group_by(kraj) %>%  filter(row_number()==1) # ena bolnišnica v mestu

kri <- cbind(kri[, c(2:4)], vrecka_id_tabela)
kri <- kri[, c(4, 1:3)]
kri <- kri[, c(1, 2, 4)]
colnames(kri) <- c("stevilka_vrecke", "hemoglobin", "datum_prejetja")
donira <- oseba[!is.na(oseba$datum_vpisa_v_evidenco),]
kri$donator <- donira$id[ match(kri$datum_prejetja, donira$datum_vpisa_v_evidenco) ]

prejemnik <- oseba[is.na(oseba$datum_vpisa_v_evidenco),]
prejemnik <- merge(prejemnik,prejemnikx, by=c("id"),all.x=TRUE)
prejemnik <- prejemnik[, c(1,16)]
colnames(prejemnik) <- c("id_prejemnika", "datum_vloge")
#stolpci id_prejemnika, ime bolnisnice in datum vloge



# pomožna tabela za lokacijo pacienta v bolnišnici

prejemnik_pomozna <- prejemnikx[ , c(1, 3, 4)]
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

#če bolnišnica ni v istem kraju kot prejemnik
ostali <- bolnisnica_nahaja1[ !(bolnisnica_nahaja1$id.x %in% nahaja1$id.x), ]
ostali <- ostali[, c(1,2,3)]
colnames(ostali)[colnames(ostali) == "drzava.x"] <- "drzava"

nahaja2 <- left_join(ostali, bolnisnica_pomozna, by="drzava")
nahaja2 <- nahaja2[, c(1,4)]
colnames(nahaja2)[colnames(nahaja2) == "id"] <- "id.y"
nahaja2 <- nahaja2 %>% group_by(id.x) %>%  filter(row_number()==1)

lokacija <- rbind.data.frame(nahaja1, nahaja2)
colnames(lokacija) <- c("id_prejemnika", "id_bolnisnice")


# tabeli prejemnik dodamo dobljeno lokacijo

prejemnik <- merge(prejemnik, lokacija ,by="id_prejemnika")
colnames(prejemnik) <- c("id_prejemnika", "datum_vloge", "id_lokacije_zdravljenja")



# pomožna tabela za lokacijo donacije krvi donatorja

donira_pomozna <- donira[ , c(1, 3, 4)]
bolnisnica_pomozna_donira <- bolnisnica[ , c(1:4)]

#če je bolnišnica v istem kraju kot prejemnik
bolnisnica_daruje1 <- left_join(donira_pomozna, bolnisnica_pomozna_donira, by="kraj")

#funkcija, ki izbriše podatke NA
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

daruje1 <- delete.na(bolnisnica_daruje1)
daruje1 <- daruje1 %>% group_by(id.x) %>% sample_n(1)
daruje1 <- daruje1[, c(1,4)]

#če bolnišnica ni v istem kraju kot darovalec
ostali_darovalci <- bolnisnica_daruje1[ !(bolnisnica_daruje1$id.x %in% daruje1$id.x), ]
ostali_darovalci <- ostali_darovalci[, c(1,2,3)]
colnames(ostali_darovalci)[colnames(ostali_darovalci) == "drzava.x"] <- "drzava"

daruje2 <- left_join(ostali_darovalci, bolnisnica_pomozna_donira, by="drzava")
daruje2 <- daruje2[, c(1,4)]
colnames(daruje2)[colnames(daruje2) == "id"] <- "id.y"
daruje2 <- daruje2 %>% group_by(id.x) %>%  filter(row_number()==1)

lokacija_darovalca <- rbind.data.frame(daruje1, daruje2)
colnames(lokacija_darovalca) <- c("donator", "id_lokacije_darovalca")


# tabeli kri dodamo dobljeno lokacijo kot izvor vrečke krvi

kri <- merge(kri, lokacija_darovalca ,by="donator")
kri <- kri[, c(2,3,4,1,5)]
colnames(kri) <- c("stevilka_vrecke", "hemoglobin", "datum_prejetja", "donator", "hrani")

# pomožna tabela za relacijo prejme

prejemnik_kri <- oseba[is.na(oseba$datum_vpisa_v_evidenco),]
prejemnik_kri <- prejemnik_kri[, c(1,8)]
colnames(prejemnik_kri) <- c("id_prejemnika", "krvna_skupina")
prejemnik_kri <- merge(prejemnik, prejemnik_kri, by="id_prejemnika")
colnames(prejemnik_kri) <- c("id_prejemnika", "datum_vloge", "lokacija", "krvna_skupina")

donira_nova <- donira
colnames(donira_nova)[1] <- "donator"
vrecka_skupina <- merge(donira_nova, kri, by="donator")
vrecka_skupina <- vrecka_skupina[, c(1,8,13)]
colnames(vrecka_skupina) <- c("donator", "krvna_skupina_vrecka", "lokacija")

prejme <- merge(vrecka_skupina, prejemnik_kri, by="lokacija")
prejme$ujemanje <- prejme$krvna_skupina_vrecka == prejme$krvna_skupina



