# Neposredno klicanje SQL ukazov v R
library(dplyr)
library(dbplyr)
library(RPostgreSQL)

source("auth.R", encoding="UTF-8")
source("tabele.R", encoding="UTF-8")


# Povežemo se z gonilnikom za PostgreSQL
drv <- dbDriver("PostgreSQL")

#brisanje tabel
delete_table <- function(){
  # Uporabimo funkcijo tryCatch,
  # da prisilimo prekinitev povezave v primeru napake
  tryCatch({
    # Vzpostavimo povezavo z bazo
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    
    # Če tabela obstaja, jo zbrišemo, ter najprej zbrišemo tiste,
    # ki se navezujejo na druge
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS muzej CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS prostor CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS obdobje CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS umetnik CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS umetnina CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS deloval CASCADE", con=conn))
    
  }, finally = {
    dbDisconnect(conn)
  })
}

ustvari_tabele <- function(){
  tryCatch({
    conn <- dbConnect(drv, dbname = db, host = host,
                      user = user, password = password)
    #tabele
    donor <- dbSendQuery(conn, build_sql("CREATE TABLE donor (
                                         id numeric PRIMARY KEY,
                                         ime text,
                                         kraj text,
                                         drzava text,
                                         starost integer,
                                         telefon numeric,
                                         teza numeric,
                                         hemoglobin numeric,
                                         skupina text)", con=conn))
    
    prejemnik <- dbSendQuery(conn, build_sql("CREATE TABLE prejemnik (
                                           id numeric PRIMARY KEY,
                                           ime text,
                                           kraj text,
                                           drzava text,
                                           starost text,
                                           telefon text,
                                           skupina text)", con=conn))
    
    bolnisnica <- dbSendQuery(conn, build_sql("CREATE TABLE bolnisnica (
                                           id numeric PRIMARY KEY,
                                           ime text,
                                           kraj text,
                                           drzava text,
                                           direktor text)", con=conn))
    
    kri <- dbSendQuery(conn, build_sql("CREATE TABLE kri (
                                            st_vrecke numeric PRIMARY KEY,
                                            datum_prejetja date,
                                            hemoglobin numeric,
                                            krvna_skupina text)", con=conn))
    
    #tabele relacij:
    
    #deloval <- dbSendQuery(conn, build_sql("CREATE TABLE deloval(
    #                                       umetnik_id TEXT NOT NULL REFERENCES umetnik(id),
    #                                       obdobje_id TEXT NOT NULL REFERENCES obdobje(id))", con=conn))
    
    
  }, finally = {
    # Na koncu nujno prekinemo povezavo z bazo,
    # saj preveč odprtih povezav ne smemo imeti
    dbDisconnect(conn)
    # Koda v finally bloku se izvede v vsakem primeru
    # - bodisi ob koncu izvajanja try bloka,
    # ali pa po tem, ko se ta konča z napako
  })
}

#Funcija, ki vstavi podatke
#insert_data <- function(){
#  tryCatch({
#    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
#    
#    dbWriteTable(conn, name="muzej", muzej, append=T, row.names=FALSE)
#    
#  }, finally = {
#    dbDisconnect(conn) 
#    
#  })
#}

pravice <- function(){
  # Uporabimo tryCatch,(da se povežemo in bazo in odvežemo)
  # da prisilimo prekinitev povezave v primeru napake
  tryCatch({
    # Vzpostavimo povezavo
    conn <- dbConnect(drv, dbname = db, host = host,#drv=s čim se povezujemo
                      user = user, password = password)
    
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_katjam TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
  }, finally = {
    # Na koncu nujno prekinemo povezavo z bazo,
    # saj preveč odprtih povezav ne smemo imeti
    dbDisconnect(conn) #PREKINEMO POVEZAVO
    # Koda v finally bloku se izvede, preden program konča z napako
  })
}

pravice()
delete_table()
ustvari_tabele()
#insert_data()