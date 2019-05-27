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
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS donator CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS prejemnik CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS bolnisnica CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS kri CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS donira CASCADE", con=conn))
    #dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS hrani CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS nahaja CASCADE", con=conn))
    
  }, finally = {
    dbDisconnect(conn)
  })
}

ustvari_tabele <- function(){
  tryCatch({
    conn <- dbConnect(drv, dbname = db, host = host,
                      user = user, password = password)
    #tabele
    donator <- dbSendQuery(conn, build_sql("CREATE TABLE donator (
                                         id integer PRIMARY KEY,
                                         ime text,
                                         kraj text,
                                         drzava text,
                                         starost integer,
                                         email text,
                                         teza numeric,
                                         hemoglobin numeric,
                                         datum_vpisa_v_evidenco date,
                                         krvna_skupina text)", con=conn))
    
    prejemnik <- dbSendQuery(conn, build_sql("CREATE TABLE prejemnik (
                                           id integer PRIMARY KEY,
                                           ime text,
                                           kraj text,
                                           drzava text,
                                           starost text,
                                           email text,
                                           datum_vloge date,
                                           krvna_skupina text)", con=conn))
    
    bolnisnica <- dbSendQuery(conn, build_sql("CREATE TABLE bolnisnica (
                                           id integer PRIMARY KEY,
                                           ime text,
                                           kraj text,
                                           drzava text,
                                           direktor text,
                                           zaloga numeric)", con=conn))
    
    kri <- dbSendQuery(conn, build_sql("CREATE TABLE kri (
                                            stevilka_vrecke integer PRIMARY KEY,
                                            hemoglobin numeric,
                                            krvna_skupina text,
                                            datum_prejetja date)", con=conn))
    
    #tabele relacij:
    
    donira <- dbSendQuery(conn, build_sql("CREATE TABLE donira(
                                id INTEGER NOT NULL REFERENCES donator(id),
                                stevilka_vrecke INTEGER NOT NULL REFERENCES kri(stevilka_vrecke))", con=conn))
    
    #prejme <- dbSendQuery(conn, build_sql("CREATE TABLE hrani(
    #           prejemnik_id TEXT NOT NULL REFERENCES prejemnik(id),
    #           kri_stevilka_vrecke TEXT NOT NULL REFERENCES kri(stevilka_vrecke))", con=conn))
    
    #hrani <- dbSendQuery(conn, build_sql("CREATE TABLE hrani(
    #           kri_stevilka_vrecke TEXT NOT NULL REFERENCES kri(stevilka_vrecke),
    #           bolnisnica_id TEXT NOT NULL REFERENCES bolnisnica(id))", con=conn))
    
    nahaja <- dbSendQuery(conn, build_sql("CREATE TABLE nahaja(
               id_prejemnika INTEGER NOT NULL REFERENCES prejemnik(id),
               id_bolnisnice INTEGER NOT NULL REFERENCES bolnisnica(id))", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_katjam TO javnost", con=conn))
    dbSendQuery(conn, build_sql("GRANT SELECT ON ALL TABLES IN SCHEMA public TO javnost", con=conn))    
    
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))  
    
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
insert_data <- function(){
  tryCatch({
    conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
    
    dbWriteTable(conn, name="donator", donator, append=T, row.names=FALSE)
    dbWriteTable(conn, name="prejemnik", prejemnik, append=T, row.names=FALSE)
    dbWriteTable(conn, name="bolnisnica", bolnisnica, append=T, row.names=FALSE)
    dbWriteTable(conn, name="kri", kri, append=T, row.names=FALSE)
    dbWriteTable(conn, name="donira", donira, append=T, row.names=FALSE)
    #dbWriteTable(conn, name="hrani", hrani, append=T, row.names=FALSE)
    dbWriteTable(conn, name="nahaja", nahaja, append=T, row.names=FALSE)
    
  }, finally = {
    dbDisconnect(conn) 
    
  })
}

pravice <- function(){
  # Uporabimo tryCatch,(da se povežemo in bazo in odvežemo)
  # da prisilimo prekinitev povezave v primeru napake
  tryCatch({
    # Vzpostavimo povezavo
    conn <- dbConnect(drv, dbname = db, host = host,#drv=s čim se povezujemo
                      user = user, password = password)
    
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_katjam TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    

    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_katjam TO javnost", con=conn))
    dbSendQuery(conn, build_sql("GRANT SELECT ON ALL TABLES IN SCHEMA public TO javnost", con=conn))    
    

    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    
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
insert_data()