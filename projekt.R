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
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS oseba CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS bolnisnica CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS kri CASCADE", con=conn))
    dbSendQuery(conn,build_sql("DROP TABLE IF EXISTS prejemnik CASCADE", con=conn))
    
  }, finally = {
    dbDisconnect(conn)
  })
}

ustvari_tabele <- function(){
  tryCatch({
    conn <- dbConnect(drv, dbname = db, host = host,
                      user = user, password = password)
    #tabele
    oseba <- dbSendQuery(conn, build_sql("CREATE TABLE oseba (
                                         id serial PRIMARY KEY,
                                         ime text,
                                         kraj text,
                                         drzava text,
                                         starost integer,
                                         email text,
                                         teza numeric,
                                         datum_vpisa_v_evidenco date,
                                         krvna_skupina text)", con=conn))
    
    
    bolnisnica <- dbSendQuery(conn, build_sql("CREATE TABLE bolnisnica (
                                           id integer PRIMARY KEY,
                                           ime text,
                                           kraj text,
                                           drzava text,
                                           direktor text)", con=conn))
    
    kri <- dbSendQuery(conn, build_sql("CREATE TABLE kri (
                                       stevilka_vrecke serial PRIMARY KEY,
                                       hemoglobin numeric,
                                       datum_prejetja date,
                                       donator integer REFERENCES oseba(id),
                                       hrani integer REFERENCES bolnisnica(id),
                                       prejemnik integer REFERENCES oseba(id))", con=conn))
    
    
    prejemnik <- dbSendQuery(conn, build_sql("CREATE TABLE prejemnik (
                                         id_prejemnika integer REFERENCES oseba(id),
                                         datum_vloge date,
                                         id_lokacije_zdravljenja integer REFERENCES bolnisnica(id),
                                         id serial PRIMARY KEY,
                                         unique (id_prejemnika, datum_vloge, id_lokacije_zdravljenja))", con=conn))
    
    
    #pravice 
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    #dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO javnost WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO javnost", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_jonag TO javnost", con=conn))
    dbSendQuery(conn, build_sql("GRANT SELECT ON ALL TABLES IN SCHEMA public TO javnost", con=conn))
    dbSendQuery(conn, build_sql("GRANT INSERT ON ALL TABLES IN SCHEMA public TO javnost", con = conn))
    dbSendQuery(conn, build_sql("GRANT UPDATE ON ALL TABLES IN SCHEMA public TO javnost", con = conn))   
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))  
    
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
    
    dbWriteTable(conn, name="oseba", oseba, append=T, row.names=FALSE)
    dbWriteTable(conn, name="bolnisnica", bolnisnica, append=T, row.names=FALSE)
    dbWriteTable(conn, name="kri", kri, append=T, row.names=FALSE)
    dbWriteTable(conn, name="prejemnik", prejemnik, append=T, row.names=FALSE)

    prejemniki <- dbGetQuery(conn,
                            build_sql("SELECT oseba.id, krvna_skupina, id_lokacije_zdravljenja
                                      FROM oseba JOIN prejemnik ON oseba.id = id_prejemnika
                                      ORDER BY datum_vloge",
                                      con=conn))
    
    for (i in 1:nrow(prejemnik)) {
      vrecka <- dbGetQuery(conn,
                           build_sql("SELECT stevilka_vrecke FROM kri
                                     JOIN oseba ON donator = oseba.id
                                     WHERE hrani = ", prejemniki[i, "id_lokacije_zdravljenja"], "
                                     AND krvna_skupina = ", prejemniki[i, "krvna_skupina"], "
                                     AND prejemnik IS NULL
                                     ORDER BY RANDOM()
                                     LIMIT 1", con=conn))
      
      if (nrow(vrecka) > 0) {
        dbSendQuery(conn,
                    build_sql("UPDATE kri SET prejemnik = ", prejemniki[i, "id"],"
                              WHERE stevilka_vrecke = ", vrecka$stevilka_vrecke,
                              con=conn))
      } 
    }
    
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
    
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_jonag TO jonag WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_jonag TO javnost WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    #dbSendQuery(conn, build_sql("GRANT ALL ON SCHEMA public TO javnost WITH GRANT OPTION", con=conn))
    
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    #dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO javnost WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO jonag WITH GRANT OPTION", con=conn))
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO javnost", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT CONNECT ON DATABASE sem2019_jonag TO javnost", con=conn))
    dbSendQuery(conn, build_sql("GRANT SELECT ON ALL TABLES IN SCHEMA public TO javnost", con=conn))
    dbSendQuery(conn, build_sql("GRANT INSERT ON ALL TABLES IN SCHEMA public TO javnost", con = conn))
    dbSendQuery(conn, build_sql("GRANT UPDATE ON ALL TABLES IN SCHEMA public TO javnost", con = conn))  
    
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL TABLES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    
    dbSendQuery(conn, build_sql("GRANT ALL ON ALL SEQUENCES IN SCHEMA public TO katjam WITH GRANT OPTION", con=conn))
    
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
