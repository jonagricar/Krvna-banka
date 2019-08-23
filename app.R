source("libraries.R")
source("auth_public.R")
 


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("VPIS", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Uporabnik", label = tagList(icon("user"), "Uporabnik")),
                   passwordInput("passwd", placeholder="Geslo", label = tagList(icon("unlock-alt"), "Geslo")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "VPIS", style = "color: white; background-color:#DF3232;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Napačno uporabniško ime ali geslo!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Uporabnik: sestra  Geslo: sestra"),
                     br(),
                     tags$code("Uporabnik: zdravnik  Geslo: zdravnik")
                   ))
)

credentials = data.frame(
  username_id = c("sestra", "zdravnik"),
  passod   = sapply(c("sestra", "zdravnik"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

header <- dashboardHeader( title = "Evropska krvna banka", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "red")

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  #povežemo se z bazo
  drv <- dbDriver("PostgreSQL")
  conn <- dbConnect(drv, dbname = db, host = host, user = user, password = password)
  
  dbGetQuery(conn, "SET CLIENT_ENCODING TO 'utf8'; SET NAMES 'utf8'")
  
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    dbDisconnect(conn) 
  })
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #ffffff !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(
          menuItem("Glavna stran", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Seznam bolnic", tabName = "Bolnice", icon = icon("th")),
          menuItem("Pacienti", tabName = "Pacienti", icon = icon("th")),
          menuItem("Zaloga krvi", tabName = "Kri", icon = icon("th")),
          menuItem("Obrazec", tabName = "Obrazec", icon = icon("th"))
        )
      }
      else{
        sidebarMenu(
          menuItem("Glavna stran", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Seznam bolnic", tabName = "Bolnice", icon = icon("th")),
          menuItem("Pacienti", tabName = "Pacienti", icon = icon("th")),
          menuItem("Zaloga krvi", tabName = "Kri", icon = icon("th"))
        )
        
      }
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            fluidRow(
              box(width = 12, title = "Podatki o pacientih in donatorjih", dataTableOutput('results'))
            ))
          ,
          tabItem(
            tabName ="Bolnice",
            fluidRow(
              box(width = 12, title = "Podatki o bolnicah", dataTableOutput('results_b'))
            )
          ),
          tabItem(
            tabName ="Pacienti",
            fluidRow(
              box(width = 12, title = "Podatki o pacientih", dataTableOutput('results_p'))
            )
          ),
          tabItem(
            tabName ="Kri",
            fluidRow(
              box(width = 12, title = "Zaloga krvi", dataTableOutput('results_k'))
            )
          ),
          tabItem(
            tabName ="Obrazec",
            fluidPage(
              titlePanel("Obrazec za oddajo krvi"),
              div(
                tags$style(HTML("
                    input:invalid {
                    background-color: #FFCCCC;
                    }")),
                #### Set up shinyjs ####
                useShinyjs(),
                
                ### shinyBS ###
                bsAlert("alert"),
                id = "form",
                textInput("imepri", "Ime in priimek", ""),
                textInput("kraj1", "Kraj"),
                selectInput("drzava1", "Država",
                            c("",  "Avstrija", "Belgija", "Bosna in Hercegovina", "Danska", "Estonija", "Švica", "Francija", "Italija",
                              "Lihtenštajn", "Švedska", "Luksemburg", "Norveška","Hrvaška", "Nemčija", "Slovenija","Portugalska","Romunija",
                              "Združeno Kraljestvo","Črna Gora", "Turčija", "Avstralija", "Bolgarija", "Grčija", "Ciper", "Madžarska",
                              "Malta","Poljska", "Češka", "Španija", "Latvija", "Litva", "Finska", "Irska", "Islandija", "Rusija", "Slovaška", 
                              "Nizozemska", "Makedonija", "Ukrajina", "Srbija", "Albanija", "Andora", "Armenija", "Azerbajdžan", "Belorusija",
                              "Gruzija", "Moldavija", "Monako", "Kosovo")),
                sliderInput("sta", "Starost", 18, 65, 30, ticks = TRUE),
                textInput("email1", "E-mail"),
                numericInput("teza1", "Teža v kg", value = NULL, min = 50, max = 150, step =  0.1),
                numericInput("hemo", "Hemoglobin", value = NULL, min = 100, max = 200, step =  0.01),
                dateInput("dat", "Datum vpisa v evidenco", format = "yyyy-mm-dd"),
                selectInput("skup", "Krvna skupina",
                            c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "0+", "0-")),
                checkboxInput("ze_doniral", "Oseba je v preteklosti kri že donirala", FALSE),
                useShinyalert(),
                actionButton("submit", "Dodaj v bazo", class = "btn-primary")
              )
            )
          )
        )
      } 
      else {
        tabItems(
          tabItem(
            tabName ="dashboard", class = "active",
            fluidRow(
              box(width = 12, title = "Podatki o pacientih in donatorjih", dataTableOutput('results'))
            ))
          ,
          tabItem(
            tabName ="Bolnice",
            fluidRow(
              box(width = 12, title = "Podatki o bolnicah", dataTableOutput('results_b'))
            )
          ),
          tabItem(
            tabName ="Pacienti",
            fluidRow(
              box(width = 12, title = "Podatki o pacientih", dataTableOutput('results_p'))
            )
          ),
          tabItem(
            tabName ="Kri",
            fluidRow(
              box(width = 12, title = "Zaloga krvi", dataTableOutput('results_k'))
            )
          )
         )
        
      }
      
    }
    else {
      loginpage
    }
  })
  output$results <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM oseba", con=conn))
  })
  output$results_b <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM bolnisnica", con = conn))
  })
  output$results_p <-  DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM prejemnik", con = conn))
  })
  output$results_k <- DT::renderDataTable({
    dbGetQuery(conn, build_sql("SELECT * FROM kri", con=conn))
  })
 
  #vstavljanje podatkov
  observeEvent(input$submit, {
    if (!(is.na(input$imepri)) && !(is.na(input$kraj1)) && !(is.na(input$drzava1)) && !(is.na(input$email1)) && !(is.na(input$skup)) && 
        !(is.na(input$dat)) && input$teza1 >= 50 && input$teza1 <= 150 && input$sta >= 18 && input$sta <= 65 && 
        input$hemo >= 100 && input$hemo <= 200) {
      dbSendQuery(conn, build_sql("INSERT INTO oseba (ime, kraj, drzava, starost, email, teza, krvna_skupina, datum_vpisa_v_evidenco)
                                VALUES (", input$imepri, ",", input$kraj1, ", ", input$drzava1, ",", input$sta, ",", input$email1, ",",
                                input$teza1, ",", input$skup, ",", input$dat, ");", con = conn))
      id_oseba <- dbGetQuery(conn, build_sql("INSERT INTO oseba (ime, kraj, drzava, starost, email, teza, krvna_skupina, datum_vpisa_v_evidenco)
                                             VALUES (", input$imepri, ",", input$kraj1, ", ", input$drzava1, ",", input$sta, ",", input$email1, ",",
                                           input$teza1, ",", input$skup, ",", input$dat, ") RETURNING id;", con = conn))
      id_bolnica <- dbGetQuery(conn, build_sql("SELECT id FROM bolnisnica
                                              WHERE drzava = ", input$drzava1, "
                                              ORDER BY kraj = ", input$kraj1, " DESC
                                              LIMIT 1;", con=conn))
      dbSendQuery(conn, build_sql("INSERT INTO kri (hemoglobin, datum_prejetja, donator, hrani)
                                 VALUES (", input$hemo, ",", input$dat, ",", id_oseba[1,1], ",", id_bolnica[1,1],");",
                                con=conn))
      shinyalert("OK!", "Donator uspešno dodan.", type = "success")
    } else {
      if (is.na(input$imepri)) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Vpišite ime!",
                    content = "Polje z imenom ne sme biti prazno!", style = "danger")
      }
      if (is.na(input$kraj1)) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Vpišite kraj!",
                    content = "Polje s krajem ne sme biti prazno!", style = "danger")
      }
      if (is.na(input$drzava1)) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Manjka država!",
                    content = "V spustnem seznamu izberite državo!", style = "danger")
      }
      if (is.na(input$email1)) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Vpišite e-mail darovalca!",
                    content = "Polje z e-mailom ne sme biti prazno!", style = "danger")
      }
      if (is.na(input$skup)) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Manjka krvna skupina darovalca!",
                    content = "V spustnem seznamu izberite krvno skupino!", style = "danger")
      }
      if (is.na(input$dat)) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Izberite datum!",
                    content = "'Polje z datumom ne sme biti prazno!", style = "danger")
      }
      if (is.na(input$teza1) | input$teza1 <= 50 | input$teza1 >= 150) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Neveljavna teža!",
                    content = "Teža mora biti med 50kg in 150kg!", style = "danger")
      }
      if (is.na(input$sta) | input$sta <= 18 | input$sta >= 65) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Neveljavna starost!",
                    content = "Darovalec mora biti star med 18 in 65 let!", style = "danger")
      }
      if (is.na(input$hemo) | input$hemo <= 100 | input$hemo >= 200) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Neveljavna vsebnost hemoglobina!",
                    content = "Vsebnost hemoglobina mora biti med 100 in 200!", style = "danger")
      }
    }
  })
}

runApp(list(ui = ui, server = server))