source("libraries.R")
source("auth_public.R")

#open in browser, da delajo šumniki 
#če presežeš max št povezav, zaženi:
#RPostgreSQL::dbDisconnect(RPostgreSQL::dbListConnections(RPostgreSQL::PostgreSQL())[[1]]) 

#Začetna stran
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

#uporabniki in dostopi
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

#obvezna polja za obrazec
fieldsMandatory <- c("imepri", "kraj1", "drzava1", "email1", "sta")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

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
  
  #ali je pravo ime in geslo
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
  
  #gumb za logout iz aplikacije
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #ffffff !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  #da vidi zdravnik dodaten zavihek obrazec
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
      #zavihki ki jih vidi zdravnik
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
              h4("Vsa polja so obvezna!"),
              div(
                tags$style(HTML("
                    input:invalid {
                    background-color: #FFCCCC;
                    }")),
                #### Set up shinyjs ####
                useShinyjs(),
                shinyjs::inlineCSS(appCSS),
                
                ### shinyBS ###
                bsAlert("alert"),
                id = "form",
                textInput("imepri", "Ime in priimek", ""),
                textInput("kraj1", "Kraj"),
                selectInput("drzava1", "Drzava",
                            c("",  "Avstrija", "Belgija", "Bosna in Hercegovina", "Danska", "Estonija", "Svica", "Francija", "Italija",
                              "Lihtenstajn", "Svedska", "Luksemburg", "Norveska","Hrvaska", "Nemcija", "Slovenija","Portugalska","Romunija",
                              "Zdruzeno Kraljestvo","Crna Gora", "Turcija", "Avstralija", "Bolgarija", "Grcija", "Ciper", "Madzarska",
                              "Malta","Poljska", "Ceska", "Spanija", "Latvija", "Litva", "Finska", "Irska", "Islandija", "Rusija", "Slovaska", 
                              "Nizozemska", "Makedonija", "Ukrajina", "Srbija", "Albanija", "Andora", "Armenija", "Azerbajdzan", "Belorusija",
                              "Gruzija", "Moldavija", "Monako", "Kosovo")),
                sliderInput("sta", "Starost", 18, 65, 30, ticks = TRUE),
                textInput("email1", "E-mail"),
                numericInput("teza1", "Teza v kg", value = NULL, min = 50, max = 150, step =  0.1),
                numericInput("hemo", "Hemoglobin", value = NULL, min = 100, max = 200, step =  0.01),
                dateInput("dat", "Datum vpisa v evidenco", format = "yyyy-mm-dd"),
                selectInput("skup", "Krvna skupina",
                            c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "0+", "0-")),
                checkboxInput("ze_doniral", "Oseba je v preteklosti kri ze donirala", FALSE),
                useShinyalert(),
                actionButton("submit", "Dodaj v bazo", class = "btn-primary")
              )
            )
          )
        )
      } 
      else {
        #zavihki ki jih vidi sestra
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
  #dodajanje tabel iz baze
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
  observe({
    # ali so obvezna polja izpolnjena
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  observeEvent(input$submit, {
    #pregleda če teža, hemoglobin in starost ustrezajo in vstavi podatke iz obrazca v tabele
    if (input$teza1 >= 50 && input$teza1 <= 150 && input$sta >= 18 && input$sta <= 65 && 
        input$hemo >= 100 && input$hemo <= 200) {
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
      shinyalert("OK!", "Donator dodan v sistem.", type = "success")
    } 
    #če niso ustrezni podatki vrne opozorilo
    if (is.na(input$teza1) | input$teza1 <= 50 | input$teza1 >= 150) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Neveljavna teza!",
                    content = "Teza mora biti med 50kg in 150kg!", style = "danger")
    }
    if (is.na(input$sta) | input$sta <= 18 | input$sta >= 65) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Neveljavna starost!",
                    content = "Darovalec mora biti star med 18 in 65 let!", style = "danger")
    }
    if (is.na(input$hemo) | input$hemo <= 100 | input$hemo >= 200) {
        createAlert(session, "alert", "myValueAlert", title = "Opozorilo: Neveljavna vsebnost hemoglobina!",
                    content = "Vsebnost hemoglobina mora biti med 100 in 200!", style = "danger")
    }
  })
}

runApp(list(ui = ui, server = server))