source("libraries.R")
source("auth_public.R", encoding="UTF-8")

original.locale <- Sys.getlocale(category="LC_CTYPE")
Sys.setlocale("LC_CTYPE", "Slovenian_Slovenia.1250") 


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("VPIS", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="UporabniÅ¡ko ime", label = tagList(icon("user"), "UporabniÅ¡ko ime")),
                   passwordInput("passwd", placeholder="Geslo", label = tagList(icon("unlock-alt"), "Geslo")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "VPIS", style = "color: white; background-color:#DF3232;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("NapaÄno uporabniÅ¡ko ime ali geslo!",
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
  #poveÄ¹Ä¾emo se z bazo
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
  #  observe({
  #    input$submit
  #    
  #    feedbackDanger(
  #      inputId = "teza",
  #      condition = nchar(input$teza) >= 50 & nchar(input$teza) <= 150,
  #      text =  "TeÄ¹Ä¾a mora biti med 50kg in 150kg"
  #    )
  #  })
  
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
          menuItem("Bolnišnice", tabName = "Bolnišnice", icon = icon("th")),
          menuItem("Pacienti", tabName = "Pacienti", icon = icon("th")),
          menuItem("Zaloga krvi", tabName = "Kri", icon = icon("th")),
          menuItem("Obrazec", tabName = "Obrazec", icon = icon("th"))
        )
      }
      else{
        sidebarMenu(
          menuItem("Glavna stran", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Bolnišnice", tabName = "Bolnišnice", icon = icon("th")),
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
            tabName ="Bolnišnice",
            fluidRow(
              box(width = 12, title = "Bolnišnice", dataTableOutput('results_b'))
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
                id = "form",
                #useShinyFeedback(),
                textInput("imepri", "Ime in priimek", ""),
                textInput("kraj1", "Kraj"),
                selectInput("drzava1", "Država",
                            c("",  "Avstrija", "Belgija", "Bosna in Hercegovina", "Danska", "Estonija", "Švica", "Francija", "Italija",
                              "Lihtenštajn", "Švedska", "Luksemburg", "Norveška","Hrvaška", "NemÄija", "Slovenija","Portugalska","Romunija",
                              "Združeno Kraljestvo","Èrna Gora", "Turèija", "Avstralija", "Bolgarija", "Grèija", "Ciper", "Madžarska",
                              "Malta","Poljska", "Èeška", "Španija", "Latvija", "Litva", "Finska", "Irska", "Islandija", "Rusija", "Slovaška", 
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
            tabName ="Bolnišnice",
            fluidRow(
              box(width = 12, title = "Bolnišnice", dataTableOutput('results_b'))
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
    # poskusili dobiti id z returning - ne deluje:
    #dodan_id <- dbGetQuery(conn, build_sql("INSERT INTO oseba (ime, kraj, drzava, starost, email, teza, krvna_skupina, datum_vpisa_v_evidenco)
    #                            VALUES (", input$imepri, ",", input$kraj1, ", ", input$drzava1, ",", input$sta, ",", input$email1, ",",
    #                                       input$teza1, ",", input$skup, ",", input$dat, ") RETURNING id;", con=conn))
    #dbSendQuery(conn, build_sql("INSERT INTO kri (hemoglobin, datum_prejetja, donator)
    #                            VALUES (", input$hemo, ",", input$dat, ",", dodan_id, ");", con=conn))
    dbSendQuery(conn, build_sql("INSERT INTO oseba (ime, kraj, drzava, starost, email, teza, krvna_skupina, datum_vpisa_v_evidenco)
                                VALUES (", input$imepri, ",", input$kraj1, ", ", input$drzava1, ",", input$sta, ",", input$email1, ",",
                                input$teza1, ",", input$skup, ",", input$dat, ");", con = conn))
    id_bolnica <- dbGetQuery(conn, build_sql("IF kraj =", input$kraj1,
                                               "SELECT id FROM bolnisnica
                                             ELSE
                                               SELECT id FROM bolnisnica
                                               WHERE drzava=", input$drzava1, "
                                               LIMIT 1);", con=conn))
                                
    dbSendQuery(conn, build_sql("INSERT INTO kri (hemoglobin, datum_prejetja, hrani)
                                VALUES (", input$hemo, ",", input$dat,",", id_bolnica,");", con=conn))
    shinyalert("OK!", "Donator uspešno dodan.", type = "success")
  })
}

runApp(list(ui = ui, server = server))