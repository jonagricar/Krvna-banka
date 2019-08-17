source("libraries.R")
source("tabele.R", encoding="UTF-8")
#source("projekt.R", encoding="UTF-8")


# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("VPIS", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Uporabniško ime", label = tagList(icon("user"), "Uporabniško ime")),
                   passwordInput("passwd", placeholder="Geslo", label = tagList(icon("unlock-alt"), "Geslo")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "VPIS", style = "color: white; background-color:#DF3232;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Napacno uporabniško ime ali geslo!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Uporabnik: sestra  Password: sestra"),
                     br(),
                     tags$code("Uporabnik: zdravnik  Password: zdravnik")
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
#      text =  "Teža mora biti med 50kg in 150kg"
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
          menuItem("Pacienti", tabName = "Pacienti", icon = icon("th")),
          menuItem("Donatorji", tabName = "Donatorji", icon = icon("th")),
          menuItem("Zaloga krvi", tabName = "Kri", icon = icon("th")),
          menuItem("Obrazec", tabName = "Obrazec", icon = icon("th"))
        )
      }
      else{
        sidebarMenu(
          menuItem("Glavna stran", tabName = "dashboard", icon = icon("dashboard")),
          menuItem("Pacienti", tabName = "Pacienti", icon = icon("th")),
          menuItem("Donatorji", tabName = "Donatorji", icon = icon("th")),
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
            tabName ="Pacienti",
            fluidRow(
              box(width = 12, title = "Podatki o pacientih", dataTableOutput('results_p'))
            )
          ),
          tabItem(
            tabName ="Donatorji",
            fluidRow(
              box(width = 12, title = "Podatki o donatorjih", dataTableOutput('results_d'))
            )
          ),
          tabItem(
            tabName ="Kri",
            h2("Coming soon :)")
          ),
          tabItem(
            tabName ="Obrazec",
            fluidPage(
              titlePanel("Obrazec za oddajo krvi"),
              div(
                id = "form",
                #useShinyFeedback(),
                textInput("ime", "Ime in priimek", ""),
                textInput("kraj", "Kraj"),
                selectInput("drzava", "Država",
                          c("",  "Avstrija", "Belgija", "Bosna in Hercegovina", "Danska", "Estonija", "Švica", "Francija", "Italija",
                            "Lihtenštajn", "Švedska", "Luksemburg", "Norveška","Hrvaška", "Nemcija", "Slovenija","Portugalska","Romunija",
                            "Združeno Kraljestvo","Crna Gora", "Turcija", "Avstralija", "Bolgarija", "Grcija", "Ciper", "Madžarska",
                            "Malta","Poljska", "Ceška", "Španija", "Latvija", "Litva", "Finska", "Irska", "Islandija", "Rusija", "Slovaška", 
                            "Nizozemska", "Makedonija", "Ukrajina", "Srbija", "Albanija", "Andora", "Armenija", "Azerbajdžan", "Belorusija",
                            "Gruzija", "Moldavija", "Monako", "Kosovo")),
                sliderInput("starost", "Starost", 18, 65, 30, ticks = TRUE),
                textInput("email", "E-mail"),
                numericInput("teza", "Teža v kg", value = NULL, min = 50, max = 150, step =  0.1),
                numericInput("hemo", "Hemoglobin", value = NULL, min = 100, max = 200, step =  0.01),
                dateInput("datum", "Datum vpisa v evidenco", format = "yyyy-mm-dd"),
                selectInput("skupina", "Krvna skupina",
                            c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "0+", "0-")),
                checkboxInput("ze_doniral", "Oseba je v preteklosti kri že donirala", FALSE),

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
            tabName ="Pacienti",
            fluidRow(
              box(width = 12, title = "Podatki o pacientih", dataTableOutput('results_p'))
            )
          ),
          tabItem(
            tabName ="Donatorji",
            fluidRow(
              box(width = 12, title = "Podatki o donatorjih", dataTableOutput('results_d'))
            )
          ),
          tabItem(
            tabName ="Kri",
            h2("Coming soon :)")
          )
        )
      }
      
    }
    else {
      loginpage
    }
  })
  output$results <-  DT::renderDataTable({
    datatable(oseba, options = list(autoWidth = TRUE,
                                   searching = TRUE))
  })
  output$results_p <-  DT::renderDataTable({
    datatable(prejemnik_z_lokacijo, options = list(autoWidth = TRUE,
                                    searching = TRUE))
  })
  output$results_d <-  DT::renderDataTable({
    datatable(donator, options = list(autoWidth = TRUE,
                                    searching = TRUE))
  })
}

runApp(list(ui = ui, server = server))