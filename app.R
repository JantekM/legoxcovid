library(shiny)
library(stringr)
library(lubridate)
library(dplyr, warn.conflicts = F)

jhu_cases <-
  as.data.frame(
    data.table::fread(
      "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    )
  )
jhu_cases = subset(jhu_cases, jhu_cases$`Country/Region` == "Poland")
polandCases = jhu_cases[5:length(jhu_cases)]

start_day <- "2020-4-3"

ui <- fluidPage(
  tags$head(includeHTML("GA_header.html")),
  titlePanel("Zestaw lego w oparciu o dzienną liczbę nowych przypadków COVID-19"),

  sidebarLayout(
    sidebarPanel(
      dateInput(
        "chosenDate",
        "Wybór dnia",
        value = NULL,
        min = start_day,
        max = Sys.Date(),
        language = "pl"
      ),
      actionButton(
        "previousDayButton",
        "",
        icon = icon("calendar-minus", lib = "font-awesome")
      ),
      actionButton("todayButton",
                   "Dzisiaj"),
      actionButton(
        "nextDayButton",
        "",
        icon = icon("calendar-plus", lib = "font-awesome")
      )
    ),


    mainPanel(
      uiOutput("setImg"),
      textOutput("setNum"),
      textOutput("setName"),
      textOutput("setYearReleased"),
      textOutput("setNumOfPieces")
    )
  )
)

server <- function(input, output, session) {
  timer <- reactiveTimer(1000 * 60 * 10) #10 minutes

  scrapeLegoData <- function(number) {
    legoPage = readLines(paste0('https://brickset.com/sets/', number))

    legoExists = grep("Sorry, we don't have a set with that number in our database.",
                      legoPage)
    legoExists <- ifelse(length(legoExists) > 0, F, T)
    if (legoExists)
    {
      imgLine = grep(
        '<img class="hidden" src="https://images.brickset.com/sets/images/',
        legoPage,
        fixed = T,
        value = T
      )
      pattern = '(?<=<img class=\"hidden" src=\").+(?=\"\\s/>)'
      imgUrl = str_extract(imgLine, pattern)

      #pattern = '(?<=<dd>).+(?=</dd>)'
      pattern = '(?<=>)[^<>]+(?=<)'

      lineNumber = grep("<dt>Set number</dt>", legoPage)
      lego.setNumber = str_extract(legoPage[lineNumber + 1], pattern)

      lineNumber = grep("<dt>Name</dt>", legoPage)
      lego.setName = str_extract(legoPage[lineNumber + 1], pattern)

      lineNumber = grep("<dt>Year released</dt>", legoPage)
      lego.setYearReleased = str_extract(legoPage[lineNumber + 1], pattern)

      lineNumber = grep("<dt>Pieces</dt>", legoPage)
      lego.setNumOfPieces = str_extract(legoPage[lineNumber + 1], pattern)

      return(
        list(
          imgUrl,
          lego.setNumber,
          lego.setName,
          lego.setYearReleased,
          lego.setNumOfPieces
        )
      )
    }
    else{
      print(paste("no lego found for", number))
      return(scrapeLegoData(2137))
    }

  }

  getTodayDate <- function() {
    if (hour(now()) > 12) {
      return(Sys.Date())
    } else{
      return(Sys.Date() - 1)
    }
  }

  dateToString <- function(date) {
    return(paste0(
      month(date),
      "/",
      day(date),
      "/",
      substr(as.character(year(date)), start = 3, stop = 4)
    ))
  }

  getCovidFromDate <- function(date) {
    newCases <-
      pull(polandCases, dateToString(date - 1)) - pull(polandCases, dateToString(date - 2))
  }

  legoData <-
    reactive({
      scrapeLegoData(getCovidFromDate(input$chosenDate))
    })


  observeEvent(input$todayButton, {
    updateDateInput(session,
                    "chosenDate",
                    value = Sys.Date())
  })

  observeEvent(input$previousDayButton, {
    if (input$chosenDate + 1 > start_day)
    {
      updateDateInput(session,
                      "chosenDate",
                      value = input$chosenDate - 1)
    }
  })

  observeEvent(input$nextDayButton, {
    if (input$chosenDate < Sys.Date())
    {
      updateDateInput(session,
                      "chosenDate",
                      value = input$chosenDate + 1)
    }
  })

  # output$setImg <- renderImage({
  #   # myurl <- legoData()[[1]]
  #   # z <- tempfile()
  #   # download.file(myurl,z,mode="wb")
  #   # imgFile <- readJPEG(z)
  #   # file.remove(z) # cleanup
  #   list(src = image_read(legoData()[[1]]), alt = 'Brak dostępnego zdjęcia')
  # }, deleteFile = T)

  output$setImg <- renderUI({
    tags$img(src = legoData()[[1]])
  })

  output$setNum <- renderText({
    legoData()[[2]]
  })
  output$setName <- renderText({
    legoData()[[3]]
  })
  output$setYearReleased <- renderText({
    legoData()[[4]]
  })
  output$setNumOfPieces <- renderText({
    legoData()[[5]]
  })
}

# Run the application
shinyApp(ui = ui, server = server)
