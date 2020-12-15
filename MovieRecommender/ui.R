## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(shinyalert)
source('functions/helpers.R')
source('functions/genere.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          dashboardSidebar(
          sidebarMenu(
            menuItem("Movie Genre Recommendation", tabName = "genere", icon = icon("dashboard")),
            menuItem("Movie Recomendation", icon = icon("th"), tabName = "recommend")
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
            tabItems(
              tabItem(tabName = "genere",
              fluidRow(
                  box(width = 12, title = "Step 1: Select a genre from below", status = "info", solidHeader = TRUE, collapsible = TRUE,
                      selectInput("genere","Genere:",choices= genre_list)
                  )
                ),
              fluidRow(
                  
                  box(
                    width = 12, status = "info", solidHeader = TRUE,
                    title = "Step 2: Discover movies you might like",
                    br(),
                    withBusyIndicatorUI(
                      actionButton("generebtn", "Click here to get genre recommendations", class = "btn-warning")
                    ),
                    br(),
                    tableOutput("genereresults")
                  )
               )
            ),
            tabItem(tabName = "recommend",
                      fluidRow(
                        box(width = 12, title = "Step 1: Rate movies you have watched", status = "info", solidHeader = TRUE, collapsible = TRUE,
                            div(class = "rateitems",
                                uiOutput('ratings')
                            )
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("results")
                        )
                      )
              )
            
          )
    )
  )
)