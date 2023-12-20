library(nflreadr)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(gt)
library(paletteer)
library(webshot)
library(ggthemes)
library(readr)
library(ggtext)
library(ggforce)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(na.tools)
library(stringr)
library(magick)
library(ggbeeswarm)
library(vip)
library(gtExtras)
library(nflfastR)
library(knitr)
library(nflplotR)
library(shiny)
library(shinythemes)
library(stringi)
library(reactable)
library(reactablefmtr)

pbp <- read_csv("https://raw.githubusercontent.com/arjunmenon10/NFLCoverages/main/nflcov.csv")
qbs <- unique(pbp$full_name)
defenses <- unique(pbp$defteam)
logos <- teams_colors_logos |> select(team_abbr, team_logo_espn)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),

  titlePanel("NFL Quarterback and Defensive Coverage Efficiency"),
  mainPanel(
    navbarPage("By: Arjun Menon & Alex Feazelle",
               tabPanel("Individual Coverage (QB)",
                        fluidRow(
                          column(4, align = 'center',
                                 selectInput("coveragechoose1", "Coverage", c("Cover 0", "Cover 1", "Cover 2", "Cover 2 Man",
                                                                              "Cover 3", "Cover 4", "Cover 6"), selected = "Cover 1")),
                          column(4, align = "center",
                                 numericInput("minDB", "Minimum Dropbacks", value = 15)),
                          column(4, align = "center",
                                 sliderInput("weekchoose1", "Weeks", value = c(1, 22), min = 1, max = 22)),
                          br(),
                          uiOutput("covtabletitle"),
                          reactableOutput("indtbl1"), width = 12
                        )),
               tabPanel("Individual QB vs Coverages",
                        fluidRow(
                          column(6, align = 'center',
                                 selectInput("qbchoose1", "Quarterback", c(sort(unique(as.character(qbs)))), selected = 'Patrick Mahomes')),
                          column(6, align = "center",
                                 sliderInput("weekchoose2", "Weeks", value = c(1, 22), min = 1, max = 22)),
                          br(),
                          tableOutput("qbtbl1"))),
               tabPanel("Individual Coverage (Defense)",
                        fluidRow(
                          column(6, align = 'center',
                                 selectInput("coveragechoose2", "Coverage", c("Cover 0", "Cover 1", "Cover 2", "Cover 2 Man",
                                                                              "Cover 3", "Cover 4", "Cover 6"), selected = "Cover 1")),
                          column(6, align = "center",
                                 sliderInput("weekchoose3", "Weeks", value = c(1, 22), min = 1, max = 22)),
                          br(),
                          uiOutput("covtabletitleD"),
                          reactableOutput("indtblD"), width = 12
                        )),
               tabPanel("Individual Defense Coverages",
                        fluidRow(
                          column(6, align = 'center',
                                 selectInput("defchoose1", "Defense", c(sort(unique(as.character(defenses)))), selected = 'CLE')),
                          column(4, align = "center",
                                 sliderInput("weekchoose4", "Weeks", value = c(1, 22), min = 1, max = 22)),
                          br(),
                          tableOutput("deftbl1")))
  )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$covtabletitle <- renderUI(tags$h1("Quarterback Metrics when facing ", input$coveragechoose1))
  

  output$indtbl1 <- renderReactable({
    
    covqbs <- pbp |> 
      filter(defense_coverage_type == input$coveragechoose1, week >= input$weekchoose1[1] & week <= input$weekchoose1[2]) |> 
      group_by(full_name, headshot_url) |> 
      summarise(
        EPAp = round(mean(epa, na.rm = T), 2),
        CPOE = round(mean(cpoe, na.rm = T), 3)/100,
        attempts = n(),
        completions = sum(complete_pass, na.rm = T),
        compperc = mean(complete_pass, na.rm = T),
        passyards = sum(receiving_yards, na.rm = T),
        YPA = round(mean(receiving_yards, na.rm = T), 2),
        YAC = sum(yards_after_catch, na.rm = T),
        ADOT = round(mean(ngs_air_yards, na.rm = T), 2),
        TTT = round(mean(time_to_throw, na.rm = T), 2),
        TDs = sum(pass_touchdown, na.rm = T),
        INTs = sum(interception, na.rm = T)
      ) |> filter(attempts >= input$minDB) |> 
      arrange(-EPAp)
    
    qbstotplays <- pbp |> 
      group_by(full_name) |> 
      summarise(totplays = n())
    
    covqbs <- left_join(covqbs, qbstotplays, by = 'full_name') |> 
      mutate(percsnaps  = attempts/totplays) |> select(-totplays) |> 
      select(full_name, headshot_url, percsnaps, everything())
    
    reactable(covqbs,
              compact = FALSE,
              pagination = FALSE,
              columns = list(
                full_name = colDef(name = "QB",
                                   minWidth = 90,
                                   align = "center", sticky = "left"),
                headshot_url = colDef(name = "",
                                        maxWidth = 35,
                                        cell = embed_img(),
                                        align = "center", sticky = 'left'),
                percsnaps = colDef(name = "% of DBs", maxWidth = 65, align = "center", format = colFormat(percent = TRUE, digits = 1)),
                EPAp = colDef(name = "EPA/Play", maxWidth = 85, align = "center",
                                   cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                CPOE = colDef(name = "CPOE", maxWidth = 75, align = "center", format = colFormat(percent = TRUE, digits = 1)),
                attempts = colDef(name = "Attempts", maxWidth = 80, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                completions = colDef(name = "Completions", maxWidth = 85, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                compperc = colDef(name = "Completion %", maxWidth = 85, align = "center", format = colFormat(percent = TRUE, digits = 1)),
                passyards = colDef(name = "Passing Yards", maxWidth = 75, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                YPA = colDef(name = "YPA", maxWidth = 50, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                YAC = colDef(name = "YAC", maxWidth = 50, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                ADOT = colDef(name = "ADOT", maxWidth = 60, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                TTT = colDef(name = "TTT", maxWidth = 50, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                TDs = colDef(name = "TD", maxWidth = 50, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                INTs = colDef(name = "INT", maxWidth = 50, align = "center",
                              cell = color_tiles(covqbs, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f")))
              ), fullWidth = TRUE)
    
    
    
  })
  
  output$qbtbl1 <- render_gt({
    
    qbdata <- pbp |> 
      filter(full_name == input$qbchoose1, week >= input$weekchoose2[1] & week <= input$weekchoose2[2]) |> 
      group_by(full_name, defense_coverage_type) |>
      summarise(
        ### add stats here
      )
    
    ### add GT Table here
    
    
  }, height = 600, width = 850)
  
  output$covtabletitleD <- renderUI(tags$h1("Defensive Metrics when playing ", input$coveragechoose2))
  
  
  output$indtblD <- renderReactable({
    
    covdef <- pbp |> 
      filter(defense_coverage_type == input$coveragechoose2, week >= input$weekchoose3[1] & week <= input$weekchoose3[2]) |> 
      group_by(defteam) |> 
      summarise(
        EPAp = round(mean(epa, na.rm = T), 2),
        CPOE = round(mean(cpoe, na.rm = T), 3)/100,
        attempts = n(),
        completions = sum(complete_pass, na.rm = T),
        compperc = mean(complete_pass, na.rm = T),
        passyards = sum(receiving_yards, na.rm = T),
        YPA = round(mean(receiving_yards, na.rm = T), 2),
        YAC = sum(yards_after_catch, na.rm = T),
        ADOT = round(mean(ngs_air_yards, na.rm = T), 2),
        TTT = round(mean(time_to_throw, na.rm = T), 2),
        TDs = sum(pass_touchdown, na.rm = T),
        INTs = sum(interception, na.rm = T)
      ) |>
      arrange(EPAp) |> left_join(logos, by = c('defteam' = 'team_abbr')) |> 
      select(defteam, team_logo_espn, everything())
    
    deftotplays <- pbp |> 
      group_by(defteam) |> 
      summarise(totplays = n())
    
    covdef <- left_join(covdef, deftotplays, by = 'defteam') |> 
      mutate(percsnaps  = attempts/totplays) |> select(-totplays) |> 
      select(defteam, team_logo_espn, percsnaps, everything())
    
    reactable(covdef,
              compact = FALSE,
              pagination = FALSE,
              columns = list(
                defteam = colDef(name = "Defense",
                                   minWidth = 60,
                                   align = "center", sticky = "left"),
                team_logo_espn = colDef(name = "",
                                      maxWidth = 35,
                                      cell = embed_img(),
                                      align = "center", sticky = 'left'),
                percsnaps = colDef(name = "% of Snaps", maxWidth = 65, align = "center", format = colFormat(percent = TRUE, digits = 1)),
                EPAp = colDef(name = "EPA/Play", maxWidth = 85, align = "center",
                              cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                CPOE = colDef(name = "CPOE", maxWidth = 75, align = "center", format = colFormat(percent = TRUE, digits = 1)),
                attempts = colDef(name = "Attempts", maxWidth = 80, align = "center",
                                  cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                completions = colDef(name = "Completions", maxWidth = 85, align = "center",
                                     cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                compperc = colDef(name = "Completion %", maxWidth = 85, align = "center", format = colFormat(percent = TRUE, digits = 1)),
                passyards = colDef(name = "Passing Yards", maxWidth = 75, align = "center",
                                   cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                YPA = colDef(name = "YPA", maxWidth = 50, align = "center",
                             cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                YAC = colDef(name = "YAC", maxWidth = 50, align = "center",
                             cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                ADOT = colDef(name = "ADOT", maxWidth = 60, align = "center",
                              cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                TTT = colDef(name = "TTT", maxWidth = 50, align = "center",
                             cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                TDs = colDef(name = "TD", maxWidth = 50, align = "center",
                             cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                INTs = colDef(name = "INT", maxWidth = 50, align = "center",
                              cell = color_tiles(covdef, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759")))
              ), fullWidth = TRUE)
    
    
    
  })
  
  output$defbl1 <- render_gt({
    
    qbdata <- pbp |> 
      filter(defteam == input$defchoose1, week >= input$weekchoose4[1] & week <= input$weekchoose4[2]) |> 
      group_by(defteam, defense_coverage_type) |>
      summarise(
        ### add stats here
      )
    
    ### add GT Table here
    
    
  }, height = 600, width = 850)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
