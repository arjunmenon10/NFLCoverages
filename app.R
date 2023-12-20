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

teamsdf <- teams_colors_logos |> 
  filter(!team_abbr %in% c('LAR', 'SD', 'OAK', 'STL'))
teams <- unique(teamsdf$team_abbr)
rm(teamsdf)

rosters <- load_rosters(2022:2023) |> 
  group_by(gsis_id) |> slice(1)

pbpoinitial23 <- read_csv("https://raw.githubusercontent.com/arjunmenon10/TheScout/main/shinypbp23.csv")
pbpoinitial <- pbpoinitial23
pbp4th <- read_csv("https://raw.githubusercontent.com/arjunmenon10/TheScout/main/pbp4th.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("The Scout"),
  
  mainPanel(
    navbarPage("By: Arjun Menon",
               tabPanel("Offense",
                        fluidRow(
                          column(4, align = "center",
                                 selectInput("offchoose1", "Offense", c(sort(unique(as.character(teams)))), selected = 'MIA')),
                          column(4, align = "center",
                                 sliderInput("sznchoose1", "Season", value = c(2023, 2023), min = 2023, max = 2023)),
                          column(4, align = "center",
                                 sliderInput("weekchoose1", "Weeks", value = c(1, 22), min = 1, max = 22)),
                          br(),
                          column(12, align = 'center',
                                 uiOutput("offenseheading"),
                                 tags$h4("Offensive Overview (all situations)")),
                          tableOutput("offstats"),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Down")),
                          tableOutput('runpassdown'),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Down & Distance")),
                          tableOutput('runpassdowndist'),
                          column(12, align = 'center', 
                                 tags$h4("Offensive Formation Efficiency")),
                          tableOutput('shotguneff'),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Offensive Formation")),
                          tableOutput('runpassshotgun'),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Offensive Formation & Down")),
                          tableOutput('runpassshotgundown'),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Offensive Formation & Down & Distance")),
                          tableOutput('runpassshotgundowndist'),
                          column(12, align = 'center', 
                                 tags$h4("Offensive Personnel Efficiency")),
                          tableOutput('perseff'),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Personnel")),
                          tableOutput('runpasspers'),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Personnel & Offensive Formation")),
                          tableOutput('runpasspersshotgun'),
                          column(12, align = 'center', 
                                 tags$h4("Run Pass Splits by Personnel & Down & Distance")),
                          tableOutput('runpasspersdowndist'),
                          column(12, align = 'center', 
                                 tags$h4("Offensive Efficiency vs Defenders in Box")),
                          tableOutput('offeffmib'),
                          column(12, align = 'center', 
                                 tags$h4("Offensive Efficiency vs # of DBs")),
                          tableOutput('offeffnDBs'),
                          column(12, align = 'center', 
                                 tags$h4("Offensive Efficiency vs Defensive Personnel")),
                          tableOutput('offeffdefpers'),
                          column(12, align = 'center', 
                                 tags$h4("Offensive Efficiency vs # of Pass Rushers")),
                          tableOutput('offeffpassrush'),
                          column(12, align = 'center', 
                                 tags$h4("Run Game Tendencies")),
                          plotOutput('rungame'),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          plotOutput('rungameEPA'),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          column(12, align = 'center', 
                                 tags$h4("Pass Game Tendencies")),
                          tableOutput('passtbl'),
                          column(12, align = 'center', 
                                 tags$h4("Player & Personnel Tendencies")),
                          tableOutput('playerpers'),
                          column(12, align = 'center', 
                                 tags$h4("4th Down Decision Making")),
                          plotOutput('fourthdown'),
                        )),
               tabPanel("Glossary",
                        fluidRow(
                          column(12, align = "left",
                                 tags$h5("EPA/Play: Assigning a point value for every play contextualized like factors such as down, distance, and yards to endzone"),
                                 tags$h5("Success Rate: Any play with an EPA value greater than 0"),
                                 tags$h5("PROE: Pass Rate over Expected"),
                                 tags$h5("Explosive Rate: Passes of 20+ yards or rushes of 10+ yards"),
                                 tags$h5("EDEPA: Early down EPA, so only looking at run and pass plays on 1st and 2nd down"),
                                 tags$h5("Run Locations: End = Outside Tackles, Tackle = Between Tackle & Guard, Guard = Between Guard & Center, 
                                           Middle = Up the Gut. Higher bars = better for offense, Lower bars = better for defense."),
                                 tags$h5("What rows are highlighted? Any tendency on offense where there's a run/pass % greater than 75% and a large enough 
                                           sample size for it to be significant enough. Or for some tables, when the run/pass % is at least 10% higher than league 
                                           average. As in, if you were to give this report to a coach, highlighting a row with 
                                           1 or 2 plays run and a 100% run rate won't be helpful for that coach. But highlighting a row where a team runs 90% of 
                                           the time out of 21 personnel and there's a 20 play sample size would be enough (I know 20 is small, but football has small 
                                           sample sizes, what can we do)."),
                                 tags$h5("On defense the top defensive tendency on a specific down or vs a specific offensive personnel/formation is 
                                           highlighted to stand out as the main thing to watch for."),
                                 br(),
                                 tags$h5("One important thing to note for pretty much all the tendency and efficiency charts: All data included there was with 
                                          plays outside of the low redzone (so using -99 to +13), non-garbage time situations (win probability between 5% & 95%), 
                                           and non-two minute situations. From my experience and the research I've done, this is how coaches prefer things and it 
                                           really creates more of a neutral situation to look at tendencies off of. This may be different than how other teams/people 
                                           would do it and that's totally fine, but for the sake of consistency and what I know best, these are the parameters I chose 
                                           to use."))
                        )),
               tabPanel("About",
                        fluidRow(
                          column(12, align = "left",
                                 tags$h5("This entire app is built off the data stemming from the nflreadr package.
                                           Everything here is free data accessible by anyone, nothing comes from a paid
                                           subscription. The goal of the app is ultimately to give people the ability to 
                                           look at things that may not be easily calculated, or would take time to manually chart.
                                           Additionally, I hope to increase people's curiousity of the sport, and how analytics can be 
                                           tied into the game of football in general. I believe this app can give people the ability 
                                           to understand the game better, and be able to read things in real time while the game is going on. 
                                           I also want to be able to help people, so all of my code will be open-sourced and viewable by anyone, 
                                           so if anyone wants to try and recreate these tables themselves, or even build a better version of 
                                           this app, the code and data are all there for the taking. Github: https://github.com/arjunmenon10/TheScout"),
                                 br(),
                                 tags$h5("I want to sincerely thank all of the people responsible for maintaining the nflreadr package, 
                                           as none of this would be possible without them. People like Ben Baldwin, Sebastian Carl, Tan Ho, John Edwards, 
                                           Lee Sharpe, and anyone else who's contributed to such a wonderful package. Without it, I likely wouldn't have been 
                                           afforded all the opportunities I've received while being a student at the University of Michigan."),
                                 br(),
                                 tags$h5("If anyone has any feedback, questions, or just wants to talk ball, you can find me @arjunmenon100 on Twitter, 
                                           and my email is arjunme@umich.edu. Feel free to reach out if you want, and I'll do my best to respond in a timely manner.")),
                          br(),
                          tags$h5("Link to Defensive Scout: https://arjunmenon100.shinyapps.io/TheScoutDefense/"),
                        )))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$offenseheading <- renderUI(tags$h1(input$offchoose1, " Offensive Scouting Report"))
  
  
  output$offstats <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2])
    
    pbpo <- pbpo |> 
      filter(pass == 1 | rush == 1) |> 
      group_by(posteam, defteam, week, drive) |> 
      mutate(startfield = first(yardline_100))
      
    
    stats <- pbpo |> 
      mutate(expl = ifelse(rush == 1 & yards_gained >= 10 | pass == 1 & yards_gained >= 20, 1, 0)) |> 
      filter(pass == 1 | rush == 1, posteam == input$offchoose1) |> 
      group_by(posteam) |> 
      summarise(
        `EPA/play` = mean(epa, na.rm = T),
        `Success Rate` = mean(success, na.rm = T),
        `EPA/Pass` = mean(epa[pass == 1], na.rm = T),
        `EPA/Rush` = mean(epa[rush == 1], na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100,
        `Early Down EPA/Play` = mean(epa[down %in% c(1, 2)], na.rm = T),
        `Explosive Rate` = mean(expl, na.rm = T),
        `Explosive Pass Rate (20+ yards)` = mean(expl[pass == 1], na.rm = T),
        `Explosive Rush Rate (10+ yards)` = mean(expl[rush == 1], na.rm = T)
      ) |> pivot_longer(!posteam, names_to = "Stat", values_to = "Number") |> 
      select(-posteam)
    
    fieldpos <- pbpo |> 
      group_by(posteam, defteam, week, drive) |> slice(1) |> 
      filter(posteam == input$offchoose1) |> 
      group_by(posteam) |> 
      summarise(
        `Average Starting Field Position` = mean(startfield, na.rm = T)
      ) |> pull(`Average Starting Field Position`)
    
    Stat = 'Average Starting Field Position'
    Number = fieldpos
    dfjoin = data.frame(Stat, Number)
    
    stats <- rbind(stats, dfjoin)
    
    rank = pbpo |> 
      mutate(expl = ifelse(rush == 1 & yards_gained >= 10 | pass == 1 & yards_gained >= 20, 1, 0)) |> 
      filter(pass == 1 | rush == 1) |> 
      group_by(posteam) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100,
        explosives = mean(expl, na.rm = T),
        explosivespass = mean(expl[pass == 1], na.rm = T),
        explosivesrush = mean(expl[rush == 1], na.rm = T),
        EDEPA = mean(epa[down %in% c(1, 2)], na.rm = T)
      ) |> ungroup() |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        EPApassrank = rank(-EPApass, ties.method = "first"),
        EPArushrank = rank(-EPArush, ties.method = "first"),
        PROErank = rank(-PROE, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first"),
        explosivepassrank = rank(-explosivespass, ties.method = 'first'),
        explosiverushrank = rank(-explosivesrush, ties.method = 'first'),
        EDEPArank = rank(-EDEPA, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      select(posteam, EPAplayrank, successrank, EPApassrank, EPArushrank, PROErank, EDEPArank, 
             explosivesrank, explosivepassrank, explosiverushrank) |>
      pivot_longer(!posteam, names_to = "Stat", values_to = "Rank") |> 
      select(-posteam, -Stat)
    
    fieldposranktm <- pbpo |> 
      group_by(posteam, defteam, week, drive) |> slice(1) |> 
      group_by(posteam) |> 
      summarise(
        `Average Starting Field Position` = mean(startfield, na.rm = T)
      ) |> ungroup() |> 
      mutate(fieldposrank = rank(`Average Starting Field Position`, ties.method = 'first')) |>
      filter(posteam == input$offchoose1) |> 
      pull(fieldposrank)
    
    
    rank <- rbind(rank, fieldposranktm)
    
    statsall <- cbind(stats, rank)
    
    statsall |> 
      gt() |> 
      fmt_number(columns = Number, rows = c(1, 3, 4, 6, 10), decimals = 2) |> 
      fmt_percent(columns = Number, rows = c(2, 5, 7, 8, 9), decimals = 2) |> 
      cols_label(
        Number = ""
      ) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      data_color( 
        columns = Rank,
        target_columns = Number,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))
    
  }, width = 900)
  
  output$runpassdown <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120)
    
    runpassdownT <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(down) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> filter(!is.na(down))
    
    runpassdownlg <- pbpo |> 
      filter(rush == 1 | pass == 1) |> 
      group_by(down) |> 
      summarise(
        `Run % Lg` = mean(rush, na.rm = T),
        `Pass % Lg` = mean(pass, na.rm = T),
        PROEL = mean(pass_oe, na.rm = T)/100
      ) |> filter(!is.na(down))
    
    allRP <- left_join(runpassdownT, runpassdownlg, by = 'down')
    
    allRP |> 
      select(down, Plays, `Run %`, `Run % Lg`, `Pass %`, `Pass % Lg`, PROE, PROEL) |> 
      mutate(dummy_var = ifelse(abs(`Run %` - `Run % Lg`) >= 0.075 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`Run %`:PROEL), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$runpassdowndist <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        `Down & Distance` = case_when(
          down == 1 & ydstogo == 10 ~ '1st & 10',
          down == 1 & ydstogo > 10 ~ '1st and 11+',
          down == 2 & ydstogo == 1 ~ '2nd & 1',
          down == 2 & ydstogo == 2 ~ '2nd & 2',
          down == 2 & ydstogo >= 3 & ydstogo <= 6 ~ '2nd & 3-6',
          down == 2 & ydstogo >= 7 & ydstogo <= 9 ~ '2nd & 7-9',
          down == 2 & ydstogo >= 10 ~ '2nd & 10+',
          down == 3 & ydstogo == 1 ~ '3rd & 1',
          down == 3 & ydstogo == 2 ~ '3rd & 2',
          down == 3 & ydstogo >= 3 & ydstogo <= 6 ~ '3rd & 3-6',
          down == 3 & ydstogo >= 7 & ydstogo <= 9 ~ '3rd & 7-9',
          down == 3 & ydstogo >= 10 ~ '3rd & 10+',
          TRUE ~ '4th Down'
        )
      )
    
    pbpo$`Down & Distance` <- factor(pbpo$`Down & Distance`, levels = c('1st & 10', '1st & 11+',
                                                                        '2nd & 1', '2nd & 2',
                                                                        '2nd & 3-6', '2nd & 7-9', '2nd & 10+',
                                                                        '3rd & 1', '3rd & 2', '3rd & 3-6',
                                                                        '3rd & 7-9', '3rd & 10+',
                                                                        '4th Down'))
    
    runpassdownT <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(`Down & Distance`) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> 
      filter(`Down & Distance` != "Other")
    
    
    runpassdownT |> 
      select(`Down & Distance`, Plays, `Run %`, `Pass %`,  PROE) |> 
      mutate(dummy_var = ifelse(abs(`Run %`) >= 0.75 & Plays >= 5 | abs(`Run %`) <= 0.25 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`Run %`:PROE), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$shotguneff <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        offense_formation = case_when(
          offense_formation %in% c("SINGLEBACK", "I_FORM", 'JUMBO') ~ 'UC',
          offense_formation %in% c('SHOTGUN') ~ 'Shotgun',
          offense_formation %in% c('EMPTY') ~ 'Empty',
          offense_formation %in% c('PISTOL') ~ 'Pistol',
          TRUE ~ 'Other'
        )
      ) |> filter(offense_formation != "Other")
    
    SGeff <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(offense_formation) |> 
      summarise(
        Plays = n(),
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100,
        explosives = mean(yards_gained >= 15, na.rm = T),
        EDEPA = mean(epa[down %in% c(1, 2)], na.rm = T)
      ) |> ungroup() |> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays)
    
    SGeffrank <- pbpo |> 
      filter(pass == 1 | rush == 1) |> 
      group_by(posteam, offense_formation) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100,
        explosives = mean(yards_gained >= 15, na.rm = T),
        EDEPA = mean(epa[down %in% c(1, 2)], na.rm = T)
      ) |> group_by(offense_formation) |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        EPApassrank = rank(-EPApass, ties.method = "first"),
        EPArushrank = rank(-EPArush, ties.method = "first"),
        PROErank = rank(-PROE, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first"),
        EDEPArank = rank(-EDEPA, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> ungroup() |> 
      select(offense_formation, EPAplayrank, successrank, EPApassrank, EPArushrank, PROErank, explosivesrank,
             EDEPArank)
    
    allSGeff <- left_join(SGeff, SGeffrank, by = 'offense_formation')
    
    
    allSGeff |> 
      select(offense_formation, Plays,  `% of Plays`, everything()) |> 
      gt() |> 
      fmt_percent(c(successr, PROE, explosives, `% of Plays`), decimals = 2) |> 
      fmt_number(c(EPAplay, EPApass, EPArush, EDEPA), decimals = 2) |> 
      gt_merge_stack(col1 = EPAplay, col2 = EPAplayrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = successr, col2 = successrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPApass, col2 = EPApassrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPArush, col2 = EPArushrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = PROE, col2 = PROErank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = explosives, col2 = explosivesrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EDEPA, col2 = EDEPArank, palette = c('black', 'blue')) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      cols_label(
        EPAplay = "EPA/Play",
        successr = "Success Rate",
        EPApass = "EPA/Pass",
        EPArush = "EPA/Rush",
        PROE = "PROE",
        explosives = "Explosive %",
        EDEPA = "EDEPA"
      ) |> 
      tab_source_note("Table: Arjun Menon | League rank in blue under value") |> 
      data_color( 
        columns = EPAplayrank,
        target_columns = EPAplay,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = successrank,
        target_columns = successr,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPApassrank,
        target_columns = EPApass,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPArushrank,
        target_columns = EPArush,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = PROErank,
        target_columns = PROE,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = explosivesrank,
        target_columns = explosives,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EDEPArank,
        target_columns = EDEPA,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) 
    
  }, width = 900)
  
  output$runpassshotgun <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        offense_formation = case_when(
          offense_formation %in% c("SINGLEBACK", "I_FORM", 'JUMBO') ~ 'UC',
          offense_formation %in% c('SHOTGUN') ~ 'Shotgun',
          offense_formation %in% c('EMPTY') ~ 'Empty',
          offense_formation %in% c('PISTOL') ~ 'Pistol',
          TRUE ~ 'Other'
        )
      ) |> filter(offense_formation != "Other")
    
    runpassSG <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(offense_formation) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> ungroup() |> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays)
    
    
    runpassSG |> 
      select(offense_formation, Plays,  `% of Plays`, `Run %`, `Pass %`,  PROE) |> 
      mutate(dummy_var = ifelse(abs(`Run %`) >= 0.75 & Plays >= 5 | abs(`Run %`) <= 0.25 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`% of Plays`:PROE), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$runpassshotgundown <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        shotgun = ifelse(shotgun == 1, "Shotgun", "Under Center")
      )
    
    runpassSGd <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(shotgun, down) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> group_by(down) |> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> filter(!is.na(down))
    
    
    runpassSGd |> 
      ungroup() |> 
      arrange(down) |> 
      select(down, shotgun, Plays,  `% of Plays`, `Run %`, `Pass %`,  PROE) |> 
      mutate(dummy_var = ifelse(abs(`Run %`) >= 0.75 & Plays >= 5 | abs(`Run %`) <= 0.25 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`% of Plays`:PROE), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$runpassshotgundowndist <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        offense_formation = case_when(
          offense_formation %in% c("SINGLEBACK", "I_FORM", 'JUMBO') ~ 'UC',
          offense_formation %in% c('SHOTGUN') ~ 'Shotgun',
          offense_formation %in% c('EMPTY') ~ 'Empty',
          offense_formation %in% c('PISTOL') ~ 'Pistol',
          TRUE ~ 'Other'
        )
      ) |> filter(offense_formation != "Other") |> 
      mutate(
        `Down & Distance` = case_when(
          down == 1 & ydstogo == 10 ~ '1st & 10',
          down == 1 & ydstogo > 10 ~ '1st and 11+',
          down == 2 & ydstogo == 1 ~ '2nd & 1',
          down == 2 & ydstogo == 2 ~ '2nd & 2',
          down == 2 & ydstogo >= 3 & ydstogo <= 6 ~ '2nd & 3-6',
          down == 2 & ydstogo >= 7 & ydstogo <= 9 ~ '2nd & 7-9',
          down == 2 & ydstogo >= 10 ~ '2nd & 10+',
          down == 3 & ydstogo == 1 ~ '3rd & 1',
          down == 3 & ydstogo == 2 ~ '3rd & 2',
          down == 3 & ydstogo >= 3 & ydstogo <= 6 ~ '3rd & 3-6',
          down == 3 & ydstogo >= 7 & ydstogo <= 9 ~ '3rd & 7-9',
          down == 3 & ydstogo >= 10 ~ '3rd & 10+',
          TRUE ~ '4th Down'
        )
      )
    
    pbpo$`Down & Distance` <- factor(pbpo$`Down & Distance`, levels = c('1st & 10', '1st & 11+',
                                                                        '2nd & 1', '2nd & 2',
                                                                        '2nd & 3-6', '2nd & 7-9', '2nd & 10+',
                                                                        '3rd & 1', '3rd & 2', '3rd & 3-6',
                                                                        '3rd & 7-9', '3rd & 10+',
                                                                        '4th Down'))
    
    runpassSGd <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(offense_formation, `Down & Distance`) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> group_by(`Down & Distance`) |> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> filter(!is.na(`Down & Distance`)) |> 
      filter(`% of Plays` >= 0.025)
    
    
    runpassSGd |> 
      ungroup() |> 
      arrange(`Down & Distance`) |> 
      select(`Down & Distance`, offense_formation, Plays,  `% of Plays`, `Run %`, `Pass %`,  PROE) |> 
      mutate(dummy_var = ifelse(abs(`Run %`) >= 0.75 & Plays >= 5 | abs(`Run %`) <= 0.25 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`% of Plays`:PROE), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$perseff <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        rb_count = as.numeric(stri_sub(gsub(" RB.*$", "", offense_personnel), -1)),
        wr_count = as.numeric(stri_sub(gsub(" WR.*$", "", offense_personnel), -1)),
        te_count = as.numeric(stri_sub(gsub(" TE.*$", "", offense_personnel), -1)),
        Personnel = paste0(rb_count, te_count),
        Personnel = case_when(
          str_detect(offense_personnel, "6 OL") ~ paste0(Personnel, "H"),
          TRUE ~ Personnel
        )
      )
    
    
    statspers <- pbpo |> 
      filter(pass == 1 | rush == 1, posteam == input$offchoose1) |> 
      group_by(posteam, Personnel) |> 
      summarise(
        Plays = n(),
        `EPA/Play` = mean(epa, na.rm = T),
        `Success Rate` = mean(success, na.rm = T),
        `EPA/Pass` = mean(epa[pass == 1], na.rm = T),
        `EPA/Rush` = mean(epa[rush == 1], na.rm = T),
        `Explosive Rate` = mean(yards_gained >= 15, na.rm = T)
      ) |>  group_by(posteam)|> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> ungroup() |> 
      select(-totplays, -posteam) |> filter(`% of Plays` >= 0.025)
    
    rankpers = pbpo |> 
      filter(pass == 1 | rush == 1) |> 
      group_by(posteam, Personnel) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> group_by(Personnel) |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        EPApassrank = rank(-EPApass, ties.method = "first"),
        EPArushrank = rank(-EPArush, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      select(Personnel, EPAplayrank, successrank, EPApassrank, EPArushrank, explosivesrank)
    
    persall <- left_join(statspers, rankpers, by = 'Personnel')
    
    
    persall |> 
      ungroup() |> 
      arrange(-`% of Plays`) |> 
      select(Personnel, Plays,  `% of Plays`, `EPA/Play`, `Success Rate`, `EPA/Pass`, `EPA/Rush`,
             `Explosive Rate`, everything()) |> 
      gt() |> 
      fmt_percent(c(`% of Plays`, `Success Rate`, `Explosive Rate`), decimals = 2) |> 
      fmt_number(c(`EPA/Play`, `EPA/Rush`, `EPA/Pass`), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_merge_stack(col1 = `EPA/Play`, col2 = EPAplayrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = `Success Rate`, col2 = successrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = `EPA/Pass`, col2 = EPApassrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 =  `EPA/Rush`, col2 = EPArushrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = `Explosive Rate`, col2 = explosivesrank, palette = c('black', 'blue')) |> 
      data_color( 
        columns = EPAplayrank,
        target_columns = `EPA/Play`,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = successrank,
        target_columns = `Success Rate`,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPApassrank,
        target_columns = `EPA/Pass`,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPArushrank,
        target_columns = `EPA/Rush`,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = explosivesrank,
        target_columns = `Explosive Rate`,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))
    
    
  }, width = 900)
  
  output$runpasspers <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        rb_count = as.numeric(stri_sub(gsub(" RB.*$", "", offense_personnel), -1)),
        wr_count = as.numeric(stri_sub(gsub(" WR.*$", "", offense_personnel), -1)),
        te_count = as.numeric(stri_sub(gsub(" TE.*$", "", offense_personnel), -1)),
        Personnel = paste0(rb_count, te_count),
        Personnel = case_when(
          str_detect(offense_personnel, "6 OL") ~ paste0(Personnel, "H"),
          TRUE ~ Personnel
        )
      )
    
    
    runpasspersonnel <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(Personnel) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> ungroup() |> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> filter(`% of Plays` >= 0.025)
    
    
    runpasspersonnel |> 
      ungroup() |> 
      arrange(-`% of Plays`) |> 
      select(Personnel, Plays,  `% of Plays`, `Run %`, `Pass %`,  PROE) |> 
      mutate(dummy_var = ifelse(abs(`Run %`) >= 0.75 & Plays >= 5 | abs(`Run %`) <= 0.25 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`% of Plays`:PROE), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$runpasspersshotgun <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        rb_count = as.numeric(stri_sub(gsub(" RB.*$", "", offense_personnel), -1)),
        wr_count = as.numeric(stri_sub(gsub(" WR.*$", "", offense_personnel), -1)),
        te_count = as.numeric(stri_sub(gsub(" TE.*$", "", offense_personnel), -1)),
        Personnel = paste0(rb_count, te_count),
        Personnel = case_when(
          str_detect(offense_personnel, "6 OL") ~ paste0(Personnel, "H"),
          TRUE ~ Personnel
        )
      )
    
    
    runpasspersonnelshotgun <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      mutate(
        offense_formation = case_when(
          offense_formation %in% c("SINGLEBACK", "I_FORM", 'JUMBO') ~ 'UC',
          offense_formation %in% c('SHOTGUN') ~ 'Shotgun',
          offense_formation %in% c('EMPTY') ~ 'Empty',
          offense_formation %in% c('PISTOL') ~ 'Pistol',
          TRUE ~ 'Other'
        )
      ) |> 
      group_by(Personnel, offense_formation) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> ungroup() |> group_by(Personnel) |> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> filter(`% of Plays` >= 0.025) |> ungroup()
    
    
    runpasspersonnelshotgun |> 
      ungroup() |> 
      arrange(Personnel, offense_formation) |> 
      select(Personnel, offense_formation, Plays,  `% of Plays`, `Run %`, `Pass %`,  PROE) |> 
      mutate(dummy_var = ifelse(abs(`Run %`) >= 0.75 & Plays >= 5 | abs(`Run %`) <= 0.25 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`% of Plays`:PROE), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$runpasspersdowndist <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120) |> 
      mutate(
        rb_count = as.numeric(stri_sub(gsub(" RB.*$", "", offense_personnel), -1)),
        wr_count = as.numeric(stri_sub(gsub(" WR.*$", "", offense_personnel), -1)),
        te_count = as.numeric(stri_sub(gsub(" TE.*$", "", offense_personnel), -1)),
        Personnel = paste0(rb_count, te_count),
        Personnel = case_when(
          str_detect(offense_personnel, "6 OL") ~ paste0(Personnel, "H"),
          TRUE ~ Personnel
        )
      ) |> 
      mutate(
        `Down & Distance` = case_when(
          down == 1 & ydstogo == 10 ~ '1st & 10',
          down == 1 & ydstogo > 10 ~ '1st and 11+',
          down == 2 & ydstogo == 1 ~ '2nd & 1',
          down == 2 & ydstogo == 2 ~ '2nd & 2',
          down == 2 & ydstogo >= 3 & ydstogo <= 6 ~ '2nd & 3-6',
          down == 2 & ydstogo >= 7 & ydstogo <= 9 ~ '2nd & 7-9',
          down == 2 & ydstogo >= 10 ~ '2nd & 10+',
          down == 3 & ydstogo == 1 ~ '3rd & 1',
          down == 3 & ydstogo == 2 ~ '3rd & 2',
          down == 3 & ydstogo >= 3 & ydstogo <= 6 ~ '3rd & 3-6',
          down == 3 & ydstogo >= 7 & ydstogo <= 9 ~ '3rd & 7-9',
          down == 3 & ydstogo >= 10 ~ '3rd & 10+',
          TRUE ~ '4th Down'
        )
      )
    
    pbpo$`Down & Distance` <- factor(pbpo$`Down & Distance`, levels = c('1st & 10', '1st & 11+',
                                                                        '2nd & 1', '2nd & 2',
                                                                        '2nd & 3-6', '2nd & 7-9', '2nd & 10+',
                                                                        '3rd & 1', '3rd & 2', '3rd & 3-6',
                                                                        '3rd & 7-9', '3rd & 10+',
                                                                        '4th Down'))
    
    
    runpasspersonneldowndist <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(Personnel, `Down & Distance`) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        PROE = mean(pass_oe, na.rm = T)/100
      ) |> ungroup() |> group_by(`Down & Distance`) |> 
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> filter(`% of Plays` >= 0.05) |> ungroup() |> 
      filter(!is.na(`Down & Distance`))
    
    
    runpasspersonneldowndist |> 
      ungroup() |> 
      arrange(`Down & Distance`, -`% of Plays`) |> 
      select(`Down & Distance`, Personnel,  Plays,  `% of Plays`, `Run %`, `Pass %`,  PROE) |> 
      mutate(dummy_var = ifelse(abs(`Run %`) >= 0.75 & Plays >= 5 | abs(`Run %`) <= 0.25 & Plays >= 5, T, F)) |> 
      gt() |> 
      fmt_percent(c(`% of Plays`:PROE), decimals = 2) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      tab_source_note("Table: Arjun Menon") %>%
      gt_highlight_rows(
        columns = c(`Run %`, `Pass %`),
        rows = (dummy_var == 'TRUE'),
        fill = "#1170aa"
      ) |> 
      cols_hide(columns = dummy_var)
    
  }, width = 900)
  
  output$offeffmib <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120)
    
    pbpo$defenders_in_box <- as.character(pbpo$defenders_in_box)
    
    pbpo <- pbpo |> 
      mutate(
        defenders_in_box = case_when(
          defenders_in_box <= 4 ~ '4 or less',
          defenders_in_box >= 9 ~ '9 or more',
          TRUE ~ defenders_in_box
        )
      )
    
    pbpo$defenders_in_box <- factor(pbpo$defenders_in_box, levels = c('4 or less', '5', '6',
                                                                      '7', '8', '9 or more'))
    
    
    offeffbox <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(defenders_in_box) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> ungroup() |>
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> 
      filter(!is.na(`defenders_in_box`))
    
    rankeff = pbpo |> 
      filter(pass == 1 | rush == 1) |> 
      group_by(posteam, defenders_in_box) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> group_by(defenders_in_box) |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        EPApassrank = rank(-EPApass, ties.method = "first"),
        EPArushrank = rank(-EPArush, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      select(defenders_in_box, EPAplayrank, successrank, EPApassrank, EPArushrank, explosivesrank)
    
    offeffall <- left_join(offeffbox, rankeff, by = 'defenders_in_box')
    
    
    offeffall |> 
      select(defenders_in_box, Plays,  `% of Plays`, `Run %`, `Pass %`, everything()) |> 
      gt() |> 
      fmt_percent(c(successr, explosives, `% of Plays`, `Run %`, `Pass %`), decimals = 2) |> 
      fmt_number(c(EPAplay, EPApass, EPArush), decimals = 2) |> 
      gt_merge_stack(col1 = EPAplay, col2 = EPAplayrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = successr, col2 = successrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPApass, col2 = EPApassrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPArush, col2 = EPArushrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = explosives, col2 = explosivesrank, palette = c('black', 'blue')) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      cols_label(
        defenders_in_box = "Defenders In Box",
        EPAplay = 'EPA/Play',
        successr = 'Success Rate',
        EPApass = 'EPA/Pass',
        EPArush = 'EPA/Rush',
        explosives = 'Explosive %'
      ) |> 
      tab_source_note("Table: Arjun Menon | League rank in blue under value") |> 
      data_color( 
        columns = EPAplayrank,
        target_columns = EPAplay,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = successrank,
        target_columns = successr,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPApassrank,
        target_columns = EPApass,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPArushrank,
        target_columns = EPArush,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = explosivesrank,
        target_columns = explosives,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))  
    
  }, width = 900)
  
  output$offeffnDBs <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120)
    
    pbpo <- pbpo |> 
      mutate(
        numDBs = case_when(
          grepl("4 DB", defense_personnel) ~ 'Base (4)',
          grepl("5 DB", defense_personnel) ~ 'Nickel (5)',
          grepl("6 DB", defense_personnel) ~ 'Dime (6)',
          grepl("7 DB", defense_personnel) ~ 'Quarter (7)',
          TRUE ~ 'Other'
        )
      ) |> filter(numDBs != "Other")
    
    pbpo$numDBs <- factor(pbpo$numDBs, levels = c('Base (4)', 'Nickel (5)',
                                                  'Dime (6)', 'Quarter (7)'))
    
    
    offeffbdbs <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(numDBs) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> ungroup() |>
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> 
      filter(!is.na(numDBs))
    
    rankeff = pbpo |> 
      filter(pass == 1 | rush == 1) |> 
      group_by(posteam, numDBs) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> group_by(numDBs) |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        EPApassrank = rank(-EPApass, ties.method = "first"),
        EPArushrank = rank(-EPArush, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      select(numDBs, EPAplayrank, successrank, EPApassrank, EPArushrank, explosivesrank)
    
    offeffall <- left_join(offeffbdbs, rankeff, by = 'numDBs')
    
    
    offeffall |> 
      select(numDBs, Plays,  `% of Plays`, `Run %`, `Pass %`, everything()) |> 
      gt() |> 
      fmt_percent(c(successr, explosives, `% of Plays`, `Run %`, `Pass %`), decimals = 2) |> 
      fmt_number(c(EPAplay, EPApass, EPArush), decimals = 2) |> 
      gt_merge_stack(col1 = EPAplay, col2 = EPAplayrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = successr, col2 = successrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPApass, col2 = EPApassrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPArush, col2 = EPArushrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = explosives, col2 = explosivesrank, palette = c('black', 'blue')) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      cols_label(
        numDBs = "# of DBs",
        EPAplay = 'EPA/Play',
        successr = 'Success Rate',
        EPApass = 'EPA/Pass',
        EPArush = 'EPA/Rush',
        explosives = 'Explosive %'
      ) |> 
      tab_source_note("Table: Arjun Menon | League rank in blue under value") |> 
      data_color( 
        columns = EPAplayrank,
        target_columns = EPAplay,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = successrank,
        target_columns = successr,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPApassrank,
        target_columns = EPApass,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPArushrank,
        target_columns = EPArush,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = explosivesrank,
        target_columns = explosives,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))  
    
  }, width = 900)
  
  output$offeffdefpers <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120)
    
    pbpo <- pbpo |> 
      filter(!str_detect(defense_personnel, "RB|WR|QB|TE|OL")) |> 
      mutate(
        defense_personnel = paste0(substr(defense_personnel, start = 1, stop = 1), "-",
                                   substr(defense_personnel, start = 7, stop = 7), "-",
                                   substr(defense_personnel, start = 13, stop = 13))
      )
    
    
    offeffbox <- pbpo |> 
      filter(posteam == input$offchoose1, rush == 1 | pass == 1) |> 
      group_by(defense_personnel) |> 
      summarise(
        Plays = n(),
        `Run %` = mean(rush, na.rm = T),
        `Pass %` = mean(pass, na.rm = T),
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> ungroup() |>
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays) |> 
      filter(!is.na(defense_personnel), `% of Plays` >= 0.025)
    
    rankeff = pbpo |> 
      filter(pass == 1 | rush == 1) |> 
      group_by(posteam, defense_personnel) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        EPApass = mean(epa[pass == 1], na.rm = T),
        EPArush = mean(epa[rush == 1], na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> group_by(defense_personnel) |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        EPApassrank = rank(-EPApass, ties.method = "first"),
        EPArushrank = rank(-EPArush, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      select(defense_personnel, EPAplayrank, successrank, EPApassrank, EPArushrank, explosivesrank)
    
    offeffall <- left_join(offeffbox, rankeff, by = 'defense_personnel')
    
    
    offeffall |> 
      select(defense_personnel, Plays,  `% of Plays`, `Run %`, `Pass %`, everything()) |> 
      gt() |> 
      fmt_percent(c(successr, explosives, `% of Plays`, `Run %`, `Pass %`), decimals = 2) |> 
      fmt_number(c(EPAplay, EPApass, EPArush), decimals = 2) |> 
      gt_merge_stack(col1 = EPAplay, col2 = EPAplayrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = successr, col2 = successrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPApass, col2 = EPApassrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = EPArush, col2 = EPArushrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = explosives, col2 = explosivesrank, palette = c('black', 'blue')) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      cols_label(
        defense_personnel = "Defensive Personnel",
        EPAplay = 'EPA/Play',
        successr = 'Success Rate',
        EPApass = 'EPA/Pass',
        EPArush = 'EPA/Rush',
        explosives = 'Explosive %'
      ) |> 
      tab_source_note("Table: Arjun Menon | League rank in blue under value") |> 
      data_color( 
        columns = EPAplayrank,
        target_columns = EPAplay,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = successrank,
        target_columns = successr,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPApassrank,
        target_columns = EPApass,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = EPArushrank,
        target_columns = EPArush,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = explosivesrank,
        target_columns = explosives,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))  
    
  }, width = 900)
  
  output$offeffpassrush <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120)
    
    
    offeffpassrush <- pbpo |> 
      filter(posteam == input$offchoose1, pass == 1, !is.na(number_of_pass_rushers)) |> 
      group_by(number_of_pass_rushers) |> 
      summarise(
        Plays = n(),
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> ungroup() |>
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Plays/totplays
      ) |> select(-totplays)
    
    offeffblitz <- pbpo |> 
      mutate(explosive = ifelse(yards_gained >= 15, 1, 0)) |> 
      filter(posteam == input$offchoose1, pass == 1, !is.na(number_of_pass_rushers)) |> 
      group_by(posteam) |> 
      summarise(
        Plays = sum(pass),
        Playsblitz = sum(pass[number_of_pass_rushers >= 5], na.rm = T),
        EPAplay = mean(epa[number_of_pass_rushers >= 5], na.rm = T),
        successr = mean(success[number_of_pass_rushers >= 5], na.rm = T),
        explosives = mean(explosive[number_of_pass_rushers >= 5], na.rm = T)
      ) |> ungroup() |>
      mutate(
        totplays = sum(Plays),
        `% of Plays` = Playsblitz/totplays
      ) |> select(posteam, Playsblitz, EPAplay, successr, explosives,  `% of Plays`) |> 
      rename(number_of_pass_rushers = 'posteam',
             Plays = 'Playsblitz') |> 
      mutate(number_of_pass_rushers = "Blitz (5+ rushers)")
    
    offeffpassrushall <- rbind(offeffpassrush, offeffblitz)
    
    rankeff = pbpo |> 
      filter(pass == 1, !is.na(number_of_pass_rushers)) |> 
      group_by(posteam, number_of_pass_rushers) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        explosives = mean(yards_gained >= 15, na.rm = T)
      ) |> group_by(number_of_pass_rushers) |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      select(number_of_pass_rushers, EPAplayrank, successrank, explosivesrank)
    
    rankblitz = pbpo |> 
      mutate(explosive = ifelse(yards_gained >= 15, 1, 0)) |> 
      filter(pass == 1, !is.na(number_of_pass_rushers)) |> 
      group_by(posteam) |> 
      summarise(
        EPAplay = mean(epa[number_of_pass_rushers >= 5], na.rm = T),
        successr = mean(success[number_of_pass_rushers >= 5], na.rm = T),
        explosives = mean(explosive[number_of_pass_rushers >= 5], na.rm = T)
      ) |> ungroup() |> 
      mutate(
        EPAplayrank = rank(-EPAplay, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        explosivesrank = rank(-explosives, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      select(posteam, EPAplayrank, successrank, explosivesrank) |> 
      rename(number_of_pass_rushers = 'posteam') |> 
      mutate(number_of_pass_rushers = "Blitz (5+ rushers)")
    
    rankeff$number_of_pass_rushers <- as.character(rankeff$number_of_pass_rushers)
    
    rankpassrush <- rbind(rankeff, rankblitz)
    
    offeffall <- left_join(offeffpassrushall, rankpassrush, by = 'number_of_pass_rushers')
    
    offeffall |> 
      select(number_of_pass_rushers, Plays,  `% of Plays`, everything()) |>
      arrange(number_of_pass_rushers) |> 
      gt() |> 
      fmt_percent(c(successr, explosives, `% of Plays`), decimals = 2) |> 
      fmt_number(c(EPAplay), decimals = 2) |> 
      gt_merge_stack(col1 = EPAplay, col2 = EPAplayrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = successr, col2 = successrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = explosives, col2 = explosivesrank, palette = c('black', 'blue')) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      cols_label(
        number_of_pass_rushers = "Pass Rushers",
        EPAplay = 'EPA/Play',
        successr = 'Success Rate',
        explosives = 'Explosive %'
      ) |> 
      tab_source_note("Table: Arjun Menon | League rank in blue under value") |> 
      data_color( 
        columns = EPAplayrank,
        target_columns = EPAplay,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = successrank,
        target_columns = successr,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = explosivesrank,
        target_columns = explosives,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))  
    
  }, width = 900)
  
  output$rungame <- renderPlot({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120, rush == 1)
    
    pbpo$run_gap[is.na(pbpo$run_gap)] <- "middle"
    
    pbpo <- pbpo |> 
      mutate(
        runspot = paste(run_location, run_gap),
        runspot = ifelse(runspot == "middle middle", "middle", runspot)
      )
    
    pbpo$runspot <- factor(pbpo$runspot, levels = c("left end", "left tackle", "left guard",
                                                    "middle", "right guard", "right tackle",
                                                    "right end"))
    
    rungametend <- pbpo |> 
      filter(posteam == input$offchoose1, !is.na(runspot)) |> 
      group_by(posteam, runspot) |> 
      summarise(
        rushes = sum(rush),
        successr = mean(success, na.rm = T)
      ) |> ungroup() |> 
      mutate(totrush = sum(rushes),
             percrush = round(rushes/totrush, 2))
    
    rungamelg <- pbpo |> 
      filter(!is.na(runspot)) |> 
      group_by(runspot) |> 
      summarise(
        rushes = sum(rush),
        successr = mean(success, na.rm = T)
      ) |> ungroup() |> 
      mutate(totrush = sum(rushes),
             percrush = round(rushes/totrush, 2)) |> 
      mutate(posteam = 'NFL') |> 
      select(posteam, runspot, everything())
    
    rungameall <- rbind(rungametend, rungamelg) |> 
      left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
    
    rungameall$team_color[is.na(rungameall$team_color)] <- "lightgray"
    rungameall$team_logo_espn[is.na(rungameall$team_logo_espn)] <- "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1200px-National_Football_League_logo.svg.png"
    
    rungameall |> 
      ungroup() |> 
      ggplot(aes(x = runspot, y = successr))+
      geom_bar(aes(group = posteam), width = 0.75, stat = 'identity', position = position_dodge(), fill = rungameall$team_color)+
      geom_image(aes(y = successr + 0.025, group = posteam), image = rungameall$team_logo_espn, asp = 16/9, size = 0.05,
                 position = position_dodge(width = 0.9))+
      geom_text(aes(group = posteam, y = successr/2, label = paste(rushes, "rushes", "(", paste0(percrush*100, "%"), ")"), angle = 90),
                position = position_dodge(width = 0.9), color = 'white', size = 8)+
      theme_bw()+
      theme(axis.title = element_text(size = 18)) + ylab('Rushing Success Rate') + xlab("Run Location")+
      theme(panel.grid.minor=element_blank())+
      theme(axis.text = element_text(size = 17))+
      theme(plot.title = element_markdown(size = 18, hjust = 0.5, face = "bold"),
            plot.subtitle = element_markdown(size = 17, hjust = 0.5),
            legend.position = 'none')+
      scale_y_continuous(breaks = scales::pretty_breaks(n=8), labels = scales::percent_format())
    
    
  }, height = 600, width = 850)
  
  output$rungameEPA <- renderPlot({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             wp >= 0.05 & wp <= 0.95, yardline_100 >= 13, half_seconds_remaining > 120, rush == 1)
    
    pbpo$run_gap[is.na(pbpo$run_gap)] <- "middle"
    
    pbpo <- pbpo |> 
      mutate(
        runspot = paste(run_location, run_gap),
        runspot = ifelse(runspot == "middle middle", "middle", runspot)
      )
    
    pbpo$runspot <- factor(pbpo$runspot, levels = c("left end", "left tackle", "left guard",
                                                    "middle", "right guard", "right tackle",
                                                    "right end"))
    
    rungametend <- pbpo |> 
      filter(posteam == input$offchoose1, !is.na(runspot)) |> 
      group_by(posteam, runspot) |> 
      summarise(
        rushes = sum(rush),
        EPA = mean(epa, na.rm = T)
      ) |> ungroup() |> 
      mutate(totrush = sum(rushes),
             percrush = round(rushes/totrush, 2))
    
    rungamelg <- pbpo |> 
      filter(!is.na(runspot)) |> 
      group_by(runspot) |> 
      summarise(
        rushes = sum(rush),
        EPA = mean(epa, na.rm = T)
      ) |> ungroup() |> 
      mutate(totrush = sum(rushes),
             percrush = round(rushes/totrush, 2)) |> 
      mutate(posteam = 'NFL') |> 
      select(posteam, runspot, everything())
    
    rungameall <- rbind(rungametend, rungamelg) |> 
      left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
    
    rungameall$team_color[is.na(rungameall$team_color)] <- "lightgray"
    rungameall$team_logo_espn[is.na(rungameall$team_logo_espn)] <- "https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/National_Football_League_logo.svg/1200px-National_Football_League_logo.svg.png"
    
    rungameall |> 
      ungroup() |> 
      ggplot(aes(x = runspot, y = EPA))+
      geom_bar(aes(group = posteam), width = 0.75, stat = 'identity', position = position_dodge(), fill = rungameall$team_color)+
      geom_image(aes(y = EPA + 0.01, group = posteam), image = rungameall$team_logo_espn, asp = 16/9, size = 0.05,
                 position = position_dodge(width = 0.9))+
      theme_bw()+
      theme(axis.title = element_text(size = 18)) + ylab('EPA/rush') + xlab("Run Location")+
      theme(panel.grid.minor=element_blank())+
      theme(axis.text = element_text(size = 17))+
      theme(plot.title = element_markdown(size = 18, hjust = 0.5, face = "bold"),
            plot.subtitle = element_markdown(size = 17, hjust = 0.5),
            legend.position = 'none')+
      scale_y_continuous(breaks = scales::pretty_breaks(n=8))
    
    
  }, height = 600, width = 850)
  
  output$passtbl <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             pass == 1)
    
    passlength <- pbpo |> 
      filter(!is.na(pass_location), posteam == input$offchoose1) |> 
      mutate(
        pass_length = case_when(
          air_yards < 0 ~ 'Behind LOS',
          air_yards <= 9 & air_yards >= 0 ~ 'Short',
          air_yards > 9 & air_yards <= 19 ~ 'Intermediate',
          air_yards > 19 ~ 'Deep'
        ),
        pass_length = as.factor(pass_length)
      ) |> 
      group_by(pass_location, pass_length) |> 
      summarise(
        EPAplay = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        CPOE = mean(cpoe, na.rm = T)/100,
        explosive = mean(yards_gained >= 15, na.rm = T),
        YAC = mean(yards_after_catch, na.rm = T),
        passes = n()
      ) |> ungroup() |> 
      mutate(totpasses = sum(passes),
             perc = passes/totpasses)
    
    lgpasslength <- pbpo |> 
      filter(!is.na(pass_location)) |> 
      mutate(
        pass_length = case_when(
          air_yards < 0 ~ 'Behind LOS',
          air_yards <= 9 & air_yards >= 0 ~ 'Short',
          air_yards > 9 & air_yards <= 19 ~ 'Intermediate',
          air_yards > 19 ~ 'Deep'
        ),
        pass_length = as.factor(pass_length)
      ) |> 
      group_by(pass_location, pass_length, posteam) |> 
      summarise(
        EPA = mean(epa, na.rm = T),
        successr = mean(success, na.rm = T),
        CPOE = mean(cpoe, na.rm = T),
        explosive = mean(yards_gained >= 15, na.rm = T),
        YAC = mean(yards_after_catch, na.rm = T),
        passes = n()
      ) |> 
      group_by(pass_location, pass_length) |> 
      mutate(
        EPArank = rank(-EPA, ties.method = "first"),
        successrank = rank(-successr, ties.method = "first"),
        CPOErank = rank(-CPOE, ties.method = "first"),
        explosiverank = rank(-explosive, ties.method = "first"),
        YACrank = rank(-YAC, ties.method = "first")
      ) |> filter(posteam == input$offchoose1) |> 
      ungroup() |> select(pass_location, pass_length, EPArank, successrank, CPOErank, explosiverank, YACrank)
    
    passlengthtbl <- left_join(passlength, lgpasslength, by = c('pass_location', 'pass_length'))
    
    passlengthtbl$pass_length <- factor(passlengthtbl$pass_length, levels = c('Behind LOS', 'Short', 'Intermediate', 'Deep'))
    
    passlengthtbl |>  
      arrange(pass_location, pass_length) |> 
      gt() |> 
      fmt_percent(c(successr, explosive, perc, CPOE), decimals = 2) |> 
      fmt_number(c(EPAplay, YAC), decimals = 2) |> 
      gt_merge_stack(col1 = EPAplay, col2 = EPArank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = successr, col2 = successrank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = explosive, col2 = explosiverank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = CPOE, col2 = CPOErank, palette = c('black', 'blue')) |> 
      gt_merge_stack(col1 = YAC, col2 = YACrank, palette = c('black', 'blue')) |> 
      cols_align("center") %>%
      opt_row_striping() %>%
      gt_theme_538() |> 
      cols_label(
        perc = "% of Plays",
        EPAplay = 'EPA/Play',
        successr = 'Success Rate',
        explosive = 'Explosive %',
        pass_location = "Pass Location",
        pass_length = "Pass Length"
      ) |> 
      tab_source_note("Table: Arjun Menon | League rank in blue under value") |> 
      data_color( 
        columns = EPArank,
        target_columns = EPAplay,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = successrank,
        target_columns = successr,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        )) |> 
      data_color( 
        columns = explosiverank,
        target_columns = explosive,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))  |> 
      data_color( 
        columns = CPOErank,
        target_columns = CPOE,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))  |> 
      data_color( 
        columns = YACrank,
        target_columns = YAC,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = c(1, 32) 
        ))  |> 
      data_color( 
        columns = perc,
        target_columns = perc,
        colors = scales::col_numeric( 
          palette = c("#4e9f50", "#87d180", "white", "#feb5a2", "#ed444a"),
          domain = NULL,
          reverse = TRUE
        ))  
    
  }, width = 900)
  
  
  output$playerpers <- render_gt({
    
    pbpo <- pbpoinitial
    
    pbpo <- pbpo |> 
      filter(posteam == input$offchoose1, week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2],
             pass == 1 | rush == 1, yardline_100 >= 13, half_seconds_remaining > 120)
    
    off_formation1 <- pbpo |> 
      mutate(snap_num = row_number()) |> 
      select(week, snap_num, posteam, defteam, gsis_id = offense_players, offense_personnel) |> 
      separate_rows(offense_personnel, sep = ",") |> 
      mutate(offense_personnel = str_squish(offense_personnel)) |> 
      separate(offense_personnel, into = c("x1", "x2"), sep = " ") |> 
      mutate(x1 = as.numeric(x1)) |> 
      pivot_wider(names_from = "x2", values_from = "x1") |> 
      janitor::clean_names()
    
    OPP <- unique(off_formation1$defteam)
    
    off_formation2 <- off_formation1 |> 
      separate_rows(gsis_id, sep = ";")
    
    # Get player/roster info
    r <- load_players() |> 
      select(short_name, gsis_id, position_group, position)
    
    off_formation3 <-  
      left_join(off_formation2, r, by = "gsis_id") |> 
      filter(position_group %in% c("RB", "TE", "WR")) |> 
      relocate(short_name, position_group, position, .after = gsis_id) |> 
      group_by(gsis_id) |> 
      mutate(player_snaps = n()) |> 
      ungroup() |> 
      mutate(position_group = factor(position_group, c("RB", "TE", "WR"))) |> 
      mutate(formation = paste(rb, te, sep = "-")) |> 
      arrange(position_group, player_snaps, desc(short_name)) |> 
      filter(formation %in% c('1-1', '1-2', '1-3', '2-2', '1-0', '2-1'))
    off_formation4 <- off_formation3 |> 
      group_by(formation, gsis_id) |> 
      mutate(formation_player_snaps = n()) |> 
      ungroup() |> 
      select(position_group, name = short_name, player_snaps, formation, formation_player_snaps) |> 
      distinct() |> 
      arrange(formation) |> 
      pivot_wider(names_from = formation, values_from = formation_player_snaps) |> 
      arrange(position_group, -player_snaps) |> 
      select(-player_snaps)
    
    formation_row <- off_formation3 |> 
      select(snap_num, formation) |> 
      distinct() |> 
      group_by(formation) |> 
      summarize(formation_snaps = n()) |> 
      mutate(name = input$offchoose1) |>
      mutate(position_group = "Team") |> 
      pivot_wider(names_from = formation, values_from = formation_snaps)
    
    off_formation <- bind_rows(formation_row, off_formation4) |> 
      mutate(position_group = factor(position_group, c("Team", "RB", "TE", "WR"))) |> 
      mutate(total = rowSums(across(where(is.numeric)), na.rm = T)) |> 
      filter(total > 1)
    # Table-------------------------------------------------------------------------
    
    formation_cols <- ncol(off_formation) - 1
    
    
    
    off_formation |> 
      filter(position_group != 'Team') |> 
      gt(groupname_col = "position_group") |> 
      gt_theme_espn() |> 
      sub_missing(columns = everything(), rows = everything(), missing_text = "-") |> 
      cols_label(name = "Name") |> 
      tab_spanner(
        label = "Personnel (RB-TE)",
        columns = (3:formation_cols)
      ) |> 
      tab_style(
        style = list(
          cell_text(weight = "bold", align = "center")
        ),
        locations = list(
          cells_title(groups = c("title", "subtitle"))
        )
      ) |>
      
      tab_style(
        style = list(
          cell_text(align = "left")
        ),
        locations = cells_column_labels(1)
      ) |>
      
      tab_source_note(source_note = "Table: @arjunmenon100 | Inspiration: @josephjefe") 
    
    
    
  }, width = 900)
  
  output$fourthdown <- renderPlot({
    
    pbpo <- pbp4th
    
    pbpo <- pbpo |> 
      filter(week >= input$weekchoose1[1] & week <= input$weekchoose1[2], season >= input$sznchoose1[1] & season <= input$sznchoose1[2])
    
    currentO <- pbpo %>%
      filter(go_boost > 1.5, !is.na(go_boost), !is.na(go)) %>%
      filter(vegas_wp > .05) %>%
      group_by(posteam, season) %>%
      summarize(go = mean(go)/100) |> 
      left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))
    
    team <- currentO |> 
      filter(posteam == input$offchoose1)
    
    currentO |> 
      ggplot(aes(x = reorder(posteam, go), y = go))+
      geom_bar(width = 0.75, stat = 'identity', fill = "grey", color = 'black', alpha = 0.5)+
      geom_bar(data = team, fill = team$team_color, stat = 'identity', width = 0.75)+
      theme_bw()+
      labs(title = "How often do teams go for it when they should",
           caption = 'By: Arjun Menon | @arjunmenon100 | Data: NFL4th',
           subtitle = glue::glue(""))+
      theme(axis.title = element_text(size = 18)) + xlab('Offense') + ylab("Go rate when they should")+
      theme(panel.grid.minor=element_blank())+
      theme(axis.text = element_text(size = 18))+
      theme(plot.title = element_markdown(size = 21, hjust = 0.5, face = "bold"),
            axis.text.x = element_nfl_logo(size = 1),
            plot.subtitle = element_markdown(size = 20, hjust = 0.5))+
      scale_y_continuous(breaks = scales::pretty_breaks(n=8), labels = scales::percent_format())
    
    
  }, height = 600, width = 850)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
