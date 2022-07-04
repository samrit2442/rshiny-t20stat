# library(cricketr)
library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(DT)
library(purrr)
library(RcppArmadillo)
library(shiny)
library(shinythemes)
library(shinydashboard)
# library(shinydashboardPlus)
library(fontawesome)
library(htmltools)
library(scales)

# t20 <- read.csv("t20_data_12_May_2022.csv", header = T) ### Reading file from external sources


####################### DATA PREPROSESSING ###########################################

# source("Creating_BBB_data.R")
t20_bkp <- t20  ## Backup of the original raw data

# colnames(t20) ## Showing the column names

t20[t20 == ""] <- NA  ## Filling the blank entries as NA
t20[is.na(t20)] <- 0  ## Filling the NAs as 0

# glimpse(t20) ## To see the variable structure

t20$over = ceiling(t20$ball) ## Creating a column for nth over number (n = 1 to 20)
t20$over_type = ifelse(t20$over >= 1 & t20$over <= 6, "Powerplay",
                       ifelse(t20$over >= 7 & t20$over <= 16, "Middle Over", "Death Over"))
t20$isDot = ifelse(t20$runs_off_bat == 0, 1, 0) ## No of dots
t20$isOne = ifelse(t20$runs_off_bat == 1, 1, 0) ## No of 1's
t20$isTwo = ifelse(t20$runs_off_bat == 2, 1, 0) ## No of 2's
t20$isThree = ifelse(t20$runs_off_bat == 3, 1, 0) ## No of 3's
t20$isFour = ifelse(t20$runs_off_bat == 4, 1, 0) ## No of 4's
t20$isSix = ifelse(t20$runs_off_bat == 6, 1, 0) ## No of 6's
t20$isOut = ifelse(t20$wicket_type != "0", 1, 0) ## No of out

t20$Runs = t20$runs_off_bat + t20$extras
tot_mat = t20 %>% select(match_id) %>% unique() %>% nrow()

players_name_list = unique(c(t20$striker, t20$non_striker, t20$bowler))

### For Batting Analysis

t20 <- t20[t20$innings == 1 | t20$innings == 2, ] # Removing Super-Over Balls

########################### ui ###############################################

header = dashboardHeader(title = "Performance Analysis in T20I", titleWidth = 450)
sidebar = dashboardSidebar(selectInput("player_name", "Select your Cricketer", choices = as.character(players_name_list)),
                           sidebarMenu(
                             menuItem("Batting Analysis", tabName = "bat", icon = icon("untappd", lib = "font-awesome")),
                             menuItem("Bowling Analysis", tabName = "bowl", icon = icon("basketball-ball", lib = "font-awesome")),
                             menuItem("About", tabName = "about", icon = icon("address-card", lib = "font-awesome"))
                           ))

body = dashboardBody(
        tabItems(
          tabItem(tabName = "bat",
          span(textOutput("name", inline = T), style="font-size: 45px; font-style: bold", uiOutput("flag", inline = T)),
          tags$br(),
                    column(12,
                            valueBoxOutput("value1", width = 2),
                            valueBoxOutput("value2", width = 2),
                            valueBoxOutput("value3", width = 2),
                            valueBoxOutput("value4", width = 2),
                            valueBoxOutput("value5", width = 2),
                            valueBoxOutput("value6", width = 2),
                            valueBoxOutput("value7", width = 2),
                            valueBoxOutput("value8", width = 2),
                            valueBoxOutput("value9", width = 2),
                            valueBoxOutput("value10", width = 2),
                            valueBoxOutput("value11", width = 2),
                            valueBoxOutput("value12", width = 2)),
                            
                            h3("Runs Distribution"),
                            
                            fluidRow(
                            box(title = "Histogram of Runs", solidHeader = T, collapsible = T, status = "primary",
                                plotlyOutput("hist")),

                            box(title = "Dismissal Type", solidHeader = T, collapsible = T, status = "primary",
                                plotlyOutput("donut"))),
          
                            fluidRow(
                            box(title = "Phase of Play", solidHeader = T, collapsible = T, status = "primary",
                                plotlyOutput("table1")))
          ),
          
            tabItem(tabName = "bowl", h1("Bowling Analysis"),tags$hr(),
                    column(12,
                           valueBoxOutput("value13", width = 2),
                           valueBoxOutput("value14", width = 2),
                           valueBoxOutput("value15", width = 2),
                           valueBoxOutput("value16", width = 2),
                           valueBoxOutput("value17", width = 2),
                           valueBoxOutput("value18", width = 2),
                           valueBoxOutput("value19", width = 2),
                           valueBoxOutput("value20", width = 2),
                           valueBoxOutput("value21", width = 2),
                           valueBoxOutput("value22", width = 2),
                           valueBoxOutput("value23", width = 2))
                           # valueBoxOutput("value24", width = 2))
                    
                    ),
          
            tabItem(tabName = "about", h2("About the R Shiny App"), 
                    h4(p(style = "text-align: justify; font-size = 14px",
                    "The R Shiny App", tags$b("T20performR"), "is intended to attract cricket enthusiasts who wish to accumulate
                       every bit of information about their favourite players. This is an open-source encyclopaedia of cricketers
                       those who are cricbees and have an analytical mindset. The work is designed to demonstrate the performance
                       analysis of T20I cricketers statistically, whereas exploratory data analysis and visualisation tools provide
                       the key insights of them. Not only it will capture the individual performances of the cricket stars at the
                       granular level just by selecting them from the dropdown menu but also it equips the comparative study of
                       multiple cricketers by some appropriate metrics. The summary statistics of batting and bowling of the
                       individuals are displayed separately on two pages. This dashboard is based on the ball-by-ball data from the
                       website", tags$a(href = "https://cricsheet.org/", "cricsheet"), "filtered by T20 International matches for 
                       men only. The dataset is updated in the backend on every final day of each month. The algorithm uses the 
                       necessary R libraries (packages) as well as the scratch codes to process the assimilated data and the required analysis.")),
                    h5(tags$em(tags$b(paste("*Last updated on Jun 19, 2022  23:59:59 IST (", tot_mat,
                               " matches covered)"))), align = "right"), ## Date Update
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    h4("Developed by", tags$b(tags$i("Samrit Pramanik")), align = "center"),
                    h6(tags$hr()),
                    h1(tags$a(href = "https://www.linkedin.com/in/samritpramanik24/", icon("linkedin")), 
                    tags$a(href = "mailto:samrit.2442@gmail.com", icon("envelope")),
                    tags$a(href = "https://www.facebook.com/samrit.pramanik24ps/" , icon("facebook-square")),
                    tags$a(href = "https://github.com/samrit2442", icon("github")),
                    tags$a(href = "https://www.instagram.com/dark_cosmos24/", icon("instagram")), 
                    tags$a(href = "https://api.whatsapp.com/send?phone=919038337857", icon("whatsapp")), # tags$style(HTML("color: green"))
                    tags$a(href = "https://t.me/darkcosmos24", icon("telegram")), 
                    tags$a(href = "https://twitter.com/Samrit2442", icon("twitter")), align = "center"),
                    tags$br(),
                    h1(icon("r-project"), style = "font-size: 100px",  align = "center"),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    # tags$blockquote("Shiny-Box is still under continuous development. Please look forward to future updates!"),
                    h5("Copyright", icon("copyright"), " 2022 ", tags$a(href = "https://shiny.rstudio.com/", "Shiny - RStudio"), ". All Rights Reserved."))
))

ui <- dashboardPage(header = header, sidebar = sidebar, body = body, skin = "black",  title = "Performance Analysis")    


####################################### server ################################

server <- function(session, input, output) {
    
  # updateSelectizeInput(session, , server = T)
  
    stat_react <- reactive({
        ## Computing Total Number of Dismissal
        
        diss <- t20 %>% dplyr::filter(player_dismissed == input$player_name) %>%
            dplyr::select(match_id,  wicket_type, over_type, bowler)
        
        # Removing Wide Balls
        
        plyr_data <- t20 %>% dplyr::filter(striker == input$player_name & wides == 0)
        
        ## Computing Total Innings played
        
        innings <- t20 %>% dplyr::filter(striker == input$player_name | non_striker == input$player_name) %>% 
            dplyr::summarise(Innings = n_distinct(match_id)) %>% as.numeric()
        
        ## Deducing Innings wise Statistics
        
        stat1 <- plyr_data %>% dplyr::filter(striker == input$player_name) %>% 
            dplyr::group_by(start_date, match_id) %>% 
            dplyr::summarise(Runs = sum(runs_off_bat), Balls = length(runs_off_bat),
                             SR = round(Runs/Balls*100,2), 
                             Fours = sum(isFour), Sixes = sum(isSix),
                             Dots = sum(isDot))
        
        stat2 <- left_join(stat1, diss) ## Innings wise Player's Data
        
        stat2$wicket_type <- ifelse(is.na(stat2$wicket_type), "not out", stat2$wicket_type)
        stat2$isThirty <- ifelse(stat2$Runs >= 30 & stat2$Runs < 50, 1, 0) # No of 30's
        stat2$isFifty <- ifelse(stat2$Runs >= 50 & stat2$Runs < 100, 1, 0) # No of 50's
        stat2$isHundred <- ifelse(stat2$Runs >= 100, 1, 0) # No of 100's
        stat2$isNO <- ifelse(stat2$wicket_type == "not out", 1, 0) ## Not Out Innings flag
        
        stat3 <- plyr_data %>% dplyr::filter(striker == input$player_name) %>%
            dplyr::group_by(match_id) %>% dplyr::select(match_id, innings, bowling_team, venue) %>% unique()
        
        stat4 <- left_join(stat2, stat3)   
        
        stat5 <- t20 %>% dplyr::group_by(match_id, innings) %>% 
            dplyr::summarise(team_runs = sum(Runs))
        
        stat6 <- left_join(stat4, stat5, by = c("match_id", "innings"))  
        
        stat6$contribution = round(stat6$Runs/stat6$team_runs*100, 2)
        stat6$Year = substr(stat6$start_date, 1, 4)
        
        stat6$bowler <- ifelse(stat6$wicket_type == "run out", NA, stat6$bowler)
        stat6     # Complete innings wise player data
        
    }) # Creating Innings wise dataset
    
    stat_react4 <- reactive({
      
      data1 <- t20 %>% dplyr::filter(striker == input$player_name & wides == 0)
      stat7 <- data1 %>% dplyr::group_by(over_type) %>% 
        dplyr::summarise(Runs = sum(runs_off_bat), Six = sum(isSix), Four = sum(isFour),
                         SR = round(sum(runs_off_bat)/length(runs_off_bat)*100,2))
      
      stat8 <- table(stat_react()$over_type) %>% t() %>% as.data.frame()
      stat8 <- stat8[,-1]
      colnames(stat8) = c("over_type", "Dismissed")
      table1 <- left_join(stat7, stat8) %>% dplyr::arrange(desc(over_type))
      
      
      tab1 <- plot_ly(
        type = 'table',
        columnorder = 1:6,
        columnwidth = rep(120,6),
        header = list(
          values = c("<b>Over Type</b>","<b>Runs</b>","<b>Sixes</b>","<b>Fours</b>","<b>SR</b>","<b>Dismissed</b>"),
          align = rep('center', ncol(table1)),
          line = list(width = 1.5, color = 'black'),
          fill = list(color = 'rgb(235, 100, 230)'),
          font = list(size = 16, color = "white"),
          height = 50
        ),
        cells = list(
          values = t(as.matrix(unname(table1))),
          align = rep('center', ncol(table1)),
          line = list(color = "black", width = 1.2),
          fill = list(color = 'rgba(228, 222, 249, 0.65)'),
          font = list(size = 14, color = "black"),
          height = 55
        ))
      
      tab1
    })
    
    
    # output$flag <- renderUI({
    #   tags$img(src = "india.png", width = 50, height = 50)
    # })
    
    output$name <- renderText({paste("Career Overview of", input$player_name)})
    
    output$value1 <- renderValueBox({
      valueBox(nrow(stat_react()), "Total Innings Played", color = "light-blue")
    }) # Total Innings Played
    
    output$value2 <- renderValueBox({
      valueBox(sum(stat_react()$Runs), "Total Runs Scored", color = "blue")
    }) # Total Runs Scored
    
    output$value3 <- renderValueBox({
      valueBox(sum(stat_react()$Balls), "Total Balls Faced", color = "teal")
    }) # Total Balls Faced
    
    output$value4 <- renderValueBox({
      valueBox(sum(stat_react()$isNO), "Not Outs", color = "green")
    }) # Not Outs
    
    output$value5 <- renderValueBox({
      valueBox(round(sum(stat_react()$Runs)/(nrow(stat_react())-sum(stat_react()$isNO)),2), "Batting Average", color = "navy")
    }) # Batting Average
    
    output$value6 <- renderValueBox({
      valueBox(round(mean(stat_react()$Runs),2), "Runs Per Innings", color = "black")
    }) # Runs Per Innings
    
    output$value7 <- renderValueBox({
      valueBox(round(sum(stat_react()$Runs)/(sum(stat_react()$Balls))*100,2), "Strike Rate", color = "red")
    }) # Strike Rate
    
    output$value8 <- renderValueBox({
      valueBox(paste0(sum(stat_react()$isThirty), "/",
                     sum(stat_react()$isFifty), "/",
                     sum(stat_react()$isHundred)), "30+ / 50s / 100s", color = "olive")
    }) # 30s/50s/100s
    
    output$value9 <- renderValueBox({
      HS = ""
      if(stat_react()[which.max(stat_react()$Runs), "isNO"] == 1)
        HS = paste0(max(stat_react()$Runs),"*","(", stat_react()[which.max(stat_react()$Runs), "Balls"],")")
      else
        HS = paste0(max(stat_react()$Runs),"(", stat_react()[which.max(stat_react()$Runs), "Balls"],")")
      valueBox(HS, "Highest Score", color = "purple")
    }) # Highest Score
    
    output$value10 <- renderValueBox({
      valueBox(paste0(sum(stat_react()$Fours), "/", sum(stat_react()$Sixes)), "Fours / Sixes", color = "maroon")
    }) # 4s/6s
    
    output$value11 <- renderValueBox({
      valueBox(paste0(round((sum(stat_react()$Fours) + sum(stat_react()$Sixes))/sum(stat_react()$Balls)*100,2),"%"), "Boundary Percentage", color = "orange")
    }) # Boundary %
    
    output$value12 <- renderValueBox({
      valueBox(paste0(round(sum(stat_react()$Dots)/sum(stat_react()$Balls)*100,2),"%"), "Dot Percentage", color = "fuchsia")
    }) # Dot %
    
    stat_react2 <- reactive({
      fig1 <- plot_ly(x = stat_react()$Runs, type = "histogram")
      fig1
    })
    
    output$hist <- renderPlotly({stat_react2()}) # Histogram of Runs
    
    stat_react3 <- reactive({
      
      stat9 = stat_react() %>% dplyr::filter(wicket_type != "not out")
      dd = data.frame(table(stat9$wicket_type))
      
      fig <- dd %>% plot_ly(labels = ~Var1, values = ~Freq) %>%
        add_pie(hole = 0.5) %>% 
        layout(showlegend = T,
               xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
               yaxis = list(showgrid = F, zeroline = F, showticklabels = F))
      fig
    })
    
    output$donut <- renderPlotly({stat_react3()}) # Donut Chart of Dismissal Types
    
    output$table1 <- renderPlotly({stat_react4()}) # Table-1
    
    stat_react_bowl1 <- reactive({
      
      wk_var = c("stumped", "caught", "hit wicket", "bowled", "caught and bowled", "lbw")
      
      t20$isBowler_wicket = ifelse(t20$wicket_type %in% wk_var,1,0)
      
      ## To find out Runs and Wickets at each innings
      
      bw_stat1 <- t20 %>% dplyr::filter(bowler == input$player_name & legbyes == 0 & byes == 0 & penalty == 0) %>% 
        dplyr::group_by(start_date, match_id) %>%
        dplyr::summarise(Runs = sum(Runs), Wickets = sum(isBowler_wicket))
      
      ## To find out Dot Balls at each innings
      
      bw_stat2 <- t20 %>% dplyr::filter(bowler == input$player_name) %>% 
        dplyr::mutate(isBowlDot = ifelse(runs_off_bat + wides + noballs == 0,1,0)) %>% 
        dplyr::group_by(match_id) %>% 
        dplyr::summarise(Dots = sum(isBowlDot))
      
      bw_stat <- left_join(bw_stat1, bw_stat2, by = "match_id")
      
      ## To find out total balls bowled in each innings
      
      bw_stat3 <- t20 %>% dplyr::filter(bowler == input$player_name & wides == 0 & noballs == 0) %>%
        dplyr::group_by(match_id) %>% 
        dplyr::summarise(Balls = length(runs_off_bat))
      
      bw_stat <- left_join(bw_stat, bw_stat3, by = "match_id")
      
      # https://youtu.be/UmDItbiDV6o (23:08 - 23:45) ### Highligths of Ban vs SL 18 Sep 2007
      
      bw_stat4 <- t20 %>% dplyr::filter(bowler == input$player_name) %>%
        dplyr::mutate(BowlRuns = runs_off_bat + wides + noballs) %>%
        dplyr::group_by(match_id, over) %>%
        dplyr::summarise(isMaiden = ifelse(sum(BowlRuns) == 0,1,0)) %>% 
        dplyr::group_by(match_id) %>% dplyr::summarise(Maiden = sum(isMaiden))
      
      bw_stat <- left_join(bw_stat, bw_stat4, by = "match_id")
      
      bw_stat5 <- t20 %>% dplyr::filter(bowler == input$player_name) %>%
        dplyr::group_by(match_id) %>% dplyr::select(match_id, innings, venue, batting_team) %>% unique()
      
      bw_stat <- left_join(bw_stat, bw_stat5, by = "match_id")
      
      bw_stat = bw_stat %>% dplyr::mutate(Econ = round(Runs/Balls*6,2),
                                          is4wkt = ifelse(Wickets == 4, 1, 0),
                                          is5wkt = ifelse(Wickets >= 5, 1, 0))
      bw_stat
    }) ### For Bowling Analysis
    
    output$value13 <- renderValueBox({
      valueBox(nrow(stat_react_bowl1()), "Total Innings Bowled", color = "light-blue")
    }) # Innings
    
    output$value14 <- renderValueBox({
      valueBox(sum(stat_react_bowl1()$Balls), "Total Balls Bowled", color = "aqua")
    }) # Balls
    
    output$value15 <- renderValueBox({
      valueBox(sum(stat_react_bowl1()$Runs), "Total Runs Conceded", color = "navy")
    }) # Runs
    
    output$value16 <- renderValueBox({
      valueBox(sum(stat_react_bowl1()$Wickets), "Total Wickets Taken", color = "red")
    }) # Wickets
    
    output$value17 <- renderValueBox({
      BBI = ""
      W = max(stat_react_bowl1()$Wickets)
      R = min(stat_react_bowl1()$Runs[which(stat_react_bowl1()$Wickets == W)])
      if(sum(stat_react_bowl1()$Wickets) == 0)
        BBI = paste0("-")
      else
        BBI = paste0(W,"/",R)
      valueBox(BBI, "Best Bowling Figure", color = "teal")
    }) # BBI
    
    output$value18 <- renderValueBox({
      valueBox(sum(stat_react_bowl1()$is4wkt), "4 Wickets Hauls", color = "olive")
    }) # 4Wkts
    
    output$value19 <- renderValueBox({
      valueBox(sum(stat_react_bowl1()$is5wkt), "5 Wickets Hauls", color = "orange")
    }) # 5Wkts
    
    output$value20 <- renderValueBox({
      valueBox(round(sum(stat_react_bowl1()$Runs)/sum(stat_react_bowl1()$Wickets),2), "Bowling Average", color = "lime")
    }) # Bowling Average
    
    output$value21 <- renderValueBox({
      valueBox(round(sum(stat_react_bowl1()$Runs)/sum(stat_react_bowl1()$Balls)*6,2), "Economy Rate", color = "maroon")
    }) # Economy Rate
    
    output$value22 <- renderValueBox({
      valueBox(round(sum(stat_react_bowl1()$Balls)/sum(stat_react_bowl1()$Wickets),2), "Bowling Strike Rate", color = "purple")
    }) # Bowling Strike Rate
    
    output$value23 <- renderValueBox({
      valueBox(paste0(round(sum(stat_react_bowl1()$Dots)/sum(stat_react_bowl1()$Balls)*100,2),"%"), "Dot Ball Percentage", color = "purple")
    }) # Dot%
}

# .rs.files.restoreBindings()

shinyApp(ui = ui, server = server)
