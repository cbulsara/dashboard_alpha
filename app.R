library(shiny)
library(shinydashboard)
library(shinythemes)
library(httr)
library(jsonlite)
library(dplyr)
library(rvest)
library(magrittr)
library(tidyverse)
library(stringr)
library(DT)

source("~/dashboard_alpha/sep.R")
source("~/dashboard_alpha/incidents.R")
source("~/dashboard_alpha/tasks.R")
source("~/dashboard_alpha/projects.R")
source("~/dashboard_alpha/siem_logsources.R")
source("~/dashboard_alpha/siem_offenses.R")
source("~/dashboard_alpha/certfeed.R")

ui <- dashboardPage(
  skin = 'green',
  dashboardHeader(title = "InfoSec Dashboard Alpha"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Shields Up",
        tabName = 'shieldsup',
        icon = icon("dashboard"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "Security Operations",
        tabName = 'secops',
        icon = icon("dashboard"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "Endpoint Protection",
        tabName = 'sep',
        icon = icon("th"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "SIEM",
        tabName = 'siem',
        icon = icon("truck"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "Projects",
        tabName = 'projects',
        icon = icon("th"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      )
    )
  ),
  dashboardBody(
    tabItems(
    
    #---------Shields Up
    tabItem(
      tabName = 'shieldsup', style = "background-color:#333c4c;",
        column(8, 
         fluidRow(height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>External Threat Level</font></b></h2>"),
             a(href = "http://isc.sans.org/",
               target = "_blank", uiOutput("infocon")),
             a(href = "http://www.symantec.com/security_response/threatcon/",
               target = "_blank", uiOutput("threatcon"))#,
             #a(href="http://webapp.iss.net/gtoc/",
             #target="_blank", uiOutput("alertcon"))
         ),
         fluidRow(height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>Perimeter KPIs</font></b></h2>"),
            uiOutput('twofaCoverage'),
            uiOutput('perimeterIDS'),
            uiOutput('perimeterIPS'),
            uiOutput('emailFirewall')
         ),
         fluidRow(height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>Site Protection KPIs</font></b></h2>"),
            uiOutput('ljIDSIPS'),
            uiOutput('greenIDSIPS'),
            uiOutput('encinitasIDS/IPS'),
            uiOutput('mercyIDSIPS'),
            uiOutput('chulaIDSIPS'),
            uiOutput('cpIDSIPS')
         ),
         fluidRow(height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>Endpoint Protection KPIs</font></b></h2>"),
            uiOutput('protectedPercent_1'),
            uiOutput('osCompliance_1'),
            renderSparkline(spkchr_Protected)
         ),
         fluidRow(height = 150, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>Incident Metrics</font></b></h2>"),
            box(plotOutput("incidentSummary", height = "150px"), width = 8)
         )
        ),
        column(4, style = "background-color:#333c4c;",
          fluidRow(height = 1150, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>Recent Threat Intel</font></b></h2>"),
            DT::dataTableOutput('certfeedDT')
          )
        )
    ),
    #----------Security Operations
    tabItem(
      tabName = 'secops',
      fluidRow(height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>KPIs</font></b></h2>"),
               column(6,
                      uiOutput('attcPhishingTask')),
               column(6,
                      uiOutput('attcMalwareTask'))),
      fluidRow(height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>Metrics</font></b></h2>"),
               box(plotOutput("secopsYTDSummary")),
               box(plotOutput("secopsYTDATTC")))
    ),
    
    #----------Endpoint Protection
    tabItem(
      tabName = 'sep',
      fluidRow(
        height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>KPIs</font></b></h2>"),
        uiOutput('protectedPercent_2'),
        uiOutput('osCompliance_2'),
        renderSparkline(spkchr_Protected)
      ),
      fluidRow(height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>Metrics</font></b></h2>"),
               box(plotOutput("sepProtected")),
               box(plotOutput("sepOS"))),
      fluidRow(height = 250, style = "background-color:#333c4c;",
               box(plotOutput("sepScanCompliant")),
               box(plotOutput(
                 "sepPatternCompliant"
               )))
    ),
    
    #----------SIEM
    tabItem(
      tabName = 'siem',
      fluidRow(
        height = 250, style = "background-color:#333c4c;", HTML("<h2><b><font color = 'white'>KPIs</font></b></h2>"),
        uiOutput("errorPercent"),
        uiOutput("offenseToSIRT"),
        uiOutput("logsourceEP")
      )
    ),
    
    #----------Projects
    tabItem(tabName = 'projects',
            fluidRow(
              column(
              6,
              tabsetPanel(
                id = 'tb',
                tabPanel('Projects', DT::dataTableOutput('tab_projects')),
                tabPanel('Risk Assessments', DT::dataTableOutput('tab_ra'))
              )
            )))
  ))
)

# Define server logic required to draw plots
server <- function(input, output) {
  output$sepProtected <- renderPlot({
    print(gg_Protected)
  })
  output$sepOS <- renderPlot({
    print(gg_OS)
  })
  output$sepScanCompliant <- renderPlot({
    print(gg_Scan_Compliant)
  })
  output$sepPatternCompliant <- renderPlot({
    print(gg_Pattern_Compliant)
  })
  output$incidentSummary <- renderPlot({
    print(gg_Summary)
  })
  output$infocon <- renderUI({
    sans_url <- "https://isc.sans.edu/infocon.txt"
    sans <-
      read_html("https://isc.sans.edu/infocon.txt") %>% html_node("body") %>% html_text()
    
    valueBox(
      value = toupper(sans),
      subtitle = "SANS Infocon",
      icon = icon("bullseye"),
      color = sans
    )
    
  })
  output$threatcon <- renderUI({
    pg <- read_html("http://www.symantec.com/security_response/#")
    pg %>%
      html_nodes("div.colContentThreatCon > a") %>%
      html_text() -> threatcon_text
    threatcon_text <- sapply(strsplit(threatcon_text, ":"), "[", 1)
    
    tcon_map <- c("green", "yellow", "orange", "red")
    names(tcon_map) <-
      c("Level 1", "Level 2", "Level 3", "Level 4")
    threatcon_color <-
      unname(tcon_map[gsub(":.*$", "", threatcon_text)])
    
    threatcon_text <- gsub("^.*:", "", threatcon_text)
    
    valueBox(
      value = threatcon_text,
      subtitle = "Symantec ThreatCon",
      icon = icon("tachometer"),
      color = threatcon_color
    )
    
  })
  
  output$alertcon <- renderUI({
    pg <- read_html("https://exchange.xforce.ibmcloud.com/")
    pg %>%
      html_nodes("#dashboard-header > div.alertcon > p > span") %>%
      html_text() %>%
      gsub(" -.*$", "", .) -> alertcon_text
    
    acon_map <- c("green", "blue", "yellow", "red")
    names(acon_map) <-
      c("AlertCon 1", "AlertCon 2", "AlertCon 3", "AlertCon 4")
    alertcon_color <- unname(acon_map[alertcon_text])
    
    valueBox(
      value = alertcon_text,
      subtitle = "IBM X-Force",
      icon = icon("warning"),
      color = alertcon_color
    )
    
  })
  output$tab_projects <- DT::renderDataTable({
    dt_projects
    #df_projects %>% filter(df_projects$type == 'Project')
  })
  
  output$tab_ra <- DT::renderDataTable({
    dt_ra
    #df_projects %>% filter(df_projects$type == 'Risk Assessment')
  })
  output$certfeedDT <- DT::renderDataTable({
    dt_CERT
    #df_projects %>% filter(df_projects$type == 'Risk Assessment')
  })
  output$errorPercent <- renderUI({
    valueBox(
      value = scales::percent(kpi_LogSourceErrorPercent),
      subtitle = 'Percentage of Log Sources in "Error" Status',
      icon = icon('tachometer'),
      color = kpi_LogSourceErrorPercentColor
    )
  })
  output$logsourceEP <- renderUI({
    valueBox(
      value = scales::percent(kpi_EPPercent),
      subtitle = 'Percentage of Events to EP',
      icon = icon('tachometer'),
      color = kpi_EPPercentColor
    )
  })
  output$offenseToSIRT <- renderUI({
    valueBox(
      value = paste(kpi_SIEMOffenseToSirt, "%"),
      subtitle = 'Percentage of Offenses that Result in SIRTs',
      icon = icon('tachometer'),
      color = kpi_SIEMOffenseToSirtColor
    )
  })
  output$attcPhishingTask <- renderUI({
    valueBox(
      value = paste(
        filter(tasks_Means, tasks_Means$type == 'phishing') %>% .$attch,
        " Hours"
      ),
      subtitle = 'Average IS Reaction Time to Phishing',
      icon = icon('tachometer'),
      color = filter(tasks_Means, tasks_Means$type == 'phishing') %>% .$color
    )
  })
  output$attcMalwareTask <- renderUI({
    valueBox(
      value = paste(
        filter(tasks_Means, tasks_Means$type == 'malware') %>% .$attch,
        " Hours"
      ),
      subtitle = 'Average IS Reaction Time to Malware',
      icon = icon('tachometer'),
      color = filter(tasks_Means, tasks_Means$type == 'malware') %>% .$color
    )
  })
  output$secopsYTDSummary <- renderPlot({
    print(gg_ytd_summary)
  })
  output$secopsYTDATTC <- renderPlot({
    print(gg_ytd_attc)
  })
  output$twofaCoverage <- renderUI({
    valueBox(
      width = 3,
      value = '20%',
      subtitle = '% Entry Points Protected by 2FA',
      icon = icon('warning'),
      color = 'black'
    )
  })
  output$protectedPercent_1 <- renderUI({
    infoBox(
      title = HTML(paste(
        '<b>% Endpoints', 'Protected</b>', sep = '<br/>'
      )),
      value = kpi_protected_percent,
      subtitle = spkchr_Protected,
      icon = icon('tachometer'),
      color = 'yellow',
      fill = TRUE,
      width = 4
    )
  })
  output$osCompliance_1 <- renderUI({
    infoBox(
      title = HTML(paste(
        '<b># Endpoints with', 'Non-Compliant OS</b>', sep = '<br/>'
      )),
      value = kpi_os_compliant,
      subtitle = spkchr_OS,
      icon = icon('tachometer'),
      color = 'red',
      fill = TRUE,
      width = 4
    )
  })
  output$protectedPercent_2 <- renderUI({
    infoBox(
      title = HTML(paste(
        '<b>% Endpoints', 'Protected</b>', sep = '<br/>'
      )),
      value = kpi_protected_percent,
      subtitle = spkchr_Protected,
      icon = icon('tachometer'),
      color = 'yellow',
      fill = TRUE,
      width = 2
    )
  })
  output$osCompliance_2 <- renderUI({
    infoBox(
      title = HTML(paste(
        '<b># Endpoints with', 'Non-Compliante OS</b>', sep = '<br/>'
      )),
      value = kpi_os_compliant,
      subtitle = spkchr_OS,
      icon = icon('tachometer'),
      color = 'red',
      fill = TRUE,
      width = 2
    )
  })
  output$perimeterIDS <- renderUI({
    valueBox(
      width = 3,
      value = '50%',
      subtitle = 'Perimeter IDS Effectiveness',
      icon = icon('warning'),
      color = 'red'
    )
  })
  output$perimeterIPS <- renderUI({
    valueBox(
      width = 3,
      value = '0%',
      subtitle = 'Perimeter IPS Effectiveness',
      icon = icon('warning'),
      color = 'black'
    )
  })
  output$emailFirewall <- renderUI({
    valueBox(
      width = 3,
      value = '70%',
      subtitle = 'E-mail Firewall Effectiveness',
      icon = icon('warning'),
      color = 'yellow'
    )
  })
  output$ljIDSIPS <- renderUI({
    kpi_percent <- scales::percent(
      df_KPIIDSIPS %>% filter(kpi_name == 'La Jolla') %>% .$kpi_value)
    
    kpi_color <- df_KPIIDSIPS %>% filter(kpi_name == 'La Jolla') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'La Jolla IDS/IPS Operational Effectiveness',
      icon = icon('warning'),
      color = kpi_color
    )
  })
  output$greenIDSIPS <- renderUI({
    kpi_percent <- scales::percent(
      df_KPIIDSIPS %>% filter(kpi_name == 'Green') %>% .$kpi_value)
    
    kpi_color <- df_KPIIDSIPS %>% filter(kpi_name == 'Green') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'Green IDS/IPS Operational Effectiveness',
      icon = icon('warning'),
      color = kpi_color
    )
  })
  output$mercyIDSIPS <- renderUI({
    kpi_percent <- scales::percent(
      df_KPIIDSIPS %>% filter(kpi_name == 'Mercy') %>% .$kpi_value)
    
    kpi_color <- df_KPIIDSIPS %>% filter(kpi_name == 'Mercy') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'Mercy IDS/IPS Operational Effectiveness',
      icon = icon('warning'),
      color = kpi_color
    )
  })
  output$chulaIDSIPS <- renderUI({
    kpi_percent <- scales::percent(
      df_KPIIDSIPS %>% filter(kpi_name == 'Chula Vista') %>% .$kpi_value)
    
    kpi_color <- df_KPIIDSIPS %>% filter(kpi_name == 'Chula Vista') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'Chula Vista IDS/IPS Operational Effectiveness',
      icon = icon('warning'),
      color = kpi_color
    )
  })
  output$encinitasIDSIPS <- renderUI({
    kpi_percent <- scales::percent(
      df_KPIIDSIPS %>% filter(kpi_name == 'Encinitas') %>% .$kpi_value)
    
    kpi_color <- df_KPIIDSIPS %>% filter(kpi_name == 'Encinitas') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'Encinitas IDS/IPS Operational Effectiveness',
      icon = icon('warning'),
      color = kpi_color
    )
  })
  output$cpIDSIPS <- renderUI({
    kpi_percent <- scales::percent(
      df_KPIIDSIPS %>% filter(kpi_name == 'Campus Point') %>% .$kpi_value)
    
    kpi_color <- df_KPIIDSIPS %>% filter(kpi_name == 'Campus Point') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'Campus Point IDS/IPS Operational Effectiveness',
      icon = icon('warning'),
      color = kpi_color
    )
  })
}


# Run the application
shinyApp(ui = ui, server = server)
