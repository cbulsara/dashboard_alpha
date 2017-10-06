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
source("~/dashboard_alpha/phishingsim.R")

ui <- dashboardPage(
  skin = 'blue',
  dashboardHeader(title = "Dashboard Alpha"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Shields Up",
        tabName = 'shieldsup',
        icon = icon("umbrella"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "Security Operations",
        tabName = 'secops',
        icon = icon("shield"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "Endpoint Protection",
        tabName = 'sep',
        icon = icon("laptop"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "SIEM",
        tabName = 'siem',
        icon = icon("eye"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "Projects",
        tabName = 'projects',
        icon = icon("wrench"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      ),
      menuItem(
        "Phishing Simulation",
        tabName = 'phishing',
        icon = icon("wrench"),
        badgeLabel = 'WIP',
        badgeColor = 'red'
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
    
    #---------Shields Up
    tabItem(
      tabName = 'shieldsup', 
        column(8,
         fluidRow(height = 250,  HTML("<h2><b><font color = 'white'>External Threat Level</font></b></h2>"),
             a(href = "http://isc.sans.org/",
               target = "_blank", uiOutput("infocon")),
             a(href = "http://www.symantec.com/security_response/threatcon/",
               target = "_blank", uiOutput("threatcon"))#,
             #a(href="http://webapp.iss.net/gtoc/",
             #target="_blank", uiOutput("alertcon"))
         ),
         fluidRow(height = 250,  HTML("<h2><b><font color = 'white'>Perimeter KPIs</font></b></h2>"),
            uiOutput('twofaCoverage'),
            uiOutput('perimeterIDSIPS'),
            uiOutput('emailFirewall')
         ),
         fluidRow(height = 250,  HTML("<h2><b><font color = 'white'>Site Protection KPIs</font></b></h2>"),
            uiOutput('ljIDSIPS'),
            uiOutput('greenIDSIPS'),
            uiOutput('encinitasIDS/IPS'),
            uiOutput('mercyIDSIPS'),
            uiOutput('chulaIDSIPS')
         ),
         fluidRow(height = 250,  HTML("<h2><b><font color = 'white'>Endpoint Protection KPIs</font></b></h2>"),
            uiOutput('protectedPercent_1'),
            uiOutput('osCompliance_1'),
            renderSparkline(spkchr_Protected)
         ),
         fluidRow(height = 150,  HTML("<h2><b><font color = 'white'>Incident Metrics</font></b></h2>"),
            box(plotOutput("incidentSummary", height = "150px"), width = 8)
         )
        ),
        column(4, align = 'left',
          fluidRow(height = 1150,  HTML("<h2><b><font color = 'white'>Recent Threat Intel</font></b></h2>"),
            DT::dataTableOutput('certfeedDT')
          )
        )
    ),
    #----------Security Operations
    tabItem(
      tabName = 'secops',
      fluidRow(height = 250,  HTML("<h2><b><font color = 'white'>KPIs</font></b></h2>"),
               column(6,
                      uiOutput('attcPhishingTask'),
                      uiOutput('attcMalwareTask')
                )
      ),
      fluidRow(height = 250,  HTML("<h2><b><font color = 'white'>Metrics</font></b></h2>"),
               column(8,
               box(plotOutput("secopsYTDSummary")),
               box(plotOutput("secopsYTDATTC"))
               )
      )
    ),
    
    #----------Endpoint Protection
    tabItem(
      tabName = 'sep',
      fluidRow(
        height = 250,  HTML("<h2><b><font color = 'white'>KPIs</font></b></h2>"),
        uiOutput('protectedPercent_2'),
        uiOutput('osCompliance_2'),
        renderSparkline(spkchr_Protected)
      ),
      fluidRow(height = 250,  HTML("<h2><b><font color = 'white'>Metrics</font></b></h2>"),
               box(plotOutput("sepProtected")),
               box(plotOutput("sepOS"))),
      fluidRow(height = 250, 
               box(plotOutput("sepScanCompliant")),
               box(plotOutput(
                 "sepPatternCompliant"
               )))
    ),
    
    #----------SIEM
    tabItem(
      tabName = 'siem',
      column(10, 
        fluidRow(
          height = 250,  HTML("<h2><b><font color = 'white'>KPIs</font></b></h2>"),
          uiOutput("errorPercent"),
          uiOutput("offenseToSIRT"),
          uiOutput("logsourceEP")
        ),
        fluidRow(
          height = 250,  HTML("<h2><b><font color = 'white'>Metrics</font></b></h2>"),
          column(5, 
            plotOutput("plotSIEMError")
          ),
          column(5, 
            plotOutput("plotSIEMEP")
          )
        )
      )
    ),
    
    #----------Projects
    tabItem(tabName = 'projects',
            fluidRow(
              column(
              12,
              box(tabsetPanel(
                id = 'tb',
                tabPanel('Projects', DT::dataTableOutput('tab_projects')),
                tabPanel('Risk Assessments', DT::dataTableOutput('tab_ra'))), width = 12
              )
            ))),
    tabItem(tabName = 'phishing',
            fluidRow(
              column(6, 
                     fluidRow(
                       height = 250,  HTML("<h2><b><font color = 'white'>KPIs</font></b></h2>"),
                       uiOutput("phishingCompliant")),
                     fluidRow(
                       height = 250,  HTML("<h2><b><font color = 'white'>Metrics</font></b></h2>"),
                       plotOutput("plotPhishingMetrics"))
                     )
              )
            )
    )
  )
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
      icon = icon("globe"),
      color = sans
    )
    
  })
  output$threatcon <- renderUI({
    pg <- read_html("http://www.symantec.com/security_response/threatcon")
    pg %>%
      html_nodes("div.bckSolidWht.bckPadLG > h3") %>%
      html_text() -> threatcon_text
    threatcon_text <- sapply(strsplit(threatcon_text, ": "), "[", 2)
  
    tcon_map <- c("green", "yellow", "orange", "red")
    names(tcon_map) <-
      c("Normal.", "Elevated.", "High.", "Extreme.")
    threatcon_color <-
      unname(tcon_map[gsub(":.*$", "", threatcon_text)])
    
    threatcon_text <- gsub("^.*:", "", threatcon_text)
    threatcon_text <- gsub("\\.", "", threatcon_text)
    
    valueBox(
      value = threatcon_text,
      subtitle = "Symantec ThreatCon",
      icon = icon("globe"),
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
    infoBox(
      title = HTML(paste(
        '<b>% Log Sources in', 'Error Status</b>', sep = '<br/>'
      )),
      value = scales::percent(kpi_LogSourceErrorPercent),
      subtitle = spkchr_SIEMError,
      icon = icon('tachometer'),
      color = 'red',
      fill = TRUE,
      width = 3
    )
  })
  output$logsourceEP <- renderUI({
    infoBox(
      title = HTML(paste(
        '<b>% Events to', 'Event Processor</b>', sep = '<br/>'
      )),
      value = scales::percent(kpi_EPPercent),
      subtitle = spkchr_SIEMEP,
      icon = icon('tachometer'),
      color = 'red',
      fill = TRUE,
      width = 3
    )
  })
  output$offenseToSIRT <- renderUI({
    valueBox(
      value = paste(kpi_SIEMOffenseToSirt, "%"),
      subtitle = 'Percentage of Offenses that Result in SIRTs',
      icon = icon('tachometer'),
      color = kpi_SIEMOffenseToSirtColor,
      width = 3
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
      value = scales::percent(kpi_protected_percent),
      subtitle = spkchr_Protected,
      icon = icon('tachometer'),
      color = kpi_protected_color,
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
      value = percent(kpi_protected_percent),
      subtitle = spkchr_Protected,
      icon = icon('tachometer'),
      color = kpi_protected_color,
      fill = TRUE,
      width = 2
    )
  })
  output$osCompliance_2 <- renderUI({
    infoBox(
      title = HTML(paste(
        '<b># Endpoints with', 'Non-Compliant OS</b>', sep = '<br/>'
      )),
      value = kpi_os_compliant,
      subtitle = spkchr_OS,
      icon = icon('tachometer'),
      color = 'red',
      fill = TRUE,
      width = 2
    )
  })
  output$perimeterIDSIPS <- renderUI({
    kpi_percent <- scales::percent(
      df_KPIIDSIPS %>% filter(kpi_name == 'Campus Point') %>% .$kpi_value)
    
    kpi_color <- df_KPIIDSIPS %>% filter(kpi_name == 'Campus Point') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'Perimeter IDS/IPS Operational Effectiveness',
      icon = icon('warning'),
      color = kpi_color
    )
  })
  output$emailFirewall <- renderUI({
    valueBox(
      width = 3,
      value = '85%',
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
  output$plotSIEMError <- renderPlot({
    print(gg_SIEMError)
  })
  output$plotSIEMEP <- renderPlot({
    print(gg_SIEMEP)
  })
  output$phishingCompliant <- renderUI({
    kpi_percent <- scales::percent(
      df_phishing_kpi_compliant %>% filter(compliant == 'TRUE') %>% .$p %>% round(2))

    
    kpi_color <-  df_phishing_kpi_compliant %>% filter(compliant == 'TRUE') %>% .$kpi_color
    
    valueBox(
      width = 2,
      value = kpi_percent,
      subtitle = 'Phishing Simulation User Compliance',
      icon = icon('warning'),
      color = kpi_color
    )
  })
  output$plotPhishingMetrics <- renderPlot({
    print(gg_phishing_metrics)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
