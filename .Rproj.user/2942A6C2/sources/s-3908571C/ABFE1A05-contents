#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DBI)
library(shiny)
library(highcharter)
library(dplyr)
library(dbplyr)
library(xts)
library(tidyverse)
library(shinydashboard)
library(shinyjs)
library(shinycustomloader)
library(shinyBS)
library(config)
library(readxl)
library(RODBC)

if_local <- Sys.getenv('SHINY_PORT')==""

if(if_local){
  Sys.setenv(R_CONFIG_ACTIVE = "local")
}
if(!if_local){
  Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
}

config <- config::get()
con <- DBI::dbConnect(odbc::odbc(), 
                      UID = config$username,
                      TDS_Version = config$TDS_version,
                      Port = config$Port,
                      Encrypt = config$encrypt,
                      TrustServerCertificate = config$TSC,
                      Driver=config$driver,
                      Server = config$server, Database = config$database,
                      Authentication = config$auth,
                      timeoutSecs = 30)


dashboard_ui_slider_date_end <- Sys.Date()
dashboard_ui_slider_date_start <- paste(Sys.Date()-7,"00:00:00",sep=" ")



# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "CCEI-RTED DashBoard",dropdownMenu()),
  dashboardSidebar(
    sidebarMenu( id = "rted_menu",
                 menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
                 menuItem("Prince Edward Island",tabName = "pei", icon = icon("chart-bar")),
                 menuItem("Nova Scotia",tabName = "NS", icon = icon("chart-bar")),
                 menuItem("New Brunswick",tabName = "NB", icon = icon("chart-bar")),
                 menuItem("Newfoundland & Labrador",tabName = "NFL", icon = icon("chart-bar")),
                 menuItem("Quebec",tabName = "QB", icon = icon("chart-bar")),
                 menuItem("Ontario",tabName = "ON", icon = icon("chart-bar")),
                 menuItem("Alberta",tabName = "AB", icon = icon("chart-bar")),
                 menuItem("British Coloumbia",tabName = "BC", icon = icon("chart-bar")),
                 menuItem("Downloads",tabName = "Dwn", icon = icon("download")),
                 menuItem("Data Dictionary",tabName = "DD", icon = icon("book"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$style(HTML("


.box.box-solid.box-success>.box-header {
  color:#fff;
  background:#26374A
                    }

.box.box-solid.box-success{
border-bottom-color:#26374A;
border-left-color:#26374A;
border-right-color:#26374A;
border-top-color:#26374A;
box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
transition: 0.3s;
}

.box.box-solid.box-success:hover{
box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);
}

.box.box-solid.box-warning>.box-header {
  color:#fff;
  background:#26374A
                    }

.box.box-solid.box-warning{
border-bottom-color:#ecf0f5;
border-left-color:#ecf0f5;
border-right-color:#ecf0f5;
border-top-color:#26374A;
}

.skin-blue .main-header .navbar, .skin-blue .main-header .logo{
color:#fff;
background:#26374A
}

#live_clock{
text-align: center;
}

#nb_filter{
width: 100px;
text-align: center;
}

#NB_1{
margin-top: 15px;
margin-bottom : -15px;
}


                                    ")),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 12,offset = 3,
                       box(
                         title = "Date & Time UTC", status = "success", solidHeader = TRUE,
                         collapsible = TRUE,
                         h3(textOutput("timer"),id = "live_clock")
                       ))
              ),
              fluidRow(
                box(
                  title = "Prince Edward Island",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("PEI_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_pei","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)
                ),
                box(
                  title = "Nova Scotia",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("NS_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_NS","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                box(
                  title = "New Brunswick",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("NB_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_NB","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
              ),
              fluidRow(
                box(
                  title = "Ontario",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("ON_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_ON","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                box(
                  title = "Alberta",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("AB_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_AB","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                box(
                  title = "British Coloumbia",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("BC_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_BC","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
              ),
              fluidRow(
                box(
                  title = "Newfoundland & Labrador",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("NFL_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_NFL","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
                box(
                  title = "Quebec",width = 4, status = "success", solidHeader = TRUE,
                  collapsible = TRUE,
                  column(
                    fluidRow(withLoader(highchartOutput("QB_load"), type = "html", loader = "loader3")),
                    fluidRow(bsButton("button_QB","For More Detailed Information, Click Here", icon = icon("chart-bar"), style = "primary", block = TRUE)),width = 12)),
              )),
      
      tabItem(tabName = "pei",
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "PEI Load", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_pei", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3,conditionalPanel(condition = "input.select_fr_pei != 1", sliderInput("pei_dates_1",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_pei == 1",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei == 6",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei == 3",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei == 4",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei == 5",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                           )
                         ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "PEI Fuel Type", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_pei_2", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_pei_2 != 1",sliderInput("pei_dates_2",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_pei_2 == 1",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_WIND_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_2 == 6",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_WIND_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_2 == 3",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_WIND_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_2 == 4",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_WIND_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_2 == 5",
                                           fluidRow(withLoader(highchartOutput("PEI_ON_WIND_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "PEI Import & Export", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_pei_3", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_pei_3 != 1",sliderInput("pei_dates_3",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_pei_3 == 1",
                                           fluidRow(withLoader(highchartOutput("PEI_EXPORT_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_3 == 6",
                                           fluidRow(withLoader(highchartOutput("PEI_EXPORT_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_3 == 3",
                                           fluidRow(withLoader(highchartOutput("PEI_EXPORT_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_3 == 4",
                                           fluidRow(withLoader(highchartOutput("PEI_EXPORT_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_3 == 5",
                                           fluidRow(withLoader(highchartOutput("PEI_EXPORT_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "PEI Wind Percentage", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_pei_4", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_pei_4 != 1",sliderInput("pei_dates_4",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_pei_4 == 1",
                                           fluidRow(withLoader(highchartOutput("PEI_LOCAL_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_4 == 6",
                                           fluidRow(withLoader(highchartOutput("PEI_LOCAL_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_4 == 3",
                                           fluidRow(withLoader(highchartOutput("PEI_LOCAL_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_4 == 4",
                                           fluidRow(withLoader(highchartOutput("PEI_LOCAL_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_pei_4 == 5",
                                           fluidRow(withLoader(highchartOutput("PEI_LOCAL_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       )))
      ),
      tabItem(tabName = "NS",
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NS Load", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_ns", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns != 1",sliderInput("ns_dates_1",
                                                                                                                                     "Dates",
                                                                                                                                     min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                     max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                     value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                     timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_ns == 1",
                                           fluidRow(withLoader(highchartOutput("NS_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns == 6",
                                           fluidRow(withLoader(highchartOutput("NS_LOAD_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns == 3",
                                           fluidRow(withLoader(highchartOutput("NS_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns == 4",
                                           fluidRow(withLoader(highchartOutput("NS_LOAD_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns == 5",
                                           fluidRow(withLoader(highchartOutput("NS_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NS Export & Import - I", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_ns_1", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_1 != 1",sliderInput("ns_dates_2",
                                                                                                                                     "Dates",
                                                                                                                                     min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                     max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                     value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                     timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_ns_1 == 1",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_1_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_1 == 6",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_1_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_1 == 3",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_1_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_1 == 4",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_1_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_1 == 5",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_1_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NS Export & Import - II", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_ns_2", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_2 != 1",sliderInput("ns_dates_3",
                                                                                                                                     "Dates",
                                                                                                                                     min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                     max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                     value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                     timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_ns_2 == 1",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_2_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_2 == 6",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_2_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_2 == 3",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_2_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_2 == 4",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_2_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_2 == 5",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_2_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NS Export & Import - III", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_ns_3", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_3 != 1",sliderInput("ns_dates_4",
                                                                                                                                     "Dates",
                                                                                                                                     min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                     max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                     value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                     timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_ns_3 == 1",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_3_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_3 == 6",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_3_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_3 == 3",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_3_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_3 == 4",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_3_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_3 == 5",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_3_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NS Export & Import - IV", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_ns_4", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_ns_4 != 1",sliderInput("ns_dates_5",
                                                                                                                                     "Dates",
                                                                                                                                     min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                     max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                     value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                     timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_ns_4 == 1",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_4_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_4 == 6",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_4_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_4 == 3",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_4_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_4 == 4",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_4_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_ns_4 == 5",
                                           fluidRow(withLoader(highchartOutput("NS_EXP_4_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       )))
              ),
      tabItem(tabName = "NB",
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NB Load", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_nb", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb != 1",sliderInput("nb_dates_1",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_nb == 1",
                                           fluidRow(withLoader(highchartOutput("NB_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb == 6",
                                           fluidRow(withLoader(highchartOutput("NB_LOAD_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb == 3",
                                           fluidRow(withLoader(highchartOutput("NB_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb == 4",
                                           fluidRow(withLoader(highchartOutput("NB_LOAD_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb == 5",
                                           fluidRow(withLoader(highchartOutput("NB_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NB Demand", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_nb_2", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb_2 != 1",sliderInput("nb_dates_2",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_nb_2 == 1",
                                           fluidRow(withLoader(highchartOutput("NB_DEMAND_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_2 == 6",
                                           fluidRow(withLoader(highchartOutput("NB_DEMAND_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_2 == 3",
                                           fluidRow(withLoader(highchartOutput("NB_DEMAND_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_2 == 4",
                                           fluidRow(withLoader(highchartOutput("NB_DEMAND_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_2 == 5",
                                           fluidRow(withLoader(highchartOutput("NB_DEMAND_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NB 10 Min Reserve", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_nb_3", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb_3 != 1",sliderInput("nb_dates_3",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_nb_3 == 1",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_3 == 6",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_3 == 3",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_3 == 4",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_3 == 5",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NB 30 Min Reserve", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_nb_4", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nb_4 != 1",sliderInput("nb_dates_4",
                                                                              "Dates",
                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                              timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_nb_4 == 1",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE2_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_4 == 6",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE2_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_4 == 3",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE2_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_4 == 4",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE2_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nb_4 == 5",
                                           fluidRow(withLoader(highchartOutput("NB_RESERVE2_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       )))
              
      ),
      tabItem(tabName = "ON",
      fluidRow(
        column(width =10, offset = 2,
               box(
                 title = "ON Total Energy", width = 10, status = "warning", solidHeader = TRUE,
                 collapsible = TRUE,
                 fluidRow(column(width = 2, selectInput("select_fr_on1", h4("Select Frequency"), 
                                                        choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                          column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on1 != 1",sliderInput("on_dates_1",
                                                                                                                             "Dates",
                                                                                                                             min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                             max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                             value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                             timeFormat="%Y-%m-%d")))
                 ),
                 conditionalPanel( condition = "input.select_fr_on1 == 1",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_YEARLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on1 == 6",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_ALL"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on1 == 3",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_WEEKLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on1 == 4",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_DAILY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on1 == 5",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_ENERGY_HOURLY"),type = "html", loader = "loader3"))),
                 fluidRow(
                   column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                   column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                 )
               ))),
      fluidRow(
        column(width =10, offset = 2,
               box(
                 title = "ON Total Loss", width = 10, status = "warning", solidHeader = TRUE,
                 collapsible = TRUE,
                 fluidRow(column(width = 2, selectInput("select_fr_on2", h4("Select Frequency"), 
                                                        choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                          column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on2 != 1",sliderInput("on_dates_2",
                                                                                                                              "Dates",
                                                                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                              timeFormat="%Y-%m-%d")))
                 ),
                 conditionalPanel( condition = "input.select_fr_on2 == 1",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_YEARLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on2 == 6",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_ALL"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on2 == 3",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_WEEKLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on2 == 4",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_DAILY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on2 == 5",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOSS_HOURLY"),type = "html", loader = "loader3"))),
                 fluidRow(
                   column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                   column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                 )
               ))),
      fluidRow(
        column(width =10, offset = 2,
               box(
                 title = "ON Total Load", width = 10, status = "warning", solidHeader = TRUE,
                 collapsible = TRUE,
                 fluidRow(column(width = 2, selectInput("select_fr_on3", h4("Select Frequency"), 
                                                        choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                          column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on3 != 1",sliderInput("on_dates_3",
                                                                                                                              "Dates",
                                                                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                              timeFormat="%Y-%m-%d")))
                 ),
                 conditionalPanel( condition = "input.select_fr_on3 == 1",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on3 == 6",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_ALL"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on3 == 3",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on3 == 4",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_DAILY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on3 == 5",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                 fluidRow(
                   column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                   column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                 )
               ))),
      fluidRow(
        column(width =10, offset = 2,
               box(
                 title = "ON Demand", width = 10, status = "warning", solidHeader = TRUE,
                 collapsible = TRUE,
                 fluidRow(column(width = 2, selectInput("select_fr_on4", h4("Select Frequency"), 
                                                        choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                          column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_on4 != 1",sliderInput("on_dates_4",
                                                                                                                              "Dates",
                                                                                                                              min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                              max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                              value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                              timeFormat="%Y-%m-%d")))
                 ),
                 conditionalPanel( condition = "input.select_fr_on4 == 1",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_YEARLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on4 == 6",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_ALL"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on4 == 3",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_WEEKLY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on4 == 4",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_DAILY"),type = "html", loader = "loader3"))),
                 conditionalPanel( condition = "input.select_fr_on4 == 5",
                                   fluidRow(withLoader(highchartOutput("ON_TOT_DEMAND_HOURLY"),type = "html", loader = "loader3"))),
                 fluidRow(
                   column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                   column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                 )
               )))
      ),
      tabItem(tabName = "NFL",
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "NFL Load", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_nfl", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_nfl != 1",sliderInput("nfl_dates_1",
                                                                                                                                      "Dates",
                                                                                                                                      min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                      max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                      value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                      timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_nfl == 1",
                                           fluidRow(withLoader(highchartOutput("NFL_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nfl == 6",
                                           fluidRow(withLoader(highchartOutput("NFL_LOAD_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nfl == 3",
                                           fluidRow(withLoader(highchartOutput("NFL_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nfl == 4",
                                           fluidRow(withLoader(highchartOutput("NFL_LOAD_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_nfl == 5",
                                           fluidRow(withLoader(highchartOutput("NFL_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       )))
              ),
      tabItem(tabName = "QB",h2("hello world")),
      tabItem(tabName = "AB",h2("hello world")),
      tabItem(tabName = "BC",
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "BC Balancing_Authority_Load (BAL)", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_bc_1", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_bc_1 != 1",sliderInput("bc_dates_1",
                                                                                                                                      "Dates",
                                                                                                                                      min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                      max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                      value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                      timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_bc_1 == 1",
                                           fluidRow(withLoader(highchartOutput("BC_LOAD_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_1 == 6",
                                           fluidRow(withLoader(highchartOutput("BC_LOAD_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_1 == 3",
                                           fluidRow(withLoader(highchartOutput("BC_LOAD_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_1 == 4",
                                           fluidRow(withLoader(highchartOutput("BC_LOAD_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_1 == 5",
                                           fluidRow(withLoader(highchartOutput("BC_LOAD_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "BC - US Exchange", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_bc_2", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_bc_2 != 1",sliderInput("bc_dates_2",
                                                                                                                                      "Dates",
                                                                                                                                      min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                      max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                      value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                      timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_bc_2 == 1",
                                           fluidRow(withLoader(highchartOutput("BC_US_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_2 == 6",
                                           fluidRow(withLoader(highchartOutput("BC_US_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_2 == 3",
                                           fluidRow(withLoader(highchartOutput("BC_US_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_2 == 4",
                                           fluidRow(withLoader(highchartOutput("BC_US_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_2 == 5",
                                           fluidRow(withLoader(highchartOutput("BC_US_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       ))),
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "BC - AB Exchange", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE,
                         fluidRow(column(width = 2, selectInput("select_fr_bc_3", h4("Select Frequency"), 
                                                                choices = list("Yearly" = 1,"Weekly" = 3, "Daily" = 4, "Hourly" = 5, "15 Min" = 6), selected = 6)),
                                  column(width = 4, offset = 5.3, conditionalPanel(condition = "input.select_fr_bc_3 != 1",sliderInput("bc_dates_3",
                                                                                                                                      "Dates",
                                                                                                                                      min = as.Date(dashboard_ui_slider_date_start,"%Y-%m-%d"),
                                                                                                                                      max = as.Date(dashboard_ui_slider_date_end,"%Y-%m-%d"),
                                                                                                                                      value=c(as.Date(dashboard_ui_slider_date_start),as.Date(dashboard_ui_slider_date_end)),
                                                                                                                                      timeFormat="%Y-%m-%d")))
                         ),
                         conditionalPanel( condition = "input.select_fr_bc_3 == 1",
                                           fluidRow(withLoader(highchartOutput("BC_AB_YEARLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_3 == 6",
                                           fluidRow(withLoader(highchartOutput("BC_AB_ALL"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_3 == 3",
                                           fluidRow(withLoader(highchartOutput("BC_AB_WEEKLY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_3 == 4",
                                           fluidRow(withLoader(highchartOutput("BC_AB_DAILY"),type = "html", loader = "loader3"))),
                         conditionalPanel( condition = "input.select_fr_bc_3 == 5",
                                           fluidRow(withLoader(highchartOutput("BC_AB_HOURLY"),type = "html", loader = "loader3"))),
                         fluidRow(
                           column(width = 10, helpText("Note: Selecting longer date range or frequency like daily, hourly can take longer time to render graphs.")),
                           column(width = 2, bsButton("button_pei_ind_1","Download Data", icon = icon("download"), style = "primary", block = TRUE))
                         )
                       )))
              ),
      tabItem(tabName = "DD",
              fluidRow(
                column(width =10, offset = 2,
                       box(
                         title = "Data Dictionary", width = 10, status = "warning", solidHeader = TRUE,
                         collapsible = TRUE, dataTableOutput("DATA_DICTIONARY_TABLE"))))
              ),
      tabItem(tabName = "Dwn",
              fluidRow(
                box(
                  title = "New Brunswick",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  fluidRow(
                  column(width = 6,
                         dateRangeInput("nb_date_range", h5("Date range")),
                         actionButton("nb_filter", "Apply")
                  ),
                  column(width = 4,
                         radioButtons("radio_nb", h3("Tables"),
                                      choices = list("None" = 0, "nb_realtime_alldata_new_brunswick_utc" = 1),selected = 0),
                         conditionalPanel( condition = "input.radio_nb == 1",
                                           fluidRow(id="nb_dwn_1",downloadButton("data_nb", "Download Data"))),
                         conditionalPanel( condition = "input.radio_nb == 0",
                                           helpText("Note: Please Select a File to Download"))
                  )),
                  fluidRow(id = "dwn1", column(bsAlert("NB_1"), width = 12)),
                  fluidRow(helpText("Note: Please apply filter before downloading data (Eg. for default dates press apply, for selected dates, select dates and press apply."))
                ),
                box(
                  title = "NovaScotia",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("ns_date_range", h5("Date range")),
                         actionButton("ns_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         fluidRow(radioButtons("radio_ns", h3("Tables"),
                                               choices = list("None" = 0,"ns_realtime_load_novascotia_utc" = 1,"ns_realtime_trade_novascotia_utc" = 2),selected = 0)),
                         conditionalPanel( condition = "input.radio_ns == 1",
                                           fluidRow(downloadButton("data_ns_1", "Download Data"))),
                         conditionalPanel( condition = "input.radio_ns == 0",
                                           fluidRow(helpText("Note: Please Select a File to Download"))),
                         conditionalPanel( condition = "input.radio_ns == 2",
                                           fluidRow(downloadButton("data_ns_2", "Download Data")))
                  )
                )),
              fluidRow(
                box(
                  title = "Alberta",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("ab_date_range", h5("Date range")),
                         actionButton("ab_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         radioButtons("radio_ab", h3("Tables"),
                                      choices = list("None" = 0,"nb_realtime_alldata_new_brunswick_utc" = 1),selected = 0),
                         conditionalPanel( condition = "input.radio_ab == 1",
                                           downloadButton("data_ab", "Download Data")),
                         conditionalPanel( condition = "input.radio_ab == 0",
                                           helpText("Note: Please Select a File to Download"))
                  )
                  
                ),
                box(
                  title = "British Coloumbia",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("bc_date_range", h5("Date range")),
                         actionButton("bc_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         fluidRow(radioButtons("radio_bc", h3("Tables"),
                                               choices = list("None" = 0,"ns_realtime_load_novascotia_utc" = 1,"ns_realtime_trade_novascotia_utc" = 2),selected = 0)),
                         conditionalPanel( condition = "input.radio_bc == 1",
                                           fluidRow(downloadButton("data_bc_1", "Download Data"))),
                         conditionalPanel( condition = "input.radio_bc == 0",
                                           fluidRow(helpText("Note: Please Select a File to Download"))),
                         conditionalPanel( condition = "input.radio_bc == 2",
                                           fluidRow(downloadButton("data_bc_2", "Download Data")))
                  )
                )),
              fluidRow(
                box(
                  title = "Ontario",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("on_date_range", h5("Date range")),
                         actionButton("on_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         radioButtons("radio_on", h3("Tables"),
                                      choices = list("None" = 0,"nb_realtime_alldata_new_brunswick_utc" = 1),selected = 0),
                         conditionalPanel( condition = "input.radio_on == 1",
                                           downloadButton("data_on", "Download Data")),
                         conditionalPanel( condition = "input.radio_on == 0",
                                           helpText("Note: Please Select a File to Download"))
                  )
                  
                ),
                box(
                  title = "Prince Edward Island",status = "success", solidHeader = TRUE,collapsible = TRUE,
                  column(width = 6,
                         dateRangeInput("pei_date_range", h5("Date range")),
                         actionButton("pei_filter", "Apply")
                  ),
                  column(offset = 0.7,width = 6,
                         fluidRow(radioButtons("radio_pei", h3("Tables"),
                                               choices = list("None" = 0,"ns_realtime_load_novascotia_utc" = 1,"ns_realtime_trade_novascotia_utc" = 2),selected = 0)),
                         conditionalPanel( condition = "input.radio_pei == 1",
                                           fluidRow(downloadButton("data_pei_1", "Download Data"))),
                         conditionalPanel( condition = "input.radio_pei == 0",
                                           fluidRow(helpText("Note: Please Select a File to Download"))),
                         conditionalPanel( condition = "input.radio_pei == 2",
                                           fluidRow(downloadButton("data_pei_2", "Download Data")))
                  )
                )))
      
    ))  
)


#bussines logic

Previous_date <- as.Date(Sys.Date())-(5*365)
previous_time <- paste(Previous_date,"00:00:00",sep=" ")

Previous_date_1 <- as.Date(Sys.Date())-(1)
previous_time_1 <- paste(Previous_date_1,"00:00:00",sep=" ")



abload_data <- tbl(con, config$provinces$AB$table15) %>% arrange(Date_time_local) %>% collect()
abload_date <- as.Date(tail(abload_data$Date_time_local,1))
abload_subset <- subset(abload_data,subset = Date_time_local >= (abload_date - 1) & Date_time_local <= abload_date)
ab_load_ts_1 <-  xts(abload_subset$Calgary,abload_subset$Date_time_local)
ab_load_ts_2 <-  xts(abload_subset$Central,abload_subset$Date_time_local)
ab_load_ts_3 <-  xts(abload_subset$Edmonton,abload_subset$Date_time_local)
ab_load_ts_4 <-  xts(abload_subset$Northeast,abload_subset$Date_time_local)
ab_load_ts_5 <-  xts(abload_subset$Northwest,abload_subset$Date_time_local)
ab_load_ts_6 <-  xts(abload_subset$South,abload_subset$Date_time_local)

ab_load_chart <- highchart() %>% 
  hc_xAxis(type = "datetime") %>%
  hc_add_series(ab_load_ts_1, name="Calgary Load", type = "line")%>%
  hc_add_series(ab_load_ts_2, name="Central Load", type = "line",color = "red")%>%
  hc_add_series(ab_load_ts_3, name="Edmonton Load", type = "line",color = "lightgreen")%>%
  hc_add_series(ab_load_ts_4, name="Northeast Load", type = "line",color = "purple")%>%
  hc_add_series(ab_load_ts_5, name="Northwest Load", type = "line",color = "orange")%>%
  hc_add_series(ab_load_ts_6, name="South Load", type = "line",color = "brown")%>%
  hc_navigator(enabled = TRUE)

pei_ind_dat <- tbl(con, config$provinces$PEI$table1) %>% arrange(Date_time_local) %>% collect()
nb_ind_dat <- tbl(con, config$provinces$NB$table1) %>% arrange(Date_time_local) %>% collect()
ns_ind_dat <- tbl(con, config$provinces$NS$table1) %>% arrange(Date_time_local) %>% collect()
bc_ind_dat <- tbl(con, config$provinces$BC$table1) %>% arrange(Date_time_local) %>% collect()
ab_ind_dat <- tbl(con, config$provinces$AB$table15) %>% arrange(Date_time_local) %>% collect()
on_ind_dat <- tbl(con, config$provinces$ONT$table1) %>% arrange(date_time_local) %>% collect()
bc_ind_dat_1 <- tbl(con, config$provinces$BC$table3) %>% arrange(date_time_local) %>% collect()
ns_ind_dat_1 <- tbl(con, config$provinces$NS$table2) %>% arrange(Date_time_local) %>% collect()
nfl_ind_dat <- tbl(con, config$provinces$NFL$table1) %>% arrange(Date_time_local) %>% collect()

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  output$timer <- renderText({invalidateLater(1000, session)
    paste("",Sys.time())})
  
  check_db_nb <- function(){tbl(con, config$provinces$NB$table1) %>% count(Date_time_local)}
  get_data_nb <- function(){tbl(con, config$provinces$NB$table1) %>% arrange(Date_time_local) %>% collect()}
  nbload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_nb, valueFunc = get_data_nb)
  nbload_data <- reactive({nbload_data_pre()})
  #nbload_data_mean <- reactive{(mean(nbload_data()$nb_load))} 
  nbload_subset <- reactive({subset(nbload_data(),subset = nbload_data()$Date_time_local >= previous_time_1)})
  nb_load_ts <-  reactive({xts(nbload_subset()$nb_load,nbload_subset()$Date_time_local)})
  
  output$NB_load <- renderHighchart({ 
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_load_ts(), type = "line", id = "ts", name = "Load: ") %>% 
      hc_navigator(enabled = TRUE)
  })
  
  check_db_ns <- function(){tbl(con, config$provinces$NS$table1) %>% count(Date_time_local)}
  get_data_ns <- function(){tbl(con, config$provinces$NS$table1) %>% arrange(Date_time_local) %>% collect()}
  nsload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_ns, valueFunc = get_data_ns)
  nsload_data <- reactive({nsload_data_pre()})
  nsload_subset <- reactive({subset(nsload_data(),subset = nsload_data()$Date_time_local >= previous_time_1)})
  ns_load_ts <-  reactive({xts(nsload_subset()$Net_Load,nsload_subset()$Date_time_local)})
  
  output$NS_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_ts(), type = "line", name = "Load: ", color = "green") %>% 
      hc_navigator(enabled = TRUE)}) 
  
  bcload_data <- tbl(con, config$provinces$BC$table1) %>% arrange(Date_time_local) %>% collect()
  bcload_date <- as.Date(tail(bcload_data$Date_time_local,1))
  bcload_subset <- subset(bcload_data,subset = Date_time_local >= (bcload_date - 4) & Date_time_local <= bcload_date)
  bc_load_ts <-  xts(bcload_subset$Balancing_Authority_Load,bcload_subset$Date_time_local)
  
  output$BC_load <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_ts, type = "line", name = "Load: ", color = "pink") %>% 
      hc_navigator(enabled = TRUE)})
  
  output$AB_load <- renderHighchart({ab_load_chart})
  
  check_db_on <- function(){tbl(con, config$provinces$ONT$table1) %>% count(date_time_local)}
  get_data_on <- function(){tbl(con, config$provinces$ONT$table1) %>% arrange(date_time_local) %>% collect()}
  onload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                  checkFunc = check_db_on, valueFunc = get_data_on)
  onload_data <- reactive({onload_data_pre()})
  onload_subset <- reactive({subset(onload_data(),subset = onload_data()$date_time_local >= previous_time_1)})
  on_load_ts <-  reactive({xts(onload_subset()$total_load,onload_subset()$date_time_local)})
  
  output$ON_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_load_ts(), type = "line", name = "Load: ", color = "orange") %>% 
      hc_navigator(enabled = TRUE)})
  
  check_db_pei <- function(){tbl(con, config$provinces$PEI$table1) %>% count(Date_time_local)}
  get_data_pei <- function(){tbl(con, config$provinces$PEI$table1) %>% arrange(Date_time_local) %>% collect()}
  peiload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                   checkFunc = check_db_pei, valueFunc = get_data_pei)
  peiload_data <- reactive({peiload_data_pre()})
  peiload_subset <- reactive({subset(peiload_data(),subset = peiload_data()$Date_time_local >= previous_time_1)})
  pei_load_ts <-  reactive({xts(peiload_subset()$on_island_load,peiload_subset()$Date_time_local)})
  
  output$PEI_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_load_ts(), type = "line", name = "Load: ", color = "red") %>% 
      hc_navigator(enabled = TRUE)}) 
  
  check_db_nfl <- function(){tbl(con, config$provinces$NFL$table1) %>% count(Date_time_local)}
  get_data_nfl <- function(){tbl(con, config$provinces$NFL$table1) %>% arrange(Date_time_local) %>% collect()}
  nflload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                   checkFunc = check_db_nfl, valueFunc = get_data_nfl)
  nflload_data <- reactive({nflload_data_pre()})
  nflload_subset <- reactive({subset(nflload_data(),subset = nflload_data()$Date_time_local >= previous_time_1)})
  nfl_load_ts <-  reactive({xts(nflload_subset()$Net_Load_MW,nflload_subset()$Date_time_local)})
  
  output$NFL_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_ts(), type = "line", name = "Load: ", color = "red") %>% 
      hc_navigator(enabled = TRUE)})
  
  check_db_qb <- function(){tbl(con, config$provinces$QUEBEC$table1) %>% count(Date_time_local)}
  get_data_qb <- function(){tbl(con, config$provinces$QUEBEC$table1) %>% arrange(Date_time_local) %>% collect()}
  qbload_data_pre <- reactivePoll(intervalMillis = 300000, session = session,
                                   checkFunc = check_db_qb, valueFunc = get_data_qb)
  qbload_data <- reactive({qbload_data_pre()})
  qbload_subset <- reactive({subset(qbload_data(),subset = qbload_data()$Date_time_local >= previous_time_1)})
  qb_load_ts <-  reactive({xts(qbload_subset()$total_demand,qbload_subset()$Date_time_local)})
  
  output$QB_load <- renderHighchart({highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(qb_load_ts(), type = "line", name = "Load: ", color = "red") %>% 
      hc_navigator(enabled = TRUE)})
  
  
  pei_date_ind_1_1 <- reactive({paste(input$pei_dates_1[1],"00:00:00",sep = " ")})
  pei_date_ind_1_2 <- reactive({paste(input$pei_dates_1[2],"00:00:00",sep = " ")})
  pei_ind_subset_dat <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_1_1() & pei_ind_dat$Date_time_local <= pei_date_ind_1_2()))})
  pei_ind_dat_ts <- reactive({xts(pei_ind_dat$on_island_load,pei_ind_dat$Date_time_local)})
  pei_ind_subset_ts <- reactive({xts(pei_ind_subset_dat()$on_island_load,pei_ind_subset_dat()$Date_time_local)})
  pei_ind_dat_ts_yearly <- reactive({to.yearly(pei_ind_dat_ts())})
  pei_ind_dat_ts_monthly <- reactive({to.monthly(pei_ind_subset_ts())})
  pei_ind_dat_ts_weekly <- reactive({to.weekly(pei_ind_subset_ts())})
  pei_ind_dat_ts_daily <- reactive({to.daily(pei_ind_subset_ts())})
  pei_ind_dat_ts_hourly <- reactive({
    pei_hr <- to.hourly(pei_ind_subset_ts()) 
  return(pei_hr)
    })
  
  output$PEI_ON_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_yearly()[,(colnames(pei_ind_dat_ts_yearly()) %in% c('pei_ind_dat_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_yearly()[,(colnames(pei_ind_dat_ts_yearly()) %in% c('pei_ind_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_weekly()[,(colnames(pei_ind_dat_ts_weekly()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_weekly()[,(colnames(pei_ind_dat_ts_weekly()) %in% c('pei_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_daily()[,(colnames(pei_ind_dat_ts_daily()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_daily()[,(colnames(pei_ind_dat_ts_daily()) %in% c('pei_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_hourly()[,(colnames(pei_ind_dat_ts_hourly()) %in% c('pei_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_hourly()[,(colnames(pei_ind_dat_ts_hourly()) %in% c('pei_ind_subset_ts().Low'))], type = "spline", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_ind_subset_ts(), type = "line", name = "Load: ")
  })
  
  pei_date_ind_2_1 <- reactive({paste(input$pei_dates_2[1],"00:00:00",sep = " ")})
  pei_date_ind_2_2 <- reactive({paste(input$pei_dates_2[2],"00:00:00",sep = " ")})
  pei_ind_subset_dat_2 <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_2_1() & pei_ind_dat$Date_time_local <= pei_date_ind_2_2()))})
  pei_ind_dat_ts_2 <- reactive({xts(pei_ind_dat$on_island_wind,pei_ind_dat$Date_time_local)})
  pei_ind_dat_ts_2_fossil <- reactive({xts(pei_ind_dat$on_island_fossil,pei_ind_dat$Date_time_local)})
  pei_ind_subset_ts_2 <- reactive({xts(pei_ind_subset_dat_2()$on_island_wind,pei_ind_subset_dat_2()$Date_time_local)})
  pei_ind_subset_ts_2_fossil <- reactive({xts(pei_ind_subset_dat_2()$on_island_fossil,pei_ind_subset_dat_2()$Date_time_local)})
  pei_ind_dat_ts_yearly_2 <- reactive({to.yearly(pei_ind_dat_ts_2())})
  pei_ind_dat_ts_monthly_2 <- reactive({to.monthly(pei_ind_subset_ts_2())})
  pei_ind_dat_ts_weekly_2 <- reactive({to.weekly(pei_ind_subset_ts_2())})
  pei_ind_dat_ts_daily_2 <- reactive({to.daily(pei_ind_subset_ts_2())})
  pei_ind_dat_ts_hourly_2 <- reactive({to.hourly(pei_ind_subset_ts_2())})
  pei_ind_dat_ts_yearly_2_fossil <- reactive({to.yearly(pei_ind_dat_ts_2_fossil())})
  pei_ind_dat_ts_monthly_2_fossil<- reactive({to.monthly(pei_ind_subset_ts_2_fossil())})
  pei_ind_dat_ts_weekly_2_fossil <- reactive({to.weekly(pei_ind_subset_ts_2_fossil())})
  pei_ind_dat_ts_daily_2_fossil <- reactive({to.daily(pei_ind_subset_ts_2_fossil())})
  pei_ind_dat_ts_hourly_2_fossil <- reactive({to.hourly(pei_ind_subset_ts_2_fossil())})
  
  output$PEI_ON_WIND_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_yearly_2()[,(colnames(pei_ind_dat_ts_yearly_2()) %in% c('pei_ind_dat_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_yearly_2()[,(colnames(pei_ind_dat_ts_yearly_2()) %in% c('pei_ind_dat_ts_2().Low'))], type = "line", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_yearly_2_fossil()[,(colnames(pei_ind_dat_ts_yearly_2_fossil()) %in% c('pei_ind_dat_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_yearly_2_fossil()[,(colnames(pei_ind_dat_ts_yearly_2_fossil()) %in% c('pei_ind_dat_ts_2_fossil().Low'))], type = "line", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_weekly_2()[,(colnames(pei_ind_dat_ts_weekly_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_weekly_2()[,(colnames(pei_ind_dat_ts_weekly_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "area", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_weekly_2_fossil()[,(colnames(pei_ind_dat_ts_weekly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_weekly_2_fossil()[,(colnames(pei_ind_dat_ts_weekly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "area", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_daily_2()[,(colnames(pei_ind_dat_ts_daily_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_daily_2()[,(colnames(pei_ind_dat_ts_daily_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "area", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_daily_2_fossil()[,(colnames(pei_ind_dat_ts_daily_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_daily_2_fossil()[,(colnames(pei_ind_dat_ts_daily_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "area", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_ind_dat_ts_hourly_2()[,(colnames(pei_ind_dat_ts_hourly_2()) %in% c('pei_ind_subset_ts_2().High'))], type = "line", name = "High Wind Load: ", color = "lightgreen") %>%
      hc_add_series(pei_ind_dat_ts_hourly_2()[,(colnames(pei_ind_dat_ts_hourly_2()) %in% c('pei_ind_subset_ts_2().Low'))], type = "spline", name = "Low Wind Load: ", color = "red") %>%
      hc_add_series(pei_ind_dat_ts_hourly_2_fossil()[,(colnames(pei_ind_dat_ts_hourly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().High'))], type = "line", name = "High Fossil Load: ", color = "pink") %>%
      hc_add_series(pei_ind_dat_ts_hourly_2_fossil()[,(colnames(pei_ind_dat_ts_hourly_2_fossil()) %in% c('pei_ind_subset_ts_2_fossil().Low'))], type = "spline", name = "Low Fossil Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_ON_WIND_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_ind_subset_ts_2(), type = "line", name = "Wind Load: ", color = "lightgreen")%>%
      hc_add_series(pei_ind_subset_ts_2_fossil(), type = "line", name = "Fossil Load: ", color = "pink")
  })
  
  pei_date_ind_3_1 <- reactive({paste(input$pei_dates_3[1],"00:00:00",sep = " ")})
  pei_date_ind_3_2 <- reactive({paste(input$pei_dates_3[2],"00:00:00",sep = " ")})
  pei_exp_subset_dat <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_3_1() & pei_ind_dat$Date_time_local <= pei_date_ind_3_2()))})
  pei_exp_dat_ts <- reactive({xts(pei_ind_dat$wind_export,pei_ind_dat$Date_time_local)})
  pei_exp_local_dat_ts <- reactive({xts(pei_ind_dat$wind_local,pei_ind_dat$Date_time_local)})
  pei_exp_subset_ts <- reactive({xts(pei_exp_subset_dat()$wind_export,pei_exp_subset_dat()$Date_time_local)})
  pei_exp_local_subset_ts <- reactive({xts(pei_exp_subset_dat()$wind_local,pei_exp_subset_dat()$Date_time_local)})
  pei_exp_dat_ts_yearly <- reactive({to.yearly(pei_exp_dat_ts())})
  pei_exp_dat_ts_monthly <- reactive({to.monthly(pei_exp_subset_ts())})
  pei_exp_dat_ts_weekly <- reactive({to.weekly(pei_exp_subset_ts())})
  pei_exp_dat_ts_daily <- reactive({to.daily(pei_exp_subset_ts())})
  pei_exp_dat_ts_hourly <- reactive({to.hourly(pei_exp_subset_ts())})
  pei_exp_local_dat_ts_yearly <- reactive({to.yearly(pei_exp_local_dat_ts())})
  pei_exp_local_dat_ts_monthly <- reactive({to.monthly(pei_exp_local_subset_ts())})
  pei_exp_local_dat_ts_weekly <- reactive({to.weekly(pei_exp_local_subset_ts())})
  pei_exp_local_dat_ts_daily <- reactive({to.daily(pei_exp_local_subset_ts())})
  pei_exp_local_dat_ts_hourly <- reactive({to.hourly(pei_exp_local_subset_ts())})
  
  output$PEI_EXPORT_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_yearly()[,(colnames(pei_exp_dat_ts_yearly()) %in% c('pei_exp_dat_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_yearly()[,(colnames(pei_exp_dat_ts_yearly()) %in% c('pei_exp_dat_ts().Low'))], type = "line", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_yearly()[,(colnames(pei_exp_local_dat_ts_yearly()) %in% c('pei_exp_local_dat_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_yearly()[,(colnames(pei_exp_local_dat_ts_yearly()) %in% c('pei_exp_local_dat_ts().Low'))], type = "line", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_weekly()[,(colnames(pei_exp_dat_ts_weekly()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_weekly()[,(colnames(pei_exp_dat_ts_weekly()) %in% c('pei_exp_subset_ts().Low'))], type = "area", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_weekly()[,(colnames(pei_exp_local_dat_ts_weekly()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_weekly()[,(colnames(pei_exp_local_dat_ts_weekly()) %in% c('pei_exp_local_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_daily()[,(colnames(pei_exp_dat_ts_daily()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_daily()[,(colnames(pei_exp_dat_ts_daily()) %in% c('pei_exp_subset_ts().Low'))], type = "area", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_daily()[,(colnames(pei_exp_local_dat_ts_daily()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_daily()[,(colnames(pei_exp_local_dat_ts_daily()) %in% c('pei_exp_local_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_exp_dat_ts_hourly()[,(colnames(pei_exp_dat_ts_hourly()) %in% c('pei_exp_subset_ts().High'))], type = "line", name = "High Export Load: ", color = "lightgreen") %>%
      hc_add_series(pei_exp_dat_ts_hourly()[,(colnames(pei_exp_dat_ts_hourly()) %in% c('pei_exp_subset_ts().Low'))], type = "spline", name = "Low Export Load: ", color = "red") %>%
      hc_add_series(pei_exp_local_dat_ts_hourly()[,(colnames(pei_exp_local_dat_ts_hourly()) %in% c('pei_exp_local_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_dat_ts_hourly()[,(colnames(pei_exp_local_dat_ts_hourly()) %in% c('pei_exp_local_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "blue") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_EXPORT_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_exp_subset_ts(), type = "line", name = "Export Load: ", color = "pink") %>%
      hc_add_series(pei_exp_local_subset_ts(), type = "line", name = "Local Load: ", color = "lightgreen")
  })
  
  pei_date_ind_4_1 <- reactive({paste(input$pei_dates_4[1],"00:00:00",sep = " ")})
  pei_date_ind_4_2 <- reactive({paste(input$pei_dates_4[2],"00:00:00",sep = " ")})
  pei_loc_subset_dat <- reactive({subset(pei_ind_dat,subset = (pei_ind_dat$Date_time_local >= pei_date_ind_4_1() & pei_ind_dat$Date_time_local <= pei_date_ind_4_2()))})
  pei_loc_dat_ts <- reactive({xts(pei_ind_dat$percentage_wind,pei_ind_dat$Date_time_local)})
  pei_loc_subset_ts <- reactive({xts(pei_loc_subset_dat()$percentage_wind,pei_loc_subset_dat()$Date_time_local)})
  pei_loc_dat_ts_yearly <- reactive({to.yearly(pei_loc_dat_ts())})
  pei_loc_dat_ts_monthly <- reactive({to.monthly(pei_loc_subset_ts())})
  pei_loc_dat_ts_weekly <- reactive({to.weekly(pei_loc_subset_ts())})
  pei_loc_dat_ts_daily <- reactive({to.daily(pei_loc_subset_ts())})
  pei_loc_dat_ts_hourly <- reactive({to.hourly(pei_loc_subset_ts())})
  
  
  output$PEI_LOCAL_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_yearly()[,(colnames(pei_loc_dat_ts_yearly()) %in% c('pei_loc_dat_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_yearly()[,(colnames(pei_loc_dat_ts_yearly()) %in% c('pei_loc_dat_ts().Low'))], type = "line", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_weekly()[,(colnames(pei_loc_dat_ts_weekly()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_weekly()[,(colnames(pei_loc_dat_ts_weekly()) %in% c('pei_loc_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_daily()[,(colnames(pei_loc_dat_ts_daily()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_daily()[,(colnames(pei_loc_dat_ts_daily()) %in% c('pei_loc_subset_ts().Low'))], type = "area", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(pei_loc_dat_ts_hourly()[,(colnames(pei_loc_dat_ts_hourly()) %in% c('pei_loc_subset_ts().High'))], type = "line", name = "High Local Load: ", color = "lightgreen") %>%
      hc_add_series(pei_loc_dat_ts_hourly()[,(colnames(pei_loc_dat_ts_hourly()) %in% c('pei_loc_subset_ts().Low'))], type = "spline", name = "Low Local Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$PEI_LOCAL_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(pei_loc_subset_ts(), type = "spline", name = "Local Load: ", color = "brown")
  })
  
  nb_date_ind_1_1 <- reactive({paste(input$nb_dates_1[1],"00:00:00",sep = " ")})
  nb_date_ind_1_2 <- reactive({paste(input$nb_dates_1[2],"00:00:00",sep = " ")})
  nb_ind_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_1_1() & nb_ind_dat$Date_time_local <= nb_date_ind_1_2()))})
  nb_ind_dat_ts <- reactive({xts(nb_ind_dat$nb_load,nb_ind_dat$Date_time_local)})
  nb_ind_subset_ts <- reactive({xts(nb_ind_subset_dat()$nb_load,nb_ind_subset_dat()$Date_time_local)})
  nb_ind_dat_ts_yearly <- reactive({to.yearly(nb_ind_dat_ts())})
  nb_ind_dat_ts_monthly <- reactive({to.monthly(nb_ind_subset_ts())})
  nb_ind_dat_ts_weekly <- reactive({to.weekly(nb_ind_subset_ts())})
  nb_ind_dat_ts_daily <- reactive({to.daily(nb_ind_subset_ts())})
  nb_ind_dat_ts_hourly <- reactive({to.hourly(nb_ind_subset_ts())})
  output$NB_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_yearly()[,(colnames(nb_ind_dat_ts_yearly()) %in% c('nb_ind_dat_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_yearly()[,(colnames(nb_ind_dat_ts_yearly()) %in% c('nb_ind_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_weekly()[,(colnames(nb_ind_dat_ts_weekly()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_weekly()[,(colnames(nb_ind_dat_ts_weekly()) %in% c('nb_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_daily()[,(colnames(nb_ind_dat_ts_daily()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_daily()[,(colnames(nb_ind_dat_ts_daily()) %in% c('nb_ind_subset_ts().Low'))], type = "area", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_ind_dat_ts_hourly()[,(colnames(nb_ind_dat_ts_hourly()) %in% c('nb_ind_subset_ts().High'))], type = "line", name = "High Load: ", color = "lightgreen") %>%
      hc_add_series(nb_ind_dat_ts_hourly()[,(colnames(nb_ind_dat_ts_hourly()) %in% c('nb_ind_subset_ts().Low'))], type = "spline", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nb_ind_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  nb_date_ind_2_1 <- reactive({paste(input$nb_dates_2[1],"00:00:00",sep = " ")})
  nb_date_ind_2_2 <- reactive({paste(input$nb_dates_2[2],"00:00:00",sep = " ")})
  nb_dem_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_2_1() & nb_ind_dat$Date_time_local <= nb_date_ind_2_2()))})
  nb_dem_dat_ts <- reactive({xts(nb_ind_dat$nb_demand,nb_ind_dat$Date_time_local)})
  nb_dem_subset_ts <- reactive({xts(nb_dem_subset_dat()$nb_demand,nb_dem_subset_dat()$Date_time_local)})
  nb_dem_dat_ts_yearly <- reactive({to.yearly(nb_dem_dat_ts())})
  nb_dem_dat_ts_monthly <- reactive({to.monthly(nb_dem_subset_ts())})
  nb_dem_dat_ts_weekly <- reactive({to.weekly(nb_dem_subset_ts())})
  nb_dem_dat_ts_daily <- reactive({to.daily(nb_dem_subset_ts())})
  nb_dem_dat_ts_hourly <- reactive({to.hourly(nb_dem_subset_ts())})
  
  output$NB_DEMAND_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_yearly()[,(colnames(nb_dem_dat_ts_yearly()) %in% c('nb_dem_dat_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_yearly()[,(colnames(nb_dem_dat_ts_yearly()) %in% c('nb_dem_dat_ts().Low'))], type = "line", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_weekly()[,(colnames(nb_dem_dat_ts_weekly()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_weekly()[,(colnames(nb_dem_dat_ts_weekly()) %in% c('nb_dem_subset_ts().Low'))], type = "area", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_daily()[,(colnames(nb_dem_dat_ts_daily()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_daily()[,(colnames(nb_dem_dat_ts_daily()) %in% c('nb_dem_subset_ts().Low'))], type = "area", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_dem_dat_ts_hourly()[,(colnames(nb_dem_dat_ts_hourly()) %in% c('nb_dem_subset_ts().High'))], type = "line", name = "High Demand: ", color = "lightgreen") %>%
      hc_add_series(nb_dem_dat_ts_hourly()[,(colnames(nb_dem_dat_ts_hourly()) %in% c('nb_dem_subset_ts().Low'))], type = "spline", name = "Low Demand: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_DEMAND_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nb_dem_subset_ts(), type = "area", name = "Demand: ", color = "purple")
  })
  
  nb_date_ind_3_1 <- reactive({paste(input$nb_dates_3[1],"00:00:00",sep = " ")})
  nb_date_ind_3_2 <- reactive({paste(input$nb_dates_3[2],"00:00:00",sep = " ")})
  nb_10_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_3_1() & nb_ind_dat$Date_time_local <= nb_date_ind_3_2()))})
  nb_10_dat_ts <- reactive({xts(nb_ind_dat$min_10_reserve_margin,nb_ind_dat$Date_time_local)})
  nb_10_subset_ts <- reactive({xts(nb_10_subset_dat()$min_10_reserve_margin,nb_10_subset_dat()$Date_time_local)})
  nb_10_dat_ts_yearly <- reactive({to.yearly(nb_10_dat_ts())})
  nb_10_dat_ts_monthly <- reactive({to.monthly(nb_10_subset_ts())})
  nb_10_dat_ts_weekly <- reactive({to.weekly(nb_10_subset_ts())})
  nb_10_dat_ts_daily <- reactive({to.daily(nb_10_subset_ts())})
  nb_10_dat_ts_hourly <- reactive({to.hourly(nb_10_subset_ts())})
  
  output$NB_RESERVE_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_yearly()[,(colnames(nb_10_dat_ts_yearly()) %in% c('nb_10_dat_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_yearly()[,(colnames(nb_10_dat_ts_yearly()) %in% c('nb_10_dat_ts().Low'))], type = "line", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_weekly()[,(colnames(nb_10_dat_ts_weekly()) %in% c('nb_10_subset_ts().High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_weekly()[,(colnames(nb_10_dat_ts_weekly()) %in% c('nb_10_subset_ts().Low'))], type = "area", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_daily()[,(colnames(nb_10_dat_ts_daily()) %in% c('nb_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_daily()[,(colnames(nb_10_dat_ts_daily()) %in% c('nb_10_subset_ts.Low'))], type = "area", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_10_dat_ts_hourly()[,(colnames(nb_10_dat_ts_hourly()) %in% c('nb_10_subset_ts.High'))], type = "line", name = "High Reserve: ", color = "lightgreen") %>%
      hc_add_series(nb_10_dat_ts_hourly()[,(colnames(nb_10_dat_ts_hourly()) %in% c('nb_10_subset_ts.Low'))], type = "area", name = "Low Reserve: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      #hc_loading() %>%
      hc_add_series(nb_10_subset_ts(), type = "lollipop", name = "Reserve Load: ", color = "orange")
  })
  
  nb_date_ind_4_1 <- reactive({paste(input$nb_dates_4[1],"00:00:00",sep = " ")})
  nb_date_ind_4_2 <- reactive({paste(input$nb_dates_4[2],"00:00:00",sep = " ")})
  nb_30_subset_dat <- reactive({subset(nb_ind_dat,subset = (nb_ind_dat$Date_time_local >= nb_date_ind_4_1() & nb_ind_dat$Date_time_local <= nb_date_ind_4_2()))})
  nb_30_dat_ts <- reactive({xts(nb_ind_dat$min_30_reserve_margin,nb_ind_dat$Date_time_local)})
  nb_30_subset_ts <- reactive({xts(nb_30_subset_dat()$min_30_reserve_margin,nb_30_subset_dat()$Date_time_local)})
  nb_30_dat_ts_yearly <- reactive({to.yearly(nb_30_dat_ts())})
  nb_30_dat_ts_monthly <- reactive({to.monthly(nb_30_subset_ts())})
  nb_30_dat_ts_weekly <- reactive({to.weekly(nb_30_subset_ts())})
  nb_30_dat_ts_daily <- reactive({to.daily(nb_30_subset_ts())})
  nb_30_dat_ts_hourly <- reactive({to.hourly(nb_30_subset_ts())})
  
  output$NB_RESERVE2_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_yearly()[,(colnames(nb_30_dat_ts_yearly()) %in% c('nb_30_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_yearly()[,(colnames(nb_30_dat_ts_yearly()) %in% c('nb_30_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_weekly()[,(colnames(nb_30_dat_ts_weekly()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_weekly()[,(colnames(nb_30_dat_ts_weekly()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_daily()[,(colnames(nb_30_dat_ts_daily()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_daily()[,(colnames(nb_30_dat_ts_daily()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nb_30_dat_ts_hourly()[,(colnames(nb_30_dat_ts_hourly()) %in% c('nb_30_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nb_30_dat_ts_hourly()[,(colnames(nb_30_dat_ts_hourly()) %in% c('nb_30_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NB_RESERVE2_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nb_30_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  nfl_date_ind_1_1 <- reactive({paste(input$nfl_dates_1[1],"00:00:00",sep = " ")})
  nfl_date_ind_1_2 <- reactive({paste(input$nfl_dates_1[2],"00:00:00",sep = " ")})
  nfl_load_subset_dat <- reactive({subset(nfl_ind_dat,subset = (nfl_ind_dat$Date_time_local >= nfl_date_ind_1_1() & nfl_ind_dat$Date_time_local <= nfl_date_ind_1_2()))})
  nfl_load_dat_ts <- reactive({xts(nfl_ind_dat$Net_Load_MW,nfl_ind_dat$Date_time_local)})
  nfl_load_subset_ts <- reactive({xts(nfl_load_subset_dat()$Net_Load_MW,nfl_load_subset_dat()$Date_time_local)})
  nfl_load_dat_ts_yearly <- reactive({to.yearly(nfl_load_dat_ts())})
  nfl_load_dat_ts_monthly <- reactive({to.monthly(nfl_load_subset_ts())})
  nfl_load_dat_ts_weekly <- reactive({to.weekly(nfl_load_subset_ts())})
  nfl_load_dat_ts_daily <- reactive({to.daily(nfl_load_subset_ts())})
  nfl_load_dat_ts_hourly <- reactive({to.hourly(nfl_load_subset_ts())})
  
  output$NFL_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_yearly()[,(colnames(nfl_load_dat_ts_yearly()) %in% c('nfl_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_yearly()[,(colnames(nfl_load_dat_ts_yearly()) %in% c('nfl_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_weekly()[,(colnames(nfl_load_dat_ts_weekly()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_weekly()[,(colnames(nfl_load_dat_ts_weekly()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_daily()[,(colnames(nfl_load_dat_ts_daily()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_daily()[,(colnames(nfl_load_dat_ts_daily()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(nfl_load_dat_ts_hourly()[,(colnames(nfl_load_dat_ts_hourly()) %in% c('nfl_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(nfl_load_dat_ts_hourly()[,(colnames(nfl_load_dat_ts_hourly()) %in% c('nfl_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NFL_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(nfl_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  bc_date_ind_1_1 <- reactive({paste(input$bc_dates_1[1],"00:00:00",sep = " ")})
  bc_date_ind_1_2 <- reactive({paste(input$bc_dates_1[2],"00:00:00",sep = " ")})
  bc_date_ind_2_1 <- reactive({paste(input$bc_dates_2[1],"00:00:00",sep = " ")})
  bc_date_ind_2_2 <- reactive({paste(input$bc_dates_2[2],"00:00:00",sep = " ")})
  bc_date_ind_3_1 <- reactive({paste(input$bc_dates_3[1],"00:00:00",sep = " ")})
  bc_date_ind_3_2 <- reactive({paste(input$bc_dates_3[2],"00:00:00",sep = " ")})
  
  bc_load_subset_dat_1 <- reactive({subset(bc_ind_dat,subset = (bc_ind_dat$Date_time_local >= bc_date_ind_1_1() & bc_ind_dat$Date_time_local <= bc_date_ind_1_2()))})
  bc_load_subset_dat_2 <- reactive({subset(bc_ind_dat_1,subset = (bc_ind_dat_1$Date_time_local >= bc_date_ind_2_1() & bc_ind_dat_1$Date_time_local <= bc_date_ind_2_2()))})
  bc_load_subset_dat_3 <- reactive({subset(bc_ind_dat_1,subset = (bc_ind_dat_1$Date_time_local >= bc_date_ind_3_1() & bc_ind_dat_1$Date_time_local <= bc_date_ind_3_2()))})
  
  bc_load_dat_ts_1 <- reactive({xts(bc_ind_dat$Balancing_Authority_Load,bc_ind_dat$Date_time_local)})
  bc_load_dat_ts_2 <- reactive({xts(bc_ind_dat_1$BC_US_net_actual_flow,bc_ind_dat_1$Date_time_local)})
  bc_load_dat_ts_3 <- reactive({xts(bc_ind_dat_1$BC_AB_net_actual_flow,bc_ind_dat_1$Date_time_local)})
  
  bc_load_subset_ts_1 <- reactive({xts(bc_load_subset_dat_1()$Balancing_Authority_Load,bc_load_subset_dat_1()$Date_time_local)})
  bc_load_subset_ts_2 <- reactive({xts(bc_load_subset_dat_2()$BC_US_net_actual_flow,bc_load_subset_dat_2()$Date_time_local)})
  bc_load_subset_ts_3 <- reactive({xts(bc_load_subset_dat_3()$BC_AB_net_actual_flow,bc_load_subset_dat_3()$Date_time_local)})
  
  
  bc_load_dat_ts_1_yearly <- reactive({to.yearly(bc_load_dat_ts_1())})
  bc_load_dat_ts_1_monthly <- reactive({to.monthly(bc_load_subset_ts_1())})
  bc_load_dat_ts_1_weekly <- reactive({to.weekly(bc_load_subset_ts_1())})
  bc_load_dat_ts_1_daily <- reactive({to.daily(bc_load_subset_ts_1())})
  bc_load_dat_ts_1_hourly <- reactive({to.hourly(bc_load_subset_ts_1())})
  
  bc_load_dat_ts_2_yearly <- reactive({to.yearly(bc_load_dat_ts_2())})
  bc_load_dat_ts_2_monthly <- reactive({to.monthly(bc_load_subset_ts_2())})
  bc_load_dat_ts_2_weekly <- reactive({to.weekly(bc_load_subset_ts_2())})
  bc_load_dat_ts_2_daily <- reactive({to.daily(bc_load_subset_ts_2())})
  bc_load_dat_ts_2_hourly <- reactive({to.hourly(bc_load_subset_ts_2())})
  
  bc_load_dat_ts_3_yearly <- reactive({to.yearly(bc_load_dat_ts_3())})
  bc_load_dat_ts_3_monthly <- reactive({to.monthly(bc_load_subset_ts_3())})
  bc_load_dat_ts_3_weekly <- reactive({to.weekly(bc_load_subset_ts_3())})
  bc_load_dat_ts_3_daily <- reactive({to.daily(bc_load_subset_ts_3())})
  bc_load_dat_ts_3_hourly <- reactive({to.hourly(bc_load_subset_ts_3())})
  
  output$BC_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_yearly()[,(colnames(bc_load_dat_ts_1_yearly()) %in% c('bc_load_dat_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_yearly()[,(colnames(bc_load_dat_ts_1_yearly()) %in% c('bc_load_dat_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_weekly()[,(colnames(bc_load_dat_ts_1_weekly()) %in% c('bc_load_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_weekly()[,(colnames(bc_load_dat_ts_1_weekly()) %in% c('bc_load_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_daily()[,(colnames(bc_load_dat_ts_1_daily()) %in% c('bc_load_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_daily()[,(colnames(bc_load_dat_ts_1_daily()) %in% c('bc_load_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_1_hourly()[,(colnames(bc_load_dat_ts_1_hourly()) %in% c('bc_load_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_1_hourly()[,(colnames(bc_load_dat_ts_1_hourly()) %in% c('bc_load_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(bc_load_subset_ts_1(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$BC_US_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_yearly()[,(colnames(bc_load_dat_ts_2_yearly()) %in% c('bc_load_dat_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_yearly()[,(colnames(bc_load_dat_ts_2_yearly()) %in% c('bc_load_dat_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_weekly()[,(colnames(bc_load_dat_ts_2_weekly()) %in% c('bc_load_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_weekly()[,(colnames(bc_load_dat_ts_2_weekly()) %in% c('bc_load_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_daily()[,(colnames(bc_load_dat_ts_2_daily()) %in% c('bc_load_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_daily()[,(colnames(bc_load_dat_ts_2_daily()) %in% c('bc_load_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_2_hourly()[,(colnames(bc_load_dat_ts_2_hourly()) %in% c('bc_load_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_2_hourly()[,(colnames(bc_load_dat_ts_2_hourly()) %in% c('bc_load_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_US_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(bc_load_subset_ts_2(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$BC_AB_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_yearly()[,(colnames(bc_load_dat_ts_3_yearly()) %in% c('bc_load_dat_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_yearly()[,(colnames(bc_load_dat_ts_3_yearly()) %in% c('bc_load_dat_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_weekly()[,(colnames(bc_load_dat_ts_3_weekly()) %in% c('bc_load_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_weekly()[,(colnames(bc_load_dat_ts_3_weekly()) %in% c('bc_load_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_daily()[,(colnames(bc_load_dat_ts_3_daily()) %in% c('bc_load_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_daily()[,(colnames(bc_load_dat_ts_3_daily()) %in% c('bc_load_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(bc_load_dat_ts_3_hourly()[,(colnames(bc_load_dat_ts_3_hourly()) %in% c('bc_load_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(bc_load_dat_ts_3_hourly()[,(colnames(bc_load_dat_ts_3_hourly()) %in% c('bc_load_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$BC_AB_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(bc_load_subset_ts_3(), type = "line", name = "High Load: ", color = "green")
  })
  
  ns_date_ind_1_1 <- reactive({paste(input$ns_dates_1[1],"00:00:00",sep = " ")})
  ns_date_ind_1_2 <- reactive({paste(input$ns_dates_1[2],"00:00:00",sep = " ")})
  ns_load_subset_dat <- reactive({subset(ns_ind_dat,subset = (ns_ind_dat$Date_time_local >= ns_date_ind_1_1() & ns_ind_dat$Date_time_local <= ns_date_ind_1_2()))})
  ns_load_dat_ts <- reactive({xts(ns_ind_dat$Net_Load,ns_ind_dat$Date_time_local)})
  ns_load_subset_ts <- reactive({xts(ns_load_subset_dat()$Net_Load,ns_load_subset_dat()$Date_time_local)})
  ns_load_dat_ts_yearly <- reactive({to.yearly(ns_load_dat_ts())})
  ns_load_dat_ts_monthly <- reactive({to.monthly(ns_load_subset_ts())})
  ns_load_dat_ts_weekly <- reactive({to.weekly(ns_load_subset_ts())})
  ns_load_dat_ts_daily <- reactive({to.daily(ns_load_subset_ts())})
  ns_load_dat_ts_hourly <- reactive({to.hourly(ns_load_subset_ts())})
  
  output$NS_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_yearly()[,(colnames(ns_load_dat_ts_yearly()) %in% c('ns_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_yearly()[,(colnames(ns_load_dat_ts_yearly()) %in% c('ns_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_weekly()[,(colnames(ns_load_dat_ts_weekly()) %in% c('ns_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_weekly()[,(colnames(ns_load_dat_ts_weekly()) %in% c('ns_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_daily()[,(colnames(ns_load_dat_ts_daily()) %in% c('ns_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_daily()[,(colnames(ns_load_dat_ts_daily()) %in% c('ns_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_load_dat_ts_hourly()[,(colnames(ns_load_dat_ts_hourly()) %in% c('ns_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_load_dat_ts_hourly()[,(colnames(ns_load_dat_ts_hourly()) %in% c('ns_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  ns_date_ind_2_1 <- reactive({paste(input$ns_dates_2[1],"00:00:00",sep = " ")})
  ns_date_ind_2_2 <- reactive({paste(input$ns_dates_2[2],"00:00:00",sep = " ")})
  ns_date_ind_3_1 <- reactive({paste(input$ns_dates_3[1],"00:00:00",sep = " ")})
  ns_date_ind_3_2 <- reactive({paste(input$ns_dates_3[2],"00:00:00",sep = " ")})
  ns_date_ind_4_1 <- reactive({paste(input$ns_dates_4[1],"00:00:00",sep = " ")})
  ns_date_ind_4_2 <- reactive({paste(input$ns_dates_4[2],"00:00:00",sep = " ")})
  ns_date_ind_5_1 <- reactive({paste(input$ns_dates_5[1],"00:00:00",sep = " ")})
  ns_date_ind_5_2 <- reactive({paste(input$ns_dates_5[2],"00:00:00",sep = " ")})
  
  ns_exp_subset_dat_1 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_2_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_2_2()))})
  ns_exp_subset_dat_2 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_3_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_3_2()))})
  ns_exp_subset_dat_3 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_4_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_4_2()))})
  ns_exp_subset_dat_4 <- reactive({subset(ns_ind_dat_1,subset = (ns_ind_dat_1$Date_time_local >= ns_date_ind_5_1() & ns_ind_dat_1$Date_time_local <= ns_date_ind_5_2()))})
  
  ns_exp_dat_ts_1 <- reactive({xts(ns_ind_dat_1$Highlands_Export,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_2 <- reactive({xts(ns_ind_dat_1$East_End_Export_At_Sydney,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_3 <- reactive({xts(ns_ind_dat_1$East_End_Export_At_East_Bay,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_4 <- reactive({xts(ns_ind_dat_1$Cape_Breton_Export,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_5 <- reactive({xts(ns_ind_dat_1$Onslow_Import,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_6 <- reactive({xts(ns_ind_dat_1$NS_Export,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_7 <- reactive({xts(ns_ind_dat_1$Onslow_South,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_8 <- reactive({xts(ns_ind_dat_1$Flow_Into_Metro,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_9 <- reactive({xts(ns_ind_dat_1$Western_Import,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_10 <- reactive({xts(ns_ind_dat_1$Valley_Import,ns_ind_dat_1$Date_time_local)})
  ns_exp_dat_ts_11 <- reactive({xts(ns_ind_dat_1$Maritime_Link_Import,ns_ind_dat_1$Date_time_local)})
  
  ns_exp_subset_ts_1 <- reactive({xts(ns_exp_subset_dat_1()$Highlands_Export,ns_exp_subset_dat_1()$Date_time_local)})
  ns_exp_subset_ts_2 <- reactive({xts(ns_exp_subset_dat_1()$East_End_Export_At_Sydney,ns_exp_subset_dat_1()$Date_time_local)})
  ns_exp_subset_ts_3 <- reactive({xts(ns_exp_subset_dat_1()$East_End_Export_At_East_Bay,ns_exp_subset_dat_1()$Date_time_local)})
  
  ns_exp_subset_ts_4 <- reactive({xts(ns_exp_subset_dat_2()$Cape_Breton_Export,ns_exp_subset_dat_2()$Date_time_local)})
  ns_exp_subset_ts_5 <- reactive({xts(ns_exp_subset_dat_2()$NS_Export,ns_exp_subset_dat_2()$Date_time_local)})
  ns_exp_subset_ts_6 <- reactive({xts(ns_exp_subset_dat_2()$Flow_Into_Metro,ns_exp_subset_dat_2()$Date_time_local)})
  
  ns_exp_subset_ts_7 <- reactive({xts(ns_exp_subset_dat_3()$Western_Import,ns_exp_subset_dat_3()$Date_time_local)})
  ns_exp_subset_ts_8 <- reactive({xts(ns_exp_subset_dat_3()$Valley_Import,ns_exp_subset_dat_3()$Date_time_local)})
  ns_exp_subset_ts_9 <- reactive({xts(ns_exp_subset_dat_3()$Maritime_Link_Import,ns_exp_subset_dat_3()$Date_time_local)})
  
  ns_exp_subset_ts_10 <- reactive({xts(ns_exp_subset_dat_4()$Onslow_Import,ns_exp_subset_dat_4()$Date_time_local)})
  ns_exp_subset_ts_11 <- reactive({xts(ns_exp_subset_dat_4()$Onslow_South,ns_exp_subset_dat_4()$Date_time_local)})
  
  ns_exp_dat_ts_1_yearly <- reactive({to.yearly(ns_exp_dat_ts_1())})
  ns_exp_dat_ts_1_monthly <- reactive({to.monthly(ns_exp_subset_ts_1())})
  ns_exp_dat_ts_1_weekly <- reactive({to.weekly(ns_exp_subset_ts_1())})
  ns_exp_dat_ts_1_daily <- reactive({to.daily(ns_exp_subset_ts_1())})
  ns_exp_dat_ts_1_hourly <- reactive({to.hourly(ns_exp_subset_ts_1())})
  
  ns_exp_dat_ts_2_yearly <- reactive({to.yearly(ns_exp_dat_ts_2())})
  ns_exp_dat_ts_2_monthly <- reactive({to.monthly(ns_exp_subset_ts_2())})
  ns_exp_dat_ts_2_weekly <- reactive({to.weekly(ns_exp_subset_ts_2())})
  ns_exp_dat_ts_2_daily <- reactive({to.daily(ns_exp_subset_ts_2())})
  ns_exp_dat_ts_2_hourly <- reactive({to.hourly(ns_exp_subset_ts_2())})
  
  ns_exp_dat_ts_3_yearly <- reactive({to.yearly(ns_exp_dat_ts_3())})
  ns_exp_dat_ts_3_monthly <- reactive({to.monthly(ns_exp_subset_ts_3())})
  ns_exp_dat_ts_3_weekly <- reactive({to.weekly(ns_exp_subset_ts_3())})
  ns_exp_dat_ts_3_daily <- reactive({to.daily(ns_exp_subset_ts_3())})
  ns_exp_dat_ts_3_hourly <- reactive({to.hourly(ns_exp_subset_ts_3())})
  
  ns_exp_dat_ts_4_yearly <- reactive({to.yearly(ns_exp_dat_ts_4())})
  ns_exp_dat_ts_4_monthly <- reactive({to.monthly(ns_exp_subset_ts_4())})
  ns_exp_dat_ts_4_weekly <- reactive({to.weekly(ns_exp_subset_ts_4())})
  ns_exp_dat_ts_4_daily <- reactive({to.daily(ns_exp_subset_ts_4())})
  ns_exp_dat_ts_4_hourly <- reactive({to.hourly(ns_exp_subset_ts_4())})
  
  ns_exp_dat_ts_5_yearly <- reactive({to.yearly(ns_exp_dat_ts_5())})
  ns_exp_dat_ts_5_monthly <- reactive({to.monthly(ns_exp_subset_ts_5())})
  ns_exp_dat_ts_5_weekly <- reactive({to.weekly(ns_exp_subset_ts_5())})
  ns_exp_dat_ts_5_daily <- reactive({to.daily(ns_exp_subset_ts_5())})
  ns_exp_dat_ts_5_hourly <- reactive({to.hourly(ns_exp_subset_ts_5())})
  
  ns_exp_dat_ts_6_yearly <- reactive({to.yearly(ns_exp_dat_ts_6())})
  ns_exp_dat_ts_6_monthly <- reactive({to.monthly(ns_exp_subset_ts_6())})
  ns_exp_dat_ts_6_weekly <- reactive({to.weekly(ns_exp_subset_ts_6())})
  ns_exp_dat_ts_6_daily <- reactive({to.daily(ns_exp_subset_ts_6())})
  ns_exp_dat_ts_6_hourly <- reactive({to.hourly(ns_exp_subset_ts_6())})
  
  ns_exp_dat_ts_7_yearly <- reactive({to.yearly(ns_exp_dat_ts_7())})
  ns_exp_dat_ts_7_monthly <- reactive({to.monthly(ns_exp_subset_ts_7())})
  ns_exp_dat_ts_7_weekly <- reactive({to.weekly(ns_exp_subset_ts_7())})
  ns_exp_dat_ts_7_daily <- reactive({to.daily(ns_exp_subset_ts_7())})
  ns_exp_dat_ts_7_hourly <- reactive({to.hourly(ns_exp_subset_ts_7())})
  
  ns_exp_dat_ts_8_yearly <- reactive({to.yearly(ns_exp_dat_ts_8())})
  ns_exp_dat_ts_8_monthly <- reactive({to.monthly(ns_exp_subset_ts_8())})
  ns_exp_dat_ts_8_weekly <- reactive({to.weekly(ns_exp_subset_ts_8())})
  ns_exp_dat_ts_8_daily <- reactive({to.daily(ns_exp_subset_ts_8())})
  ns_exp_dat_ts_8_hourly <- reactive({to.hourly(ns_exp_subset_ts_8())})
  
  ns_exp_dat_ts_9_yearly <- reactive({to.yearly(ns_exp_dat_ts_9())})
  ns_exp_dat_ts_9_monthly <- reactive({to.monthly(ns_exp_subset_ts_9())})
  ns_exp_dat_ts_9_weekly <- reactive({to.weekly(ns_exp_subset_ts_9())})
  ns_exp_dat_ts_9_daily <- reactive({to.daily(ns_exp_subset_ts_9())})
  ns_exp_dat_ts_9_hourly <- reactive({to.hourly(ns_exp_subset_ts_9())})
  
  ns_exp_dat_ts_10_yearly <- reactive({to.yearly(ns_exp_dat_ts_10())})
  ns_exp_dat_ts_10_monthly <- reactive({to.monthly(ns_exp_subset_ts_10())})
  ns_exp_dat_ts_10_weekly <- reactive({to.weekly(ns_exp_subset_ts_10())})
  ns_exp_dat_ts_10_daily <- reactive({to.daily(ns_exp_subset_ts_10())})
  ns_exp_dat_ts_10_hourly <- reactive({to.hourly(ns_exp_subset_ts_10())})
  
  ns_exp_dat_ts_11_yearly <- reactive({to.yearly(ns_exp_dat_ts_11())})
  ns_exp_dat_ts_11_monthly <- reactive({to.monthly(ns_exp_subset_ts_11())})
  ns_exp_dat_ts_11_weekly <- reactive({to.weekly(ns_exp_subset_ts_11())})
  ns_exp_dat_ts_11_daily <- reactive({to.daily(ns_exp_subset_ts_11())})
  ns_exp_dat_ts_11_hourly <- reactive({to.hourly(ns_exp_subset_ts_11())})
  
  output$NS_EXP_1_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_yearly()[,(colnames(ns_exp_dat_ts_1_yearly()) %in% c('ns_exp_dat_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_yearly()[,(colnames(ns_exp_dat_ts_1_yearly()) %in% c('ns_exp_dat_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_yearly()[,(colnames(ns_exp_dat_ts_2_yearly()) %in% c('ns_exp_dat_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_yearly()[,(colnames(ns_exp_dat_ts_2_yearly()) %in% c('ns_exp_dat_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_yearly()[,(colnames(ns_exp_dat_ts_3_yearly()) %in% c('ns_exp_dat_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_yearly()[,(colnames(ns_exp_dat_ts_3_yearly()) %in% c('ns_exp_dat_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_weekly()[,(colnames(ns_exp_dat_ts_1_weekly()) %in% c('ns_exp_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_weekly()[,(colnames(ns_exp_dat_ts_1_weekly()) %in% c('ns_exp_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_weekly()[,(colnames(ns_exp_dat_ts_2_weekly()) %in% c('ns_exp_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_weekly()[,(colnames(ns_exp_dat_ts_2_weekly()) %in% c('ns_exp_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_weekly()[,(colnames(ns_exp_dat_ts_3_weekly()) %in% c('ns_exp_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_weekly()[,(colnames(ns_exp_dat_ts_3_weekly()) %in% c('ns_exp_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_daily()[,(colnames(ns_exp_dat_ts_1_daily()) %in% c('ns_exp_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_daily()[,(colnames(ns_exp_dat_ts_1_daily()) %in% c('ns_exp_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_daily()[,(colnames(ns_exp_dat_ts_2_daily()) %in% c('ns_exp_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_daily()[,(colnames(ns_exp_dat_ts_2_daily()) %in% c('ns_exp_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_daily()[,(colnames(ns_exp_dat_ts_3_daily()) %in% c('ns_exp_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_daily()[,(colnames(ns_exp_dat_ts_3_daily()) %in% c('ns_exp_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_1_hourly()[,(colnames(ns_exp_dat_ts_1_hourly()) %in% c('ns_exp_subset_ts_1().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_1_hourly()[,(colnames(ns_exp_dat_ts_1_hourly()) %in% c('ns_exp_subset_ts_1().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_2_hourly()[,(colnames(ns_exp_dat_ts_2_hourly()) %in% c('ns_exp_subset_ts_2().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_2_hourly()[,(colnames(ns_exp_dat_ts_2_hourly()) %in% c('ns_exp_subset_ts_2().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_3_hourly()[,(colnames(ns_exp_dat_ts_3_hourly()) %in% c('ns_exp_subset_ts_3().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_3_hourly()[,(colnames(ns_exp_dat_ts_3_hourly()) %in% c('ns_exp_subset_ts_3().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_1_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_1(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_2(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_3(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$NS_EXP_2_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_yearly()[,(colnames(ns_exp_dat_ts_4_yearly()) %in% c('ns_exp_dat_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_yearly()[,(colnames(ns_exp_dat_ts_4_yearly()) %in% c('ns_exp_dat_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_yearly()[,(colnames(ns_exp_dat_ts_5_yearly()) %in% c('ns_exp_dat_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_yearly()[,(colnames(ns_exp_dat_ts_5_yearly()) %in% c('ns_exp_dat_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_yearly()[,(colnames(ns_exp_dat_ts_6_yearly()) %in% c('ns_exp_dat_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_yearly()[,(colnames(ns_exp_dat_ts_6_yearly()) %in% c('ns_exp_dat_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_weekly()[,(colnames(ns_exp_dat_ts_4_weekly()) %in% c('ns_exp_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_weekly()[,(colnames(ns_exp_dat_ts_4_weekly()) %in% c('ns_exp_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_weekly()[,(colnames(ns_exp_dat_ts_5_weekly()) %in% c('ns_exp_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_weekly()[,(colnames(ns_exp_dat_ts_5_weekly()) %in% c('ns_exp_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_weekly()[,(colnames(ns_exp_dat_ts_6_weekly()) %in% c('ns_exp_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_weekly()[,(colnames(ns_exp_dat_ts_6_weekly()) %in% c('ns_exp_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_daily()[,(colnames(ns_exp_dat_ts_4_daily()) %in% c('ns_exp_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_daily()[,(colnames(ns_exp_dat_ts_4_daily()) %in% c('ns_exp_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_daily()[,(colnames(ns_exp_dat_ts_5_daily()) %in% c('ns_exp_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_daily()[,(colnames(ns_exp_dat_ts_5_daily()) %in% c('ns_exp_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_daily()[,(colnames(ns_exp_dat_ts_6_daily()) %in% c('ns_exp_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_daily()[,(colnames(ns_exp_dat_ts_6_daily()) %in% c('ns_exp_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_4_hourly()[,(colnames(ns_exp_dat_ts_4_hourly()) %in% c('ns_exp_subset_ts_4().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_4_hourly()[,(colnames(ns_exp_dat_ts_4_hourly()) %in% c('ns_exp_subset_ts_4().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_5_hourly()[,(colnames(ns_exp_dat_ts_5_hourly()) %in% c('ns_exp_subset_ts_5().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_5_hourly()[,(colnames(ns_exp_dat_ts_5_hourly()) %in% c('ns_exp_subset_ts_5().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_6_hourly()[,(colnames(ns_exp_dat_ts_6_hourly()) %in% c('ns_exp_subset_ts_6().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_6_hourly()[,(colnames(ns_exp_dat_ts_6_hourly()) %in% c('ns_exp_subset_ts_6().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_2_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_4(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_5(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_6(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$NS_EXP_3_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_yearly()[,(colnames(ns_exp_dat_ts_7_yearly()) %in% c('ns_exp_dat_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_yearly()[,(colnames(ns_exp_dat_ts_7_yearly()) %in% c('ns_exp_dat_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_yearly()[,(colnames(ns_exp_dat_ts_8_yearly()) %in% c('ns_exp_dat_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_yearly()[,(colnames(ns_exp_dat_ts_8_yearly()) %in% c('ns_exp_dat_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_yearly()[,(colnames(ns_exp_dat_ts_9_yearly()) %in% c('ns_exp_dat_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_yearly()[,(colnames(ns_exp_dat_ts_9_yearly()) %in% c('ns_exp_dat_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_weekly()[,(colnames(ns_exp_dat_ts_7_weekly()) %in% c('ns_exp_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_weekly()[,(colnames(ns_exp_dat_ts_7_weekly()) %in% c('ns_exp_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_weekly()[,(colnames(ns_exp_dat_ts_8_weekly()) %in% c('ns_exp_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_weekly()[,(colnames(ns_exp_dat_ts_8_weekly()) %in% c('ns_exp_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_weekly()[,(colnames(ns_exp_dat_ts_9_weekly()) %in% c('ns_exp_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_weekly()[,(colnames(ns_exp_dat_ts_9_weekly()) %in% c('ns_exp_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_daily()[,(colnames(ns_exp_dat_ts_7_daily()) %in% c('ns_exp_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_daily()[,(colnames(ns_exp_dat_ts_7_daily()) %in% c('ns_exp_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_daily()[,(colnames(ns_exp_dat_ts_8_daily()) %in% c('ns_exp_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_daily()[,(colnames(ns_exp_dat_ts_8_daily()) %in% c('ns_exp_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_daily()[,(colnames(ns_exp_dat_ts_9_daily()) %in% c('ns_exp_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_daily()[,(colnames(ns_exp_dat_ts_9_daily()) %in% c('ns_exp_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_7_hourly()[,(colnames(ns_exp_dat_ts_7_hourly()) %in% c('ns_exp_subset_ts_7().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_7_hourly()[,(colnames(ns_exp_dat_ts_7_hourly()) %in% c('ns_exp_subset_ts_7().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_8_hourly()[,(colnames(ns_exp_dat_ts_8_hourly()) %in% c('ns_exp_subset_ts_8().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_8_hourly()[,(colnames(ns_exp_dat_ts_8_hourly()) %in% c('ns_exp_subset_ts_8().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_9_hourly()[,(colnames(ns_exp_dat_ts_9_hourly()) %in% c('ns_exp_subset_ts_9().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_9_hourly()[,(colnames(ns_exp_dat_ts_9_hourly()) %in% c('ns_exp_subset_ts_9().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_3_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_7(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_8(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_9(), type = "line", name = "High Load: ", color = "green")
  })
  
  output$NS_EXP_4_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_yearly()[,(colnames(ns_exp_dat_ts_10_yearly()) %in% c('ns_exp_dat_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_yearly()[,(colnames(ns_exp_dat_ts_10_yearly()) %in% c('ns_exp_dat_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_yearly()[,(colnames(ns_exp_dat_ts_11_yearly()) %in% c('ns_exp_dat_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_yearly()[,(colnames(ns_exp_dat_ts_11_yearly()) %in% c('ns_exp_dat_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_weekly()[,(colnames(ns_exp_dat_ts_10_weekly()) %in% c('ns_exp_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_weekly()[,(colnames(ns_exp_dat_ts_10_weekly()) %in% c('ns_exp_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_weekly()[,(colnames(ns_exp_dat_ts_11_weekly()) %in% c('ns_exp_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_weekly()[,(colnames(ns_exp_dat_ts_11_weekly()) %in% c('ns_exp_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_daily()[,(colnames(ns_exp_dat_ts_10_daily()) %in% c('ns_exp_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_daily()[,(colnames(ns_exp_dat_ts_10_daily()) %in% c('ns_exp_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_daily()[,(colnames(ns_exp_dat_ts_11_daily()) %in% c('ns_exp_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_daily()[,(colnames(ns_exp_dat_ts_11_daily()) %in% c('ns_exp_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(ns_exp_dat_ts_10_hourly()[,(colnames(ns_exp_dat_ts_10_hourly()) %in% c('ns_exp_subset_ts_10().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_10_hourly()[,(colnames(ns_exp_dat_ts_10_hourly()) %in% c('ns_exp_subset_ts_10().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_add_series(ns_exp_dat_ts_11_hourly()[,(colnames(ns_exp_dat_ts_11_hourly()) %in% c('ns_exp_subset_ts_11().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(ns_exp_dat_ts_11_hourly()[,(colnames(ns_exp_dat_ts_11_hourly()) %in% c('ns_exp_subset_ts_11().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$NS_EXP_4_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(ns_exp_subset_ts_10(), type = "line", name = "High Load: ", color = "green")%>%
      hc_add_series(ns_exp_subset_ts_11(), type = "line", name = "High Load: ", color = "green")
  })
  
     
  
  on_date_ind_1_1 <- reactive({paste(input$on_dates_1[1],"00:00:00",sep = " ")})
  on_date_ind_1_2 <- reactive({paste(input$on_dates_1[2],"00:00:00",sep = " ")})
  on_tot_energy_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_1_1() & on_ind_dat$date_time_local <= on_date_ind_1_2()))})
  on_tot_energy_dat_ts <- reactive({xts(on_ind_dat$total_energy,on_ind_dat$date_time_local)})
  on_tot_energy_subset_ts <- reactive({xts(on_tot_energy_subset_dat()$total_energy,on_tot_energy_subset_dat()$date_time_local)})
  on_tot_energy_dat_ts_yearly <- reactive({to.yearly(on_tot_energy_dat_ts())})
  on_tot_energy_dat_ts_monthly <- reactive({to.monthly(on_tot_energy_subset_ts())})
  on_tot_energy_dat_ts_weekly <- reactive({to.weekly(on_tot_energy_subset_ts())})
  on_tot_energy_dat_ts_daily <- reactive({to.daily(on_tot_energy_subset_ts())})
  on_tot_energy_dat_ts_hourly <- reactive({to.hourly(on_tot_energy_subset_ts())})
  
  output$ON_TOT_ENERGY_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_yearly()[,(colnames(on_tot_energy_dat_ts_yearly()) %in% c('on_tot_energy_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_yearly()[,(colnames(on_tot_energy_dat_ts_yearly()) %in% c('on_tot_energy_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_weekly()[,(colnames(on_tot_energy_dat_ts_weekly()) %in% c('on_tot_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_weekly()[,(colnames(on_tot_energy_dat_ts_weekly()) %in% c('on_tot_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_daily()[,(colnames(on_tot_energy_dat_ts_daily()) %in% c('on_tot_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_daily()[,(colnames(on_tot_energy_dat_ts_daily()) %in% c('on_tot_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_energy_dat_ts_hourly()[,(colnames(on_tot_energy_dat_ts_hourly()) %in% c('on_tot_energy_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_energy_dat_ts_hourly()[,(colnames(on_tot_energy_dat_ts_hourly()) %in% c('on_tot_energy_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_ENERGY_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_energy_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  on_date_ind_2_1 <- reactive({paste(input$on_dates_2[1],"00:00:00",sep = " ")})
  on_date_ind_2_2 <- reactive({paste(input$on_dates_2[2],"00:00:00",sep = " ")})
  on_tot_loss_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_2_1() & on_ind_dat$date_time_local <= on_date_ind_2_2()))})
  on_tot_loss_dat_ts <- reactive({xts(on_ind_dat$total_loss,on_ind_dat$date_time_local)})
  on_tot_loss_subset_ts <- reactive({xts(on_tot_loss_subset_dat()$total_loss,on_tot_loss_subset_dat()$date_time_local)})
  on_tot_loss_dat_ts_yearly <- reactive({to.yearly(on_tot_loss_dat_ts())})
  on_tot_loss_dat_ts_monthly <- reactive({to.monthly(on_tot_loss_subset_ts())})
  on_tot_loss_dat_ts_weekly <- reactive({to.weekly(on_tot_loss_subset_ts())})
  on_tot_loss_dat_ts_daily <- reactive({to.daily(on_tot_loss_subset_ts())})
  on_tot_loss_dat_ts_hourly <- reactive({to.hourly(on_tot_loss_subset_ts())})
  
  output$ON_TOT_LOSS_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_yearly()[,(colnames(on_tot_loss_dat_ts_yearly()) %in% c('on_tot_loss_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_yearly()[,(colnames(on_tot_loss_dat_ts_yearly()) %in% c('on_tot_loss_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_weekly()[,(colnames(on_tot_loss_dat_ts_weekly()) %in% c('on_tot_loss_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_weekly()[,(colnames(on_tot_loss_dat_ts_weekly()) %in% c('on_tot_loss_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_daily()[,(colnames(on_tot_loss_dat_ts_daily()) %in% c('on_tot_loss_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_daily()[,(colnames(on_tot_loss_dat_ts_daily()) %in% c('on_tot_loss_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_loss_dat_ts_hourly()[,(colnames(on_tot_loss_dat_ts_hourly()) %in% c('on_tot_loss_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_loss_dat_ts_hourly()[,(colnames(on_tot_loss_dat_ts_hourly()) %in% c('on_tot_loss_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOSS_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_loss_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  on_date_ind_3_1 <- reactive({paste(input$on_dates_3[1],"00:00:00",sep = " ")})
  on_date_ind_3_2 <- reactive({paste(input$on_dates_3[2],"00:00:00",sep = " ")})
  on_tot_load_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_3_1() & on_ind_dat$date_time_local <= on_date_ind_3_2()))})
  on_tot_load_dat_ts <- reactive({xts(on_ind_dat$total_load,on_ind_dat$date_time_local)})
  on_tot_load_subset_ts <- reactive({xts(on_tot_load_subset_dat()$total_load,on_tot_load_subset_dat()$date_time_local)})
  on_tot_load_dat_ts_yearly <- reactive({to.yearly(on_tot_load_dat_ts())})
  on_tot_load_dat_ts_monthly <- reactive({to.monthly(on_tot_load_subset_ts())})
  on_tot_load_dat_ts_weekly <- reactive({to.weekly(on_tot_load_subset_ts())})
  on_tot_load_dat_ts_daily <- reactive({to.daily(on_tot_load_subset_ts())})
  on_tot_load_dat_ts_hourly <- reactive({to.hourly(on_tot_load_subset_ts())})
  
  output$ON_TOT_LOAD_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_yearly()[,(colnames(on_tot_load_dat_ts_yearly()) %in% c('on_tot_load_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_yearly()[,(colnames(on_tot_load_dat_ts_yearly()) %in% c('on_tot_load_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_weekly()[,(colnames(on_tot_load_dat_ts_weekly()) %in% c('on_tot_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_weekly()[,(colnames(on_tot_load_dat_ts_weekly()) %in% c('on_tot_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_daily()[,(colnames(on_tot_load_dat_ts_daily()) %in% c('on_tot_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_daily()[,(colnames(on_tot_load_dat_ts_daily()) %in% c('on_tot_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_load_dat_ts_hourly()[,(colnames(on_tot_load_dat_ts_hourly()) %in% c('on_tot_load_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_load_dat_ts_hourly()[,(colnames(on_tot_load_dat_ts_hourly()) %in% c('on_tot_load_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_LOAD_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_load_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  on_date_ind_4_1 <- reactive({paste(input$on_dates_4[1],"00:00:00",sep = " ")})
  on_date_ind_4_2 <- reactive({paste(input$on_dates_4[2],"00:00:00",sep = " ")})
  on_tot_demand_subset_dat <- reactive({subset(on_ind_dat,subset = (on_ind_dat$date_time_local >= on_date_ind_4_1() & on_ind_dat$date_time_local <= on_date_ind_4_2()))})
  on_tot_demand_dat_ts <- reactive({xts(on_ind_dat$ontario_demand,on_ind_dat$date_time_local)})
  on_tot_demand_subset_ts <- reactive({xts(on_tot_demand_subset_dat()$ontario_demand,on_tot_demand_subset_dat()$date_time_local)})
  on_tot_demand_dat_ts_yearly <- reactive({to.yearly(on_tot_demand_dat_ts())})
  on_tot_demand_dat_ts_monthly <- reactive({to.monthly(on_tot_demand_subset_ts())})
  on_tot_demand_dat_ts_weekly <- reactive({to.weekly(on_tot_demand_subset_ts())})
  on_tot_demand_dat_ts_daily <- reactive({to.daily(on_tot_demand_subset_ts())})
  on_tot_demand_dat_ts_hourly <- reactive({to.hourly(on_tot_demand_subset_ts())})
  
  output$ON_TOT_DEMAND_YEARLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_yearly()[,(colnames(on_tot_demand_dat_ts_yearly()) %in% c('on_tot_demand_dat_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_yearly()[,(colnames(on_tot_demand_dat_ts_yearly()) %in% c('on_tot_demand_dat_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_WEEKLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_weekly()[,(colnames(on_tot_demand_dat_ts_weekly()) %in% c('on_tot_demand_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_weekly()[,(colnames(on_tot_demand_dat_ts_weekly()) %in% c('on_tot_demand_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_DAILY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_daily()[,(colnames(on_tot_demand_dat_ts_daily()) %in% c('on_tot_demand_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_daily()[,(colnames(on_tot_demand_dat_ts_daily()) %in% c('on_tot_demand_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_HOURLY <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% 
      hc_add_series(on_tot_demand_dat_ts_hourly()[,(colnames(on_tot_demand_dat_ts_hourly()) %in% c('on_tot_demand_subset_ts().High'))], type = "line", name = "High Load: ", color = "green") %>%
      hc_add_series(on_tot_demand_dat_ts_hourly()[,(colnames(on_tot_demand_dat_ts_hourly()) %in% c('on_tot_demand_subset_ts().Low'))], type = "line", name = "Low Load: ", color = "red") %>%
      hc_navigator(enabled = TRUE)})
  output$ON_TOT_DEMAND_ALL <- renderHighchart({
    highchart() %>% 
      hc_xAxis(type = "datetime") %>% hc_navigator(enabled = TRUE) %>% 
      hc_add_series(on_tot_demand_subset_ts(), type = "line", name = "High Load: ", color = "green")
  })
  
  data_dict <- read_xlsx("www/data_dictionay.xlsx", sheet = config$dd_sheet)
  data_dict_df <- as.data.frame(data_dict)
  output$DATA_DICTIONARY_TABLE <- renderDataTable({data_dict_df})
  
  
  
  #Individual Province Visualization
  
  #Button Function
  
  observeEvent(input$button_pei,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "pei")
  })
  
  observeEvent(input$button_NS,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "NS")
  })
  
  observeEvent(input$button_NB,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "NB")
  })
  
  observeEvent(input$button_ON,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "ON")
  })
  
  observeEvent(input$button_AB,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "AB")
  })
  
  observeEvent(input$button_BC,{
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "BC")
  })
  
  observeEvent(input$button_pei_ind_1,{
    createAlert(session, "NB_1", content = paste("please select this table:",config$provinces$NB$table1, sep = " "), style = "info", dismiss = TRUE)
    Sys.sleep(0.5)
    updateTabsetPanel(session = session, inputId = "rted_menu", selected = "Dwn") 
  })
  
  # File Download System
  
  
  observeEvent(input$rted_menu, {
    if(input$rted_menu == "Dwn")
    {
      updateDateRangeInput(session, "nb_date_range",
                           start = head(nbload_data()$Date_time_local,1),
                           min = head(nbload_data()$Date_time_local,1),
                           end = tail(nbload_data()$Date_time_local,1),
                           max = tail(nbload_data()$Date_time_local,1))
      updateDateRangeInput(session, "nb_date_range",
                           start = head(nbload_data()$Date_time_local,1),
                           min = head(nbload_data()$Date_time_local,1),
                           end = tail(nbload_data()$Date_time_local,1),
                           max = tail(nbload_data()$Date_time_local,1))

      
      if(!is.null(input$nb_filter))
        {
        shinyjs::show("nb_dwn_1")
        observeEvent(input$nb_filter,{
          output$data_nb <- downloadHandler(
            
            filename = function() {
              "nb_realtime_alldata_new_brunswick_utc.csv"
            },
            content = function(file)
            {
              showNotification("Your File is Downloading, Please wait for some time.", type="message")
              date_1 <- paste(input$nb_date_range[1],"00:00:00",sep = " ")
              date_2 <- paste(input$nb_date_range[2],"00:00:00",sep = " ")
              nbload_subset_download <- subset(nbload_data(),subset = (nbload_data()$Date_time_local >= date_1 & nbload_data()$Date_time_local <= date_2))
              write.csv(nbload_subset_download, file, row.names = FALSE)
            }
          )
        })
        
      }
    }
    if(input$rted_menu == "pei")
    {
      #updating the dates after the data loads (PEI)
      pei_dd_1 <- as.Date(Sys.Date())-(7)
      pei_dd_1_1 <- paste(pei_dd_1,"00:00:00",sep=" ")
      pei_dd_2_2 <- as.Date(tail(pei_ind_dat$Date_time_local,1))
      updateSliderInput(session, "pei_dates_1",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "pei_dates_2",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "pei_dates_3",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "pei_dates_4",
                        min = as.Date(head(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(pei_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(pei_dd_1_1),pei_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "NB"){
      #updating the dates after the data loads (NB)
      nb_dd_1 <- as.Date(Sys.Date())-(7)
      nb_dd_1_1 <- paste(nb_dd_1,"00:00:00",sep=" ")
      nb_dd_2_2 <- as.Date(tail(nb_ind_dat$Date_time_local,1))
      updateSliderInput(session, "nb_dates_1",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "nb_dates_2",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "nb_dates_3",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "nb_dates_4",
                        min = as.Date(head(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nb_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nb_dd_1_1),nb_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "NS"){
      #updating the dates after the data loads (NB)
      ns_dd_1 <- as.Date(Sys.Date())-(7)
      ns_dd_1_1 <- paste(ns_dd_1,"00:00:00",sep=" ")
      ns_dd_2_2 <- as.Date(tail(ns_ind_dat$Date_time_local,1))
      ns_dd_3_2 <- as.Date(tail(ns_ind_dat_1$Date_time_local,1))
      updateSliderInput(session, "ns_dates_1",
                        min = as.Date(head(ns_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_2",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_3",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_4",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "ns_dates_5",
                        min = as.Date(head(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(ns_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(ns_dd_1_1),ns_dd_3_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "NFL"){
      nfl_dd_1 <- as.Date(Sys.Date())-(7)
      nfl_dd_1_1 <- paste(nfl_dd_1,"00:00:00",sep=" ")
      nfl_dd_2_2 <- as.Date(tail(nfl_ind_dat$Date_time_local,1))
      updateSliderInput(session, "nfl_dates_1",
                        min = as.Date(head(nfl_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(nfl_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(nfl_dd_1_1),nfl_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "BC"){
      bc_dd_1 <- as.Date(Sys.Date())-(7)
      bc_dd_1_1 <- paste(bc_dd_1,"00:00:00",sep=" ")
      bc_dd_2_2 <- as.Date(tail(bc_ind_dat$Date_time_local,1))
      bc_dd_3_2 <- as.Date(tail(bc_ind_dat_1$Date_time_local,1))
      updateSliderInput(session, "bc_dates_1",
                        min = as.Date(head(bc_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(bc_ind_dat$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(bc_dd_1_1),bc_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "bc_dates_2",
                        min = as.Date(head(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(bc_dd_1_1),bc_dd_3_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "bc_dates_3",
                        min = as.Date(head(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(bc_ind_dat_1$Date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(bc_dd_1_1),bc_dd_3_2),
                        timeFormat="%Y-%m-%d")
    }
    if(input$rted_menu == "ON"){
      #updating the dates after the data loads (NB)
      on_dd_1 <- as.Date(Sys.Date())-(7)
      on_dd_1_1 <- paste(on_dd_1,"00:00:00",sep=" ")
      on_dd_2_2 <- as.Date(tail(on_ind_dat$date_time_local,1))
      updateSliderInput(session, "on_dates_1",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "on_dates_2",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "on_dates_3",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
      updateSliderInput(session, "on_dates_4",
                        min = as.Date(head(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        max = as.Date(tail(on_ind_dat$date_time_local,1),"%Y-%m-%d"),
                        value=c(as.Date(on_dd_1_1),on_dd_2_2),
                        timeFormat="%Y-%m-%d")
    }
  })
  
  
  
  
  
 
  
  
  #individual pages
  
}




# Run the application 
shinyApp(ui = ui, server = server)
