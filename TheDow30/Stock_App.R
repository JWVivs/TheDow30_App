library(shiny)
library(ggplot2)
library(dplyr)
library(quantmod)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(plotly)
# devtools::install_github('ropensci/plotly')

tickers <- c("AXP", "AAPL", "BA", "CAT", "CSCO", "CVX", "XOM", "GS", "HD",
             "IBM", "INTC", "JNJ", "KO", "JPM", "MCD", "MMM", "MRK", "MSFT",
             "NKE", "PFE", "PG", "TRV", "UNH", "UTX", "VZ", "V", "WBA", "WMT",
             "DIS", "DOW")

# Generating a list of The Dow 30
dataEnv <- new.env()
getSymbols(tickers, from = "2019-02-24", to = "2020-02-24", env = dataEnv)
plist <- eapply(dataEnv, Ad)
pframe <- do.call(merge, plist)

# Converting list to a dataframe
data.frame(date = index(pframe), coredata(pframe)) -> df

# Collapsing each stock into one column
df %>%
    gather(., value = "price", key = stock, -date) -> df1

# Define UI for Stock app ----
ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Dashboard"),
                    # Sidebar content
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Application", tabName = "application", icon = icon("chart-line")),
                            menuItem("App Info", tabName = "appinfo", icon = icon("info-circle"))
                        )
                    ),
                    # Body content
                    dashboardBody(
                        tabItems(
                            # First tab content
                            tabItem(tabName = "application"),
                            # Second tab content
                            tabItem(tabName = "appinfo",
                                    h2("How to Use the App"),
                                    h3("Click on the drop-down menu to select one of the stocks featured in The Dow 30. Once selected, a scatterplot will
                 be generated, which features data regarding the stock's closing price from February 24th 2019 to February 24th 2020.
                 If you would like to view a specific timeframe, select one of the four available options from the rangeslider. You can click
                 and drag the slider to view a specific window of time. By clicking 'Reset', you will be able to view the entirety of the data again."))
                        ),
                        fluidPage(
                            
                            # App title
                            titlePanel("The Dow 30"),
                            
                            # Sidebar layout with input and output definitions
                            sidebarLayout(
                                
                                # Sidebar panel for inputs
                                sidebarPanel(
                                    
                                    # Input: Selector for variable (stock) to plot against date
                                    selectInput("stock", "Stock:",
                                                c("WBA" = "WBA.Adjusted",
                                                  "XOM" = "XOM.Adjusted",
                                                  "JNJ" = "JNJ.Adjusted",
                                                  "JPM" = "JPM.Adjusted",
                                                  "UTX" = "UTX.Adjusted",
                                                  "VZ" = "VZ.Adjusted",
                                                  "INTC" = "INTC.Adjusted",
                                                  "AXP" = "AXP.Adjusted",
                                                  "MMM" = "MMM.Adjusted",
                                                  "MCD" = "MCD.Adjusted",
                                                  "CSCO" = "CSCO.Adjusted",
                                                  "PFE" = "PFE.Adjusted",
                                                  "V" = "V.Adjusted",
                                                  "CVX" = "CVX.Adjusted",
                                                  "CAT" = "CAT.Adjusted",
                                                  "MRK" = "MRK.Adjusted",
                                                  "MSFT" = "MSFT.Adjusted",
                                                  "TRV" = "TRV.Adjusted",
                                                  "NKE" = "NKE.Adjusted",
                                                  "KO" = "KO.Adjusted",
                                                  "DOW" = "DOW.Adjusted",
                                                  "PG" = "PG.Adjusted",
                                                  "WMT" = "WMT.Adjusted",
                                                  "DIS" = "DIS.Adjusted",
                                                  "UNH" = "UNH.Adjusted",
                                                  "GS" = "GS.Adjusted",
                                                  "AAPL" = "AAPL.Adjusted",
                                                  "HD" = "HD.Adjusted",
                                                  "BA" = "BA.Adjusted",
                                                  "IBM" = "IBM.Adjusted"))
                                    
                                ),
                                
                                # Main panel for displaying outputs
                                mainPanel(
                                    
                                    # Output: Formatted text for caption
                                    h3(textOutput("caption")),
                                    
                                    # Output: Plot of the requested variable (stock) against date
                                    plotlyOutput("datePlot")
                                    
                                )
                            )
                        )))

server <- function(input, output) {
    stock.data <- reactive({
        df1 %>%
            dplyr::filter(., stock == input$stock)
    })
    
    
    print(stock.data)
    
    output$datePlot <- renderPlotly({
        plot_ly(stock.data(), x = ~date, y = ~price,
                type = "scatter",
                mode = "lines+markers",
                marker = list(size = 4,
                              color = 'rgb(0, 0, 0)'),
                line = list(
                    color = 'rgba(0, 204, 51, 1)')) %>%
            layout(
                title = "Closing Stock Prices from February 24th 2019 to February 24th 2020",
                xaxis = list(title = "Date",
                             rangeselector = list(visible = TRUE, x = 0.5, y = -0.75,
                                                  xanchor = 'center', yref = 'paper',
                                                  buttons = list(
                                                      list(
                                                          count = 7,
                                                          label = "1 Week",
                                                          step = "day",
                                                          stepmode = "backward"),
                                                      list(
                                                          count = 1,
                                                          label = "1 Month",
                                                          step = "month",
                                                          stepmode = "backward"),
                                                      list(
                                                          count = 3,
                                                          label = "3 Month",
                                                          step = "month",
                                                          stepmode = "backward"),
                                                      list(step = "all",
                                                           label = "Reset"))),
                             
                             rangeslider = list(type = "date")),
                
                yaxis = list(title = "Price"))
        
    })
}

shinyApp(ui = ui, server = server)