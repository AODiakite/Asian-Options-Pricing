
source("functions.R")

library(argonDash)
library(argonR)
library(shiny)
library(shinybusy)


# Define UI -----
ui <- argonDashPage(
  title = "Asian options",
  author = "Diakité & Ettadlaoui",
  description = "Argon Dash Test",
  header = argonDashHeader(
    gradient = TRUE,
    color = "primary",
    separator = TRUE,
    separator_color = "secondary",
    argonH1("Asian options pricing",display = 3)
  ),
  footer = argonDashFooter(copyrights = "@Diakité & Ettadlaoui, 2022",src = "https://github.com/AODiakite/Asian-Options-Pricing"),
  sidebar = argonDashSidebar(
    vertical = T,skin = "dark",
    background = "secondary",size = "md",
    side = "left",
    id = "my_sidebar",
    dropdownMenus = argonDropNav(),
    argonSidebarHeader(title = "Main Menu"),
    argonSidebarDivider(),
    argonSidebarMenu(

      argonSidebarItem(
        tabName = "AssetChoice",
        icon = argonIcon(name = "folder-17", color = "info"),
        "Asset choice"
      ),
      tags$br(),
      argonSidebarItem(
        tabName = "Pricing",
        icon = argonIcon(name = "money-coins", color = "green"),
        "Pricing"
      )
    )
  ),
  body = argonDashBody(
    argonTabItems(
      argonTabItem(
        tabName = "AssetChoice",
        add_busy_spinner(spin = "cube-grid",
                         margins = c(300,600),width = '100px',height = '100px',color="#5e72e4"),
        uiOutput("asset_info"),
        argonCard(
          width = 12,
          src = NULL,
          icon = argonIcon("briefcase-24"),
          status = "primary",
          shadow = TRUE,
          border_level = 0,
          hover_shadow = TRUE,
          title = "Asset choice",
          hover_lift = F,
          argonRow(
            argonColumn(
              width =  9,
              selectInput("asset_selected", "Choose your asset", choices = c(" ",list_instrument()) ,
              multiple = FALSE,width = "100%")
            ),
            argonColumn(
              width =  3,
              actionButton("choice", "Select",
                           style = "margin-top:25px; color: #fff; background-color: #5e72e4; border-color: #5e72e4",width = "100%")
            )
          )
        ),
        argonRow(
          argonColumn(
            width = 12,
            argonTabSet(
              id = "tab-1",
              card_wrapper = TRUE,
              horizontal = TRUE,
              circle = FALSE,
              size = "sm",
              width = 12,
              iconList = list(argonIcon("sound-wave"),argonIcon("chart-bar-32")),
              argonTab(
                tabName = "Plot",
                active = T,
                plotly::plotlyOutput("plot")
              ),
              argonTab(
                tabName = "Data",
                DT::dataTableOutput("data")
              )
            )

            )
        )


      ),
      argonTabItem(
        tabName = "Pricing",
        add_busy_spinner(spin = "cube-grid",
                         margins = c(300,600),width = '100px',height = '100px',color = "#5e72e4"),
        uiOutput("price_ui"),
        argonRow(
          argonColumn(
            width = 6,
            argonCard(
              width = 12,
              src = NULL,
              icon = argonIcon("settings"),
              status = "primary",
              shadow = TRUE,
              border_level = 0,
              hover_shadow = TRUE,
              title = "Pricing settings",
              hover_lift = T,
              fluidRow(
                column(
                  6,
                  selectInput("option","Option",choices = c("Call","Put"))
                ),
                column(
                  6,
                  selectInput(
                    inputId = "mean_type",
                    label = "Averaging",
                    choices = c("Arithmetic","Geometric")
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  numericInput("nsim",
                               "Simulations (nsim)",
                               min = 1,
                               value = 50
                               )
                ),
                column(
                  6,
                  numericInput(
                    inputId = "N",
                    label = "Intervals (N)",
                    min = 1,
                    value = 10000
                  )
                )
              ),
              argonRow(
                argonColumn(
                  width = 6,
                  selectInput(
                    inputId = "strike_type",
                    label = "Type of Strike",
                    choices = c("Fixed","Floting")
                  )
                ),
                argonColumn(
                  width = 6,
                  selectInput(
                    inputId = "schema",
                    label = "Schema",
                    choices = c("Riemann","Trapezoid")
                  )

                )
              ),
              fluidRow(
                column(
                  6,
                  numericInput("strike_value",
                               "Fixed strike (K)",
                               min = 1,
                               value = 0
                  )
                ),
                column(
                  6,
                  numericInput(
                    inputId = "r",
                    label = "Interest rate (r)",
                    min = 0,max = 1,
                    value = 0.05
                  )
                )
              )
            )
          ),
          argonColumn(
            width = 6,
            argonCard(
              width = 12,
              src = NULL,
              icon = icon("cogs"),
              status = "primary",
              shadow = TRUE,
              border_level = 0,
              hover_shadow = TRUE,
              hover_lift = T,shadow_size = 0.7,
              title = "Calculation",
              tags$h3("Maturity"),
              tags$hr(),
              uiOutput("periode"),
              actionButton("pricing_btn","Pricing",
                           icon = argonIcon( "zoom-split-in"),class = "btn-block")
            )
          )
        ),
        argonRow(
          argonColumn(
            width = 12,
            argonTabSet(
              id = "tab-1",
              card_wrapper = TRUE,
              horizontal = TRUE,
              circle = FALSE,
              size = "sm",
              width = 12,
              iconList = list(argonIcon("sound-wave"),icon("chart-line")),
              argonTab(
                tabName = "Generated paths",
                active = T,
                plotOutput("plot_simulated")
              ),
              argonTab(
                tabName = "Medium path",
                plotOutput("plot_sim_mean")
              )
            )
          )
        )
      )
    )
  )
)

# SERVER -----
server <- function(input, output) {
  # geting ticker----
  ticker0 <- reactive(
    {
      get_ticker(input$asset_selected)
    }
  ) %>% bindEvent(input$choice)

  # Download historical data
  df <- reactive(
    {
      casabourse::daily.data(ticker0(),"01-01-2019",My_format(Sys.Date()))
    }
  )

  # Data output-----
  output$data <- DT::renderDataTable(
    {
      df()
    },
  options = list(
    paging = FALSE, ## paginate the output
    pageLength = 10,
    scrollX = F,
    searching = FALSE,
    scrollY = 300
  )
  )
  # Plot output----
  output$plot <- renderPlotly({
    plot_df(df())
  })
  # Drift & sigma ----
  drift <- reactive(
    historical(df())$drift
  )
  Sigma <- reactive(
    historical(df())$sigma
  )
  # Asset info box ------
  output$asset_info <- renderUI({
    argonRow(
      argonColumn(width = 6,center = T,
                  argonInfoCard(width = 12,
                                value = paste0(round(100*drift(),2)," %"),
                                title = "Historical drift",
                                icon = argonIcon("money-coins"),
                                icon_background = "success",
                                shadow = TRUE,
                                gradient = T,
                                background_color = "default",
                                hover_lift = TRUE
                  )

      ),
      argonColumn(width = 6,center = T,
                  argonInfoCard(width = 12,
                                value = paste0(round(100*Sigma(),2)," %"),
                                title = "Historical volatility",
                                icon = argonIcon("spaceship"),
                                icon_background = "danger",
                                gradient = T,
                                background_color = "warning",
                                shadow = TRUE,
                                hover_lift = TRUE
                  )

      )
    )
  })
  # maturity ui
  output$periode <- renderUI({
    argonRow(
      argonColumn(
        width = 6,
        selectInput(
          inputId = "from",
          label = "Start",
          choices = df()[["Date"]]
        )
      ),
      argonColumn(
        width = 6,
        dateInput(
          inputId = "to",
          label = "Expiration"
        )

      )
    )
  })
  #Maturity----
  TT <- reactive({
    as.numeric((ymd(input$to)-ymd(input$from)))/252
  })
  S0 <- reactive({
    s0 =df() %>%
      filter(Date==input$from) %>%
      select(-Date)
    as.numeric(s0)
  })
  #GBM----
  GBM0 = reactive({
    GBM(S0 = S0(),mu = drift(),sigma = Sigma(),
        T_ = TT(),t0 = 0,N = input$N,nsim = input$nsim)
  }) %>% bindEvent(input$pricing_btn)

  output$plot_simulated <- renderPlot({
    assign("price",price(),.GlobalEnv)
    GBM0()[["plots"]]
  })
  output$plot_sim_mean <- renderPlot(
    GBM0()[["plt_mean"]]
  )
  # Pricing ----------
  price <- reactive({
    pricing(Averaging = input$mean_type,Strike_type = input$strike_type,
            Schema = input$schema,gbm = GBM0(),
            S0 = S0(),K = input$strike_value,r = input$r,T_ = TT(),sigma = Sigma())
  })
  # Pricing UI----------
  output$price_ui <- renderUI({
    argonRow(
      argonColumn(width = 6,center = T,
                  argonInfoCard(width = 12,
                                value = switch (input$option,
                                  Call = round(price()$call,3),
                                  Put = round(price()$put,3)
                                ),
                                title = paste0(input$option," price"),
                                icon = argonIcon("money-coins"),
                                icon_background = "success",
                                description = "Asian option",
                                shadow = TRUE,
                                gradient = T,
                                background_color = "default",
                                hover_lift = TRUE
                  )
      ),
      argonColumn(width = 6,center = T,
                  argonInfoCard(width = 12,
                                value = switch(
                                  input$mean_type,
                                  Arithmetic =round(GBM0()[["ST_schema1"]],3),
                                  Geometric = round(GBM0()[["ST_schema2"]],3)
                                ),
                                title = "Average with the Monte Carlo method",
                                icon = argonIcon("spaceship"),
                                icon_background = "info",
                                description = input$mean_type,
                                gradient = T,
                                background_color = "default",
                                shadow = TRUE,
                                hover_lift = TRUE
                  )

      )
    )

  })
}

# Run the application
shinyApp(ui = ui, server = server)
