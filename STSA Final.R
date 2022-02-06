### TITOLI FINALI

rm(list=ls())
graphics.off()
cat("\014")

# BISF STAGE - ERMELLINO ANDREA
# Load R packages
library(shiny)
library(shinythemes)
library(shinyWidgets) 
library(tseries)
library(plotrix) 
library(dygraphs)
library(quantmod) 


start <- "2005-01-01"

###   Calcolo dei ritorni
getSymbols( "AAPL", source="yahoo", from=start)
getSymbols( "BA", source="yahoo", from=start)
getSymbols( "NEE", source="yahoo", from=start)
getSymbols( "AMZN", source="yahoo", from=start)
getSymbols( "PFE", source="yahoo", from=start)
getSymbols( "XOM", source="yahoo", from=start)
getSymbols( "BAC", source="yahoo", from=start)
getSymbols( "KO", source="yahoo", from=start)
getSymbols( "GOOG", source="yahoo", from=start)
getSymbols( "CSCO", source="yahoo", from=start)

# Impostiamo l'ultimo giorno prima della costituzione del portafoglio (TAB PORTFOLIO SELECTION)
yesterday_1 <- "2020-12-31"
# Impostiamo l'ultimo giorno prima della costituzione del portafoglio (TAB TRAINING & TESTING STRATEGIES)
yesterday <- "2015-12-31"
fine_ritorni <- "2020-12-31" 


AAPL.r  <- periodReturn( x=AAPL, period="yearly", subset=paste0("/",fine_ritorni) )
BA.r    <- periodReturn( x=BA, period="yearly", subset=paste0("/",fine_ritorni) )
NEE.r   <- periodReturn(  x=NEE, period="yearly", subset=paste0("/",fine_ritorni) )
AMZN.r  <- periodReturn( x=AMZN, period="yearly", subset=paste0("/",fine_ritorni) )
PFE.r   <- periodReturn( x=PFE, period="yearly", subset=paste0("/",fine_ritorni) )
XOM.r   <- periodReturn(  x=XOM, period="yearly", subset=paste0("/",fine_ritorni) )
BAC.r   <- periodReturn(  x=BAC, period="yearly", subset=paste0("/",fine_ritorni) )
KO.r    <- periodReturn( x=KO, period="yearly", subset=paste0("/",fine_ritorni) )
GOOG.r  <- periodReturn( x=GOOG, period="yearly", subset=paste0("/",fine_ritorni) )
CSCO.r  <- periodReturn(  x=CSCO, period="yearly", subset=paste0("/",fine_ritorni) )

all_returns <- cbind( AAPL.r, BA.r, NEE.r, AMZN.r, PFE.r, XOM.r, BAC.r, KO.r, GOOG.r, CSCO.r)
names(all_returns) <- c( "AAPL", "BA", "NEE", "AMZN", "PFE", "XOM", "BAC", "KO", "GOOG", "CSCO")
all_stocks <- cbind(AAPL, BA, NEE, AMZN, PFE, XOM, BAC, KO, GOOG, CSCO)
tables_stocks <<- NULL


all_returns2 <- all_returns
all_stocks2 <- all_stocks




# Define UI
ui <- fluidPage(theme = shinytheme("united"),
                navbarPage(title = "Shiny Trading Strategy Analyzer",
                           
                           tabPanel("Portfolio Selection", 
                                    
                                    sidebarLayout( 
                                      sidebarPanel(h3(strong("Consultazione Stock")), 
                                                   br(),
                                                   selectInput("select", label = "Titoli di default",
                                                               choices = list("AAPL", "BA", "NEE", "AMZN", "PFE", "XOM", "BAC", "KO", "GOOG", "CSCO"),
                                                               selected = 1),
                                                   #submitButton("Submit"),
                                                   hr(),
                                                   fluidRow(column(3, verbatimTextOutput("value"))),
                                                   hr(),
                                                   textInput("altro_titolo", label = "Altro Titolo", value = ""),
                                                   actionBttn("aggiungi_titolo", label = "Aggiungi"),
                                                   uiOutput("titoli_aggiuntivi")
                                                   
                                      ),
                                      mainPanel("",
                                                dygraphOutput(outputId = "trend_plot"))
                                      
                                    ), # end siderbarLayout
                                    br(),
                                    br(),
                                    
                                    sidebarLayout( 
                                      sidebarPanel(h3(strong("Composizione del Portafoglio")), 
                                                   br(),
                                                   dateRangeInput(inputId = "data_range_input", 
                                                                  label = "Seleziona la finestra temporale", 
                                                                  start = '2007-01-01', 
                                                                  end = '2021-01-01'),
                                                   br(),
                                                   
                                                   #pickerInput(
                                                    # inputId = "id", label = "Seleziona gli Stocks desiderati",
         #vecchia selezione asset per               # choices = c("AAPL", "BA", "NEE", "AMZN", "PFE", "XOM", "BAC", "KO", "GOOG", "CSCO"),
         #costituzione portafoglio                  # selected = c("AAPL", "BA", "NEE", "AMZN", "PFE", "XOM", "BAC", "KO", "GOOG", "CSCO"),
                                                    # options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                                                    #                `count-selected-text` = "{0}/{1} stock"),
                                                    # multiple = TRUE
                                                   #),
         
                                                   uiOutput("select_stocks"), #nuova selezione asset
                                                   verbatimTextOutput(outputId = "stocks"),
                                                   br(),
                                                   
                                                   numericInput("num", label = "Inserisci il Budget", value = 100000),
                                                   #submitButton("Clicca per ottimizzare secondo Markowitz"),
                                                   fluidRow(column(3, verbatimTextOutput("num"))),
                                                   
                                      ),
                                      
                                      mainPanel("",
                                                dygraphOutput(outputId = "returns_plot")), 
                                      
                                    ), # end siderbarLayout
                                    br(),
                                    sidebarLayout( 
                                      sidebarPanel(h3(strong("Ottimizzazione del Portafoglio")), 
                                                   h4("secondo il modello di Markowitz"),
                                                   br(),
                                                   h5("Costituzione del portafogli: inizio 2021"),
                                                   br(),
                                                   verbatimTextOutput(outputId = "portfolio"),
                                                   
                                      ),
                                      
                                      mainPanel("",
                                                plotOutput(outputId = "pie_plot")),
                                      
                                    ) # end siderbarLayout
                                    
                           ), # end first tabPanel
                           
                           
                           
                           tabPanel("Training Strategies", 
                                    
                                    dateRangeInput(inputId = "finestra_temporale", 
                                                   label = "Finestra temporale per entrambe le strategie di investimento", 
                                                   start = '2007-01-01', 
                                                   end = '2021-01-01'),
                                    br(),
                                    
                                    sidebarLayout( 
                                      sidebarPanel(h3(strong("BUY & HOLD STRATEGY")), 
                                                   br(),
                                                   selectInput("period", label = "Hold Period",
                                                               choices = list("1 year" = 1, "2 years" = 2, "3 years" = 3, "4 years" = 4, "5 years" = 5),
                                                               selected = 1),
                                                   hr(),
                                                   fluidRow(column(3, verbatimTextOutput("years"))),
                                                   h5("Costituzione del portafogli: 31 Dicembre 2015"),
                                                   
                                      ),
                                      mainPanel(verbatimTextOutput(outputId = "portfolio_Buy_Hold")
                                      )
                                      
                                    ), # chiusura siderbarPanel della Buy & Hold Strategy
                                    
                                    sidebarLayout( 
                                      sidebarPanel(h3(strong("REBALANCING STRATEGY")), 
                                                   "(con ribilanciamento ogni anno)",
                                                   br(),
                                                   br(),
                                                   selectInput("period_2", label = "Hold Period",
                                                               choices = list("1 year" = 1, "2 years" = 2, "3 years" = 3, "4 years" = 4, "5 years" = 5),
                                                               selected = 1),
                                                   hr(),
                                                   fluidRow(column(3, verbatimTextOutput("years_2"))),
                                                   h5("Costituzione del portafogli: 31 Dicembre 2015"),
                                                   
                                      ),
                                      mainPanel(verbatimTextOutput(outputId = "portfolio_Rebalancing"))
                                      
                                      
                                    ), # chiusura siderbarPanel della Rebalancing Strategy
                                    
                                    radioButtons("radio", label = h3("STRATEGY SELECTION"),
                                                 choices = list("Buy & Hold Strategy" = 1, "Rebalancing Strategy" = 2), 
                                                 selected = 1),
                                    
                                    hr(),
                                    fluidRow(column(3, verbatimTextOutput("strategia_selezionata")))
                                    
                                    
                           ),    # chiusura Tab Panel delle Training Strategies
                           
                           
                           
                           tabPanel("Testing Strategies", 
                                    
                                    sidebarLayout( 
                                      sidebarPanel(h3(strong("BUY & HOLD STRATEGY"))),
                                      
                                      mainPanel(verbatimTextOutput(outputId = "testing_Buy_Hold")
                                      )
                                      
                                    ), # chiusura siderbarPanel della Buy & Hold Strategy
                                    
                                    sidebarLayout( 
                                      sidebarPanel(h3(strong("REBALANCING STRATEGY"))), 
                                      
                                      mainPanel(verbatimTextOutput(outputId = "testing_Rebalancing"))
                                      
                                      
                                    ), # chiusura siderbarPanel della Rebalancing Strategy
                                    
                                    #   QUI STAMPA SE E' STATA LA STRATEGIA MIGLIORE OPPURE No
                                    #("Hai selezionato la seguente strategia: " )
                                    
                           )    # chiusura Tab Panel della Testing Strategies
                           
                           
                ) # navbarPage
) # fluidPage





# Define server function  
server <- function(input, output, session) {
  
  #1 TAB  
  # Stock scelto per il trend plot
  #output$value <- renderPrint({input$select})  
  
  
  APP_STORAGE <- reactiveValues(list_data_stocks = list(), shares_portfolio = c())  # <-- insert in this variable all the data you want to use for plots in output
  TITOLI_AGGIUNTI <- reactiveValues(list_data_stocks = list(), shares_portfolio = c())
  ALL <- reactiveValues(returns = all_returns2, stocks = all_stocks2 )
  PORTFOLIO <- reactiveValues(stocks = c(), returns = c())
  
  observeEvent(input$select, {
    
    # Download the input selected stock from tseries library with yahoo wrapper
    APP_STORAGE$list_data_stocks[[input$select]] <- get.hist.quote(instrument=input$select, start="2005-01-01", end=Sys.Date(),
                                                                   quote="AdjClose", provider="yahoo", origin="1970-01-01",
                                                                   compression="m", retclass="zoo")
    #print(input$select)
    #print("List of stocks already downloaded: ")
    #print(as.character(names(APP_STORAGE$list_data_stocks)))
    #print(head(APP_STORAGE$list_data_stocks[[input$select]],10))
    
  })
  
  observeEvent(input$aggiungi_titolo, {
    
    APP_STORAGE$list_data_stocks[[input$altro_titolo]] <- get.hist.quote(instrument = input$altro_titolo, start="2005-01-01", end=Sys.Date(),
                                                                         quote="AdjClose", provider="yahoo", origin="1970-01-01",
                                                                         compression="m", retclass="zoo")
   
    TITOLI_AGGIUNTI$list_data_stocks[[input$altro_titolo]] <- getSymbols(input$altro_titolo, src="yahoo", auto.assign = F, from=start)
    
    
    
    #print(input$altro_titolo)
    #print("List of stocks already downloaded: ")
    #print(as.character(names(APP_STORAGE$list_data_stocks)))
    #print(head(APP_STORAGE$list_data_stocks[[input$altro_titolo]],10))
  
    #print(head(TITOLI_AGGIUNTI$list_data_stocks[[input$altro_titolo]],10))
    
    
    nuovo_titolo.r <- periodReturn( x=TITOLI_AGGIUNTI$list_data_stocks[[input$altro_titolo]], period="yearly", subset=paste0("/",fine_ritorni) )
    #print(head(nuovo_titolo.r))
    #print(head(ALL$returns))
    
    
    ALL$returns <- cbind(ALL$returns, nuovo_titolo.r)
    colnames(ALL$returns)[length(colnames(ALL$returns))] <- input$altro_titolo
    print(head(ALL$returns))
    ALL$stocks <- cbind(ALL$stocks, TITOLI_AGGIUNTI$list_data_stocks[[input$altro_titolo]])
    
    nuovo_titolo.r <- ALL$returns[,input$altro_titolo]
    
    count_missing <- 0
    for (h in 1:length(nuovo_titolo.r)){
      if(is.na(nuovo_titolo.r[h,1]))
        count_missing <- count_missing + 1
    }
    print(count_missing)
    k <- 3
    print(paste("Lunghezza ritorni:",length(nuovo_titolo.r)))
    if(count_missing > 0 && count_missing < (length(nuovo_titolo.r)*0.5)){
      
      Dist <- as.matrix(dist(cbind(nuovo_titolo.r[(count_missing+1):length(nuovo_titolo.r)], ALL$returns[(count_missing+1):length(nuovo_titolo.r),])))
      k.ix <- order(Dist[-1,1])[1:k]
      
      pred <- apply( ALL$returns[1:count_missing, k.ix], 1, mean ) # <- volendo si potrebbe rimpiazzare la media con la mediana
      
      # rescaling sul range iniziale della serie target
      pred <- pred * (max(nuovo_titolo.r[(count_missing+1):length(nuovo_titolo.r)])-min(nuovo_titolo.r[(count_missing+1):length(nuovo_titolo.r)])) + min(nuovo_titolo.r[(count_missing+1):length(nuovo_titolo.r)])
      
      # ricostruzione della serie target
      nuovo_titolo.r <- c( pred, nuovo_titolo.r[(count_missing+1):length(nuovo_titolo.r)] )
      print("FILLED")
      print(nuovo_titolo.r)
    }  
    
    
    ALL$returns[,input$altro_titolo] <- nuovo_titolo.r
    colnames(ALL$returns)[length(colnames(ALL$returns))] <- input$altro_titolo
    print(head(ALL$returns))
    ALL$stocks <- cbind(ALL$stocks, TITOLI_AGGIUNTI$list_data_stocks[[input$altro_titolo]])
  })
  

          
  observeEvent(input$aggiungi_titolo, {
    output$titoli_aggiuntivi <- renderUI({
      selectInput("select2", 
                  label = "Titoli aggiuntivi scarticati:", 
                  choices = names(TITOLI_AGGIUNTI$list_data_stocks), 
                  selected = last(names(TITOLI_AGGIUNTI$list_data_stocks)))
    })
  })        
  
  
  
  
  # Line plot of the selected stock from UI interface
  # Trend Stock
  observeEvent(input$select, {
  output$trend_plot <- renderDygraph({
    dygraph(APP_STORAGE$list_data_stocks[[input$select]],
            main = paste(input$select)) %>% 
      dyRangeSelector() 
  })
})
  
  observeEvent(input$select2, {
    output$trend_plot <- renderDygraph({
      dygraph(APP_STORAGE$list_data_stocks[[input$select2]],
              main = paste(input$select2)) %>% 
        dyRangeSelector() 
    })
  })
  
  #2 TAB
  # Data range
  output$dateRangeInput_out <- renderText({ paste ("Date Range Selected: [", input$data_range_input[1], "-", input$data_range_input[2],"]")})
  
  
  # NUOVA Scelta Asset PICKER INPUT
  output$select_stocks <- renderUI({
      pickerInput(
         inputId = "id", label = "Seleziona gli Stocks desiderati",
         choices = c("AAPL", "BA", "NEE", "AMZN", "PFE", "XOM", "BAC", "KO", "GOOG", "CSCO", names(TITOLI_AGGIUNTI$list_data_stocks)),
         selected = c("AAPL", "BA", "NEE", "AMZN", "PFE", "XOM", "BAC", "KO", "GOOG", "CSCO", names(TITOLI_AGGIUNTI$list_data_stocks)),
         options = list(`actions-box` = TRUE, `selected-text-format` = "count > 2",
                        `count-selected-text` = "{0}/{1} stock"),
         multiple = TRUE
        )
  })
  
  # VECCHIA Scelta Asset PICKER INPUT
  #output$stocks <- renderPrint({
  #  input$id
  #})
  
  # Line plot of the selected stock from UI interface
  # Returns Plot
  ## NB all_returns is xts/zoo time series class -> becareful to retrieve the information from this data structure
  ## use information of "data_range_input" for modifing the time period considered into the returns plot
  
  
  #VECCHIO DYGRAPH PORTAFOGLIO
  #output$returns_plot <- renderDygraph({
  #  dygraph( all_returns[paste0(input$data_range_input[1],"/",input$data_range_input[2]),input$id], main="Returns Plot") %>% 
  #    dyRangeSelector() 
  #})
  
  
    output$returns_plot <- renderDygraph({
      dygraph( ALL$returns[paste0(input$data_range_input[1],"/",input$data_range_input[2]),input$id], main="Returns Plot") %>% 
        dyRangeSelector() 
    })

 
  
  
  
  
  
  
  
  
  #3 TAB 
    
  #il portafoglio può essere costituito da massimo 13 titoli
    


  observeEvent(input$data_range_input, {  
  output$portfolio <- renderPrint({
    validate(need(as.numeric(as.Date(input$data_range_input[2]) - as.Date(input$data_range_input[1])) >= 4018, "Selezionare una finestra temporale di almeno 11 anni"))
    ### Selects only the adjusted close price for the selected stocks inserted by the user with variable "input$id"
    stocks <- NULL
    for (stock_i in input$id){
      stocks <- cbind(stocks, ALL$stocks[,paste0(stock_i,".Adjusted")])
      PORTFOLIO$stocks <- stocks
    }
    
    validate(need(ncol(stocks) <= 13, "Il portafoglio può essere composto da massimo 13 titoli"))
    # Markowitz optimal portfolio con impostazioni di default
    Mop <- portfolio.optim( x=ALL$returns[paste0(input$data_range_input[1],"/",input$data_range_input[2]),input$id])
    # $pw sta per Portfolio Weights, sono i pesi. La somma deve fare 1 (100%)
    # $px sono i ritorni del portafoglio con i pesi sopra indicati per ciascun anno
    # $pm = RITORNO ATTESO DEL PORTAFOGLIO
    # $ps = RISCHIO 
    
    for(i in 1:length(input$id)){
      cat(input$id[i], "\t")
    }
    
    cat("\n")
    ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
    Mop$pw <- round(Mop$pw,3)
    for (i in 1:length(Mop$pw)){
      cat(Mop$pw[i], "\t")
    }
    
    cat("\n")
    APP_STORAGE$shares_portfolio <- Mop$pw
    
    #### Budget for the portfolio calculation
    budget <- input$num
    
    #### The following "for" implement the adaptation system with which portfolio calculation computes and prints only on selected stocks
    investimento <- 0
    list_shares <- list()  # List contains the "shares" for each selected stock
    list_spesa <- list()  # List contains the "spesa" for each selected stock
    fees_buy <- 0
    fees_buy_total <- 0
    pw_i <- 1  # counter for 
    cat("Composizione del Markowitz optimal portfolio:\n")
    cat("\n")
    for (stock_i in input$id){
      list_shares[[paste0(stock_i,".shares")]] <- floor( (budget * Mop$pw[pw_i])/(as.numeric(stocks[yesterday_1, paste0(stock_i,".Adjusted")]) * 1.02))
      list_spesa[[paste0("spesa.",stock_i)]] <- list_shares[[paste0(stock_i,".shares")]] * as.numeric(stocks[yesterday_1, paste0(stock_i,".Adjusted")])
      fees_buy <- list_spesa[[paste0("spesa.",stock_i)]] * 0.02
      fees_buy_total <- fees_buy_total + fees_buy
      cat(" - ",stock_i,":",list_shares[[paste0(stock_i,".shares")]],
          "quote a $",round(as.numeric(stocks[yesterday_1, paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
          round(list_spesa[[paste0("spesa.",stock_i)]],2),"\n")
      pw_i <- pw_i + 1
      investimento <- investimento + (list_spesa[[paste0("spesa.",stock_i)]] - fees_buy)
    }
    
 
    
    
    cat("\nInvestimento per il Markowitz optimal portfolio:", round(investimento + fees_buy_total, digits = 2), "$\n")
    cat("(di cui Fees: ", round(fees_buy_total, digits = 2), "$ )" ,"\n")
    cat("\n")
    cat("Residuo:", round(budget-investimento-fees_buy_total, digits = 2), "$\n")
    cat("\n")
    cat("Questo portafoglio ha un ritorno atteso del: ", round(Mop$pm*100, digits = 2), "%")
    cat("\n")
    cat("Questo portafoglio ha un rischio del: ", round(Mop$ps*100, digits = 2), "%")
    
  })
  
  # Pie plot of the selected stock from UI interface
  output$pie_plot <- renderPlot({
    
    val <- round(APP_STORAGE$shares_portfolio,3) ## load data in dynamic way based on the choice of user
    ix <- order(val)
    val <- val[ix]
    eti <- c(input$id[ix]) # etichette
    percent <- round(100*val/sum(val), 1) # percentuale per ciascun valore
    
    Pie <- pie3D(val, radius=0.8, height = 0.1, labels = percent, explode = 0.1, main = "Portfolio", col = rainbow(length(val))) # grafico a torta 3D con percentuali e legenda esterna
    legend("right", eti, cex = 0.75, fill = rainbow(length(val)), bty="n") # riporta la legenda
  })
  
  })
  
  
  ### SERVER TAB2 - TRAINING STRATEGIES
  
  # output degli anni di investimento per la Buy & Hold Strategy
  output$years <- renderPrint({ input$period })
 
  output$years_2 <- renderPrint({ input$period_2 })
 
  
  observeEvent(input$finestra_temporale, {
  
    output$portfolio_Buy_Hold <- renderPrint({
      validate(need(as.numeric(as.Date(input$finestra_temporale[2]) - as.Date(input$finestra_temporale[1])) >= 4018, "Selezionare una finestra temporale di almeno 11 anni"))
      ### Selects only the adjusted close price for the selected stocks inserted by the user with variable "input$id"
      stocks <- NULL
      
      for (stock_i in input$id){
        stocks <- cbind(stocks, ALL$stocks[,paste0(stock_i,".Adjusted")])
      }
      
      validate(need(ncol(stocks) <= 13, "Il portafoglio può essere composto da massimo 13 titoli"))
      # Markowitz optimal portfolio con impostazioni di default
      Mop <- portfolio.optim( x=ALL$returns[paste0(input$finestra_temporale[1],"/",input$finestra_temporale[2]),input$id])
    
      # $pw sta per Portfolio Weights, sono i pesi. La somma deve fare 1 (100%)
      # $px sono i ritorni del portafoglio con i pesi sopra indicati per ciascun anno
      # $pm = RITORNO ATTESO DEL PORTAFOGLIO
      # $ps = RISCHIO 
      for(i in 1:length(input$id)){
        cat(input$id[i], "\t")
      }
      
      cat("\n")
    
      ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
      Mop$pw <- round(Mop$pw,3)
      
      for(i in 1:length(Mop$pw)){
        cat(Mop$pw[i], "\t")
      }
      
      cat("\n")
    
      APP_STORAGE$shares_portfolio <- Mop$pw
    
      #### Budget for the portfolio calculation
      budget <- input$num
    
      # scelt della data in cui vendere (con i mercati aperti)
      if(input$period == 1) {endPeriod <- "2016-12-30"}
    
      if(input$period == 2) {endPeriod <- "2017-12-29"}    
    
      if(input$period == 3) {endPeriod <- "2018-12-31"}
    
      if(input$period == 4) {endPeriod <- "2019-12-31"}
    
      if(input$period == 5) {endPeriod <- "2020-12-31"}
    
      endPeriod
    
     #### The following "for" implement the adaptation system with which portfolio calculation computes and prints only on selected stocks
      investimento <- 0
      ritorno_effettivo <- 0
      list_shares <- list()  # List contains the "shares" for each selected stock
      list_spesa <- list()  # List contains the "spesa" for each selected stock
      list_vendita <- list()
      list_profitto <- list()
      fees_buy <- 0
      fees_sell <- 0
      fees_buy_total <- 0
      fees_sell_total <- 0
      pw_i <- 1  # counter for 
      cat("Composizione del Markowitz optimal portfolio:\n")
      cat("\n")
      for (stock_i in input$id){
        list_shares[[paste0(stock_i,".shares")]] <- floor( (budget * Mop$pw[pw_i])/(as.numeric(stocks[yesterday, paste0(stock_i,".Adjusted")]) * 1.02))
        list_spesa[[paste0("spesa.",stock_i)]] <- list_shares[[paste0(stock_i,".shares")]] * as.numeric(stocks[yesterday, paste0(stock_i,".Adjusted")]) 
        fees_buy <- list_spesa[[paste0("spesa.",stock_i)]] * 0.02
        fees_buy_total <- fees_buy_total + fees_buy
        list_vendita[[paste0("vendita.",stock_i)]] <- list_shares[[paste0(stock_i,".shares")]] * as.numeric(stocks[endPeriod, paste0(stock_i,".Adjusted")]) 
        fees_sell <- list_vendita[[paste0("vendita.",stock_i)]] * 0.02
        fees_sell_total <- fees_sell_total + fees_sell
        list_profitto[[paste0("profitto.",stock_i)]] <- (list_vendita[[paste0("vendita.",stock_i)]] - fees_sell) - (list_spesa[[paste0("spesa.",stock_i)]] + fees_buy)
      
        cat(" - ",stock_i,":",list_shares[[paste0(stock_i,".shares")]],
            "quote comprate a $",round(as.numeric(stocks[yesterday, paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
            round(list_spesa[[paste0("spesa.",stock_i)]], 2),"\n")
      
      
        cat("\n")
      
        pw_i <- pw_i + 1
        investimento <- investimento + list_spesa[[paste0("spesa.",stock_i)]]
      
        ritorno_effettivo <- ritorno_effettivo + list_profitto[[paste0("profitto.",stock_i)]]
      }
    
     
    
      cat("\nInvestimento per il Markowitz optimal portfolio: $", round(investimento + fees_buy_total, digits = 2), "\n")
      cat("(di cui Fees: $", round(fees_buy_total, digits = 2), " )" ,"\n")
      cat("\n")
      cat("Residuo: $", round(budget-investimento-fees_buy_total, digits = 2), "\n")
      cat("\n")
      cat("Il valore del portafoglio atteso: $", round((investimento + fees_buy_total) + ((investimento + fees_buy_total) * (Mop$pm) * as.numeric(input$period)), 2) ,
          "   (%", round((Mop$pm*100) * as.numeric(input$period), digits = 2), ")")
      cat("\n")
      cat("Il rischio del portafoglio: ", round(Mop$ps*100, 2), " %\n")
      # Spostato nella stampa di testing
      #cat("Questo portafoglio ha un ritorno effettivo del: ", round((ritorno_effettivo/budget) * 100, digits = 2), "%")
      #cat("\n")
    
    })
  })
  
  
  
  
  output$portfolio_Rebalancing <- renderPrint({
    
    
    fine_ritorni <- "2020-12-31"     
    
   
    
    train <- NULL
    pred <- NULL
    pattern <- NULL
    stock <- NULL
    return <- NULL
    r.totali <- NULL
    stocks_reb <- NULL
    
    for(stock_i in input$id){
      pattern <- c(paste0(stock_i, ".Open"), paste0(stock_i, ".High"), paste0(stock_i, ".Low"), paste0(stock_i, ".Close"), paste0(stock_i, ".Volume"), paste0(stock_i, ".Adjusted"))
      stock <- ALL$stocks[,pattern]
      return <- periodReturn(x=stock, period="quarterly", subset = paste0("/", fine_ritorni))
      r.totali <- cbind(r.totali, return)
      PORTFOLIO$returns <- r.totali
      stocks_reb <- cbind(stocks_reb, ALL$stocks[,paste0(stock_i,".Adjusted")])
    }
    
    
    # Selezione dei soli primi 45 quadrimestri (training)
    train <- PORTFOLIO$returns[1:45,]
    
    # primo Markowitz portfolio
    Mop <- portfolio.optim( x=train )
    cat("Portfolio iniziale (fine 2015 - fine 2016):\n")
    ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
    for(i in 1:length(input$id)){
      cat(input$id[i], "\t")
    }
    
    cat("\n")
    
    ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
    Mop$pw <- round(Mop$pw,3)
    
    for(i in 1:length(Mop$pw)){
      cat(Mop$pw[i], "\t")
    }
    
    cat("\n")
    
    
    budget <- input$num
    endPeriod_reb <- "2016-12-31"
    
    
    
    #### The following "for" implement the adaptation system with which portfolio calculation computes and prints only on selected stocks
    investimento_reb <- 0
    ritorno_effettivo_reb <- 0
    list_shares_reb <- list()  # List contains the "shares" for each selected stock
    list_spesa_reb <- list()  # List contains the "spesa" for each selected stock
    list_vendita_reb <- list()
    list_profitto_reb <- list()
    #fees_reb <- 0
    fees_reb_buy <- 0
    fees_reb_sell <- 0
    fees_reb_buy_total <- 0
    fees_reb_sell_total <- 0
    pw_i_reb <- 1  # counter for 
    cat("Composizione del Markowitz optimal portfolio iniziale (fine 2015):\n")
    cat("\n")
    for (stock_i in input$id){
      list_shares_reb[[paste0(stock_i,".shares")]] <- floor( (budget * Mop$pw[pw_i_reb])/(as.numeric(stocks_reb[yesterday, paste0(stock_i,".Adjusted")]) * 1.02))
      list_spesa_reb[[paste0("spesa.",stock_i)]] <- list_shares_reb[[paste0(stock_i,".shares")]] * as.numeric(stocks_reb[yesterday, paste0(stock_i,".Adjusted")])
      fees_reb_buy <- list_spesa_reb[[paste0("spesa.",stock_i)]] * 0.02
      fees_reb_buy_total <- fees_reb_buy_total + fees_reb_buy
      list_vendita_reb[[paste0("vendita.",stock_i)]] <- list_shares_reb[[paste0(stock_i,".shares")]] * as.numeric(stocks_reb[endPeriod_reb, paste0(stock_i,".Adjusted")]) 
      fees_reb_sell <- list_vendita_reb[[paste0("vendita.",stock_i)]] * 0.02
      fees_reb_sell_total <- fees_reb_sell_total + fees_reb_sell
      list_profitto_reb[[paste0("profitto.",stock_i)]] <- (list_vendita_reb[[paste0("vendita.",stock_i)]] - fees_reb_sell) - (list_spesa_reb[[paste0("spesa.",stock_i)]] + fees_reb_buy)
      
      cat(" - ",stock_i,":",list_shares_reb[[paste0(stock_i,".shares")]],
          "quote comprate a $",round(as.numeric(stocks_reb[yesterday, paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
          round(list_spesa_reb[[paste0("spesa.",stock_i)]], 2),"\n")
      
      
      pw_i_reb <- pw_i_reb + 1
      
      investimento_reb <- investimento_reb + list_spesa_reb[[paste0("spesa.",stock_i)]]
      
      ritorno_effettivo_reb <- ritorno_effettivo_reb + list_profitto_reb[[paste0("profitto.",stock_i)]]
    }
    
    
    
    cat("\nInvestimento per il Markowitz optimal portfolio: $", round(investimento_reb + fees_reb_buy_total, digits = 2), "\n")
    cat("(di cui Fees: $", round(fees_reb_buy_total, digits = 2), " )" ,"\n")
    cat("\n")
    cat("Residuo: $", round(budget-investimento_reb-fees_reb_buy_total, digits = 2), "\n")
    cat("Il ritorno atteso del portafoglio iniziale: ",round((Mop$pm*100)*as.numeric(input$period_2) , 2)," %\n")
    cat("Il livello di rischio del portafoglio iniziale: ",round(Mop$ps*100, 2)," %\n")
    cat("\n")
    cat("\n")
    
    
    
    
    
    
    # nell'applicazione ? input$budget
    
    
    # Ribilanciamento "dinamico", basato sui ritorni attesi, per i prossimi 5 anni
    
    endPeriod_temp <- list()
    
    if(input$period_2 == 1) {
      endPeriod_reb <- "2016-12-30"
      endPeriod_temp[1] <- "2016-12-30"}
    
    if(input$period_2 == 2) {
      endPeriod_reb <- "2017-12-29"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"}    
    
    if(input$period_2 == 3) {
      endPeriod_reb <- "2018-12-31"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"
      endPeriod_temp[3] <- "2018-12-31"}
    
    if(input$period_2 == 4) {
      endPeriod_reb <- "2019-12-31"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"
      endPeriod_temp[3] <- "2018-12-31"
      endPeriod_temp[4] <- "2019-12-31"}
    
    if(input$period_2 == 5) {
      endPeriod_reb <- "2020-12-31"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"
      endPeriod_temp[3] <- "2018-12-31"
      endPeriod_temp[4] <- "2019-12-31"
      endPeriod_temp[5] <- "2020-12-31"}
    
    temp <- 1
    Mop_temp <- Mop
    Mop_final <- NULL
    
    investimento_reb_final <- investimento_reb
    fees_reb_final_buy <- fees_reb_buy
    fees_reb_final_sell <- 0
    fees_reb_final_buy_total <- fees_reb_buy_total
    fees_reb_final_sell_total <- 0
    ritorno_effettivo_reb_final <- 0
    list_shares_reb_final <- list_shares_reb  # List contains the "shares" for each selected stock
    list_spesa_reb_final <- list() # List contains the "spesa" for each selected stock
    list_vendita_reb_final <- list()
    list_profitto_reb_final <- 0
    
    investimento_reb_act <- 0
    fees_reb_act_buy <- 0
    fees_reb_act_sell <- 0
    fees_reb_act_buy_total <- 0
    fees_reb_act_sell_total <- 0
    ritorno_effettivo_reb_act <- 0
    list_shares_reb_act <- list()  # List contains the "shares" for each selected stock
    list_spesa_reb_act <- list()  # List contains the "spesa" for each selected stock
    list_vendita_reb_act <- list()
    list_profitto_reb_act <- list()
    
    investimento_reb_pre <- 0
    fees_reb_pre_buy <- 0
    fees_reb_pre_sell <- 0
    fees_reb_pre_buy_total <- 0
    fees_reb_pre_sell_total <- 0
    ritorno_effettivo_reb_pre <- 0
    list_shares_reb_pre <- list_shares_reb  # List contains the "shares" for each selected stock
    list_spesa_reb_pre <- list_spesa_reb # List contains the "spesa" for each selected stock
    list_vendita_reb_pre <- list_vendita_reb
    list_profitto_reb_pre <- list()
    
    chiusura <- 1
    
    
    
    
    
    
    for( i in 1:input$period_2) { 
      
      cat("-------------------------------------------","\n")
      # recupero i ritorni attesi da ogni stock in base a Markowitz
      pred <- NULL
      for(j in 1:ncol(train)){
        pred <- cbind(pred, Mop$pm * Mop$pw[j])
      }
      
      
      year <- format(as.Date(last(index(train))+365, format="%d/%m/%Y"),"%Y") ## estraggo l'anno successivo
      new_date <- as.Date(paste0(year,"-12-31")) ## creo la nuova data con l'anno successivo
      
      
      # Elimino il ritorno pi? vecchio e accodo la previsione del prossimo ritorno secondo Markowitz
      
      train <- train[-1,]
      train <- c(xts(pred, order.by=new_date), train)
      
      # Markowitz per ribilanciamento ribilanciare
      Mop_act <- portfolio.optim( x=train )
      
      ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
      Mop_act$pw <- round(Mop_act$pw,3)
      
      if(as.numeric(input$period_2) - chiusura >= 1){
        
        cat("Portfolio dopo il ribilanciamento del",endPeriod_temp[[temp]],":","\n\n")
        for(i in 1:length(input$id)){
          cat(input$id[i], "\t")
        }
        
        cat("\n")
        
        
        
        for(i in 1:length(Mop_act$pw)){
          cat(Mop_act$pw[i], "\t")
        }
        
        cat("\n")
        
      }
      
      ### QUI AGGIUNGERE CICLO FOR PER TENER TRACCIA DELLE AZIONI COMPRATE O VENDUTE PER CIASCUN ANNO
      
      cat("\n\n\n")
      
      
      
      
      
      
      
      
      count <- 1
      difference <- NULL
      shares <- NULL
      
      for (stock_i in input$id){
        
        difference <- Mop_act$pw[count] - Mop_temp$pw[count]
        
        if(difference > 0 && as.numeric(input$period_2) - chiusura >= 1){
          
          shares[paste0(stock_i,".shares")] <- floor( (budget * difference)/(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]) * 1.02))
          list_shares_reb_act[[paste0(stock_i,".shares")]] <- shares[paste0(stock_i,".shares")]
          list_shares_reb_final[[paste0(stock_i,".shares")]] <- list_shares_reb_final[[paste0(stock_i,".shares")]] + list_shares_reb_act[[paste0(stock_i,".shares")]]
          list_spesa_reb_act[[paste0("spesa.",stock_i)]] <- shares[paste0(stock_i,".shares")] * as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")])
          fees_reb_act_buy <- list_spesa_reb_act[[paste0("spesa.",stock_i)]] * 0.02
          fees_reb_act_buy_total <- fees_reb_act_buy_total + fees_reb_act_buy
          list_vendita_reb_act[[paste0("vendita.",stock_i)]] <- 0
          list_profitto_reb_act[[paste0("profitto.",stock_i)]] <- list_vendita_reb_act[[paste0("vendita.",stock_i)]] - (list_spesa_reb_act[[paste0("spesa.",stock_i)]] + fees_reb_act_buy)
          list_profitto_reb_final <- list_profitto_reb_final + list_profitto_reb_act[[paste0("profitto.",stock_i)]]
          
          investimento_reb_act <- investimento_reb_act + list_spesa_reb_act[[paste0("spesa.",stock_i)]] 
          
          
          
          if(shares[paste0(stock_i,".shares")] > 0){
            cat(" - ",stock_i,":",shares[paste0(stock_i,".shares")],
                "quote comprate a $", round(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
                round(list_spesa_reb_act[[paste0("spesa.",stock_i)]], 2),"\n", " - Quote attualmente possedute: ",list_shares_reb_final[[paste0(stock_i,".shares")]], "\n\n")
          }
          
        }
        
        if(difference < 0 && as.numeric(input$period_2) - chiusura >= 1){
          
          
          shares[paste0(stock_i,".shares")] <- floor( (budget * (-difference))/(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]) * 1.02))
          list_shares_reb_act[[paste0(stock_i,".shares")]] <- shares[paste0(stock_i,".shares")]
          list_shares_reb_final[[paste0(stock_i,".shares")]] <- list_shares_reb_final[[paste0(stock_i,".shares")]] - list_shares_reb_act[[paste0(stock_i,".shares")]]
          list_spesa_reb_act[[paste0("spesa.",stock_i)]] <- 0
          list_vendita_reb_act[[paste0("vendita.",stock_i)]] <- shares[paste0(stock_i,".shares")] * as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]) 
          fees_reb_act_sell <- list_vendita_reb_act[[paste0("vendita.",stock_i)]] * 0.02
          fees_reb_act_sell_total <- fees_reb_act_sell_total + fees_reb_act_sell
          list_profitto_reb_act[[paste0("profitto.",stock_i)]] <- (list_vendita_reb_act[[paste0("vendita.",stock_i)]] - fees_reb_act_sell) - list_spesa_reb_act[[paste0("spesa.",stock_i)]]
          list_profitto_reb_final <- list_profitto_reb_final + list_profitto_reb_act[[paste0("profitto.",stock_i)]]
          
          investimento_reb_act <- investimento_reb_act + list_spesa_reb_act[[paste0("spesa.",stock_i)]] 
          
          
          
          if(shares[paste0(stock_i,".shares")] > 0){
            cat(" - ",stock_i,":",shares[paste0(stock_i,".shares")],
                "quote vendute a $", round(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
                round(list_vendita_reb_act[[paste0("vendita.",stock_i)]], 2),"\n", " - Quote attualmente possedute: ",list_shares_reb_final[[paste0(stock_i,".shares")]], "\n\n")
          }
          
          
        }
        
        if (difference == 0 && as.numeric(input$period_2) - chiusura >= 1){
          list_shares_reb_act[[paste0(stock_i,".shares")]] <- list_shares_reb_final[[paste0(stock_i,".shares")]]
          list_spesa_reb_act[[paste0("spesa.",stock_i)]] <- 0
          list_vendita_reb_act[[paste0("spesa.",stock_i)]] <- 0
          list_profitto_reb_act[[paste0("profitto.",stock_i)]] <- 0
          
        }
        
        
        
        
        count <- count + 1
        
        
        
      }
      
      if(as.numeric(input$period_2) - chiusura >= 1){
        investimento_reb_final <- investimento_reb_final + investimento_reb_act
        cat("\n")
        cat("Fees totali in acquisto: $",round(fees_reb_act_buy_total, 2), "\n")
        cat("Fees totali in vendita: $",round(fees_reb_act_sell_total, 2), "\n")
        cat("\n")
        fees_reb_final_buy_total <- fees_reb_final_buy_total + fees_reb_act_buy_total
        fees_reb_final_sell_total <- fees_reb_final_sell_total + fees_reb_act_sell_total
      }
      
      temp <- temp + 1
      Mop_temp <- Mop_act
      list_shares_reb_pre <- list_shares_reb_act
      list_spesa_reb_pre <- list_spesa_reb_act
      list_vendita_reb_pre <- list_vendita_reb_act
      list_profitto_reb_pre <- list_profitto_reb_act
      
      
      
      
      cat("-------------------------------------------","\n")
      
      
      if(as.numeric(input$period_2) - chiusura < 1){
        
        tasse_final <- 0
        
        cat("Chiusura delle posizioni (",endPeriod_reb,")\n\n")
        
        for(stock_i in input$id){
          list_spesa_reb_final[[paste0("vendita.",stock_i)]] <- 0
          list_vendita_reb_final[[paste0("vendita.",stock_i)]] <- list_shares_reb_final[[paste0(stock_i,".shares")]] * as.numeric(stocks_reb[endPeriod_reb, paste0(stock_i,".Adjusted")])
          fees_reb_final_sell <- list_vendita_reb_final[[paste0("vendita.",stock_i)]] * 0.02
          tasse_final <- tasse_final + fees_reb_final_sell
          fees_reb_final_sell_total <- fees_reb_final_sell_total + fees_reb_final_sell
          list_profitto_reb_final <- list_profitto_reb_final + (list_vendita_reb_final[[paste0("vendita.",stock_i)]] - fees_reb_final_sell)
          
          
          
         
        }
        
        cat("\n")
       
      }
      
      
      chiusura <- chiusura + 1
      
    }
    
    
    ritorno_netto <- round(list_profitto_reb_final - (investimento_reb + fees_reb_final_buy_total), digits = 2)
   
    
    
    
  })
  
  
  
  
  
  ### SERVER TAB3 - TESTING STRATEGIES
  
  #stampa selezione della strategia per testing
  output$strategia_selezionata <- renderPrint({ input$radio })
  
  output$testing_Buy_Hold <- renderPrint({
    
    ### Selects only the adjusted close price for the selected stocks inserted by the user with variable "input$id"
    stocks <- NULL
    for (stock_i in input$id){
      stocks <- cbind(stocks, ALL$stocks[,paste0(stock_i,".Adjusted")])
    }
    
    # Markowitz optimal portfolio con impostazioni di default
    Mop <- portfolio.optim( x=ALL$returns[paste0(input$finestra_temporale[1],"/",input$finestra_temporale[2]),input$id])
    
    # $pw sta per Portfolio Weights, sono i pesi. La somma deve fare 1 (100%)
    # $px sono i ritorni del portafoglio con i pesi sopra indicati per ciascun anno
    # $pm = RITORNO ATTESO DEL PORTAFOGLIO
    # $ps = RISCHIO 
    for(i in 1:length(input$id)){
      cat(input$id[i], "\t")
    }
    
    cat("\n")
    
    ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
    Mop$pw <- round(Mop$pw,3)
    
    for(i in 1:length(Mop$pw)){
      cat(Mop$pw[i], "\t")
    }
    
    cat("\n")
    
    APP_STORAGE$shares_portfolio <- Mop$pw
    
    #### Budget for the portfolio calculation
    budget <- input$num
    
    # scelt della data in cui vendere (con i mercati aperti)
    if(input$period == 1) {endPeriod <- "2016-12-30"}
    
    if(input$period == 2) {endPeriod <- "2017-12-29"}    
    
    if(input$period == 3) {endPeriod <- "2018-12-31"}
    
    if(input$period == 4) {endPeriod <- "2019-12-31"}
    
    if(input$period == 5) {endPeriod <- "2020-12-31"}
    
    endPeriod
    
    #### The following "for" implement the adaptation system with which portfolio calculation computes and prints only on selected stocks
    investimento <- 0
    ritorno_effettivo <- 0
    list_shares <- list()  # List contains the "shares" for each selected stock
    list_spesa <- list()  # List contains the "spesa" for each selected stock
    list_vendita <- list()
    list_profitto <- list()
    fees_buy <- 0
    fees_buy_total <- 0
    fees_sell <- 0
    fees_sell_total <- 0
    pw_i <- 1  # counter for 
    cat("Composizione del Markowitz optimal portfolio:\n")
    cat("\n")
    for (stock_i in input$id){
      list_shares[[paste0(stock_i,".shares")]] <- floor( (budget * Mop$pw[pw_i])/(as.numeric(stocks[yesterday, paste0(stock_i,".Adjusted")]) * 1.02))
      list_spesa[[paste0("spesa.",stock_i)]] <- list_shares[[paste0(stock_i,".shares")]] * as.numeric(stocks[yesterday, paste0(stock_i,".Adjusted")]) 
      fees_buy <- list_spesa[[paste0("spesa.",stock_i)]] * 0.02
      fees_buy_total <- fees_buy_total + fees_buy
      list_vendita[[paste0("vendita.",stock_i)]] <- list_shares[[paste0(stock_i,".shares")]] * as.numeric(stocks[endPeriod, paste0(stock_i,".Adjusted")])
      fees_sell <- list_vendita[[paste0("vendita.",stock_i)]] * 0.02
      fees_sell_total <- fees_sell_total + fees_sell
      list_profitto[[paste0("profitto.",stock_i)]] <- (list_vendita[[paste0("vendita.",stock_i)]] - fees_sell) - (list_spesa[[paste0("spesa.",stock_i)]] + fees_buy)
      
      cat(" - ",stock_i,":",list_shares[[paste0(stock_i,".shares")]],
          "quote comprate a $",round(as.numeric(stocks[yesterday, paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
          round(list_spesa[[paste0("spesa.",stock_i)]], 2),"\n")
      
      cat(" - ",stock_i,":",list_shares[[paste0(stock_i,".shares")]],
          "quote vendute a $",round(as.numeric(stocks[endPeriod, paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
          round(list_vendita[[paste0("vendita.",stock_i)]], 2),"\n")
      
      cat(" -  Il profitto di ",stock_i,"vale: $",round(list_vendita[[paste0("vendita.",stock_i)]] - list_spesa[[paste0("spesa.",stock_i)]], 2), "\n")
      
      cat("\n")
      
      pw_i <- pw_i + 1
      investimento <- investimento + list_spesa[[paste0("spesa.",stock_i)]]
      
      ritorno_effettivo <- ritorno_effettivo + list_profitto[[paste0("profitto.",stock_i)]]
    }
    
   
    
    cat("\nInvestimento per il Markowitz optimal portfolio: $", round(investimento + fees_buy_total, digits = 2), "\n")
    cat("(di cui Fees: $", round(fees_buy_total, digits = 2), " )" ,"\n")
    cat("\n")
    cat("Residuo: $", round(budget-investimento-fees_buy_total, digits = 2), "\n")
    cat("\n")
    cat("Il valore del portafoglio atteso: $", round((investimento + fees_buy_total) + ((investimento + fees_buy_total) * (Mop$pm) * as.numeric(input$period)), 2) ,
        "   (%", round((Mop$pm*100) * as.numeric(input$period), digits = 2), ")")
    cat("\n")
    cat("Il valore effettivo del portafoglio: $", round(investimento + ritorno_effettivo, 2) , "   ( ", round((ritorno_effettivo/budget) * 100, digits = 2), "%)")
    cat("\n")
    
  })
  
  
  output$testing_Rebalancing <- renderPrint({
    
    fine_ritorni <- "2020-12-31"     
    
    # Ritorni titoli in 15 anni
    
    train <- NULL
    pred <- NULL
    pattern <- NULL
    stock <- NULL
    return <- NULL
    r.totali <- NULL
    stocks_reb <- NULL
    
    for(stock_i in input$id){
      pattern <- c(paste0(stock_i, ".Open"), paste0(stock_i, ".High"), paste0(stock_i, ".Low"), paste0(stock_i, ".Close"), paste0(stock_i, ".Volume"), paste0(stock_i, ".Adjusted"))
      stock <- ALL$stocks[,pattern]
      return <- periodReturn(x=stock, period="quarterly", subset = paste0("/", fine_ritorni))
      r.totali <- cbind(r.totali, return)
      PORTFOLIO$returns <- r.totali
      stocks_reb <- cbind(stocks_reb, ALL$stocks[,paste0(stock_i,".Adjusted")])
    }
    
    
    # Selezione dei soli primi 45 quadrimestri (training)
    train <- PORTFOLIO$returns[1:45,]
    
    # primo Markowitz portfolio
    Mop <- portfolio.optim( x=train )
    cat("Portfolio iniziale:\n")
    ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
    for(i in 1:length(input$id)){
      cat(input$id[i], "\t")
    }
    
    cat("\n")
    
    ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
    Mop$pw <- round(Mop$pw,3)
    
    for(i in 1:length(Mop$pw)){
      cat(Mop$pw[i], "\t")
    }
    
    cat("\n")
    
    
    budget <- input$num
    endPeriod_reb <- "2016-12-31"
 
    
    
    #### The following "for" implement the adaptation system with which portfolio calculation computes and prints only on selected stocks
    investimento_reb <- 0
    ritorno_effettivo_reb <- 0
    list_shares_reb <- list()  # List contains the "shares" for each selected stock
    list_spesa_reb <- list()  # List contains the "spesa" for each selected stock
    list_vendita_reb <- list()
    list_profitto_reb <- list()
    #fees_reb <- 0
    fees_reb_buy <- 0
    fees_reb_sell <- 0
    fees_reb_buy_total <- 0
    fees_reb_sell_total <- 0
    pw_i_reb <- 1  # counter for 
    cat("Composizione del Markowitz optimal portfolio iniziale (fine 2015):\n")
    cat("\n")
    for (stock_i in input$id){
      list_shares_reb[[paste0(stock_i,".shares")]] <- floor( (budget * Mop$pw[pw_i_reb])/(as.numeric(stocks_reb[yesterday, paste0(stock_i,".Adjusted")]) * 1.02))
      list_spesa_reb[[paste0("spesa.",stock_i)]] <- list_shares_reb[[paste0(stock_i,".shares")]] * as.numeric(stocks_reb[yesterday, paste0(stock_i,".Adjusted")])
      fees_reb_buy <- list_spesa_reb[[paste0("spesa.",stock_i)]] * 0.02
      fees_reb_buy_total <- fees_reb_buy_total + fees_reb_buy
      list_vendita_reb[[paste0("vendita.",stock_i)]] <- list_shares_reb[[paste0(stock_i,".shares")]] * as.numeric(stocks_reb[endPeriod_reb, paste0(stock_i,".Adjusted")]) 
      fees_reb_sell <- list_vendita_reb[[paste0("vendita.",stock_i)]] * 0.02
      fees_reb_sell_total <- fees_reb_sell_total + fees_reb_sell
      list_profitto_reb[[paste0("profitto.",stock_i)]] <- (list_vendita_reb[[paste0("vendita.",stock_i)]] - fees_reb_sell) - (list_spesa_reb[[paste0("spesa.",stock_i)]] + fees_reb_buy)
      
      cat(" - ",stock_i,":",list_shares_reb[[paste0(stock_i,".shares")]],
          "quote comprate a $",round(as.numeric(stocks_reb[yesterday, paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
          round(list_spesa_reb[[paste0("spesa.",stock_i)]], 2),"\n")
      
      
      pw_i_reb <- pw_i_reb + 1
     
      investimento_reb <- investimento_reb + list_spesa_reb[[paste0("spesa.",stock_i)]]
      
      ritorno_effettivo_reb <- ritorno_effettivo_reb + list_profitto_reb[[paste0("profitto.",stock_i)]]
    }
    
   
    
    cat("\nInvestimento per il Markowitz optimal portfolio: $", round(investimento_reb + fees_reb_buy_total, digits = 2), "\n")
    cat("(di cui Fees: $", round(fees_reb_buy_total, digits = 2), " )" ,"\n")
    cat("\n")
    cat("Residuo: $", round(budget-investimento_reb-fees_reb_buy_total, digits = 2), "\n")
    cat("\n")
    cat("\n")

    
    
    
    
    
    # nell'applicazione ? input$budget
    
    
    # Ribilanciamento "dinamico", basato sui ritorni attesi, per i prossimi 5 anni
    
    endPeriod_temp <- list()
    
    if(input$period_2 == 1) {
      endPeriod_reb <- "2016-12-30"
      endPeriod_temp[1] <- "2016-12-30"}
    
    if(input$period_2 == 2) {
      endPeriod_reb <- "2017-12-29"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"}    
    
    if(input$period_2 == 3) {
      endPeriod_reb <- "2018-12-31"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"
      endPeriod_temp[3] <- "2018-12-31"}
    
    if(input$period_2 == 4) {
      endPeriod_reb <- "2019-12-31"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"
      endPeriod_temp[3] <- "2018-12-31"
      endPeriod_temp[4] <- "2019-12-31"}
    
    if(input$period_2 == 5) {
      endPeriod_reb <- "2020-12-31"
      endPeriod_temp[1] <- "2016-12-30"
      endPeriod_temp[2] <- "2017-12-29"
      endPeriod_temp[3] <- "2018-12-31"
      endPeriod_temp[4] <- "2019-12-31"
      endPeriod_temp[5] <- "2020-12-31"}
    
    temp <- 1
    Mop_temp <- Mop
    Mop_final <- NULL
    
    investimento_reb_final <- investimento_reb
    fees_reb_final_buy <- fees_reb_buy
    fees_reb_final_sell <- 0
    fees_reb_final_buy_total <- fees_reb_buy_total
    fees_reb_final_sell_total <- 0
    ritorno_effettivo_reb_final <- 0
    list_shares_reb_final <- list_shares_reb  # List contains the "shares" for each selected stock
    list_spesa_reb_final <- list() # List contains the "spesa" for each selected stock
    list_vendita_reb_final <- list()
    list_profitto_reb_final <- 0
    
    investimento_reb_act <- 0
    fees_reb_act_buy <- 0
    fees_reb_act_sell <- 0
    fees_reb_act_buy_total <- 0
    fees_reb_act_sell_total <- 0
    ritorno_effettivo_reb_act <- 0
    list_shares_reb_act <- list()  # List contains the "shares" for each selected stock
    list_spesa_reb_act <- list()  # List contains the "spesa" for each selected stock
    list_vendita_reb_act <- list()
    list_profitto_reb_act <- list()
    
    investimento_reb_pre <- 0
    fees_reb_pre_buy <- 0
    fees_reb_pre_sell <- 0
    fees_reb_pre_buy_total <- 0
    fees_reb_pre_sell_total <- 0
    ritorno_effettivo_reb_pre <- 0
    list_shares_reb_pre <- list_shares_reb  # List contains the "shares" for each selected stock
    list_spesa_reb_pre <- list_spesa_reb # List contains the "spesa" for each selected stock
    list_vendita_reb_pre <- list_vendita_reb
    list_profitto_reb_pre <- list()
    
    chiusura <- 1
    
    
    
    
 
    
    for( i in 1:input$period_2) { 
      
      cat("-------------------------------------------","\n")
      # recupero i ritorni attesi da ogni stock in base a Markowitz
      pred <- NULL
      for(j in 1:ncol(train)){
        pred <- cbind(pred, Mop$pm * Mop$pw[j])
      }
      
      
      year <- format(as.Date(last(index(train))+365, format="%d/%m/%Y"),"%Y") ## estraggo l'anno successivo
      new_date <- as.Date(paste0(year,"-12-31")) ## creo la nuova data con l'anno successivo
      
      
      # Elimino il ritorno pi? vecchio e accodo la previsione del prossimo ritorno secondo Markowitz
      
      train <- train[-1,]
      train <- c(xts(pred, order.by=new_date), train)
      
      # Markowitz per ribilanciamento ribilanciare
      Mop_act <- portfolio.optim( x=train )
      
      ### ROUND OF Mop! Because it has small negative values which can approximate to 0!
      Mop_act$pw <- round(Mop_act$pw,3)
      
      if(as.numeric(input$period_2) - chiusura >= 1){
      
        cat("Portfolio dopo il ribilanciamento del",endPeriod_temp[[temp]],":","\n\n")
        for(i in 1:length(input$id)){
          cat(input$id[i], "\t")
        }
      
        cat("\n")
      
     
      
        for(i in 1:length(Mop_act$pw)){
          cat(Mop_act$pw[i], "\t")
        }
      
        cat("\n")
      
      }
      
      ### QUI AGGIUNGERE CICLO FOR PER TENER TRACCIA DELLE AZIONI COMPRATE O VENDUTE PER CIASCUN ANNO
      
      cat("\n\n\n")
      
  
      
      
      
      
      
      
      count <- 1
      difference <- NULL
      shares <- NULL
      
      for (stock_i in input$id){
        
        difference <- Mop_act$pw[count] - Mop_temp$pw[count]
        
          if(difference > 0 && as.numeric(input$period_2) - chiusura >= 1){
            
            shares[paste0(stock_i,".shares")] <- floor( (budget * difference)/(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]) * 1.02))
            list_shares_reb_act[[paste0(stock_i,".shares")]] <- shares[paste0(stock_i,".shares")]
            list_shares_reb_final[[paste0(stock_i,".shares")]] <- list_shares_reb_final[[paste0(stock_i,".shares")]] + list_shares_reb_act[[paste0(stock_i,".shares")]]
            list_spesa_reb_act[[paste0("spesa.",stock_i)]] <- shares[paste0(stock_i,".shares")] * as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")])
            fees_reb_act_buy <- list_spesa_reb_act[[paste0("spesa.",stock_i)]] * 0.02
            fees_reb_act_buy_total <- fees_reb_act_buy_total + fees_reb_act_buy
            list_vendita_reb_act[[paste0("vendita.",stock_i)]] <- 0
            list_profitto_reb_act[[paste0("profitto.",stock_i)]] <- list_vendita_reb_act[[paste0("vendita.",stock_i)]] - (list_spesa_reb_act[[paste0("spesa.",stock_i)]] + fees_reb_act_buy)
            list_profitto_reb_final <- list_profitto_reb_final + list_profitto_reb_act[[paste0("profitto.",stock_i)]]
            
            investimento_reb_act <- investimento_reb_act + list_spesa_reb_act[[paste0("spesa.",stock_i)]] 
            
            
            
            if(shares[paste0(stock_i,".shares")] > 0){
             cat(" - ",stock_i,":",shares[paste0(stock_i,".shares")],
                  "quote comprate a $", round(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
                  round(list_spesa_reb_act[[paste0("spesa.",stock_i)]], 2),"\n", " - Quote attualmente possedute: ",list_shares_reb_final[[paste0(stock_i,".shares")]], "\n\n")
              }
            
          }
        
          if(difference < 0 && as.numeric(input$period_2) - chiusura >= 1){
            
            
            shares[paste0(stock_i,".shares")] <- floor( (budget * (-difference))/(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]) * 1.02))
            list_shares_reb_act[[paste0(stock_i,".shares")]] <- shares[paste0(stock_i,".shares")]
            list_shares_reb_final[[paste0(stock_i,".shares")]] <- list_shares_reb_final[[paste0(stock_i,".shares")]] - list_shares_reb_act[[paste0(stock_i,".shares")]]
            list_spesa_reb_act[[paste0("spesa.",stock_i)]] <- 0
            list_vendita_reb_act[[paste0("vendita.",stock_i)]] <- shares[paste0(stock_i,".shares")] * as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]) 
            fees_reb_act_sell <- list_vendita_reb_act[[paste0("vendita.",stock_i)]] * 0.02
            fees_reb_act_sell_total <- fees_reb_act_sell_total + fees_reb_act_sell
            list_profitto_reb_act[[paste0("profitto.",stock_i)]] <- (list_vendita_reb_act[[paste0("vendita.",stock_i)]] - fees_reb_act_sell) - list_spesa_reb_act[[paste0("spesa.",stock_i)]]
            list_profitto_reb_final <- list_profitto_reb_final + list_profitto_reb_act[[paste0("profitto.",stock_i)]]
            
            investimento_reb_act <- investimento_reb_act + list_spesa_reb_act[[paste0("spesa.",stock_i)]] 
           
            
           
            if(shares[paste0(stock_i,".shares")] > 0){
              cat(" - ",stock_i,":",shares[paste0(stock_i,".shares")],
                  "quote vendute a $", round(as.numeric(stocks_reb[endPeriod_temp[[temp]], paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
                  round(list_vendita_reb_act[[paste0("vendita.",stock_i)]], 2),"\n", " - Quote attualmente possedute: ",list_shares_reb_final[[paste0(stock_i,".shares")]], "\n\n")
            }
            
            
            }

          if (difference == 0 && as.numeric(input$period_2) - chiusura >= 1){
            list_shares_reb_act[[paste0(stock_i,".shares")]] <- list_shares_reb_final[[paste0(stock_i,".shares")]]
            list_spesa_reb_act[[paste0("spesa.",stock_i)]] <- 0
            list_vendita_reb_act[[paste0("spesa.",stock_i)]] <- 0
            list_profitto_reb_act[[paste0("profitto.",stock_i)]] <- 0
            
          }
        
          
        
        
        count <- count + 1
        
       
        
      }
      
      if(as.numeric(input$period_2) - chiusura >= 1){
        investimento_reb_final <- investimento_reb_final + investimento_reb_act
        cat("\n")
        cat("Fees totali in acquisto: $",round(fees_reb_act_buy_total, 2), "\n")
        cat("Fees totali in vendita: $",round(fees_reb_act_sell_total, 2), "\n")
        cat("\n")
        fees_reb_final_buy_total <- fees_reb_final_buy_total + fees_reb_act_buy_total
        fees_reb_final_sell_total <- fees_reb_final_sell_total + fees_reb_act_sell_total
      }
      
      temp <- temp + 1
      Mop_temp <- Mop_act
      list_shares_reb_pre <- list_shares_reb_act
      list_spesa_reb_pre <- list_spesa_reb_act
      list_vendita_reb_pre <- list_vendita_reb_act
      list_profitto_reb_pre <- list_profitto_reb_act
      
      
      
      
      cat("-------------------------------------------","\n")
      
      
      if(as.numeric(input$period_2) - chiusura < 1){
        
        tasse_final <- 0
        
        cat("Chiusura delle posizioni (",endPeriod_reb,"):\n\n")
        
        for(stock_i in input$id){
          list_spesa_reb_final[[paste0("vendita.",stock_i)]] <- 0
          list_vendita_reb_final[[paste0("vendita.",stock_i)]] <- list_shares_reb_final[[paste0(stock_i,".shares")]] * as.numeric(stocks_reb[endPeriod_reb, paste0(stock_i,".Adjusted")])
          fees_reb_final_sell <- list_vendita_reb_final[[paste0("vendita.",stock_i)]] * 0.02
          tasse_final <- tasse_final + fees_reb_final_sell
          fees_reb_final_sell_total <- fees_reb_final_sell_total + fees_reb_final_sell
          list_profitto_reb_final <- list_profitto_reb_final + (list_vendita_reb_final[[paste0("vendita.",stock_i)]] - fees_reb_final_sell)
          
          
          
          cat(" - ",stock_i, ":", list_shares_reb_final[[paste0(stock_i,".shares")]], "quote vendute a $", 
              round(as.numeric(stocks_reb[endPeriod_reb, paste0(stock_i,".Adjusted")]), 2)," cad. --> $",
              round(list_vendita_reb_final[[paste0("vendita.",stock_i)]], 2),"\n")
        }
        
        cat("\n")
        cat("Fees totali in vendita: $", round(tasse_final, digits = 2), "\n")
      }
      
      
      chiusura <- chiusura + 1
      
    }
  
    
   ritorno_netto <- round(list_profitto_reb_final - (investimento_reb + fees_reb_final_buy_total), digits = 2)
   cat("\n")
   cat("-------------------------------------------","\n")
   cat("-------------------------------------------","\n")
   cat("Resoconto finale dell'investimento:\n","\n")
   cat("Fees totali in acquisto: $", round(fees_reb_final_buy_total, digits=2), "\n")
   cat("Fees totali in vendita: $", round(fees_reb_final_sell_total, digits=2), "\n")
   cat("Il valore del portafoglio iniziale (2015) : $", round(investimento_reb + fees_reb_buy_total, digits = 2))
   cat("\n")
   cat("Il ritorno atteso del portafoglio iniziale: ",round((Mop$pm*100)*as.numeric(input$period_2), 2)," %\n")
   cat("Il livello di rischio del portafoglio iniziale: ",round(Mop$ps*100, 2)," %\n")
   cat("Somma ottenuta alla chiusura delle posizioni durante l'intero investimento: $", round(list_profitto_reb_final, digits = 2), "\n")
   cat("Ritorno effettivo: $", ritorno_netto, "( ",round((ritorno_netto/(investimento_reb+fees_reb_buy_total)  * 100), digits = 2)," %)\n" )
   cat("\n")
  
  
    
  })
  
  
}



# Create Shiny object
shinyApp(ui = ui, server = server)

