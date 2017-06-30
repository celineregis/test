library(shiny)
library(data.table)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(quantmod)
library(RColorBrewer)
library(gridExtra)
library(formattable)
library(devtools)
library(plotly)
#install_github("hadley/ggplot2")

source('list_transactions.R')
# read in data
#inputData2         <- read.csv("./table2.csv", header = TRUE)
inputData1         <- read.csv("./poc_sample_data.csv", header = TRUE)
input_revenues     <- read.csv("./revenues.csv", header = TRUE)
dt1                <- data.table(inputData1)
dt1_raw            <- dt1[,.(month,raw_string,spend)]

  header           <- dashboardHeader(
                      title = "ADG Data Analysis"
                      )
  
  body             <- dashboardBody(
                       fluidRow(
                         column(4,
                          box( title = "Select a transaction:",status ="primary",width = NULL, solidHeader = TRUE,DT::dataTableOutput('x3') )),
                         column(8,
                          fluidRow(
                           box(title = "Key information", status = "warning",width = NULL, solidHeader = TRUE,tableOutput('x4'))),
                          fluidRow(
                          box(width = NULL, plotOutput("plot1"))))),
                       fluidRow(
                          box(title = "Transactions for all brands",status ="primary",width = 4,solidHeader = TRUE,DT::dataTableOutput('x5')),
                          box(radioButtons("metric", "Metric", c("sum", "mean","count"), inline = TRUE),plotOutput("plot2"))),
                       fluidRow(
                         box(width = 4,radioButtons("scala", "Log transform", c("false", "true"), inline = TRUE),plotlyOutput("plot3")),
                         box(width = 4,radioButtons("scala2", "Log transform", c("false", "true"), inline = TRUE),plotlyOutput("plot4"))
                         
                         )
)

  ui               <- dashboardPage(header,dashboardSidebar(disable = TRUE),body )

  
server             <- function(input, output, session) {
  

  selection_row    <- reactive ({ input$x3_rows_selected })
  
  output$x3        <- DT::renderDataTable(dt1_raw,selection = 'single',options = list(scrollY = '500px',pageLength = 50) )
  # paging = FALSE)
  # print the infos related to the selected transaction
  output$x4        <- renderTable({
                      s <- selection_row()
                      if (length(s)) {
                        mapping_table            <- data.frame(Characters=character(),Characters=character(),stringsAsFactors=FALSE)
                        
                        company_name             <- inputData1[s,"company_name"]
                        mapping_table[1,]        <- c("Selected brand",as.character(company_name))
                        ticker                   <- inputData1[s,"symbol"]
                        mapping_table[2,]        <- c("Company ticker",as.character(ticker))
                       
                       brands                    <- unique(inputData1%>% filter(grepl(ticker,symbol))%>%group_by(company_name)%>%select(company_name))
                       mapping_table[3,]         <- c("All brands",paste(as.character(brands$company_name),collapse = ", "))
                       
                       total_val                 <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(total=sum(spend))
                       mapping_table[4,]         <- c(paste("Total transactions value for",as.character(inputData1[s,"company_name"])),as.character(currency(total_val,digits = 0L)))
                      
                       total_avg                 <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(total=round(mean(spend)))
                       mapping_table[5,]         <- c(paste("Average transactions size for",as.character(inputData1[s,"company_name"])),as.character(currency(total_avg,digits = 0L)))
                       
                       total_nb                  <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(count=n())
                       mapping_table[6,]         <- c(paste("Total number of transactions for",as.character(inputData1[s,"company_name"])),paste(as.character(total_nb)))
                       
                       mapping_table
                      }},colnames=FALSE
                      )
  #table of transactions from same merchant
  output$x5        <- renderDataTable({
                      s <- selection_row()
                      if (length(s)) {
                       ticker                <- as.character(inputData1[selection_row(),"symbol"])
                       same_merchant_transac <- unique(filter(inputData1, grepl(paste("^",ticker,"$",sep = ""),symbol))["raw_string"])
                       transactions          <-  inputData1 %>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>% group_by (raw_string) %>% summarize(count=n())
                      }},
                      options = list( 
                        scrollY = '300px', paging = FALSE 
                       ), caption=paste("Transactions for ticker:",ticker) 
                      )
  metric_plot          <- reactive({
                      input$metric
                    })
  #plot the transactions statistics
  output$plot2    <- renderPlot({
                       s                                    <- selection_row()
                       if (length(s)) {
                         ticker                               <- as.character(inputData1[s,"symbol"])
                         transactions_by_company              <- inputData1 %>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>%group_by(company_name)
                         transactions_by_company[,"Month"]    <- format(as.Date(transactions_by_company$month),"%Y-%m")
                         
                         if(metric_plot()=="count"){
                           monthly_metric_by_company         <- transactions_by_company %>% group_by(company_name,Month) %>% summarize(metric=n())#%>% mutate(metric = n / sum(n))
                         }else{
                        string1                              <-'transactions_by_company %>% group_by(company_name,Month) %>% summarize(metric='
                        string2                              <- paste(metric_plot(),'(spend))',sep="")
                        ourCommand                           <- paste(string1,string2)
                        monthly_metric_by_company            <- eval(parse(text=ourCommand))
                         }

                         
                        ggplot(data=monthly_metric_by_company, aes(x=Month, y=metric, fill=company_name)) +
                          geom_bar(stat="identity", position= position_stack(reverse = TRUE))+#position="stack")+
                          scale_fill_brewer(direction = -1)+
                          ggtitle(paste("ADG Transactions Metric for",as.character(monthly_metric_by_company [1,]$company_name)))+
                          theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
                                axis.line = element_line(colour = "black"),
                                axis.text.x = element_text(angle = 90, hjust = 1),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                panel.background = element_blank())
                        
                        }})
#plot the transactions and revenues over time
  output$plot1    <- renderPlot({
                     s <- selection_row()
                     if (length(s)) {
                     ticker                 <- as.character(inputData1[s,"symbol"])
                     # retrieve all transactions for that ticker
                     transactions           <- filter(inputData1, grepl(paste("^",ticker,"$",sep = ""),symbol))
                     # get reported revenue from google for that ticker
                     #tickerFunc             <- getFinancials(Symbol=ticker, src="google")
                     #ourCommand             <- paste(tickerFunc,'$IS$Q["Revenue",]',sep = "")
                     #Total.Revenue          <- eval(parse(text=ourCommand))
                     #"^apple$"
                     input_revenues         <- filter(input_revenues, grepl(paste("^",ticker,"$",sep = ""),symbol))
                     if(nrow(input_revenues)>0){
                     
                     Revenues_quarter       <- paste("Q",apply(input_revenues["quarter"],1,as.character),sep = "")
                     Revenues_year          <- apply(input_revenues["year"],1,as.character)
      
                     transactions$Date      <- as.Date(transactions$month)
                     transactions$Quarter   <- quarters(transactions$Date) 
                     transactions$Year      <- format(transactions$Date,"%Y")
      
                     transactions_quaterly  <- transactions %>%
                                               group_by(Year,Quarter) %>%
                                               summarize(Quarterly = sum(spend))
      
      

      transac_data          <- cbind(transactions_quaterly$Year,paste(  transactions_quaterly$Quarter,transactions_quaterly$Year), transactions_quaterly$Quarterly,"ADG Total spent")
      
      #revenues data
      rev_data              <- cbind(Revenues_year,paste(Revenues_quarter,Revenues_year),input_revenues["revenue"],"Reported revenues")
      # compute scaling factor for secondary axis
      scaling_factor        <- as.numeric(max(transac_data[,3]))/max(as.numeric(rev_data[,3]))
      
      #rescale transaction data
      transac_data[,3]      <- (as.numeric(transac_data[,3]) / scaling_factor)
      
      data                  <- rbind(as.matrix(rev_data),transac_data)
      colnames(data)        <- c("Year","Quarter","Reported revenues","Type")
      data[,3]              <- as.numeric(data[,3])
      data_df               <- data.frame(data)
      df_sorted             <- arrange(data_df,Year,Quarter)

      # select last 3Y of data
      cutoff_year           <- format(as.Date(Sys.Date()-365*3),"%Y")
      idx_ok                <- which(as.numeric(as.character(df_sorted$Year))>=as.numeric(cutoff_year))
      df_sorted             <- df_sorted[idx_ok,]
      # order by quarter
      df_sorted$Quarter     <- factor(df_sorted$Quarter, levels = df_sorted$Quarter)

      df_sorted$Reported.revenues <- as.numeric((df_sorted$Reported.revenues))
      #x=Quarter
       p                     <- ggplot(data=df_sorted, aes(x= Quarter, y=Reported.revenues, fill=Type)) +
        geom_bar( stat="identity",position=position_dodge(preserve = "single")) + 
        scale_fill_brewer(direction = -1)+
        ggtitle(paste("Reported revenues vs ADG Total spent for",as.character(transactions[1,]$company_name)))+
        theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5),
              
              axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank())
      # secondary axis
      p +  scale_y_continuous( sec.axis = sec_axis(~.*scaling_factor,name ="ADG Total spent" ))+
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
   }}
    
  })
  
  scala         <- reactive({
    input$scala
  })
  scala2         <- reactive({
    input$scala2
  })
  ### pie chart for % total spent for transactions over previous calendar year

  output$plot3                       <- renderPlotly({
    
    s                                 <- selection_row()
    if(length(s)){
     
      
      inputData1$Date                 <- as.Date(inputData1$month)
      inputData1$Year                 <-  format(inputData1$Date,"%Y")
      
      ticker                          <- as.character(inputData1[s,"symbol"])
      # retrieve all transactions for that ticker
      transactions                    <- filter(inputData1, grepl(paste("^",ticker,"$",sep = ""),symbol))
      
      unique_tickers                  <- inputData1%>%group_by(symbol)%>%summarize(count=n())
      count_tickers                   <- nrow(unique_tickers)
      others                          <- count_tickers-5
      
      transactions_last_year_top5     <-  inputData1  %>% filter(grepl("2016",Year))%>%
                                          group_by(symbol)%>%
                                          summarize(spent = sum(spend))%>%
                                          top_n(n = 5, wt = spent)
      transactions_last_year_others   <-  inputData1  %>% filter(grepl("2016",Year))%>%
                                          group_by(symbol)%>%
                                          summarize(spent = sum(spend))%>%
                                          top_n(n =-others, wt = spent)
      transactions_last_year_ticker   <-  transactions  %>% filter(grepl("2016",Year))%>%
                                          group_by(symbol)%>%
                                          summarize(spent = sum(spend))
      transactions_last_year_others$symbol <- "Others"
      
      transactions_last_year_others_agg    <- transactions_last_year_others%>%group_by(symbol)%>%summarize(spent = sum(spent))
      transactions_split                   <- unique(rbind(transactions_last_year_ticker,transactions_last_year_top5,transactions_last_year_others_agg))
      
      transactions_split$category          <- "Top 5"
      transactions_split[which(transactions_split["symbol"]==ticker),"category"]<-ticker
      transactions_split[which(transactions_split["symbol"]=="Others"),"category"]<-"Others"
      boolColors <- as.character(c(ticker="#ffa500", "Top 5"="royalblue","Others"="#6699ff"))##6699cc
     
      transactions_split <- transactions_split %>% mutate(pos = cumsum(spent)- spent/2)
 
      if(length(transactions_split)){
      
        pt1<- plot_ly(transactions_split, labels = ~category, values = ~spent, type = 'pie',
                textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste( currency(spent,digits = 0L)),
                marker = list(colors = boolColors,
                              line = list(color = '#FFFFFF', width = 1)),
                #The 'pull' attribute can also be used to create space between the sectors
                showlegend = FALSE) %>%
                layout(title =  paste("ADG Total spent for 2016 - ",ticker,"/ top 5 / Others"),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),#type=ifelse(TRUE,"log","linear")
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,type="linear"))

        pt1
        
      }
    }
    })
  
  output$plot4                       <- renderPlotly({

    s                                 <- selection_row()
    if(length(s)){

      input_revenues                 <-  input_revenues[!is.na(input_revenues["symbol"]),]
      revenues_last_year             <-  input_revenues  %>% filter(grepl("2016",year))
      revenues_last_year_top5        <-  data.frame(revenues_last_year%>%
                                                    group_by (symbol)%>%
                                                    summarize(Rev=sum(as.numeric(revenue)))%>%
                                                    top_n(n = 5, wt =Rev),stringsAsFactors =FALSE)
      count_rev_tickers             <- length(unique(revenues_last_year$symbol))
      others                        <- max(1,count_rev_tickers-5)
      
      revenues_last_year_others      <-  data.frame(revenues_last_year%>%
                                                    group_by (symbol)%>%
                                                    top_n(n = -others, wt =revenue),stringsAsFactors =FALSE)#summarize(Rev=sum(as.numeric(revenue)))%>%
     revenues_last_year_others_agg    <- revenues_last_year_others%>%group_by(symbol)%>%summarize(Rev = sum(as.numeric(revenue)))
      
      
      revenues_last_year_ticker      <-  data.frame(revenues_last_year%>%
                                                      filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>%
                                                      group_by (symbol)%>%
                                                      summarize(Rev=sum(as.numeric(revenue))),stringsAsFactors =FALSE)
                                                      
      revenues_last_year_others_agg$symbol <- "Others"

     #revenues_ticker                <- data.frame(cbind(as.character(all_tickers[idx]),t(revenues_last_year[-1])),stringsAsFactors = FALSE)
       
       revenues_split                 <- unique(rbind(revenues_last_year_ticker,revenues_last_year_top5,revenues_last_year_others_agg))
     
       revenues_split$category        <- "Top 5"
       revenues_split[which(revenues_split["symbol"]==ticker),"category"]  <- ticker
       revenues_split[which(revenues_split["symbol"]=="Others"),"category"]<- "Others"
       boolColors <- as.character(c(ticker="#ffa500", "Top 5"="royalblue","Others"="#6699ff"))
       revenues_split <- revenues_split %>% mutate(pos2 = cumsum(Rev)- Rev/2)
      #p2             <- plotly_empty()
      if(length(revenues_split)){
        pt2           <- plot_ly(revenues_split, labels = ~category, values = ~Rev, type = 'pie',
                      textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      hoverinfo = 'text',
                      text = ~paste( currency(Rev,digits = 0L)),
                      marker = list(colors = boolColors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      #The 'pull' attribute can also be used to create space between the sectors
                      showlegend = FALSE) %>%
                      layout(title =  paste("Reported Revenues for 2016 - ",ticker,"/ top 5 / Others"),
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,type="linear"))
       
      
        pt2
      }
  
    }

   })
 
  
    
}
shinyApp(ui = ui, server = server)