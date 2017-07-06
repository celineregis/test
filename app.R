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
library(R.cache)
#install_github("hadley/ggplot2")

source('list_transactions.R')
# read in data

inputIndustries    <- loadCache(list(1E3))
if (is.null(inputIndustries)) {
  inputIndustries   <- read.csv("./Industry.csv", header = TRUE)
  saveCache(inputIndustries, key=list(1E3))
}

inputData1        <- loadCache(list(2E3))
if (is.null(inputData1)) {
  inputData1   <- read.csv("./poc_sample_data.csv", header = TRUE)
  saveCache(inputData1, key=list(2E3))
}

input_revenues    <- loadCache(list(3E3))
if (is.null(input_revenues)) {
  input_revenues   <- read.csv("./revenues.csv", header = TRUE)
  saveCache(input_revenues, key=list(3E3))
}

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
             box(title = "Key information", status = "warning",width = NULL, solidHeader = TRUE,
                 
                 tableOutput('x4'))),
           fluidRow(
             box(title = "Reported revenues vs ADG Total spent",status ="primary", solidHeader = TRUE,width = NULL,radioButtons("Menu", "", c("Actual", "YoY"), inline = TRUE), plotOutput("plot1"))))),
  fluidRow(
    #  color:#fff;
    box(title = textInput("mytitle","",""),   
        tags$head(tags$style("#mytitle{color: white;
                             background:#3c8dbc;
                             font-style: bold;
                             border-bottom-color:#3c8dbc;
                             border-left-color:#3c8dbc;
                             border-right-color:#3c8dbc;
                             border-top-color:#3c8dbc;
                             display: inline-block;
                             font-size: 18px;
                             margin: 0;
                             line-height: 1;
                             }")),
                         status ="primary",width = 4,solidHeader = TRUE,DT::dataTableOutput('x5')),
    box(title = "Total spend by brand",status ="primary", solidHeader = TRUE,width = 8,radioButtons("metric", "", c("sum", "mean","count"), inline = TRUE),plotOutput("plot2"))),
  fluidRow(
    box(width = 6,plotlyOutput("plot3"),radioButtons("scala", "Log transform", c("false", "true"), inline = TRUE)),
    box(width = 6,plotlyOutput("plot4"),radioButtons("scala2", "Log transform", c("false", "true"), inline = TRUE))
    
  )
        )

ui               <- dashboardPage(header,dashboardSidebar(disable = TRUE),body )


server             <- function(input, output, session) {
  
  
  selection_row    <- reactive ({ input$x3_rows_selected })
  
  output$x3        <- DT::renderDataTable({
    names(dt1_raw)           <- c("Date","Raw transaction text","Amount")
    #dt1_raw$Amount           <- currency( dt1_raw$Amount,digits = 0L)
    dt1_raw
  },
  selection = 'single',options = list(scrollY = '500px',pageLength = 50) )
  # paging = FALSE)
  # print the infos related to the selected transaction
  output$x4        <- renderTable({
    s <- selection_row()
    if (length(s)) {
      mapping_table            <- data.frame(Characters=character(),Characters=character(),stringsAsFactors=FALSE)
      
      company_name             <- inputData1[s,"company_name"]
      mapping_table[1,]        <- c(paste("<strong>", "Mapped brand","</strong>"),paste("<strong>",as.character(company_name),"</strong>"))
      ticker                   <- inputData1[s,"symbol"]
      mapping_table[2,]        <- c(paste("<strong>", "Mapped ticker","</strong>"),paste("<strong>",as.character(ticker),"</strong>"))
      
      brands                    <- unique(inputData1%>% filter(grepl(ticker,symbol))%>%group_by(company_name)%>%select(company_name))
      mapping_table[3,]         <- c("Brands identified",paste(as.character(brands$company_name),collapse = ", "))
      
      total_val                 <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(total=sum(spend))
      mapping_table[4,]         <- c(paste("Total transactions value for",as.character(inputData1[s,"company_name"])),as.character(currency(total_val,digits = 0L)))
      
      total_avg                 <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(total=round(mean(spend)))
      mapping_table[5,]         <- c(paste("Average transactions size for",as.character(inputData1[s,"company_name"])),as.character(currency(total_avg,digits = 0L)))
      
      total_nb                  <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(count=n())
      mapping_table[6,]         <- c(paste("Total number of transactions for",as.character(inputData1[s,"company_name"])),paste(as.character(total_nb)))
      
      mapping_table
    }},sanitize.text.function=function(x){x},colnames = FALSE
  )
  
  #table of transactions from same merchant
  output$x5        <- renderDataTable({
    s <- selection_row()
    if (length(s)) {
      ticker                <- as.character(inputData1[selection_row(),"symbol"])
      brand_selected        <- as.character(inputData1[selection_row(),"company_name"])
      updateTextInput(session,"mytitle",value= paste("All",ticker,"brands' transaction text"))
      
      same_merchant_transac <- unique(filter(inputData1, grepl(paste("^",ticker,"$",sep = ""),symbol))["raw_string"])
      transactions          <- inputData1 %>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>% group_by (company_name,raw_string) %>% summarize(Count=n())
      names(transactions)   <- c("Brand","Transaction text","Count")
      ##
      if(nrow(unique(transactions["Brand"]))>1){
        other_brands          <- unique(transactions[which(transactions["Brand"]!=brand_selected) ,"Brand"])
        
        datatable(transactions) %>% formatStyle(
          'Brand',
          target = 'row',
          backgroundColor = styleEqual(c(brand_selected,other_brands),c('aliceblue',"antiquewhite")))
      }else{
        transactions
      }
    }},
    options = list( 
      scrollY = '300px', paging = FALSE,searching = FALSE , order = list(1, 'asc')
    ),rownames= FALSE 
    
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
      names(transactions_by_company )      <-c("month","raw_String","spend","Brand","symbol","Month")
      if(metric_plot()=="count"){
        monthly_metric_by_company         <- transactions_by_company %>% group_by(Brand,Month) %>% summarize(metric=n())
        title_y                           <- "Number of transactions by brand"
      }else{
        string1                              <-'transactions_by_company %>% group_by(Brand,Month) %>% summarize(metric='
        string2                              <- paste(metric_plot(),'(spend))',sep="")
        ourCommand                           <- paste(string1,string2)
        monthly_metric_by_company            <- eval(parse(text=ourCommand))
        monthly_metric_by_company[,3]        <-  apply(monthly_metric_by_company[,3],1,as.numeric)/1E3 # transactions expressed in k USD
        title_y                              <- "Total spend in k USD"
        # if(max(monthly_metric_by_company[,3])>0.5){#change to billions scale
        nb_char                              <- apply(floor( monthly_metric_by_company[,3]),1,nchar)
        nb_char[which(nb_char>1)]            <- 0
        rounded_metric                       <- round(monthly_metric_by_company[,3],nb_char)
        monthly_metric_by_company[,3]        <- rounded_metric
        #}else{
        #  monthly_metric_by_company[,3]  <-round(apply(monthly_metric_by_company[,3],1,as.numeric))
        #}
        
      }
      ggplot(data=monthly_metric_by_company, aes(x=Month, y=metric, fill=Brand)) +
        geom_bar(stat="identity", position= position_stack(reverse = TRUE))+
        scale_fill_brewer(direction = -1,palette="Paired")+
        ggtitle(paste(ticker,"Total spent by brand"))+  
        
        stat_summary(fun.y = sum, aes(label = ..y.., group = Month), geom = "text",vjust = -.2)+
        labs(y=title_y)+
        theme(plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5,size=18),
              axis.line = element_line(colour = "black"),
              axis.text.x = element_text(angle = 90, hjust = 1,size=12),
              axis.text.y = element_text(size=12),
              axis.title.y = element_text(size=12,face="bold"),
              legend.text=element_text(size=12),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              legend.position = "bottom",
              panel.background = element_blank())
      
    }})
  menu_plot          <- reactive({
    input$Menu
  })
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
        
        
        transac_data           <- cbind(transactions_quaterly$Year,paste(  transactions_quaterly$Quarter,transactions_quaterly$Year), transactions_quaterly$Quarterly,"ADG Total spent")
        
        #YoY data
        transac_data_YOY       <- cbind(transactions_quaterly$Year,paste(transactions_quaterly$Year,transactions_quaterly$Quarter), transactions_quaterly$Quarterly,"ADG Total spent")
        quarter_1Y_ago         <- paste(as.numeric(transactions_quaterly$Year)-1,transactions_quaterly$Quarter)
        idx_available          <- which(quarter_1Y_ago %in% paste(transactions_quaterly$Year, transactions_quaterly$Quarter))
        data_available         <- transactions_quaterly[idx_available,]
        data_available_1Y_ago  <- transac_data_YOY[transac_data_YOY[,2] %in% quarter_1Y_ago[idx_available],]
        transac_YOY            <- cbind(paste(data_available$Quarter,data_available$Year),data_available$Quarterly/as.numeric(data_available_1Y_ago[,3])-1,"ADG Total spent")
        
        rev_quarter_1Y_ago     <- paste(input_revenues$year-1,"Q",input_revenues$quarter,sep="")
        rev_available_1Y_ago   <- input_revenues%>% filter(fiscalQ1 %in% rev_quarter_1Y_ago)
        rev_quarter            <- paste(rev_available_1Y_ago$year+1,"Q",rev_available_1Y_ago$quarter,sep="")
        rev_quarter_available  <- input_revenues%>% filter(fiscalQ1 %in% rev_quarter)
        rev_YOY                <- cbind(paste(paste("Q",rev_quarter_available$quarter,sep=""),rev_quarter_available$year),as.numeric(rev_quarter_available$revenue)/as.numeric(rev_available_1Y_ago$revenue)-1,"Reported revenues")
        #revenues data
        rev_data                               <- cbind(Revenues_year,paste(Revenues_quarter,Revenues_year),input_revenues["revenue"],"Reported revenues")
        # compute scaling factor for secondary axis
        max_rev                                <- max(apply(rev_data["revenue"],1,as.numeric))*1E6
        
        
        
        if(max_rev>500*1E6){#change to billions scale
          rev_is_billion       <- TRUE
          rev_data["revenue"]  <- apply(rev_data["revenue"],1,as.numeric)/1000
          max_rev              <- max_rev/1E9
          nb_char              <- apply(floor( rev_data["revenue"]),1,nchar)
          nb_char[which(nb_char>1)]<-0
          rounded_rev          <- round(rev_data["revenue"],nb_char)
          rev_data["revenue"]  <- rounded_rev
        }else{
          rev_data["revenue"] <- round(apply(rev_data["revenue"],1,as.numeric))
        }
        transac_data[,3]      <- as.numeric(transac_data[,3])/1E3 # transactions expressed in k USD
        nb_char               <- nchar( floor( as.numeric(transac_data[,3])))
        nb_char[which(nb_char>1)]<-0
        last_3y_transac       <- as.numeric(transac_data[which(transac_data[,1]>=as.numeric(format(as.Date(Sys.Date()-365*3),"%Y"))),3])
        nb_char               <- nb_char[which(transac_data[,1]>=as.numeric(format(as.Date(Sys.Date()-365*3),"%Y")))]
        rounded_transac       <- round(last_3y_transac,nb_char)
        
        
        scaling_factor        <- max(as.numeric(transac_data[,3]))/(max(apply(rev_data["revenue"],1,as.numeric)))#reported rev in million
        
        #rescale transaction data
        transac_data[,3]      <- as.numeric(transac_data[,3])/ scaling_factor 
        
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
        
        
        df_sorted$Reported.revenues <- as.numeric(levels(df_sorted$Reported.revenues))[df_sorted$Reported.revenues]
        
        df_sorted[which(df_sorted["Type"]=="Reported revenues"),"Rev"]<-df_sorted[which(df_sorted["Type"]=="Reported revenues"),"Reported.revenues"]
        df_sorted[which(df_sorted["Type"]=="ADG Total spent"),"Spent"]<- rounded_transac
        
        if( menu_plot()=="Actual"){
          p                     <- ggplot(data=df_sorted, aes(x= Quarter, y=Reported.revenues, fill=Type)) +
            geom_bar( stat="identity",position=position_dodge(preserve = "single")) +
            #scale_fill_manual(name = "Type", label = unique(df_sorted$Type),values = c("royalblue","#6699ff"))
            scale_fill_brewer(direction = -1,palette="Paired")+
            ggtitle(paste("Ticker",ticker, "\nReported revenues vs ADG mapped"))+
            scale_y_continuous(limits = c(0, max_rev))+
            geom_text(aes(label = Rev,size=1),position=position_dodge(width=0.7), vjust=-0.25)+
            geom_text(aes(label = Spent,size=1),position=position_dodge(width=0.7), vjust=-0.25)+
            theme(plot.title = element_text(size = 18,lineheight=.8, face="bold",hjust = 0.5),
                  axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x=element_blank(),
                  axis.title.y = element_text(colour="#9ecae1",face = "bold", size = 12),
                  legend.text=element_text(size=12),
                  legend.position = "bottom")
          p <-  p+labs(y="Reported in USD mio")
          
          if(rev_is_billion){
            p <-  p+labs(y="Reported in USD bn")
          }
          # secondary axis
          p +  scale_y_continuous( sec.axis = sec_axis(~.*scaling_factor,name ="Mapped spent in k USD"))+
            
            theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.y.right = element_text(colour="#3182bd"))
          
        }else{
          df_YOY           <- data.frame(rbind(rev_YOY ,transac_YOY))
          names(df_YOY)    <- c("Quarter","YOY","Type")
          df_YOY$Quarter   <- factor(df_YOY$Quarter, levels = df_YOY$Quarter)
          df_YOY$YOY       <- as.numeric(levels(df_YOY$YOY))[df_YOY$YOY]
          p_yoy             <- ggplot(data=df_YOY, aes(x=Quarter, y=YOY, group=Type,colour=Type)) +
            geom_line(size=1)+
            geom_point(size=2) + 
            scale_y_continuous(labels = scales::percent)+
            theme(plot.title = element_text(size = 18,lineheight=.8, face="bold",hjust = 0.5),
                  axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  axis.text.x = element_text(size=12),
                  axis.text.y = element_text(size=12),
                  axis.title.x=element_blank(),
                  axis.title.y = element_text(face = "bold", size = 12),
                  legend.text=element_text(size=12),
                  legend.position = "bottom")+
            ylab("YOY % Change") + 
            ggtitle(paste("Ticker",ticker, "\nYOY Reported Revenue vs ADG mapped")) 
          p_yoy
        }
      }
    }
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
    if(length(s)<1){return()
    }else{#if(length(s)){
      
      
      inputData1$Date                 <- as.Date(inputData1$month)
      inputData1$Year                 <-  format(inputData1$Date,"%Y")
      
      ticker                          <- as.character(inputData1[s,"symbol"])
      
      #retrieve ticker's industry
      industry_data                   <- inputIndustries %>%
        select_("tickerID","IQ_INDUSTRY_GROUP")
      
      names(industry_data)            <- c("symbolID","industry")
      
      ticker_industry                 <- filter(industry_data, grepl(paste(ticker,"$",sep = ""),symbolID))
      
      tickersID_same_industry         <- filter(industry_data, grepl(paste("^",ticker_industry$industry,"$",sep = ""),industry))
      
      tickers_unlisted                 <- unlist( strsplit(as.character(tickersID_same_industry$symbolID),":"))
      
      tickers_same_industry            <- tickers_unlisted[which(((1:(2*nrow(tickersID_same_industry)))%%2)==0)]
      
      tickers_same_industry            <-tickers_same_industry[-which(tickers_same_industry==ticker)] 
      
      
      count_tickers                   <- length(tickers_same_industry)
      
      others                          <- max(1,count_tickers-5)
      
      transactions_last_year_top5       <-  inputData1 %>% filter(grepl("2016",Year))%>%
        filter(symbol%in% tickers_same_industry)%>% 
        group_by(symbol)%>% 
        summarize(spent = sum(spend))%>%
        top_n(n = 5, wt = spent)
      
      
      transactions_last_year_others     <-  inputData1 %>% filter(grepl("2016",Year))%>%
        filter(symbol%in% tickers_same_industry)%>% 
        group_by(symbol)%>% 
        summarize(spent = sum(spend))%>%
        top_n(n = -others, wt = spent)
      transactions                      <- filter(inputData1, grepl(paste("^",ticker,"$",sep = ""),symbol))
      transactions_last_year_ticker   <-  transactions  %>% filter(grepl("2016",Year))%>%
        group_by(symbol)%>%
        summarize(spent = sum(spend))
      transactions_last_year_others$symbol <- "Others"
      
      # transactions_last_year_others_agg    <- transactions_last_year_others%>%group_by(symbol)%>%summarize(spent = sum(spent))
      transactions_split                   <- unique(rbind(transactions_last_year_ticker,transactions_last_year_top5,transactions_last_year_others))
      
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
          layout(title =  paste(ticker,"ADG Total spent for 2016","in the",ticker_industry$industry,"industry"),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
        pt1
        
      }
    }
  })
  
  output$plot4                       <- renderPlotly({
    
    s                                 <- selection_row()
    if(length(s)<1){return()
    }else{#if(length(s)){
      ticker                          <- as.character(inputData1[s,"symbol"])
      industry_data                   <- inputIndustries %>%select_("tickerID","IQ_INDUSTRY_GROUP")
      
      names(industry_data)            <- c("symbolID","industry")
      
      ticker_industry                 <- filter(industry_data, grepl(paste(ticker,"$",sep = ""),symbolID))
      
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
          layout(title = paste(ticker,"Reported Revenues for 2016 in the",ticker_industry$industry,"industry"),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE,type="linear"))
        
        
        pt2
      }
      
    }
    
  })
  
  
  
}
shinyApp(ui = ui, server = server)