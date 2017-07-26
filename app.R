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
library(readr)
library(scales)
library(shinyjs) 
#install_github("hadley/ggplot2")

# read in data

inputIndustries    <- loadCache(list(1E3))
if (is.null(inputIndustries)) {
 inputIndustries   <- read_csv("./Industries_3kv1.csv")
 saveCache(inputIndustries, key=list(1E3))
}

inputData1        <- loadCache(list(2E3))
if (is.null(inputData1)) {
  inputData1   <- read_csv("./poc_sample_data.csv")
  saveCache(inputData1, key=list(2E3))
}

input_revenues    <- loadCache(list(3E3))
if (is.null(input_revenues)) {
  input_revenues   <- read_csv("./revenues_3kv1.csv")
  saveCache(input_revenues, key=list(3E3))
}

dt1                <- data.table(inputData1)
dt1_raw            <- dt1[,.(month,raw_string,spend)]

  header           <- dashboardHeader(
                      title = "ADG Data Analysis",
                      titleWidth = 300
                      )
  
  sidebar          <- dashboardSidebar(
                              width=300,
                              box(height=805,width=NULL,DT::dataTableOutput('x3'))) 
  
  body             <- dashboardBody(
                      tags$head(tags$style(HTML('.skin-blue .main-sidebar{background-color: #3c8dbc;}'))),
                      HTML("<div  style='min-width: 1024px;'>"),
                       useShinyjs(), 
                      div(id = "greetbox-outer",
                         
                          fluidRow(
                           
                           box(height=300,width = 12,title = "Key Information", status = "warning", solidHeader = TRUE,tableOutput('x4'))),
                          div(id = "greetbox-inner",
                          fluidRow(
                          box(height=470,width = 12,status ="primary", solidHeader = FALSE,radioButtons("Menu", "", c("Actual", "YoY"), inline = TRUE), plotOutput("plot1")))
                          ),
                          fluidRow(
                          box(height=550,title = textInput("mytitle","",""),   
                              tags$head(tags$style("#mytitle{color: black;
                                                  background:white;
                                                  font-style: bold;
                                                   border-bottom-color:white;
                                                   border-left-color:white;
                                                   border-right-color:white;
                                                   border-top-color:white;
                                                   display: inline-block;
                                                   font-size: 17px;
                                                   margin: 0;
                                                   line-height: 2;
                                                   width: 400px
                                                   }")),
                              
                         status ="primary",width = 5,solidHeader = FALSE,  div(id = "greetbox-selectInput",
                         DT::dataTableOutput("x6"),

                         tags$style(HTML("table.dataTable tr.selected td,
                                         table.dataTable td.selected {
                                         background-color: white;
                                         }
                                         ")),

                          tags$head(tags$style("#x6{color: blue;
                                                    border-bottom-color:white
                                                    border-top: none;
                                                    border-bottom:none
                                                    border-bottom: 0px 
                                                   }"))),
                         DT::dataTableOutput('x5')),
                          box(height=550,title = radioButtons("metric", "", c("sum", "mean","count"), inline = TRUE),status ="primary", solidHeader = FALSE,width =7 ,plotOutput("plot2"))),
                         div(id = "greetbox-pie",
                         fluidRow(
                         box(width = 6,status ="primary",radioButtons("scala", "Log transform", c("false", "true"), inline = TRUE),plotlyOutput("plot3")),
                         box(width = 6,status ="primary",radioButtons("scala2", "Log transform", c("false", "true"), inline = TRUE),plotlyOutput("plot4"))
                       ))
                      )
                      )

  ui               <- dashboardPage(header,sidebar ,body ) 

  
server             <- function(input, output, session) {
  

  selection_row    <- reactive ({ input$x3_rows_selected })
  
  observe({
    if (length(selection_row()) ){
      show(id = "greetbox-outer")
      ticker       <- as.character(inputData1[selection_row(),"symbol"])
      rev_ticker   <- filter(input_revenues, grepl(paste("^",ticker,"$",sep = ""),symbol))
      transactions<- inputData1 %>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>% select(company_name)
      
      if (nrow(rev_ticker) &!is.na(ticker)){show(id = "greetbox-inner")} else {hide(id = "greetbox-inner")}
      if (nrow(rev_ticker)&!is.na(ticker)){show(id = "greetbox-pie")} else {hide(id = "greetbox-pie")}
      if(nrow(unique(transactions))>1){show(id = "greetbox-selectInput")} else {hide(id = "greetbox-selectInput")}
    }else {
        hide(id = "greetbox-outer")}
    })
  

  output$x3        <- DT::renderDataTable({
                      names(dt1_raw)           <- c("Date","Raw Transaction Text","Amount")
                      colnames(dt1_raw)[c(1:3)] <- paste0('<span style="color:',c("black"),'">',colnames(dt1_raw)[c(1:3)],'</span>')
                      
                      dt1_raw
                      },rownames= FALSE ,escape=F,
                      selection = 'single',options = list(scrollY = '500px',pageLength = 100,dom='ftp',
                                                          rowCallback = JS(
                                                            'function(row, data) {',
                                                            '$("td", row).each(function(i) {',
                                                            '$(this).css("color", "black");',
                                                            '});',
                                                            '}')
                                                          ) )

  # print the infos related to the selected transaction
  industry_data             <- inputIndustries %>%select_("tickerID","IQ_PRIMARY_INDUSTRY","IQ_COMPANY_NAME_LONG")
  names(industry_data)      <- c("symbolID","industry","long_name")
  
  output$x4        <- renderTable({
                      s <- selection_row()
                      if (length(s)) {
                        mapping_table            <- data.frame(Characters=character(),Characters=character(),stringsAsFactors=FALSE)
                        
                        company_name             <- inputData1[s,"company_name"]
                        ticker                   <- inputData1[s,"symbol"]
                        #retrieve ticker's industry
                        ticker_info               <- filter(industry_data, grepl(paste(":",ticker,"$",sep = ""),symbolID))
                        if(nrow(ticker_info)>0){
                        long_name                 <- strsplit(ticker_info$long_name,"\\(")[[1]][1]
                        mapping_table[1,]         <- c(paste("<strong>","Mapped Company","</strong>"),paste("<strong>",as.character(long_name),"</strong>"))
                        mapping_table[2,]         <- c(paste("<strong>", "Mapped Ticker","</strong>"),paste("<strong>",as.character(ticker),"</strong>"))
                        mapping_table[3,]         <- c("Sector Identified",as.character(ticker_info$industry))
                        mapping_table[4,]         <- c("Mapped Brand",as.character(company_name))
                        
                        brands                    <- unique(inputData1%>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>%group_by(company_name)%>%select(company_name))
                       mapping_table[5,]          <- c("Brands Identified",paste(as.character(brands$company_name),collapse = ", "))
                       
                       total_val                  <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(total=sum(spend))
                       mapping_table[6,]          <- c(paste("Total Transactions Value for",as.character(inputData1[s,"company_name"])),as.character(currency(total_val,digits = 0L)))
                      
                       total_avg                 <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(total=round(mean(spend)))
                       mapping_table[7,]         <- c(paste("Average Transactions Size for",as.character(inputData1[s,"company_name"])),as.character(currency(total_avg,digits = 0L)))
                       
                       total_nb                  <- inputData1%>% filter(grepl(as.character(inputData1[s,"company_name"]),company_name))%>%summarise(count=n())
                       mapping_table[8,]         <- c(paste("Total Number of Transactions for",as.character(inputData1[s,"company_name"])),paste(as.character(total_nb)))

                       input_revenues             <- filter(input_revenues, grepl(paste("^",ticker,"$",sep = ""),symbol))
                       
                       }else{
                         mapping_table[1,]         <- c("Mapped Brand",as.character(company_name))
                         mapping_table[2,]        <- c(paste("<strong>","No reported revenue available for",as.character(inputData1[s,"company_name"]),"</strong>"),"")
                         rev_available            <- FALSE
                         
                       }
                       mapping_table
                      }},sanitize.text.function=function(x){x},colnames = FALSE
                      )
  
  #table of transactions from same merchant
  
  output$x6 <- DT::renderDataTable({
    s                       <- selection_row()
    if (length(s)) {
      ticker                <- as.character(inputData1[selection_row(),"symbol"])
      brands                <- unique(inputData1%>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>%group_by(company_name)%>%select(company_name))
      brands[nrow(brands)+1,"company_name"] <- "All"
      tagString             <- paste("<ins>",brands$company_name,"</ins>") #<a href='#x6'></a> #, caption="Select a Brand:"
      max_length_per_row <-6
      if(length(tagString)>max_length_per_row-1){
        d                   <- matrix(tagString, nrow = ceiling(length(tagString)/max_length_per_row), byrow = TRUE)
        modulo              <- nrow(brands)%% ncol(d)
        if(modulo  !=0){
          d[nrow(d),(modulo+1):ncol(d)] <-""
        }
        DT::datatable(d ,escape=FALSE,selection=list(mode="single",target="cell"),rownames=FALSE , colnames=replicate( ncol(d), ""),options = list(columnDefs =list(className = 'dt-center', targets = 0:(nrow(brands)-1)),bSort=FALSE,paging=FALSE,searching = FALSE,dom='tp'))
       }else{
        DT::datatable(t(tagString) ,escape=FALSE,selection=list(mode="single",target="cell"),rownames=FALSE , colnames=replicate( nrow(brands), ""),options = list(columnDefs =list(className = 'dt-center', targets = 0:(nrow(brands)-1)),bSort=FALSE,paging=FALSE,searching = FALSE,dom='tp'))
      }
    }
    })

  output$x5                 <- DT::renderDataTable({
    s                       <- selection_row()
    if (length(s)) {
      ticker                <- as.character(inputData1[selection_row(),"symbol"])
      ticker_info           <- filter(industry_data, grepl(paste(":",ticker,"$",sep = ""),symbolID))
      if(nrow(ticker_info)>0){
       long_name            <- strsplit(ticker_info$long_name,"\\(")[[1]][1]
       }else{ #company name not available as not matched in the revenue file
         long_name          <-  ticker
       }
       brand_selected        <- as.character(inputData1[selection_row(),"company_name"])
       brands                <- unique(inputData1%>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>%group_by(company_name)%>%select(company_name))
      
       transactions          <- inputData1 %>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>% group_by (company_name,raw_string) %>% summarize(Count=n())
       names(transactions)   <- c("Brand","Transaction text","Count")
       updateTextInput(session,"mytitle",value= paste("All",long_name,"Brands' Transaction Text"))
     
       transactions_dt       <- DT::datatable(transactions,rownames= FALSE,
                                            options = list( 
                                              scrollY = '300px', pageLength =100,paging=FALSE,searching = FALSE,dom='tp'))
      
      if(length(brand_input())){
       cell                 <- brand_input()
       max_length_per_row=6
       length_per_row       <- ncol(matrix(c(brands$company_name,"All"), nrow = ceiling((nrow(brands)+1)/max_length_per_row), byrow = TRUE))
       
       idx                  <- (cell[,1]-1)*length_per_row+cell[,2]+1
       if(idx <=nrow(brands)){
        selected_brand       <- (t(brands))[idx]
        transactions_dt      <-  DT::datatable(data.frame(transactions) %>% filter(Brand==selected_brand),
                                              rownames= FALSE,options = list( 
                                scrollY = '300px', pageLength =100,paging=FALSE,searching = FALSE,dom='tp'))
       }
                                    
      }
      transactions_dt
    }
   }
  
  )

   brand_input<- reactive({
     input$x6_cells_selected})
    
  metric_plot          <- reactive({
                         input$metric
                    })
  #plot the transactions statistics
  output$plot2        <- renderPlot({
                       s                                      <- selection_row()
                       if (length(s)) {
                         ticker                               <- as.character(inputData1[s,"symbol"])
                         ticker_info                          <- filter(industry_data, grepl(paste(":",ticker,"$",sep = ""),symbolID))
                         if(nrow(ticker_info)>0){
                         long_name                            <- strsplit(ticker_info$long_name,"\\(")[[1]][1]
                         }else{ #company name not available as not matched in the revenue file
                           long_name                            <-  ticker
                         }
                          transactions_by_company              <- inputData1 %>% filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>%group_by(company_name)
                         # filter on the last 2 years of data
                          transactions_by_company              <- transactions_by_company %>% filter(month>(Sys.Date()-365*2))
                         
                          transactions_by_company[,"Month"]    <- format(as.Date(transactions_by_company$month),"%Y-%m")
                          colnames(transactions_by_company )   <- c("month","raw_String","spend","Brand","symbol","Month")
                         
                         if(metric_plot()=="count"){
                           monthly_metric_by_company          <- transactions_by_company %>% group_by(Brand,Month) %>% summarize(metric=n())
                           title_y                            <- "Number of Transactions by Brand"
                          
                         }else{
                           string1                              <-'transactions_by_company %>% group_by(Brand,Month) %>% summarize(metric='
                           string2                              <- paste(metric_plot(),'(spend))',sep="")
                           ourCommand                           <- paste(string1,string2)
                           monthly_metric_by_company            <- eval(parse(text=ourCommand))
                           monthly_metric_by_company[,3]        <- apply(monthly_metric_by_company[,3],1,as.numeric)/1E3 # transactions expressed in k USD
                           title_y                              <- "Total Spend in k USD"
                           nb_char                              <- apply(floor( monthly_metric_by_company[,3]),1,nchar)
                           nb_char[which(nb_char>1)]            <- 0
                           rounded_metric                       <- round(monthly_metric_by_company[,3],nb_char)
                           monthly_metric_by_company[,3]        <- rounded_metric
         
                        
                         }
                         if(metric_plot()=="mean"){
                             ggplot(data=monthly_metric_by_company, aes(x=Month, y=metric, group=Brand,colour=Brand))+
                             geom_point() + geom_line()+  
                             geom_line(size=1)+
                             geom_point(size=2) + 
                             scale_colour_brewer(direction = -1,palette="Paired")+
                             theme(plot.title = element_text(size = 18,lineheight=.8, face="bold",hjust = 0.5),
                                   axis.line = element_line(colour = "black"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor = element_blank(),
                                   panel.border = element_blank(),
                                   panel.background = element_blank(),
                                   axis.text.x = element_text(angle = 90, hjust = 1,size=12),
                                   axis.text.y = element_text(size=12),axis.title.x=element_blank(),
                                   axis.title.y = element_text(face = "bold", size = 12),
                                   legend.text=element_text(size=12),
                                   legend.position = "bottom",
                                   legend.title=element_blank())+
                             labs(y="Average Transaction Size in k USD") + scale_y_continuous(labels=unit_format(unit = "$k"))+
                             ggtitle(paste(paste(as.character(long_name),":",sep=""),"\nTotal Spend within ADG Mapped Dataset by Brand"))  
                             
                         }else{
                          g <- ggplot(data=monthly_metric_by_company, aes(x=Month, y=metric, fill=Brand)) +
                               geom_bar(stat="identity", position= position_stack(reverse = TRUE))+
                               scale_fill_brewer(direction = -1,palette="Paired")+
                               ggtitle(paste(paste(as.character(long_name),":",sep=""),"\nTotal Spend within ADG Mapped Dataset by Brand"))+  
                               labs(y=title_y)+
                               theme(plot.title = element_text(face="bold",lineheight=.8,hjust = 0.5,size=18),
                                axis.line = element_line(colour = "black"),
                                axis.text.x = element_text(angle = 90, hjust = 1,size=12),
                                axis.text.y = element_text(size=12),
                                axis.title.y = element_text(size=12,face="bold"),
                                legend.text=element_text(size=12),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.border = element_blank(),
                                legend.position = "bottom",
                                panel.background = element_blank(),
                                legend.title=element_blank())
                          
                          if(metric_plot()=="count"){
                            g <- g+stat_summary(fun.y = sum, aes(label =..y.., group = Month), geom = "text",vjust = -.2)
                          }
                          if(metric_plot()=="sum"){
                           g <- g+scale_y_continuous(labels=unit_format(unit = "$k"))+ stat_summary(fun.y = sum, aes(label =paste("$",round(..y..) ,"k",sep=""), group = Month),size = 3, geom = "text",vjust = -.2)
                          }
                          g
                        }}})
  menu_plot          <- reactive({
    input$Menu
  })
#plot the transactions and revenues over time
  output$plot1    <- renderPlot({
                     s <- selection_row()
                     if (length(s)) {
                     ticker                 <- as.character(inputData1[s,"symbol"])
                     ticker_info            <- filter(industry_data, grepl(paste(":",ticker,"$",sep = ""),symbolID))
                     long_name              <- strsplit(ticker_info$long_name,"\\(")[[1]][1]
                     
                     # retrieve all transactions for that ticker
                     transactions           <- filter(inputData1, grepl(paste("^",ticker,"$",sep = ""),symbol))

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
                    

                      transac_data           <- cbind(transactions_quaterly$Year,paste(  transactions_quaterly$Quarter,transactions_quaterly$Year), transactions_quaterly$Quarterly,"ADG Total Spend")
                     
                      #YoY data
                      transac_data_YOY       <- cbind(transactions_quaterly$Year,paste(transactions_quaterly$Year,transactions_quaterly$Quarter), transactions_quaterly$Quarterly,"ADG Total Spend")
                      quarter_1Y_ago         <- paste(as.numeric(transactions_quaterly$Year)-1,transactions_quaterly$Quarter)
                      idx_available          <- which(quarter_1Y_ago %in% paste(transactions_quaterly$Year, transactions_quaterly$Quarter))
                      data_available         <- transactions_quaterly[idx_available,]
                      data_available_1Y_ago  <- transac_data_YOY[transac_data_YOY[,2] %in% quarter_1Y_ago[idx_available],]
                      transac_YOY            <- cbind(paste(data_available$Quarter,data_available$Year),data_available$Quarterly/as.numeric(data_available_1Y_ago[,3])-1,"ADG Total Spend")
                      
                      rev_quarter_1Y_ago     <- paste(input_revenues$year-1,"Q",input_revenues$quarter,sep="")
                      rev_available_1Y_ago   <- input_revenues%>% filter(fiscalQ1 %in% rev_quarter_1Y_ago)
                      rev_quarter            <- paste(rev_available_1Y_ago$year+1,"Q",rev_available_1Y_ago$quarter,sep="")
                      rev_quarter_available  <- input_revenues%>% filter(fiscalQ1 %in% rev_quarter)
                      rev_YOY                <- cbind(paste(paste("Q",rev_quarter_available$quarter,sep=""),rev_quarter_available$year),as.numeric(rev_quarter_available$revenue)/as.numeric(rev_available_1Y_ago$revenue)-1,"Reported Revenues")
                      #revenues data
      rev_data                               <- cbind(Revenues_year,paste(Revenues_quarter,Revenues_year),input_revenues["revenue"],"Reported Revenues")
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
       display_unit         <- "b"
      }else{
        rev_data["revenue"] <- round(apply(rev_data["revenue"],1,as.numeric))
        rev_is_billion       <- FALSE
        display_unit         <- "m"
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
    
      df_sorted[which(df_sorted["Type"]=="Reported Revenues"),"Rev"]<-df_sorted[which(df_sorted["Type"]=="Reported Revenues"),"Reported.revenues"]
      df_sorted[which(df_sorted["Type"]=="ADG Total Spend"),"Spent"]<- rounded_transac
      
      #enforce same size
      singles <-df_sorted%>%group_by(Quarter) %>% filter(n()==1)
      double_spent <-singles %>% filter(Type=="Reported Revenues")
      if(nrow(double_spent)>0){
        double_spent[,"Spent"]<-"0"
      double_spent[,"Rev"]<-"NA"
      double_spent[,"Type"]<-"ADG Total Spend"
      double_spent[,"Reported.revenues"]<-0
      }
      
      double_rev <-singles %>% filter(Type=="ADG Total Spend")
      if(nrow(double_rev)>0){
        double_rev[,"Spent"]<-"NA"
        double_rev[,"Rev"]<-"0"
        double_rev[,"Type"]<-"Reported Revenues"
        double_rev[,"Reported.revenues"]<-0
      }
      df_sorted              <-  rbind(data.frame(df_sorted),data.frame(double_spent),data.frame(double_rev))%>%arrange(Year,Quarter)
      
    if( menu_plot()=="Actual"){
       p                     <- ggplot(data=df_sorted, aes(x= Quarter, y=Reported.revenues, fill=Type)) +
                                #geom_col(position = 'dodge')+
                                geom_bar( stat="identity",position = position_dodge(preserve = "single")) +
                                scale_fill_brewer(direction = -1,palette="Paired")+
                                ggtitle(paste(paste(as.character(long_name),":",sep=""), "\nReported Revenues vs ADG Mapped Total Spend"))+
                                geom_text(data=subset(df_sorted, Reported.revenues > 0 &Spent>0 & !is.na(Spent)),aes(label = ifelse(is.na(Spent),"",paste("$",Spent,"k",sep=""))),position=position_dodge(width=0.7), vjust=-0.3,hjust=1,size = 3)+
                                geom_text(data=subset(df_sorted, Reported.revenues > 0 & Rev > 0 & !is.na(Rev)),show.legend = FALSE,aes(label = ifelse(is.na(Rev),"",paste("$",Rev,display_unit,sep=""))),position=position_dodge(width=0.7), vjust=-0.3,hjust=0,size = 3) +                      
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
                                legend.title=element_blank(),
                                legend.position = "bottom")
                               
                                if(rev_is_billion){
                                    
                                 p <- p + labs(y="Reported in USD bn")
                                 p <- p +  scale_y_continuous(labels=unit_format(unit = "$bn"),limits = c(0, max_rev), sec.axis = sec_axis(~.*scaling_factor,name ="Mapped Spend in k USD",labels=unit_format(unit = "$k")))
                                   
                                }else{
                                  p <- p + labs(y="Reported in USD mio")
                                  p <- p +  scale_y_continuous(labels=unit_format(unit = "$mio"), sec.axis = sec_axis(~.*scaling_factor,name ="Mapped Spend in k USD",labels=unit_format(unit = "$k")))
                                    
                                 }
                                # secondary axis
                                
                                p+theme(axis.text.x = element_text(angle = 90, hjust = 1),axis.title.y.right = element_text(colour="#3182bd"))
                             
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
                                                   legend.position = "bottom",
                                                   legend.title=element_blank())+
                                                   ylab("YOY % Change") + 
                                                   ggtitle(paste(paste(as.character(long_name),":",sep=""), "\nYOY Reported Revenue vs ADG Mapped Total Spend")) 
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
    }else{
     
      
      inputData1$Date                 <- as.Date(inputData1$month)
      inputData1$Year                 <-  format(inputData1$Date,"%Y")
      
      ticker                          <- as.character(inputData1[s,"symbol"])
      

      ticker_industry                 <- filter(industry_data, grepl(paste(":",ticker,"$",sep = ""),symbolID))
      
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
      
      transactions_split                   <- unique(rbind(transactions_last_year_ticker,transactions_last_year_top5,transactions_last_year_others))
      
      transactions_split$category          <- "Top 5"
      transactions_split[which(transactions_split["symbol"]==ticker),"category"]<-ticker
      transactions_split[which(transactions_split["symbol"]=="Others"),"category"]<-"Others"
      boolColors<-c("#ffa500", "#6699ff","#6699ff","#6699ff","#6699ff","#6699ff","#6699ff")
      
      transactions_split <- transactions_split %>% mutate(pos = cumsum(spent)- spent/2)
      transactions_split$name        <- transactions_split$category
      transactions_split[which(transactions_split["category"]=="Top 5"),"name"] <-transactions_last_year_top5$symbol
      
 
      if(length(transactions_split)){
        t <- list(
          family = "sans serif",
          size = 14,
          color = 'black')
      if(scala()){
        pt1<- plot_ly(transactions_split, labels = ~name, values = ~log(spent), type = 'pie',
                #textposition = 'inside',
                textinfo = 'label+percent',
                insidetextfont = list(color = '#FFFFFF'),
                hoverinfo = 'text',
                text = ~paste( name),
                marker = list(colors = boolColors,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = FALSE) %>%
                layout(title =  paste("Annual Total Spend","in the",paste("<b>",ticker_industry$industry,"</b>"),"Sector"),font=t,
                       annotations=list(text="<i>Vendor Dataset, ADG Mapped</i>",yref = 'paper',y=0,yshift=-20,showarrow=FALSE),
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      }else{

        
        pt1<- plot_ly(transactions_split, labels = ~name, values = ~(spent), type = 'pie',
                      #textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      hoverinfo = 'text',
                      text = ~paste( name),
                      marker = list(colors = boolColors,
                                    line = list(color = '#FFFFFF', width = 1)),
                      showlegend = FALSE) %>%
          layout(title =  paste("Annual Total Spend","in the",paste("<b>",ticker_industry$industry,"</b>"),"Sector"),font=t,
                 annotations=list(text="<i>Vendor Dataset, ADG Mapped</i>",yref = 'paper',y=0,yshift=-20,showarrow=FALSE),
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        
      }
        
        pt1
        
      }
    }
    })
  
 
  output$plot4                       <- renderPlotly({

    s                                 <- selection_row()
    if(length(s)<1){return()
    }else{
      ticker                          <- as.character(inputData1[s,"symbol"])

      
      ticker_industry                 <- filter(industry_data, grepl(paste(":",ticker,"$",sep = ""),symbolID))
      
      tickersID_same_industry         <- filter(industry_data, grepl(paste("^",ticker_industry$industry,"$",sep = ""),industry))
      
      tickers_unlisted                 <- unlist( strsplit(as.character(tickersID_same_industry$symbolID),":"))
      
      tickers_same_industry            <- tickers_unlisted[which(((1:(2*nrow(tickersID_same_industry)))%%2)==0)]
      
      tickers_same_industry            <-tickers_same_industry[-which(tickers_same_industry==ticker)] 
      
      
      count_tickers                   <- length(tickers_same_industry)
      
      input_revenues                 <-  input_revenues[!is.na(input_revenues["symbol"]),]
      revenues_last_year             <-  input_revenues  %>% filter(grepl("2016",year))
      revenues_last_year_top5        <-  data.frame(revenues_last_year%>%
                                                      filter(symbol%in% tickers_same_industry)%>%
                                                    group_by (symbol)%>%
                                                    summarize(Rev=sum(as.numeric(revenue)))%>%
                                                    top_n(n = 5, wt =Rev),stringsAsFactors =FALSE)

      others                        <- max(1,count_tickers-5)
      
      revenues_last_year_others      <-  data.frame(revenues_last_year%>%
                                                    group_by (symbol)%>%
                                                    top_n(n = -others, wt =revenue),stringsAsFactors =FALSE)
     revenues_last_year_others_agg    <- revenues_last_year_others%>%group_by(symbol)%>%summarize(Rev = sum(as.numeric(revenue)))
      
      
      revenues_last_year_ticker      <-  data.frame(revenues_last_year%>%
                                                      filter(grepl(paste("^",ticker,"$",sep = ""),symbol))%>%
                                                      group_by (symbol)%>%
                                                      summarize(Rev=sum(as.numeric(revenue))),stringsAsFactors =FALSE)
                                                      
      revenues_last_year_others_agg$symbol <- "Others"


       revenues_split                 <- unique(rbind(revenues_last_year_ticker,revenues_last_year_top5,revenues_last_year_others_agg))
     
       revenues_split$category        <- "Top 5"
       revenues_split[which(revenues_split["symbol"]==ticker),"category"]  <- ticker
       revenues_split[which(revenues_split["symbol"]=="Others"),"category"]<- "Others"
       boolColors<-c("#ffa500", "#6699ff","#6699ff","#6699ff","#6699ff","#6699ff","#6699ff")
       revenues_split <- revenues_split %>% mutate(pos2 = cumsum(Rev)- Rev/2)
       revenues_split$name        <- revenues_split$category
       revenues_split[which(revenues_split["category"]=="Top 5"),"name"] <-revenues_last_year_top5$symbol
 
      if(length(revenues_split)){
        t <- list(
          family = "sans serif",
          size = 14,
          color = 'black')
        if(scala2()){
        pt2           <- plot_ly(revenues_split, labels = ~name, values = ~log(Rev), type = 'pie',
                      #textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      hoverinfo = 'text',
                      text = ~paste( name),
                      marker = list(colors = boolColors,
                                    line = list(color = '#FFFFFF', width = 1)),

                      showlegend = FALSE) %>%
                      layout(title = paste("Annual Reported Revenues in the",paste("<b>",ticker_industry$industry,"</b>"),"Sector"),font=t,
                             annotations=list(text="<i>Vendor Dataset, ADG Mapped</i>",yref = 'paper',y=0,yshift=-20,showarrow=FALSE),
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }else{
          pt2           <- plot_ly(revenues_split, labels = ~name, values = ~Rev, type = 'pie',
                                   #textposition = 'inside',
                                   textinfo = 'label+percent',
                                   insidetextfont = list(color = '#FFFFFF'),
                                   hoverinfo = 'text',
                                   text = ~paste( name),
                                   marker = list(colors = boolColors,
                                                 line = list(color = '#FFFFFF', width = 1)),
                                   
                                   showlegend = FALSE) %>%
            layout(title = paste("Annual Reported Revenues in the",paste("<b>",ticker_industry$industry,"</b>"),"Sector"),font=t,
                   annotations=list(text="<i>Vendor Dataset, ADG Mapped</i>",yref = 'paper',y=0,yshift=-20,showarrow=FALSE),
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
      
        pt2
      }
       
    }

   })
 
  
    
}
shinyApp(ui = ui, server = server)