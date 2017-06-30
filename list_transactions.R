library(quantmod)
list_transactions         <- function(){
  l = list()
  inputData1        <- read.csv("./poc_sample_data.csv", header = TRUE)
  
  transactions_count      <- inputData1 %>%
                              group_by(raw_string) %>%
                              summarize(Count= n())%>%
                              arrange(desc(Count))
  transactions            <- paste(transactions_count$raw_string,"(",transactions_count$Count,")")
  return (transactions)
}
