library(RMySQL)
library(reshape2)
library(plyr)
library(ggplot2)

setwd("C:/R/workspace/shared")
source("get_query.r")
load("db_creds.Rdata")

setwd("C:/R/workspace/sales_analysis/source")
opps <- read.csv("opportunities_and_products_for_psh.csv", header = T, stringsAsFactors = F)

#grab project and collapsed time data from mysql database
con <- dbConnect(dbDriver("MySQL"), user = username, password = password, dbname = "revenue_analysis")

sql <- paste("select subcloud.service_id, subcloud.opportunity_id, timelog.is_psm, subcloud.account_name, subcloud.cik, subcloud.registrant_type, 
             subcloud.solution, subcloud.SrPSM, subcloud.PSM, subcloud.CSM, subcloud.Sr_CSM, subcloud.service_name, subcloud.cs_ps, 
             subcloud.service_type, subcloud.form, subcloud.quarter_end, subcloud.filing_date, timelog.logged_date, subcloud.filing_deadline, subcloud.filing_deadline_recalc,
             subcloud.service_status, subcloud.customer_status, subcloud.year_end, subcloud.reporting_period, subcloud.service_period, subcloud.list_price, 
             subcloud.sales_price, timelog.Billable, subcloud.filing_week_num, logged_week_num, relative_week_num, sum(timelog.hours) 
             from subcloud left join timelog 
             on subcloud.service_id collate latin1_bin = timelog.service_id collate latin1_bin
             where subcloud.service_id like 'a0%' and service_status = 'Completed' and is_psm = 1 and not cs_ps = 'CS'
             group by subcloud.service_id, subcloud.account_name, timelog.is_psm, relative_week_num", sep = "")                

query <- dbGetQuery(con, sql)
dbDisconnect(con)

agg_prices <- aggregate(cbind(sales_price, list_price) ~  service_id + opportunity_id + 
                          service_name + service_type + service_type + reporting_period, data = query, FUN = sum)

opps_data <- opps[,names(opps) %in% c("Line.Item.18.Digit.Id", "Created.Date", "Close.Date", "Stage")]
opps_data <- opps_data[opps_data$Stage %in% c("Closed Won"),]
opps_data <- opps[,names(opps) %in% c("Line.Item.18.Digit.Id", "Created.Date", "Close.Date")]
names(opps_data)[names(opps_data) %in% "Line.Item.18.Digit.Id"] <- c("opportunity_id")

agg_prices <- merge(agg_prices, opps_data, by = "opportunity_id", all.x = T)
agg_prices$Close.Date <- as.Date(agg_prices$Close.Date, format = "%m/%d/%Y"); agg_prices$Created.Date <- as.Date(agg_prices$Created.Date, format = "%m/%d/%Y")
agg_prices$closed_period <- paste(year(agg_prices$Close.Date), ceiling(month(agg_prices$Close.Date)/3), sep = "")
agg_prices$created_period <- paste(year(agg_prices$Created.Date), ceiling(month(agg_prices$Created.Date)/3), sep = "")
agg_prices$month <- (year(agg_prices$Close.Date) - 2012)* 12 + month(agg_prices$Close.Date)
agg_prices$month_name <- format(agg_prices$Close.Date, format = "%b-%y")
agg_prices$month <- as.numeric(agg_prices$month)
agg_prices$closed_int = (year(agg_prices$Close.Date) - 2012)* 4 + floor(month(agg_prices$Close.Date)/3)
agg_prices$report_int = (as.numeric(substr(agg_prices$reporting_period,1,4))-2012)*4 + as.numeric(substr(agg_prices$reporting_period,6,6))

agg_prices_counts <- ddply(agg_prices, .var = c("service_type", "closed_period", "reporting_period","service_id"),
                        .fun = function(x){
                        data.frame(count = length(unique(x$service_id)),
                                  closed_int = (year(x$Close.Date) - 2012)* 4 + floor(month(x$Close.Date)/3),
                                  report_int = (as.numeric(substr(x$reporting_period,1,4))-2012)*4 + as.numeric(substr(x$reporting_period,6,6))
                                  )
                        })

excluded <- c("Rush Charges", "Auditor Review", "Migration")
#loop to plot all periods 
for (loop in unique(agg_prices$reporting_period)){
  if(dim(agg_prices[agg_prices$reporting_period %in% loop,])[1] > 100){
    closed_loop_plot <- ggplot(agg_prices[!(agg_prices$service_type %in% excluded) &
                                                !is.na(agg_prices$closed_int) &
                                                  agg_prices$reporting_period %in% loop,]) +
      geom_bar(aes(x = closed_int, fill = service_type)) +
      geom_segment(aes(x = report_int + 1, xend = report_int+ 1, y = 0, yend = 300), color = "red") +
      scale_x_continuous(labels=unique(agg_prices$reporting_period), 
                         breaks =unique(agg_prices$report_int)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste(loop," services closed by period", sep = ""))
    
    setwd("C:/R/workspace/sales_analysis/output")
    ggsave(paste("opportunity_close_by_reporting_period",loop,".png", sep = "")
           , closed_loop_plot, width = 14, height = 8.5)
  }
}

#plot services sold by individual contracts vs as a package
valid_svc <- c("Roll Forward", "Full Service Roll Forward", "Standard Import", "Detail Tagging",
               "Full Review", "Maintenance", "Full Service Standard Import")
valid_agg_prices <- agg_prices[agg_prices$service_type %in% valid_svc,]
counter <- ddply(valid_agg_prices, .var = c("opportunity_id"), .fun = function(x){
  data.frame(line_item_count = length(unique(x$service_id)))
})
valid_agg_prices <- merge(valid_agg_prices, counter, by = c("opportunity_id"), all.x = T)
valid_agg_prices$opp_type <- "package"
valid_agg_prices[valid_agg_prices$line_item_count %in% "1",]$opp_type <- "single"
valid_agg_prices <- valid_agg_prices[valid_agg_prices$reporting_period %in% c("2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3") ,]

opp_type_plot <- ggplot(valid_agg_prices) + 
  geom_bar(aes(x = report_int, fill = factor(opp_type)), position = "dodge", size = 3) +
  scale_x_continuous(labels=unique(valid_agg_prices$reporting_period), 
                   breaks =unique(valid_agg_prices$report_int)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~service_type) +
  ggtitle("services sold as part of a package vs single")

setwd("C:/R/workspace/sales_analysis/output")
ggsave("services_from_opportunity_type.png", opp_type_plot, width = 11, height = 8.5)
  