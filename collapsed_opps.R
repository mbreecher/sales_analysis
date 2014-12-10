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

#Q42013 plot
excluded <- c("Rush Charges", "Auditor Review", "Migration")
agg_prices$plot_label <- agg_prices$month_name;
agg_prices$plot_x <- agg_prices$month; agg_prices[agg_prices$plot_x <= 21 & !is.na(agg_prices$plot_x),]$plot_x <- 21
agg_prices[agg_prices$plot_x <= 21 & !is.na(agg_prices$plot_label),]$plot_label <- "Pre Q4"
Q4_closed_plot <- ggplot(agg_prices[agg_prices$reporting_period %in% "2013Q4" & !(agg_prices$service_type %in% excluded),]) +
  geom_bar(aes(x = plot_x, fill = service_type)) +
  scale_x_continuous(labels=unique(agg_prices$plot_label), 
                     breaks =unique(agg_prices$plot_x)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("2013 Q4 services opportunity close period")

setwd("C:/R/workspace/sales_analysis/output")
ggsave("2013Q4_service_close_dates_by_month.png", Q4_closed_plot, width = 11, height = 8.5)

#opportunity and service gap plot
excluded <- c("Rush Charges", "Auditor Review", "Migration")
agg_prices_counts <- ddply(agg_prices, .var = c("service_type", "closed_period", "reporting_period"),
                           .fun = function(x){
                             data.frame(count = length(unique(x$service_id)),
                                        closed_int = (year(x$Close.Date) - 2012)* 4 + floor(month(x$Close.Date)/3),
                                        report_int = (as.numeric(substr(x$reporting_period,1,4))-2012)*4 + as.numeric(substr(x$reporting_period,6,6))
                                        )
                           })

agg_prices$plot_label <- agg_prices$month_name; agg_prices$plot_x <- agg_prices$month
sold_gap_plot <- ggplot(agg_prices_counts[!(agg_prices_counts$service_type %in% excluded) &
                                            !is.na(agg_prices_counts$closed_int),]) +
  geom_linerange(aes(x = count, ymin = closed_int, ymax = report_int+1, color = service_type),
                 position = position_dodge(1)) +
  coord_flip() + 
  geom_point(aes(x = count, y = closed_int, color = "closed")) +
  geom_point(aes(x = count, y = report_int +1, color = "reported")) +
#   geom_segment(aes(x = closed_int, y = count, xend = report_int, yend = count, fill = service_type), size = 2, alpha = .5) +
  scale_y_continuous(labels=unique(agg_prices_counts$reporting_period), 
                     breaks =unique(agg_prices_counts$report_int)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("too messy")

#loop to plot all periods
for (loop in unique(agg_price_counts$reporting_period)){
  if(dim(agg_prices_counts[agg_prices_counts$reporting_period %in% loop,]) > 100){
    closed_loop_plot <- ggplot(agg_prices_counts[!(agg_prices_counts$service_type %in% excluded) &
                                                !is.na(agg_prices_counts$closed_int) &
                                                  agg_prices_counts$reporting_period %in% loop,]) +
      geom_bar(aes(x = closed_int, fill = service_type)) +
      geom_segment(aes(x = report_int +1, xend = report_int+1, y = 0, yend = 300), color = "red") +
      scale_x_continuous(labels=unique(agg_prices_counts$reporting_period), 
                         breaks =unique(agg_prices_counts$report_int)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste(loop," services closed by period", sep = ""))
    
    setwd("C:/R/workspace/sales_analysis/output")
    ggsave(paste("opportunity_close_by_reporting_period",loop,".png", sep = "")
           , closed_loop_plot, width = 14, height = 8.5)
  }
}
#plot services sold by individual contracts vs as a package
