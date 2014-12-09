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
