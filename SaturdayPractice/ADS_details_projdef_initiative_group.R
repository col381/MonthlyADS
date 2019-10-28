library(tidyverse)
revopex <- read_csv("Monday_28Oct.csv")
actual <- revopex %>%
  mutate(Initiative = recode(Cluster_Contact, 
                             `Brent Henderson` = "Environmental & Biological",
                             `Chen Cai`  = "Transport and Logistics",
                             `Gavin Walker` = "Data Science Application",
                             `Lachlan Hetherton` = "Industrial Transformation",
                             `Mahesh Prakash` = "Natural Hazards and Infrastructure",
                             `Zili Zhu` = "Risklab",
                             `Paul Cleary` = "Industrial Device Optimisation",
                             `Simon Harrison` = "Digital Human"))
Forecast <- actual%>%
  mutate(Forecast = Plan_FY - Actual_YTD)

Initiative <- Forecast%>%
  mutate(Operating = recode(Cost_Element,`Travel` = "Other Operating",`TOTAL REVENUE`="External Revenue"))%>%
  group_by(Initiative,Operating) %>%
  summarise(Actual_YTD_sum = sum(Actual_YTD,na.rm = TRUE),Plan_FY_sum = sum(Plan_FY,na.rm = TRUE),Forecast_sum = sum(Forecast,na.rm = TRUE))%>%
  filter(Operating %in% c("Other Operating", "External Revenue"))
write_csv(Initiative,"initiative.csv")

Report <- Forecast%>%
  mutate(Operating = recode(Cost_Element,`Travel` = "Other Operating",`TOTAL REVENUE`="External Revenue"))%>%
  group_by(Initiative,Operating,Project_Definition,Project_Name) %>%
  summarise(Actual_YTD_sum = sum(Actual_YTD,na.rm = TRUE),Plan_FY_sum = sum(Plan_FY,na.rm = TRUE),Forecast_sum = sum(Forecast,na.rm = TRUE))%>%
  filter(Operating %in% c("Other Operating", "External Revenue"))
write_csv(Report,"opexrev.csv")





