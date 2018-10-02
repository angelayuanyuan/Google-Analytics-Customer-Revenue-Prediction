library(data.table)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(lubridate)
library(purrr)
library(ggplot2)


train_raw <- read_csv("../all/train.csv")
test_raw <- read_csv("../all/test.csv")


# parse JSON
flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- . %>% 
  bind_cols(flatten_json(.$device)) %>%
  bind_cols(flatten_json(.$geoNetwork)) %>% 
  bind_cols(flatten_json(.$trafficSource)) %>% 
  bind_cols(flatten_json(.$totals)) %>% 
  dplyr::select(-device, -geoNetwork, -trafficSource, -totals)

train <- parse(train_raw)
test <- parse(test_raw) 

# compare train and test set
setdiff(names(train), names(test))

train <- train%>%
  dplyr::select(-campaignCode)

# missing data
options(repr.plot.height=4)
NAcol <- which(colSums(is.na(train)) > 0)
NAcount <- sort(colSums(sapply(train[NAcol], is.na)), decreasing = TRUE)
NADF <- data.frame(variable=names(NAcount), missing=NAcount)
NADF$PctMissing <- round(((NADF$missing/nrow(train))*100),1)
NADF %>%
  ggplot(aes(x=reorder(variable, PctMissing), y=PctMissing)) +
  geom_bar(stat='identity', fill='blue') + coord_flip(y=c(0,110)) +
  labs(x="", y="Percent missing") +
  geom_text(aes(label=paste0(NADF$PctMissing, "%"), hjust=-0.1))

# transaction revenue
sum(!is.na(train$transactionRevenue)) # 11515 visits generate transaction revenue

# variables' distinct valuet
var_value.tr <- data.frame(apply(train,2, n_distinct),row.names = colnames(train))
colnames(var_value.tr) <- "count"
var_value.tr.drop <- var_value.tr%>%rownames_to_column()%>% filter(count==1)
var_value.te <- data.frame(apply(test,2, n_distinct),row.names = colnames(test))
colnames(var_value.te) <- "count"
var_value.te.drop <- var_value.te%>%rownames_to_column()%>% filter(count==1)

setdiff(names(var_value.tr.drop), names(var_value.te.drop)) # 19 variables in the dataset have only one value

# drop variables with all missing value
drop_col <- var_value.tr.drop$rowname
train <- train%>%
  dplyr::select(-one_of(drop_col))
test <- test%>%
  dplyr::select(-one_of(drop_col))

# write to RDS
saveRDS(train, file = "train.rds")
saveRDS(test, file = "test.rds")
