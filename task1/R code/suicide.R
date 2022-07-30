#library

#install.packages("bruceR")
#install.packages("bruceR", dep=TRUE)
library(bruceR)

#install.packages("tidyverse")
library(tidyverse)

#install.packages("readxl")
library("readxl")

sui_col <- read_excel("C:\\Users\\Administrator\\Desktop\\college suicide2.xlsx") %>%
  dplyr::mutate(item = paste("num_",label,sep = "")) %>%
  tidyr::pivot_wider(id_cols = pap_ID, names_from = item, values_from = N_part) %>%
  dplyr::mutate(N_male = num_male, raitio_male = num_male/num_total) %>%
  dplyr::select(pap_ID, N_male, raitio_male)

write.csv(sui_col,file = "sui_col.csv")

sui_col0 <- read.csv("C:\\Users\\Administrator\\Desktop\\sui_col.csv")

sui_col1 <- read_excel("C:\\Users\\Administrator\\Desktop\\college suicide2.xlsx") %>%
  dplyr::select(pap_ID, pap_name, author, pub_year, prov, edu,
                edu_new, pub_orN,	meas, scale, line, tool_num_line, label,
                N_part, ratio_overline, dura) %>%
  dplyr::full_join(., sui_col0, by = "pap_ID") %>%
  dplyr::select(pap_name, author, pub_year, N_male, raitio_male, prov, edu,
                edu_new, pub_orN,	meas, scale, line, tool_num_line, label,
                N_part, ratio_overline, dura)
  

write_excel_csv(sui_col1,file = "sui_col1.csv")
