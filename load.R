# Author: Sifan Liu
# Date: Mon Feb 11 14:31:41 2019
# --------------
pkgs <- c('tidyverse')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)
if(any(!check)){
    pkgs.missing <- pkgs[!check]
    install.packages(pkgs.missing)
    check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
} 
# Functions
source("../Birmingham/County Cluster/func.R")
padz <- function(x, n=max(nchar(x)))gsub(" ", "0", formatC(x, width=n)) 

# DEFINE GEO -------------------------------------------------
city_FIPS <- "07000"
ct_FIPS <- "073"
msa_FIPS <- "13820"
st_FIPS <- "01"
county_FIPS <- paste0(st_FIPS, ct_FIPS)
msa100_FIPS <- as.character((read.csv("V:/Sifan/R/xwalk/top100metros.csv") %>% filter(top100==1))[["GEO.id2"]])
# peers from clustering result ================================
# Peers <- read.csv("source/counties_cluster_all.csv") %>%
#   mutate(cbsa = as.character(cbsa)) %>%
#   mutate(stcofips = padz(as.character(stcofips), 5)) %>%
#   group_by(kmeans) %>%
#   filter(msa_FIPS %in% cbsa)

# modified peers to include Nashiville, etc. ================
peerlist <- c("34980", "47260", "33340", "32820","40060", 
              "31140", "35380", "15380", "40380","26620",
              "28140", "17460", "26900", "12940", msa_FIPS)
Peers <- readxl::read_xlsx('../Birmingham/County Cluster/result/13820_Market Assessment.xlsx', sheet = "Peers")%>%
  filter(cbsa %in% peerlist)

msa_ct_FIPS <- read.csv('../R/xwalk/county2msa.csv') %>%
  mutate(fips = paste0(padz(fipsstatecode,2), padz(fipscountycode,3)),
         COUNTY = trimws(toupper(gsub("County","",countycountyequivalent))))%>%
  filter(cbsacode %in% Peers$cbsa)%>%
  mutate(cbsa = as.character(cbsacode))%>%
  select(cbsa, FIPS = fips, metro = cbsatitle, county = COUNTY)


# load opportunity industry data
folder <- "V:/Performance/Project files/Opportunity Industries/Data/Output/Final/Metros/"
files <- " BMPP Opportunity Industries - 2017 Job shares.xlsx"
places <- gsub(",","",purrr::map_chr(seq(1,nrow(Peers)),function(x)paste(Peers$cbsa[[x]],Peers$metro[[x]])))

load_data <- function(x){
  tryCatch(
    readxl::read_xlsx(paste0(folder, x,"/",paste0(x,files)),
                      sheet = "DEMO1_INROW")%>%
      mutate(cbsa = substr(x,1,5)),
    error = function(e)NULL)
}

opp <- purrr::map(places,load_data)

# Indy data
# opp[[2]] <- NULL
opp_msa <- bind_rows(opp)%>%
  filter(!is.na(Gender)) %>%
  mutate(`Share with good job` = `Share with good sub-BA job`+`Share with high-skill good job`,
         `Share with promising job` = `Share with promising sub-BA job` + `Share with high-skill promising job`)%>%
  select(-contains("sub"), - contains("high"))%>%
  gather(type, share, `Share with other job`,`Share with good job`,`Share with promising job`)

# write.csv(opp_msa,"result/opp_msa.csv")
opp_msa$type <- factor(opp_msa$type, levels = c("Share with other job","Share with promising job","Share with good job"))

ggplot(opp_msa %>% 
         filter(cbsa == msa_FIPS) %>% 
         filter(Age == "Total")%>%
         filter(Gender != "Total") %>%
         filter(Race != "Total")%>%
         filter(Education %in% c("All sub-baccalaureate levels","Baccalaureate degree or higher")), 
       aes(x = Race, y = share, fill = type, label = scales::percent(share)))+
  geom_bar(stat = "identity")+
  geom_text(position = position_stack(vjust = 0.5))+
  facet_wrap(Gender~Education,ncol = 2)+
  scale_y_continuous(labels = scales::percent, name = NULL)+
  scale_x_discrete(name = NULL)+
  scale_fill_manual(values = c("#797171","#ffd966", "#0070c0"), name = NULL)+
  coord_flip()+
  guides(fill = guide_legend(reverse = T))+
  pthemes%+%theme(legend.position = "bottom")
  


# health outcomes
library(RSocrata)
token <- "4T1vhrRM49HffDDXPFQJfiVhM"

cities <- paste0(gsub("\\,.+","",Peers$metro), collapse = "','")

cityhealth <- read.socrata(paste0("https://chronicdata.cdc.gov/resource/csmm-fdhi.csv?category=Health Outcomes&$where=cityname in",
                                  "('",cities,"')"),token)

summary(factor(cityhealth$geographiclevel))

health_chart <- cityhealth %>% 
  filter(measureid %in% c("MHLTH", "PHLTH")) %>%
  filter(geographiclevel == "City") %>%
  filter(datavaluetypeid == "AgeAdjPrv") %>%
  select(cityname, measure, data_value, year) %>%
  group_by(cityname, measure)%>%
  summarise(data_value = mean(data_value))


ggplot(health_chart, 
       aes(x = reorder(cityname, data_value), y = data_value, fill = measure, label = data_value))+
  geom_bar(stat = "identity", position = "dodge")+
  # geom_text(position = position_dodge(width = 1))+
  scale_y_continuous(name = NULL)+
  scale_x_discrete(name = NULL)+
  scale_fill_manual(values = c("#ffd966", "#0070c0"), 
                    labels = c("Mental Health", "Physical Health"), name = NULL)+
  coord_flip()+
  guides(fill = guide_legend(reverse = T))+
  pthemes

# Disconnected youth

health_tract <- cityhealth%>%
  filter(cityname == "Birmingham")%>%
  filter(geographiclevel == "Tract") %>%
  filter(measureid %in% c("MHLTH", "PHLTH")) %>%
  



Peers$county

