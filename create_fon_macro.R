source("../Dropbox/Tweets Data/R/core.R")

# Load the Historical Long-Run Data from Statistics Canada
histdata<-getTABLE("36100229")

##################################
# Nominal Gross Domestic Product #
##################################

# Create Provincial Nominal GDP levels from 1950-1980 relative to 1981
gdp50<-histdata %>%
  filter(Statistical.measures=="Linked data",
         Ref_Date>=1950,Ref_Date<=1981,
         Long.run.variables %in% c("Population",
                                   "Gross domestic product per capita, income based")) %>%
  mutate(Long.run.variables=ifelse(Long.run.variables=="Gross domestic product per capita, income based",
                                   "GDPpc","Pop")) %>%
  select(GEO,Ref_Date,Long.run.variables,Value) %>%
  spread(Long.run.variables,Value) %>%
  mutate(GDP=GDPpc*Pop) %>% 
  group_by(GEO) %>%
  mutate(relGDP=GDP/GDP[n()]) %>%
  select(Ref_Date,GEO,relGDP)

# Bring in GDP for 1981-Present
gdp81data<-getTABLE("36100221")
gdp81<-gdp81data %>%
  filter(Estimates=="Gross domestic product at market prices") %>%
  select(Ref_Date,GEO,Value)

# Merge the two GDP series together
prov_GDP<-CJ(GEO=unique(gdp81$GEO),
               Ref_Date=seq(1950,max(gdp81$Ref_Date))) %>%
  left_join(gdp50,by=c("Ref_Date","GEO")) %>%
  left_join(gdp81,by=c("Ref_Date","GEO")) %>%
  group_by(GEO) %>%
  mutate(GDP=ifelse(Ref_Date>=1981,Value,
                    relGDP*weighted.mean(Value,Ref_Date==1981))) %>%
  filter(!GEO %in% c("Outside Canada","Canada")) %>%
  left_join(provnames,by="GEO") %>% ungroup() %>%
  select(Year=Ref_Date,GEO,GDP) %>%
  mutate(GEO=factor(GEO,levels=c(tenprov,"Yukon","Northwest Territories","Nunavut",
                                 "Northwest Territories including Nunavut"))) %>%
  spread(GEO,GDP)

##############################
# Population 1867 to Present #
##############################

# Get Historical Population
pop_1867<-read_excel("../Dropbox/Research/CTJ Fiscal Transfers/Data/HistoricalPerCapita.xlsx",
                     sheet="OldPop",range="A1:J77") %>%
  filter(Ref_Date<=1920) %>%
  gather(short,pop,-Ref_Date) %>%
  mutate(pop=1000*pop)

# Get recent population data
popdata<-getTABLE("17100005") # Population by province, 1971-Present
popold<-read_excel("../Dropbox/Research/CTJ Fiscal Transfers/Data/HistoricalPopulation.xlsx") %>%
  gather(short,pop,-Ref_Date) %>%
  mutate(pop=1000*pop) %>%
  filter(Ref_Date<=1970)
prov_Pop<-popdata %>%
  filter(Sex=="Both sexes",Age.group=="All ages") %>%
  filter(!(is.na(short)),short!="CAN") %>%
  select(Ref_Date,short,pop=Value) %>%
  rbind(popold) %>%
  rbind(pop_1867) %>%
  left_join(provnames,by='short') %>%
  mutate(GEO=factor(GEO,levels=c(tenprov,"Yukon","Northwest Territories","Nunavut",
                                 "Northwest Territories including Nunavut"))) %>%
  select(Year=Ref_Date,GEO,pop) %>%
  arrange(GEO,Year) %>%
  spread(GEO,pop)

##################################################################
# Employment 1946-Present for Canada; 1976-Present for Provinces #
##################################################################

# Historical Statistics of Canada
emp_hist<-fread("../Dropbox/Outside Work and Policy/Finances of the Nation/Data/MacroData/HistoricalEmployment.csv")

#Modern Data
LFS<-getTABLE("14100327")

#Territorial Modern Data
LFSterr<-getTABLE("14100292")
LFSterr_clean<-LFSterr %>%
  filter(Sex=="Both sexes",
         Data.type=="Seasonally adjusted",
         Statistics=="Estimate",
         Age.group=="15 years and over",
         Labour.force.characteristics=="Employment") %>%
  mutate(Year=year(Ref_Date)) %>%
  group_by(GEO,Year) %>%
  summarise(Employment=mean(Value)) %>%
  mutate(Employment=1000*Employment) %>%
  drop_na()

# Compile the employment data
emp<-LFS %>%
  filter(Sex=="Both sexes",
         Age.group=="15 years and over",
         Labour.force.characteristics=="Employment") %>%
  select(Year=Ref_Date,GEO,Employment=Value) %>%
  rbind(
    emp_hist %>%
      mutate(GEO="Canada")
  ) %>%
  mutate(Employment=1000*Employment) %>%
  bind_rows(LFSterr_clean)

####################################
# Provincial CPI from 1926-Present #
####################################
cpi26<-histdata %>%
  filter(Statistical.measures=="Linked data",
         Long.run.variables=="Linked consumption price index") %>%
  select(GEO,Year=Ref_Date,CPIold=Value)

# Modern CPI for provinces from 1979 onwards
cpidata<-getTABLE("18100005") %>%
  filter(Products.and.product.groups=="All-items")
prov_CPI<-expand.grid(Year=seq(min(cpi26$Year),max(cpidata$Ref_Date)),
                     GEO=tenprov) %>%
  left_join(
    cpidata %>%
      filter(Ref_Date>=1979) %>%
      select(Year=Ref_Date,GEO,CPI=Value),by=c("GEO","Year")
  ) %>%
  left_join(cpi26,by=c("GEO","Year")) %>%
  group_by(GEO) %>%
  mutate(relcpi=CPIold/weighted.mean(CPIold,Year==1979),
         CPI=ifelse(Year>=1979,CPI,
                    relcpi*weighted.mean(CPI,Year==1979)),
         CPI=CPI/CPI[n()],
         GEO=factor(GEO,levels=tenprov)) %>%
  select(GEO,Year,CPI) %>%
  arrange(GEO,Year) %>%
  spread(GEO,CPI)

###############################################
# National Population, CPI and GDP Statistics #
###############################################

# Statistics Canada Quarterly GDP Since 1961
qtrdata<-getTABLE("36100104")
GDP61<-qtrdata %>%
  filter(GEO=="Canada" & 
           Estimates=="Gross domestic product at market prices" & 
           Prices=="Current prices" &
           Seasonal.adjustment=="Unadjusted") %>%
  mutate(Year=year(Ref_Date)) %>%
  group_by(Year) %>%
  summarise(GDP=sum(Value))

# National Population 1946-Present
popqtr<-getTABLE("17100009")
pop_canada<-popqtr %>%
  filter(GEO=="Canada") %>%
  mutate(Year=year(Ref_Date)) %>%
  group_by(Year) %>%
  summarise(Population=mean(Value))

# National Consumer Price Index 1914-Present
cpi_canada<-cpidata %>%
  filter(GEO=="Canada") %>%
  select(Year=Ref_Date,CPI=Value) %>%
  mutate(CPI=CPI/CPI[n()])
cpi_year<-max(cpi_canada$Year)

# Merge in population, GDP and inflation measures
histstats<-read_excel("../Dropbox/Outside Work and Policy/Finances of the Nation/Data/MacroData/Historical Statistics.xlsx",
                      sheet="Macro Data")

# Merge the two GDP series together
canada_macro<-data.frame(Year=seq(1867,max(pop_canada$Year))) %>%
  left_join(histstats %>% rename(GDPold=GDP,
                                 CPIold=CPI,
                                 Popold=Population),by=c("Year")) %>%
  left_join(GDP61,by=c("Year")) %>%
  left_join(pop_canada,by=c("Year")) %>%
  left_join(cpi_canada,by=c("Year")) %>%
  mutate(relGDP=GDPold/weighted.mean(GDPold,Year==min(GDP61$Year)),
         GDP=ifelse(Year>=min(GDP61$Year),GDP,
                    relGDP*weighted.mean(GDP,Year==min(GDP61$Year))),
         relpop=Popold/weighted.mean(Popold,Year==min(pop_canada$Year)),
         Population=ifelse(Year>=min(pop_canada$Year),Population,
                           relpop*weighted.mean(Population,Year==min(pop_canada$Year))),
         relcpi=CPIold/weighted.mean(CPIold,Year==min(cpi_canada$Year)),
         CPI=ifelse(Year>=min(cpi_canada$Year),CPI,
                    relcpi*weighted.mean(CPI,Year==min(cpi_canada$Year)))) %>%
  select(Year,GDP,Population,CPI) %>%
  drop_na()

######################################
# Create the Downloadable Data Files #
######################################

prov_macro<-prov_Pop %>% gather(Region,Population,-Year) %>% drop_na() %>%
  left_join(prov_CPI %>% gather(Region,CPI,-Year) %>% drop_na(),by=c("Region","Year")) %>%
  left_join(prov_GDP %>% gather(Region,GDP,-Year) %>% drop_na(),by=c("Region","Year"))
fon_macro_data <- canada_macro %>%
  mutate(Region="Canada") %>%
  rbind(prov_macro) %>%
  left_join(
    canada_macro %>% select(Year,NationalCPI=CPI),by="Year"
  ) %>%
  left_join(
    emp %>% rename(Region=GEO),by=c("Year","Region")
  ) %>%
  mutate(rGDPpc=(1000000*GDP/Population)/NationalCPI,
         rGDPwk=(1000000*GDP/Employment)/NationalCPI,
         GDPpc=(1000000*GDP/Population),
         rGDP=GDP/NationalCPI,
         NationalCPI=100*NationalCPI,
         CPI=100*CPI)
write.csv(fon_macro_data,'FON Macro Data.csv',row.names = F)

# For Tableau
fon_macro_data_long<-fon_macro_data %>%
  gather(Variable,Value,-Year,-Region) %>%
  mutate(Variable=case_when(
    Variable=="GDP" ~ "Nominal GDP (Millions)",
    Variable=="rGDP" ~ paste0("Real GDP (Millions, $ ",cpi_year,")"),
    Variable=="GDPpc" ~ "Nominal GDP Per Capita ($)",
    Variable=="rGDPpc" ~ paste0("Real GDP Per Capita ($ ",cpi_year,")"),
    Variable=="rGDPwk" ~ paste0("Real GDP Per Worker ($ ",cpi_year,")"),
    Variable=="CPI" ~ paste0("Provincial CPI (",cpi_year,"=100)"),
    Variable=="NationalCPI" ~ paste0("Canada-Wide CPI (",cpi_year,"=100)"),
    TRUE ~ as.character(Variable)
  )) %>%
  mutate(`Log Scale`=log(Value))
write.csv(fon_macro_data_long,'MacroData.csv',row.names = F)

# Fun ExcelFormatting.R after all of the above using table
table<-fon_macro_data

# Create File for Use in the MacroCleaner that Ayaka wrote

