source("../Dropbox/Tweets Data/R/core.R")

# Load the Historical Long-Run Data from Statistics Canada - Brown and MacDonald
histdata<-getTABLE("36100229")

##################################
# Nominal Gross Domestic Product #
##################################

# Impute Provincial Nominal GDP levels from 1926-1950
imputed_gdp<-histdata %>%
  filter(Long.run.variables %in% c("Household income per capita",
                                   "Population",
                                   "Gross domestic product per capita, income based"),
         Ref_Date<=1975,GEO %in% tenprov,
         Statistical.measures=="Linked data") %>%
  select(GEO,Ref_Date,var=Long.run.variables,Value) %>%
  mutate(var=case_when(
    var=="Household income per capita" ~ "income",
    var=="Population" ~ "Pop",
    var=="Gross domestic product per capita, income based" ~ "GDPpc",
    TRUE ~ var
  )) %>%
  spread(var,Value) %>%
  group_by(GEO) %>%
  mutate(estimate=income*weighted.mean(GDPpc,Ref_Date==1950)/weighted.mean(income,Ref_Date==1950),
         GDP=estimate*Pop)
# ggplot(imputed_gdp %>% filter(GEO=="British Columbia") %>%
#          gather(var,value,-Ref_Date,-GEO),aes(Ref_Date,log(value),color=var,group=var))+
#   geom_line()
# require(lfe)
# summary(felm(log(GDPpc)~log(income)|factor(GEO),data=imputed_gdp))

# Create Provincial Nominal GDP levels from 1926-1980 relative to 1981
gdp_old<-histdata %>%
  filter(Statistical.measures=="Linked data",
         Ref_Date>=1950,Ref_Date<=1981,
         Long.run.variables %in% c("Population",
                                   "Gross domestic product per capita, income based")) %>%
  mutate(Long.run.variables=ifelse(Long.run.variables=="Gross domestic product per capita, income based",
                                   "GDPpc","Pop")) %>%
  select(GEO,Ref_Date,Long.run.variables,Value) %>%
  spread(Long.run.variables,Value) %>%
  mutate(GDP=GDPpc*Pop) %>% 
  bind_rows(
    imputed_gdp %>% filter(Ref_Date<1950) %>% select(Ref_Date,GEO,GDP,Pop,GDPpc)
  ) %>%
  arrange(GEO,Ref_Date) %>%
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
               Ref_Date=seq(1926,max(gdp81$Ref_Date))) %>%
  left_join(gdp_old,by=c("Ref_Date","GEO")) %>%
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

# to merge with historical statistics D470-476 to infer employment
unemp_old<-read_excel("../Dropbox/Outside Work and Policy/Finances of the Nation/Data/MacroData/Historical Unemployment.xlsx",
                      sheet='ILD Data',range='P1:Y32') %>%
  gather(GEO,unemp_old,-Year)
for_sheet<-prov_Pop %>%
  gather(GEO,pop,-Year) %>%
  filter(Year>=1921,Year<=1976) %>%
  mutate(pop=pop/1000) %>%
  left_join(
    histdata %>%
      filter(Long.run.variables=="Unemployment rate",
             Ref_Date<=1976,
             Statistical.measures=="Linked data") %>%
      select(GEO,Year=Ref_Date,unemp=Value),
    by=c("GEO","Year")
  ) %>%
  left_join(unemp_old,by=c("GEO","Year")) %>%
  mutate(unemp=ifelse(Year<1950,unemp_old,unemp)) %>%
  select(-unemp_old) %>%
  mutate(region=case_when(
    GEO %in% c("Alberta","Saskatchewan","Manitoba") ~ "Prairie",
    GEO=="Ontario" ~ "Ontario",
    GEO=="Quebec" ~ "Quebec",
    GEO=="British Columbia" ~ "British Columbia",
    GEO=="Newfoundland and Labrador" ~ "Newfoundland",
    GEO %in% c("Prince Edward Island","Nova Scotia","New Brunswick") ~ "Maritimes"
  )) %>%
  drop_na() %>%
  mutate(adjustment=pop*(1-unemp/100)) %>%
  group_by(region,Year) %>%
  summarise(adjustment=sum(adjustment)) %>%
  spread(region,adjustment) %>%
  arrange(-Year) # copy this to the D470_476 Worksheet

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

# Historical Employee Data to Project Employment Back to 1961 for Provinces
# old<-getTABLE("14100264")
# test<-old %>%
#   filter(Seasonal.adjustment=="Unadjusted",
#          GEO!="Yukon and Northwest Territories",
#          Employees.by.type.of.industry=="Total non-agricultural industries") %>%
#   mutate(Year=year(Ref_Date)) %>%
#   group_by(GEO,Year) %>%
#   summarise(empold=mean(Value)) %>%
#   left_join(emp,by=c("GEO","Year")) %>%
#   mutate(ratio=empold/Employment) %>%
#   group_by(Year) %>%
#   mutate(ratioCDN=weighted.mean(ratio,GEO=="Canada"),
#          empoldCDN=weighted.mean(empold,GEO=="Canada"),
#          empCDN=weighted.mean(Employment,GEO=="Canada")) %>%
#   group_by(GEO) %>%
#   mutate(estimate=ratioCDN*weighted.mean(ratio,Year==1976)/weighted.mean(ratioCDN,Year==1976),
#          share=empold/empoldCDN,
#          Est1=empold/estimate,
#          Est2=empCDN*share) %>%
#   group_by(Year) %>%
#   mutate(test=ifelse(GEO=="Canada",0,Est1),
#          test=sum(test),
#          MainEst=Est1*empCDN/test)

# Create historical employment estimates for the provinces, 1946-1975
hist_prate<-read_excel("../Dropbox/Outside Work and Policy/Finances of the Nation/Data/MacroData/D470_476-eng.xlsx",
                       range="V5:AB61") %>%
  gather(region,prate,-Year)
emp_historical<-prov_Pop %>%
  gather(GEO,pop,-Year) %>%
  filter(Year>=1921,Year<=1976) %>%
  mutate(pop=pop/1000) %>%
  left_join(
    histdata %>%
      filter(Long.run.variables=="Unemployment rate",
             Ref_Date<=1976,
             Statistical.measures=="Linked data") %>%
      select(GEO,Year=Ref_Date,unemp=Value),
    by=c("GEO","Year")
  ) %>% 
  left_join(unemp_old,by=c("GEO","Year")) %>%
  mutate(unemp=ifelse(Year<1950,unemp_old,unemp)) %>%
  select(-unemp_old) %>%
  drop_na() %>%
  mutate(region=case_when(
    GEO %in% c("Alberta","Saskatchewan","Manitoba") ~ "Prairie",
    GEO=="Ontario" ~ "Ontario",
    GEO=="Quebec" ~ "Quebec",
    GEO=="British Columbia" ~ "British Columbia",
    GEO=="Newfoundland and Labrador" ~ "Newfoundland",
    GEO %in% c("Prince Edward Island","Nova Scotia","New Brunswick") ~ "Maritimes"
  )) %>%
  left_join(hist_prate,by=c("region","Year")) %>%
  left_join(emp,by=c("GEO","Year")) %>%
  group_by(GEO) %>%
  mutate(EmpEst=weighted.mean(Employment,Year==1976)*(pop/weighted.mean(pop,Year==1976))*(prate/weighted.mean(prate,Year==1976))*((1-unemp/100)/weighted.mean(1-unemp/100,Year==1976))) %>%
  group_by(Year) %>%
  mutate(Total=sum(EmpEst)) %>%
  left_join(emp %>% filter(GEO=="Canada") %>% select(Year,ActualTotal=Employment),by="Year") %>%
  ungroup()

# Append to the main employment data series
emp<-emp %>%
  bind_rows(
    emp_historical %>%
      filter(Year!=1976) %>%
      select(Year,GEO,Employment=EmpEst)
  )

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
         CPI=CPI/weighted.mean(CPI,Year==2020), # to match the FON Normalizers
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
  mutate(CPI=CPI/weighted.mean(CPI,Year==2020)) # to match the FON Normalizers
cpi_year<-2020

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

# Adjust the provincial pre-1950 GDP values to be consistent with the national total
test<-canada_macro %>%
  left_join(
    prov_GDP %>%
      gather(GEO,value,-Year) %>%
      group_by(Year) %>%
      drop_na() %>%
      summarise(allprovGDP=sum(value)),
    by="Year"
  ) %>%
  mutate(rescale=GDP/allprovGDP)
prov_GDP<-prov_GDP %>%
  gather(GEO,value,-Year) %>%
  left_join(test %>% select(Year,rescale),by="Year") %>%
  mutate(GDP=value*rescale) %>%
  select(GEO,Year,GDP) %>%
  spread(GEO,GDP)

# See if the provincial populations are consistent with the national total
test<-canada_macro %>%
  left_join(
    prov_Pop %>%
      gather(GEO,value,-Year) %>%
      group_by(Year) %>%
      drop_na() %>%
      summarise(allprov=sum(value)),
    by="Year"
  ) %>%
  mutate(rescale=Population/allprov)
# plot(test$rescale) # this is fine, let it slide: due to NWT I suspect, which is excluded

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
source('../Dropbox/Outside Work and Policy/Finances of the Nation/Data/MacroData/ExcelFormatting.R')

##############
# Rates Data #
##############

# Generate Annual Average Interest Rates
histrates<-read_excel("../Dropbox/Outside Work and Policy/Finances of the Nation/Data/MacroData/JSTdatasetR5.xlsx",
                      sheet='Data') %>%
  filter(country=="Canada") %>%
  select(Year=year,ltrate)
yields<-getTABLE("10100122")
yields_annual<-yields %>%
  filter(Rates=="Selected Government of Canada benchmark bond yields: long term") %>%
  mutate(Year=year(Ref_Date)) %>%
  group_by(Year) %>%
  summarise(ltrate=mean(Value)) %>%
  drop_na()
canada_yields<-histrates %>% filter(Year<=1975) %>%
  rbind(yields_annual)

