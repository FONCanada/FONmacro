# Auto Formatting of FON Excel Sheets
require(openxlsx)
require(tidyverse)

# Define Style Objects
percent_style<-createStyle(
  numFmt = "PERCENTAGE"
)
dollar_style<-createStyle(
  numFmt = "CURRENCY"
)
comma_style<-createStyle(
  numFmt = "COMMA"
)
number_style<-createStyle(
  numFmt = "NUMBER"
)
align_style<-createStyle(
  halign = "center"
)
toplabel_style<-createStyle(
  wrapText = T,
  textDecoration="bold"
)

# Form an Excel spreadsheet, Each Tab is a Different Variable
wb<-createWorkbook()
addWorksheet(wb,"Population")
writeData(wb,"Population",
          table %>% select(Year,Region,Value=Population) %>%
            mutate(Region=factor(Region,levels=provnames$GEO)) %>%
            spread(Region,Value))
addWorksheet(wb,"CPI (2020=100)")
writeData(wb,"CPI (2020=100)",
          table %>% select(Year,Region,Value=CPI) %>%
            mutate(Region=factor(Region,levels=provnames$GEO)) %>%
            spread(Region,Value))
addWorksheet(wb,"Nominal GDP ($M)")
writeData(wb,"Nominal GDP ($M)",
          table %>% select(Year,Region,Value=GDP) %>%
            mutate(Region=factor(Region,levels=provnames$GEO)) %>%
            spread(Region,Value))
addWorksheet(wb,"Real GDP (2020 $M)")
writeData(wb,"Real GDP (2020 $M)",
          table %>% select(Year,Region,Value=rGDP) %>%
            mutate(Region=factor(Region,levels=provnames$GEO)) %>%
            spread(Region,Value))
addWorksheet(wb,"Nominal GDP per Capita ($)")
writeData(wb,"Nominal GDP per Capita ($)",
          table %>% select(Year,Region,Value=GDPpc) %>%
            mutate(Region=factor(Region,levels=provnames$GEO)) %>%
            spread(Region,Value))
addWorksheet(wb,"Real GDP per Capita (2020 $)")
writeData(wb,"Real GDP per Capita (2020 $)",
          table %>% select(Year,Region,Value=rGDPpc) %>%
            mutate(Region=factor(Region,levels=provnames$GEO)) %>%
            spread(Region,Value))

# Apply Formatting
for (s in sheets(wb)){
  rows=dim(read.xlsx(wb,s))[1]
  cols=dim(read.xlsx(wb,s))[2]
  
  freezePane(wb,s,firstRow = TRUE) # Freeze Top Pane
  setColWidths(wb,s,1:cols,widths=13) # Set Column Width
  
  # Set styles
  addStyle(wb,s,comma_style,2:(rows+1),2:cols,gridExpand = T) # comma format
  addStyle(wb,s,align_style,1:(rows+1),1:cols,gridExpand = T,stack = T) # alignment
  addStyle(wb,s,toplabel_style,1,1:cols,gridExpand = T,stack = T) # header
}
addStyle(wb,"CPI (2020=100)",number_style,2:(rows+1),2:cols,gridExpand = T) # NUMBER format for CPI
addStyle(wb,"CPI (2020=100)",align_style,1:(rows+1),1:cols,gridExpand = T,stack = T) # alignment
addStyle(wb,"CPI (2020=100)",toplabel_style,1,1:cols,gridExpand = T,stack = T) # header

# Save
saveWorkbook(wb,"FON Macroeconomic Data.xlsx", overwrite = TRUE)
