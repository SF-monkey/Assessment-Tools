##############################################################
#How To Use This Program:
#1.Get search and get the url of the house
#2.In R, Type & Run: "source('BC_Assessment.R')"
#3.Then, Type & Run: "pid('paste_the_csv_filename_here')"
#4.The CSV file will be generated and store in "Data" folder.
#5.Also, a data sheet will be generated.
#6.By typing "Y" or "N", you can open the data sheet.
##############################################################

library(rvest)
library(httr)
library(jsonlite)


Info.df = data.frame(PID = character(),
                     #Date_info_collected = character(),
                     Address = character(),
                     #Area_Jurisdiction_Roll = character(),
                     Last_Assessment_Date = character(),
                     Total_value = character(),
                     Current_Land = character(),
                     Current_Buildings = character(),
                     Last_year_price = character(),
                     Last_year_land = character(),
                     Last_year_buildings = character(),
                     Year_Built = character(),
                     Description = character(),
                     #Bedrooms = character(),
                     #Baths = character(),
                     #Carports = character(),
                     #Garages = character(),
                     Land_Size = character(),
                     #First_Floor_Area = character(),
                     #Second_Floor_Area = character(),
                     #Basement_Finish_Area = character(),
                     #Strata_Total_Area = character(),
                     #Stories_Building = character(),
                     #Gross_Leasable_Area = character(),
                     #Net_Leasable_Area = character(),
                     #Number_Unit_Apartment = character(),
                     File_Name = character(),
                     stringsAsFactors = FALSE)

pid <- function(list){
  
  cat("Scraper in hot...\n")
  
  pid_set = read.table(list, colClasses="character")
  url = "https://www.bcassessment.ca/Property/Search/GetByPid/"
  info_url = "https://www.bcassessment.ca/Property/Info/"
  
  pid_page =lapply(pid_set, function(i){paste0(url,i)})$`V1`
  Info_page = unlist(lapply(pid_page, function(i){
    paste0(info_url,fromJSON(content(stop_for_status(GET(i)), "text"))$aaData[,5])
    }))
  
  lapply(Info_page, function(i){
    cat("|")
    
    html_pre = read_html(i)
    Address = html_text(html_nodes(html_pre, "#mainaddresstitle"))
    #areajursroll = html_text(html_nodes(html_pre, "#areajursrolltitlebox"))
    #areajursroll = gsub("Area-Jurisdiction-Roll:", "", areajursroll) # Remove text
    #areajursroll = gsub("^\\s+|\\s+$", "", areajursroll) # Remove spaces
    Total_Value = html_text(html_nodes(html_pre, "#lblTotalAssessedValue"))
    LastAssessmentDate = html_text(html_nodes(html_pre, "#lblLastAssessmentDate"))
    Curr_Land = html_text(html_nodes(html_pre, "#lblTotalAssessedLand"))
    curr_Buildings = html_text(html_nodes(html_pre, "#lblTotalAssessedBuilding"))
    Last_year_price = html_text(html_nodes(html_pre, "#lblPreviousAssessedValue"))
    Last_year_land = html_text(html_nodes(html_pre, "#lblPreviousAssessedLand"))
    Last_year_buildings = html_text(html_nodes(html_pre, "#lblPreviousAssessedBuilding"))
    year_built = html_text(html_nodes(html_pre, "#lblYearBuilt"))
    Description = html_text(html_nodes(html_pre, "#lblDescription"))
    #Bedrooms = html_text(html_nodes(html_pre, "#lblBedrooms"))
    #Baths = html_text(html_nodes(html_pre, "#lblBathRooms"))
    #Carports = html_text(html_nodes(html_pre, "#lblCarPorts"))
    #Garages = html_text(html_nodes(html_pre, "#lblGarages"))
    LandSize = html_text(html_nodes(html_pre, "#lblLandSize"))
    if (LandSize != ""){
      if (grepl("Sq", LandSize) == F){
        LandSize = paste(as.numeric(strsplit(LandSize," ")[[1]][1]) * as.numeric(strsplit(LandSize," ")[[1]][3]), "Sq Ft")
      }
    }
    #FirstFloorArea = html_text(html_nodes(html_pre, "#lblFirstFloorArea"))
    #SecondFloorArea = html_text(html_nodes(html_pre, "#lblSecondFloorArea"))
    #BasementFinishArea = html_text(html_nodes(html_pre, "#lblBasementFinishArea"))
    #StrataTotalArea = html_text(html_nodes(html_pre, "#lblStrataTotalArea"))
    #StoriesBuilding = html_text(html_nodes(html_pre, "#lblStoriesBuilding"))
    #GrossLeasableArea = html_text(html_nodes(html_pre, "#lblGrossLeasableArea"))
    #NetLeasableArea = html_text(html_nodes(html_pre, "#lblNetLeasableArea"))
    #NumberUnitApartment = html_text(html_nodes(html_pre, "#lblNumberUnitApartment"))
    PID = html_text(html_nodes(html_pre, "#lblLegalDescription p+ p"))
    PID = gsub("PID:|", "", PID) # Remove text
    PID = gsub("^\\s+|\\s+$", "", PID) # Remove spaces
    Name = paste(PID, "-", format(Sys.time(), "%F"))
    
    Info.df[1,1] = PID
    #Info.df[1,2] = format(Sys.time(), "%F %X %Z")
    Info.df[1,2] = Address
    #Info.df[1,4] = areajursroll
    Info.df[1,3] = LastAssessmentDate
    Info.df[1,4] = Total_Value
    Info.df[1,5] = Curr_Land
    Info.df[1,6] = curr_Buildings
    Info.df[1,7] = Last_year_price
    Info.df[1,8] = Last_year_land
    Info.df[1,9] = Last_year_buildings
    Info.df[1,10] = year_built
    Info.df[1,11] = Description
    #Info.df[1,14] = Bedrooms
    #Info.df[1,15] = Baths
    #Info.df[1,16] = Carports
    #Info.df[1,17] = Garages
    Info.df[1,12] = LandSize
    #Info.df[1,19] = FirstFloorArea
    #Info.df[1,20] = SecondFloorArea
    #Info.df[1,21] = BasementFinishArea
    #Info.df[1,22] = StrataTotalArea
    #Info.df[1,23] = StoriesBuilding
    #Info.df[1,24] = GrossLeasableArea
    #Info.df[1,25] = NetLeasableArea
    #Info.df[1,26] = NumberUnitApartment
    Info.df[1,13] = Name
    
    write.csv(Info.df, file=paste("./Data/", Name, ".csv", sep=""), row.names=FALSE)
    
    multmerge = function(mypath){
      filenames=list.files(path=mypath, full.names=TRUE)
      datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
      Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
    }
    
    full_data = multmerge("./Data")
    write.csv(full_data, file = "Full.csv", row.names=FALSE)
  })
  cat(" ---- Done!\n")
  
  ####Comment this if unable to open file###
  open = readline(prompt="Open file? (Y/N)\n")
  if (open == "Y"){
    shell.exec("Full.csv")
    cat("Opening file...")
  }
  else{cat("Have a nice day!")}
  ###########################################
}