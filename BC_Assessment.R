##############################################################
#How To Use This Program:
#1.Get search and get the url of the house
#2.In R, Type & Run: "source('BC_Assessment.R')"
#3.Then, Type & Run: "pid('paste_the_csv_filename_here')"
#4.The CSV file will be generated and store in "Data" folder.
#5.Also, a data sheet will be generated.
#6.By typing "Y" or "N", you can open the data sheet.
##############################################################

pid <- function(list,column){
  library(rvest)
  library(jsonlite)
  library(stringi)


  Info.df = data.frame(PID = character(),
                       Address = character(),
                       Total_value = character(),
                       Current_Land = character(),
                       Current_Buildings = character(),
                       Last_Year_price = character(),
                       Last_year_land = character(),
                       Last_year_buildings = character(),
                       Sale_History_Date = character(),
                       Sale_History_Price = character(),
                       Year_Built = character(),
                       Description = character(),
                       Land_Size = character(),
                       File_Name = character(),
                       stringsAsFactors = FALSE)
  
  
  #pid_set = read.table(list, colClasses = "character")
  pid_set = read.csv(list)
  cat("Total PIDs:",nrow(pid_set),"\n")
  cat("Checking PID validity...\n")
  url = "https://www.bcassessment.ca/Property/Search/GetByPid/"
  info_url = "https://www.bcassessment.ca/Property/Info/"
  pid_cleaned = gsub("-","",pid_set[,column])
  pid_page =unlist(lapply(pid_cleaned, function(i){paste0(url,i)}))
  pid_encode = unlist(lapply(pid_page, function(i){
    check_error = tryCatch(
      {fromJSON(i)$aaData[,5]}, 
      error = function(e){cat("ERROR: PID",stri_sub(i, -9, -1),"invalid!!!\n")})
  }))
  Info_page = unlist(lapply(pid_encode, function(i){paste0(info_url,i)}))
  
  cat("Total valid PIDs:",length(Info_page),"\n")
  cat("Generating CSV...\n")
  
  lapply(unlist(Info_page), function(i){
    cat("|")
    
    html_pre = read_html(i)
    Address = html_text(html_nodes(html_pre, "#mainaddresstitle"))
    Total_Value = html_text(html_nodes(html_pre, "#lblTotalAssessedValue"))
    Curr_Land = html_text(html_nodes(html_pre, "#lblTotalAssessedLand"))
    curr_Buildings = html_text(html_nodes(html_pre, "#lblTotalAssessedBuilding"))
    Last_year_price = html_text(html_nodes(html_pre, "#lblPreviousAssessedValue"))
    Last_year_land = html_text(html_nodes(html_pre, "#lblPreviousAssessedLand"))
    Last_year_buildings = html_text(html_nodes(html_pre, "#lblPreviousAssessedBuilding"))
    Sale_History_Date = html_text(html_nodes(html_pre, ".salesrow:nth-child(1) td:nth-child(1)"))
    Sale_History_Price = html_text(html_nodes(html_pre, ".salesrow:nth-child(1) td:nth-child(2)"))
    Year_built = html_text(html_nodes(html_pre, "#lblYearBuilt"))
    Description = html_text(html_nodes(html_pre, "#lblDescription"))
    LandSize = html_text(html_nodes(html_pre, "#lblLandSize"))
    if (length(LandSize) == 0){
      LandSize = "NA"
    }
    else {
      if (grepl("Sq", LandSize) == F){
        LandSize = paste(as.numeric(strsplit(LandSize," ")[[1]][1]) * as.numeric(strsplit(LandSize," ")[[1]][3]), "Sq Ft")
      }
      }
    
    PID = html_text(html_nodes(html_pre, "#lblLegalDescription p+ p"))
    PID = gsub("PID:|", "", PID) # Remove text
    PID = gsub("^\\s+|\\s+$", "", PID) # Remove spaces
    Name = paste(PID, "-", format(Sys.time(), "%F"))
    
    Info.df[1,1] = PID
    Info.df[1,2] = Address
    Info.df[1,3] = Total_Value
    if (length(Curr_Land) == 0){
      Info.df[1,4] = "NA"
    } else{
      Info.df[1,4] = Curr_Land
      }
    if (length(curr_Buildings) == 0){
      Info.df[1,5] = "NA"
    } else{
      Info.df[1,5] = curr_Buildings
      }
    if (length(Last_year_price) == 0){
      Info.df[1,6] = "NA"
    } else{
      Info.df[1,6] = Last_year_price
      }
    if (length(Last_year_land) == 0){
      Info.df[1,7] = "NA"
    } else{
      Info.df[1,7] = Last_year_land
      }
    if (length(Last_year_buildings) == 0){
      Info.df[1,8] = "NA"
    } else{
      Info.df[1,8] = Last_year_buildings
    }
    if (length(Sale_History_Date) == 0){
      Info.df[1,9] = "NA"
    } else{
      Info.df[1,9] = Sale_History_Date
    }
    if (length(Sale_History_Price) == 0){
      Info.df[1,10] = "NA"
    } else{
      Info.df[1,10] = Sale_History_Price
    }
    if (length(Year_built) == 0){
      Info.df[1,11] = "NA"
    } else{
      Info.df[1,11] = Year_built
      }
    if (length(Description) == 0){
      Info.df[1,12] = "NA"
    } else{
      Info.df[1,12] = Description
     }
    Info.df[1,13] = LandSize
    Info.df[1,14] = Name
    
    write.csv(Info.df, file=paste("./Data/", Name, ".csv", sep=""), row.names=FALSE)
  })
  
  multmerge = function(mypath){
    filenames=list.files(path=mypath, full.names=TRUE)
    datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
    Reduce(function(x,y) {merge(x,y,all = TRUE)}, datalist)
  }
    
  full_data = multmerge("./Data")
  write.csv(full_data, file = "Full.csv", row.names=FALSE)
  
  cat("\n---- Done!\n")
  
  ####Comment this if unable to open file###
  open = readline(prompt="Open file? (Y/N)\n")
  if (open == "Y"){
    shell.exec("Full.csv")
    cat("Opening file...")
  }
  else{cat("Have a nice day!")}
  ###########################################
}
#save(pid, file = 'pid.rda')