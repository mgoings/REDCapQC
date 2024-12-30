
##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


#' @importFrom dplyr %>% select rename filter
#' @importFrom stringr str_locate_all str_locate
#'
#' @title REDCapQC
#'
#' @description Create a QC list from REDCap data dictionary and REDCap all data export
#' @param dataset_dir Directory that holds the location of the dataset and data dictionary
#' @param dataset Name of dataset from REDCap
#'
#' @return A data frame with a list of queries.
#'  Variables will include id, form, var, txt (description of query), and report date
#' @export
REDCapQC <- function(dataset_dir, dataset) {


### REDCap metadata queries function
## Program set-up
data <- readRDS(file=dataset)
# Removing instances of repeating instruments for program specification
data <- data[!complete.cases(data$redcap_repeat_instance == ""),]
# First column in REDCap is always the ID variable
IDvar <- names(data)[1]
WorkingDirFiles <- list.files(dataset_dir)
DataDictionary_FileName <- grep("DataDictionary", WorkingDirFiles, value = T)
DatabaseName_End <- unlist(stringr::str_locate_all(WorkingDirFiles,"_DataDictionary"))[1]
DatabaseName <- substr(DataDictionary_FileName,0,DatabaseName_End-1)
rm(DatabaseName_End)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


## Import and subset data dictionary for processing
# Read in and subset data dictionary to items of interest for generating queries
Raw_MetaData <- read.csv(DataDictionary_FileName)
MetaData <- Raw_MetaData %>%
              dplyr::select(Variable...Field.Name
                     ,Branching.Logic..Show.field.only.if....
                     ,Form.Name
                     ,Field.Type
                     ,Text.Validation.Type.OR.Show.Slider.Number
                     ,Text.Validation.Min
                     ,Text.Validation.Max
                     ,Required.Field.
                     ,Field.Note) %>%
              dplyr::rename(Branching.Logic =
                     Branching.Logic..Show.field.only.if....
                     ,Field.Name = Variable...Field.Name
                     ,Text.Validation = Text.Validation.Type.OR.Show.Slider.Number) %>%
              dplyr::filter(Required.Field. == "y")
# Per program specification, logic with smart variables cannot be used.
smartVars <- c("\\[last-instance\\]")
MetaData <- MetaData[!grepl(paste(smartVars, collapse = "|")
                            , MetaData$Branching.Logic), ]
# (MIND only) Variables in this list don't have logic, and exist on a repeating
  # instrument
repeatingVars <- c('mhpe_unwell','mhpe_fever','req_id')
MetaData <- MetaData[!grepl(paste(repeatingVars, collapse = "|"), MetaData$Field.Name), ]
for (i in seq(from=1,to=length(MetaData[[1]]))) {
  MetaData$VarNum[i] <- i
}

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


## Find all date fields and make a corresponding date variable
MetaDataDts <- MetaData %>%
                dplyr::select(Field.Name, Text.Validation,VarNum) %>%
                dplyr::filter(Text.Validation %in% c("date_dmy","date_mdy","date_ymd"))
MetaDataDts$New.Field.Name <- paste0(MetaDataDts$Field.Name,".Date")
if (length(unique(MetaDataDts$Text.Validation)) != 1)  {
  print("More than 1 date format is used in this database")
} else {
  rc_dateFormat = unique(MetaDataDts$Text.Validation)
  if (rc_dateFormat == "date_dmy") {r_dateFormat <- "%d-%m-%Y"}
  else if (rc_dateFormat == "date_mdy") {r_dateFormat <- "%m-%d-%Y"}
  else if (rc_dateFormat == "date_ymd") {r_dateFormat <- "%Y-%m-%d"}
}
NumDtFields <- length(MetaDataDts$New.Field.Name)
if (NumDtFields >= 1) {
  for (i in seq(from=1,to=NumDtFields)) {
    asDateExpr <- parse(text=paste0("data$",MetaDataDts$New.Field.Name[i]
                                    ," <- as.Date(data$",MetaDataDts$Field.Name[i],")"))
    eval(asDateExpr)
  }
}

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


## "Simple replacements" where REDCap/R syntax can be directly translated one to the other
# Make simple replacements of syntax
SimpleReplace <- function(x) {
  x <- gsub(" = "," == ",x)
  x <- gsub("<>","!=",x)
  x <- gsub("isinteger\\(","is\\.integer\\(", x, ignore.case = TRUE)
  x <- gsub(" and "," & ", x, ignore.case = TRUE)
  x <- gsub(" and\n"," & ", x, ignore.case = TRUE)
  x <- gsub("\nand"," & ", x, ignore.case = TRUE)
  x <- gsub(" or "," | ", x, ignore.case = TRUE)
  x <- gsub(" or\n"," | ", x, ignore.case = TRUE)
  x <- gsub("\nor "," | ", x, ignore.case = TRUE)
  x <- gsub("\t","", x, ignore.case = TRUE)
}
Logic1 <- SimpleReplace(MetaData$Branching.Logic)


Logic2 <- Logic1
for (varNum in seq(from=1,to=length(Logic2))) {
  for (operator in c("!", "=")) {
  OperatorPerlExpr <- paste0("\\s+",operator,"=\\s+''")
  CompletePerlExpr <- paste0("\\[[^\\]]*\\]",OperatorPerlExpr)
  Replacements <- length(str_locate_all(Logic2[varNum],CompletePerlExpr)[[1]])/2
    if (Replacements >= 1) {
      for (i in seq(from=1,to=Replacements)) {
        # Find location of variable start, end, and location of <> ""/= "" logic
        StartLoc <- stringr::str_locate(Logic2[varNum],CompletePerlExpr)[1]
        EndLoc <- stringr::str_locate(Logic2[varNum],OperatorPerlExpr)
        totalLength <- nchar(Logic2[varNum])
        AllLoc <- cbind(StartLoc,EndLoc,totalLength)
        # Piece together new string of logic
        if (operator == "!") {Func = "!is.na("} else {Func = "is.na("}
        before.Logic <- substr(Logic2[varNum],1,AllLoc[1]-1)
        var <- substr(Logic2[varNum],AllLoc[1],AllLoc[2]-1)
        after.Logic <- substr(Logic2[varNum],AllLoc[3]+1,totalLength)
        Logic2[varNum] <- paste0(before.Logic,Func,var,") ",after.Logic)
      }
    }
  }
}

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


Logic3 <- Logic2
dateDiffPerlExpr <- "datediff\\(.*\\) "
for (varNum in seq(from=1,to=length(Logic3))) {
  Replacements <- length(stringr::str_locate_all(Logic3[varNum], dateDiffPerlExpr)[[1]])/2
  if (Replacements >= 1) {
    for (i in seq(from=1,to=Replacements)) {
      # Find location of datediff function, and pull into individual strings the parameters
      dateDiff.AllLoc <- stringr::str_locate(Logic3[varNum],dateDiffPerlExpr)
      dateDiff.Start <- dateDiff.AllLoc[1]
      dateDiff.End <- dateDiff.AllLoc[2]
      dateDiff.Func <- substr(Logic3[varNum],dateDiff.Start,dateDiff.AllLoc[2])
      dateDiff.NumParameters <- stringr::str_count(dateDiff.Func,pattern=",")

      # Three parameters are always required: 2 dates, and 1 for unit difference
      dateDiff.Parameter1 <- substr(dateDiff.Func,10,stringr::str_locate(dateDiff.Func,",")-1)
      dateDiff.MinusP1 <- substr(dateDiff.Func
                                ,stringr::str_locate(dateDiff.Func,",")+1
                                ,nchar(dateDiff.Func))
      dateDiff.Parameter2 <- substr(dateDiff.MinusP1,1,stringr::str_locate(dateDiff.MinusP1,",")-1)
      dateDiff.MinusP2 <- substr(dateDiff.MinusP1
                                ,stringr::str_locate(dateDiff.MinusP1,",")+1
                                ,nchar(dateDiff.MinusP1))
      dateDiff.Parameter3 <- substr(dateDiff.MinusP2,1,str_locate(dateDiff.MinusP2,",")-1)
      # Convert date constants into numeric date field for R
      if (!is.na(stringr::str_locate(dateDiff.Parameter1,"-"))[1]) {
        dateConstant1 <- substr(dateDiff.Parameter1,2,nchar(dateDiff.Parameter1=1)-1)
        diffTime.Parameter1 <- paste0("as.Date('",dateConstant1,"','",r_dateFormat,"')")
      } else {diffTime.Parameter1 <- paste0(dateDiff.Parameter1,".Date")}
      if (!is.na(stringr::str_locate(dateDiff.Parameter2,"-")[1])) {
        dateConstant2 <- substr(dateDiff.Parameter2,2,nchar(dateDiff.Parameter2)-1)
        diffTime.Parameter2 <- paste0("as.Date('",dateConstant2,"','",r_dateFormat,"')")
      } else {diffTime.Parameter2 <- paste0(dateDiff.Parameter2,".Date")}
      # Convert datediff() unit to difftime() unit
      # Difftime does not have the option for years, so will need to use weeks and
        # divide by 52.25
      if (dateDiff.Parameter3 == "'d'") {diffTime.Unit <- "'days'"}

      # Piece together new function using difftime()
      # Difftime reverses the order of dates compared to datediff()
      diffTime.Func <- paste0("difftime(",diffTime.Parameter2
                                     ,",",diffTime.Parameter1
                                     ,", units = ",diffTime.Unit,")")
      before.Logic <- substr(Logic3[varNum],1,dateDiff.Start-1)
      after.Logic  <- substr(Logic3[varNum]
                            ,dateDiff.End+1
                            ,nchar(Logic3[varNum]))
      Logic3[varNum] <- paste0(before.Logic,diffTime.Func,after.Logic)
    }
  }
}

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


# REDCap creates variables for each level of a checkbox, but has different syntax
# for the dataset variable and the metadata variable
Logic4 <- Logic3
CheckBox.PerlExpr <- "\\([0-9]+\\)"
for (varNum in seq(from=1,to=length(Logic4))) {
  Replacements <- length(stringr::str_locate_all(Logic4[varNum], CheckBox.PerlExpr)[[1]])/2
  if (Replacements >= 1) {
    for (i in seq(from=1,to=Replacements)) {
      # Find location of checkbox value code start, end
      Loc <- stringr::str_locate(Logic4[varNum],CheckBox.PerlExpr)
      totalLength <- nchar(Logic4[varNum])
      AllLoc <- cbind(Loc,totalLength)
      # Piece together new string of logic
      before.Logic <- substr(Logic4[varNum],1,AllLoc[1]-1)
      value <- substr(Logic4[varNum],AllLoc[1]+1,AllLoc[2]-1)
      after.Logic <- substr(Logic4[varNum],AllLoc[2]+1,totalLength)
      Logic4[varNum] <- paste0(before.Logic,"___",value,after.Logic)
    }
  }
}

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


SimpleReplace2 <- function(x) {
  x <- gsub("\\[","data$",x)
  x <- gsub("\\]","",x)
  x <- gsub("\\n","",x)
}
Logic5 <- SimpleReplace2(Logic4)


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


Logic6 <- Logic5
NACheck <- list()
NotNACheck <- list()
for (varNum in seq(from=1,to=length(Logic6))) {
  if (Logic6[varNum] != "") {
    NACheck[varNum] <- paste0("(",Logic6[varNum]
                              ,") & is.na(data$",MetaData$Field.Name[varNum],")")
    NotNACheck[varNum] <- paste0("(",Logic6[varNum]
                              ,") & !is.na(data$",MetaData$Field.Name[varNum],")")
  } else {
    NACheck[varNum] <- paste0("is.na(data$",MetaData$Field.Name[varNum],")")
    NotNACheck[varNum] <- paste0("is.na(data$",MetaData$Field.Name[varNum],")")
  }
}

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################


queries <- data.frame()
for (varNum in seq_along(NACheck)) {
  check <- NACheck[varNum]
  id <- na.omit(data[eval(parse(text=check)),c(IDvar)])
  if (length(id) >= 1) {
    var <- MetaData$Field.Name[varNum]
    form <- MetaData$Form.Name[varNum]
    if (MetaData$Field.Note[varNum] == "CRF indicator") {
      txt <- "Missing CRF"
    } else {txt <- "Missing"}
    queries <- rbind(queries, data.frame(id = id, form = form, var =var, txt = txt, report_dt = Sys.Date()))
  }
}
return(queries)

} # End of function

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

