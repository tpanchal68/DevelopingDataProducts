library(data.table)

# Read data file and load data
if(!exists("CollegeScorecardData")){
        CollegeScorecardData <- fread(
                #        CollegeScorecardData <- read.csv2(
                "Data/MERGED2013_PP.csv",
                header = TRUE, 
                sep = ",",
                na.strings = c('NA','', 'NULL'),
                stringsAsFactors = FALSE
        )
}
