### size of csv file: 121.9MB (7793 rows * 1729 columns) ###

system.time(read.csv('Data/MERGED2013_PP.csv', header = T))
# user  system elapsed 
# 13.76    0.07   13.85 

system.time(read.csv2('Data/MERGED2013_PP.csv', header = T))
# user  system elapsed 
# 10.42    0.07   10.48 

library(data.table)
system.time(fread('Data/MERGED2013_PP.csv', header = T, sep = ',')) 
# user  system elapsed 
# 1.34    0.00    1.35

library(bigmemory)
system.time(read.big.matrix('Data/MERGED2013_PP.csv', header = T))
# user  system elapsed 
# 10.17    1.69   12.19 

library(ff)
system.time(read.csv.ffdf(file = 'Data/MERGED2013_PP.csv', header = T))
# Doesn't work

library(sqldf)
system.time(read.csv.sql('Data/MERGED2013_PP.csv'))
# user  system elapsed 
# 17.29    0.94   18.52 
