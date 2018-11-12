
path = "~/Escritorio/USACH/Analisis de Datos/Lab1/hepatitis.data"
hepatitis <- read.table(path,sep=",", na.strings = c("?"))

names <- c("Class","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
           "ANOREXIA","LIVER BIG","LIVER FIRM","SPLEEN PALPABLE","SPIDERS",
           "ASCITES","VARICES","BILIRUBIN","ALK PHOSPHATE","SGOT","ALBUMIN",
           "PROTIME","ISTOLOGY")

colnames(hepatitis) <- names

getmode <- function(x){
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x,uniqv)))]
}

hepatitis.without.na <- na.omit(hepatitis)

means <- sapply(hepatitis.without.na,mean)
medians <- sapply(hepatitis.without.na,median)
modes <- sapply(hepatitis.without.na,getmode)
