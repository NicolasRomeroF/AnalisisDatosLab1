library(gam)

path = "~/Escritorio/USACH/Analisis de Datos/Lab1/hepatitis.data"
hepatitis <- read.table(path,sep=",", na.strings = c("?"))

names <- c("Class","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
           "ANOREXIA","LIVER BIG","LIVER FIRM","SPLEEN PALPABLE","SPIDERS",
           "ASCITES","VARICES","BILIRUBIN","ALK PHOSPHATE","SGOT","ALBUMIN",
           "PROTIME","ISTOLOGY")

#colnames(hepatitis) <- names

means <- sapply(hepatitis,mean, na.rm = TRUE)
