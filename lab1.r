library(ggplot2)

#path = "~/Escritorio/USACH/Analisis de Datos/Lab1/hepatitis.data"
path = "~/Documentos/AnalisisDatosLab1/hepatitis.data"
hepatitis <- read.table(path,sep=",", na.strings = c("?"))

names <- c("Class","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
           "ANOREXIA","LIVER_BIG","LIVER_FIRM","SPLEEN_PALPABLE","SPIDERS",
           "ASCITES","VARICES","BILIRUBIN","ALK_PHOSPHATE","SGOT","ALBUMIN",
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
vars <- sapply(hepatitis.without.na,var)

p.1 <- ggplot(hepatitis.without.na, aes(x=NULL, y=AGE)) + geom_boxplot()

p.2 <- ggplot(hepatitis.without.na, aes(x=NULL, y=BILIRUBIN)) + geom_boxplot()

p.3 <- ggplot(hepatitis.without.na, aes(x=NULL, y=ALK_PHOSPHATE)) + geom_boxplot()

p.4 <- ggplot(hepatitis.without.na, aes(x=NULL, y=SGOT)) + geom_boxplot()

p.5 <- ggplot(hepatitis.without.na, aes(x=NULL, y=ALBUMIN)) + geom_boxplot()

p.6 <- ggplot(hepatitis.without.na, aes(x=NULL, y=PROTIME)) + geom_boxplot()

show(p.1)
show(p.2)
show(p.3)
show(p.4)
show(p.5)
show(p.6)
