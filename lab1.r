library(ggplot2)

path = "~/Escritorio/USACH/Analisis de Datos/Lab1/hepatitis.data"
#path = "~/Documentos/AnalisisDatosLab1/hepatitis.data"
hepatitis <- read.table(path,sep=",", na.strings = c("?"))

names <- c("CLASS","AGE","SEX","STEROID","ANTIVIRALS","FATIGUE","MALAISE",
           "ANOREXIA","LIVER_BIG","LIVER_FIRM","SPLEEN_PALPABLE","SPIDERS",
           "ASCITES","VARICES","BILIRUBIN","ALK_PHOSPHATE","SGOT","ALBUMIN",
           "PROTIME","HISTOLOGY")

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

hepatitis.die <- hepatitis.without.na[which(hepatitis.without.na$CLASS == 1),]
hepatitis.live <- hepatitis.without.na[which(hepatitis.without.na$CLASS == 2),]

print(summary(hepatitis.die))
print(summary(hepatitis.live))
# p.1 <- ggplot(hepatitis.without.na, aes(x=NULL, y=AGE)) + geom_boxplot()
# 
# p.2 <- ggplot(hepatitis.without.na, aes(x=NULL, y=BILIRUBIN)) + geom_boxplot()
# 
# p.3 <- ggplot(hepatitis.without.na, aes(x=NULL, y=ALK_PHOSPHATE)) + geom_boxplot()
# 
# p.4 <- ggplot(hepatitis.without.na, aes(x=NULL, y=SGOT)) + geom_boxplot()
# 
# p.5 <- ggplot(hepatitis.without.na, aes(x=NULL, y=ALBUMIN)) + geom_boxplot()
# 
# p.6 <- ggplot(hepatitis.without.na, aes(x=NULL, y=PROTIME)) + geom_boxplot()
# 
# show(p.1)
# show(p.2)
# show(p.3)
# show(p.4)
# show(p.5)
# show(p.6)

SEX <- hepatitis.without.na[["SEX"]]
sex <- rep("male", length(SEX))
sex[SEX == 2] <- "female"
b.1 <-ggplot(data=hepatitis.without.na, aes(x=sex, fill=CLASS)) + geom_bar()
show(b.1)

STEROID <- hepatitis.without.na[["STEROID"]]
steroid <- rep("no", length(STEROID))
steroid[STEROID == 2] <- "yes"
b.2 <-ggplot(data=hepatitis.without.na, aes(x=steroid, fill=CLASS)) + geom_bar()
show(b.2)
# 
# ANTIVIRALS <- hepatitis.without.na[["ANTIVIRALS"]]
# antivirals <- rep("no", length(ANTIVIRALS))
# antivirals[ANTIVIRALS == 2] <- "yes"
# b.3 <-ggplot(data=hepatitis.without.na, aes(x=antivirals)) + geom_bar()
# show(b.3)
# 
# FATIGUE <- hepatitis.without.na[["FATIGUE"]]
# fatigue <- rep("no", length(FATIGUE))
# fatigue[FATIGUE== 2] <- "yes"
# b.4 <-ggplot(data=hepatitis.without.na, aes(x=fatigue)) + geom_bar()
# show(b.4)
# 
# MALAISE <- hepatitis.without.na[["MALAISE"]]
# malaise <- rep("no", length(MALAISE))
# malaise[MALAISE== 2] <- "yes"
# b.5 <-ggplot(data=hepatitis.without.na, aes(x=malaise)) + geom_bar()
# show(b.5)
# 
# ANOREXIA <- hepatitis.without.na[["ANOREXIA"]]
# anorexia <- rep("no", length(ANOREXIA))
# anorexia[ANOREXIA== 2] <- "yes"
# b.6 <-ggplot(data=hepatitis.without.na, aes(x=anorexia)) + geom_bar()
# show(b.6)
# 
# LIVER_BIG <- hepatitis.without.na[["LIVER_BIG"]]
# liver_big <- rep("no", length(LIVER_BIG))
# liver_big[LIVER_BIG== 2] <- "yes"
# b.7 <-ggplot(data=hepatitis.without.na, aes(x=liver_big)) + geom_bar()
# show(b.7)
# 
# LIVER_FIRM <- hepatitis.without.na[["LIVER_FIRM"]]
# liver_firm <- rep("no", length(LIVER_FIRM))
# liver_firm[LIVER_FIRM== 2] <- "yes"
# b.8 <-ggplot(data=hepatitis.without.na, aes(x=liver_firm)) + geom_bar()
# show(b.8)
# 
# SPLEEN_PALPABLE <- hepatitis.without.na[["SPLEEN_PALPABLE"]]
# spleen_palable <- rep("no", length(SPLEEN_PALPABLE))
# spleen_palable[SPLEEN_PALPABLE== 2] <- "yes"
# b.9 <-ggplot(data=hepatitis.without.na, aes(x=spleen_palable)) + geom_bar()
# show(b.9)
# 
# SPIDERS <- hepatitis.without.na[["SPIDERS"]]
# spiders <- rep("no", length(SPIDERS))
# spiders[SPIDERS== 2] <- "yes"
# b.10 <-ggplot(data=hepatitis.without.na, aes(x=spiders)) + geom_bar()
# show(b.10)
# 
# ASCITES <- hepatitis.without.na[["ASCITES"]]
# ascites <- rep("no", length(ASCITES))
# ascites[ASCITES== 2] <- "yes"
# b.11 <-ggplot(data=hepatitis.without.na, aes(x=ascites)) + geom_bar()
# show(b.11)
# 
# VARICES <- hepatitis.without.na[["VARICES"]]
# varices <- rep("no", length(VARICES))
# varices[VARICES== 2] <- "yes"
# b.12 <-ggplot(data=hepatitis.without.na, aes(x=varices)) + geom_bar()
# show(b.12)
# 
# HISTOLOGY <- hepatitis.without.na[["HISTOLOGY"]]
# histology <- rep("no", length(HISTOLOGY))
# histology[HISTOLOGY== 2] <- "yes"
# b.13 <-ggplot(data=hepatitis.without.na, aes(x=histology)) + geom_bar()
# show(b.13)

