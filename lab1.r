library(ggplot2)
library(gridExtra)

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

SEX <- hepatitis.die[["SEX"]]
sex <- rep("male", length(SEX))
sex[SEX == 2] <- "female"
b.d.1 <-ggplot(data=hepatitis.die, aes(x=sex,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.1)

SEX <- hepatitis.live[["SEX"]]
sex <- rep("male", length(SEX))
sex[SEX == 2] <- "female"
b.l.1 <-ggplot(data=hepatitis.live, aes(x=sex,y = ..prop..,group= 1)) + geom_bar(stat="count")  +ggtitle("Class: LIVE")
show(b.l.1)

STEROID <- hepatitis.die[["STEROID"]]
steroid <- rep("no", length(STEROID))
steroid[STEROID == 2] <- "yes"
b.d.2 <-ggplot(data=hepatitis.die, aes(x=steroid, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.2)

STEROID <- hepatitis.live[["STEROID"]]
steroid <- rep("no", length(STEROID))
steroid[STEROID == 2] <- "yes"
b.l.2 <-ggplot(data=hepatitis.live, aes(x=steroid, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.2)

ANTIVIRALS <- hepatitis.die[["ANTIVIRALS"]]
antivirals <- rep("no", length(ANTIVIRALS))
antivirals[ANTIVIRALS == 2] <- "yes"
b.d.3 <-ggplot(data=hepatitis.die, aes(x=antivirals, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.3)

ANTIVIRALS <- hepatitis.live[["ANTIVIRALS"]]
antivirals <- rep("no", length(ANTIVIRALS))
antivirals[ANTIVIRALS == 2] <- "yes"
b.l.3 <-ggplot(data=hepatitis.live, aes(x=antivirals, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.3)

FATIGUE <- hepatitis.die[["FATIGUE"]]
fatigue <- rep("no", length(FATIGUE))
fatigue[FATIGUE== 2] <- "yes"
b.d.4 <-ggplot(data=hepatitis.die, aes(x=fatigue, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.4)

FATIGUE <- hepatitis.live[["FATIGUE"]]
fatigue <- rep("no", length(FATIGUE))
fatigue[FATIGUE== 2] <- "yes"
b.l.4 <-ggplot(data=hepatitis.live, aes(x=fatigue, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.4)

MALAISE <- hepatitis.die[["MALAISE"]]
malaise <- rep("no", length(MALAISE))
malaise[MALAISE== 2] <- "yes"
b.d.5 <-ggplot(data=hepatitis.die, aes(x=malaise, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.5)

MALAISE <- hepatitis.live[["MALAISE"]]
malaise <- rep("no", length(MALAISE))
malaise[MALAISE== 2] <- "yes"
b.l.5 <-ggplot(data=hepatitis.live, aes(x=malaise, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.5)

ANOREXIA <- hepatitis.die[["ANOREXIA"]]
anorexia <- rep("no", length(ANOREXIA))
anorexia[ANOREXIA== 2] <- "yes"
b.d.6 <-ggplot(data=hepatitis.die, aes(x=anorexia, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.6)

ANOREXIA <- hepatitis.live[["ANOREXIA"]]
anorexia <- rep("no", length(ANOREXIA))
anorexia[ANOREXIA== 2] <- "yes"
b.l.6 <-ggplot(data=hepatitis.live, aes(x=anorexia, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.6)

LIVER_BIG <- hepatitis.die[["LIVER_BIG"]]
liver_big <- rep("no", length(LIVER_BIG))
liver_big[LIVER_BIG== 2] <- "yes"
b.d.7 <-ggplot(data=hepatitis.die, aes(x=liver_big, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.7)

LIVER_BIG <- hepatitis.live[["LIVER_BIG"]]
liver_big <- rep("no", length(LIVER_BIG))
liver_big[LIVER_BIG== 2] <- "yes"
b.l.7 <-ggplot(data=hepatitis.live, aes(x=liver_big, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.7)

LIVER_FIRM <- hepatitis.die[["LIVER_FIRM"]]
liver_firm <- rep("no", length(LIVER_FIRM))
liver_firm[LIVER_FIRM== 2] <- "yes"
b.d.8 <-ggplot(data=hepatitis.die, aes(x=liver_firm, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.8)

LIVER_FIRM <- hepatitis.live[["LIVER_FIRM"]]
liver_firm <- rep("no", length(LIVER_FIRM))
liver_firm[LIVER_FIRM== 2] <- "yes"
b.l.8 <-ggplot(data=hepatitis.live, aes(x=liver_firm, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.8)

SPLEEN_PALPABLE <- hepatitis.die[["SPLEEN_PALPABLE"]]
spleen_palpable <- rep("no", length(SPLEEN_PALPABLE))
spleen_palpable[SPLEEN_PALPABLE== 2] <- "yes"
b.d.9 <-ggplot(data=hepatitis.die, aes(x=spleen_palpable, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.9)

SPLEEN_PALPABLE <- hepatitis.live[["SPLEEN_PALPABLE"]]
spleen_palpable <- rep("no", length(SPLEEN_PALPABLE))
spleen_palpable[SPLEEN_PALPABLE== 2] <- "yes"
b.l.9 <-ggplot(data=hepatitis.live, aes(x=spleen_palpable, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.9)

SPIDERS <- hepatitis.die[["SPIDERS"]]
spiders <- rep("no", length(SPIDERS))
spiders[SPIDERS== 2] <- "yes"
b.d.10 <-ggplot(data=hepatitis.die, aes(x=spiders, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.10)

SPIDERS <- hepatitis.live[["SPIDERS"]]
spiders <- rep("no", length(SPIDERS))
spiders[SPIDERS== 2] <- "yes"
b.l.10 <-ggplot(data=hepatitis.live, aes(x=spiders, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.10)

ASCITES <- hepatitis.die[["ASCITES"]]
ascites <- rep("no", length(ASCITES))
ascites[ASCITES== 2] <- "yes"
b.d.11 <-ggplot(data=hepatitis.die, aes(x=ascites, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.11)

ASCITES <- hepatitis.live[["ASCITES"]]
ascites <- rep("no", length(ASCITES))
ascites[ASCITES== 2] <- "yes"
b.l.11 <-ggplot(data=hepatitis.live, aes(x=ascites, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.11)

VARICES <- hepatitis.die[["VARICES"]]
varices <- rep("no", length(VARICES))
varices[VARICES== 2] <- "yes"
b.d.12 <-ggplot(data=hepatitis.die, aes(x=varices, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.12)

VARICES <- hepatitis.live[["VARICES"]]
varices <- rep("no", length(VARICES))
varices[VARICES== 2] <- "yes"
b.l.12 <-ggplot(data=hepatitis.live, aes(x=varices, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.12)

HISTOLOGY <- hepatitis.die[["HISTOLOGY"]]
histology <- rep("no", length(HISTOLOGY))
histology[HISTOLOGY== 2] <- "yes"
b.d.13 <-ggplot(data=hepatitis.die, aes(x=histology, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: DIE")
show(b.d.13)

HISTOLOGY <- hepatitis.live[["HISTOLOGY"]]
histology <- rep("no", length(HISTOLOGY))
histology[HISTOLOGY== 2] <- "yes"
b.l.13 <-ggplot(data=hepatitis.live, aes(x=histology, y = ..prop..,group = 1)) + geom_bar(stat="count") +ggtitle("Class: LIVE")
show(b.l.13)
