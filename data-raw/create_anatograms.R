#----------------------------------------------------------------------------------#
# Creates anatograms
# Date: 06/27/2020
# Author: Joel Becker

# Notes:
#   Much code adapted from:
#   https://github.com/jespermaag/gganatogram/blob/master/data-raw/createAnatograms.R
#----------------------------------------------------------------------------------#


########################################################
######################## Set-up ########################
########################################################

# load libraries
packages <- c("dplyr", "ggplot2", "ggpolypath")
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)


########################################################
#################### List functions ####################
########################################################

extractCoords <- function(coords, name, transMatrix) {
    c <- strsplit(coords, " ")
    c[[1]]
    c[[1]][c(grep("M", c[[1]]) + 1, grep("M", c[[1]]) + 2)] <- NA
    c[[1]] <- c[[1]][grep("[[:alpha:]]", c[[1]], invert=TRUE)]

    anatCoord <- as.data.frame(
      lapply(
        c,
        function(u) matrix(
          as.numeric(unlist(strsplit(u, ","))),
          ncol=2,
          byrow=TRUE
        )
      )
    )
    anatCoord$X2[is.na(anatCoord$X1)] <- NA
    anatCoord$X1[is.na(anatCoord$X2)] <- NA
    anatCoord$id <- name

    if (length(transMatrix[grep('matrix', transMatrix)])>0) {
        transForm <- gsub('matrix\\(|\\)', '', transMatrix)
        transForm <- as.numeric(strsplit(transForm, ",")[[1]])

        anatCoord$x <-  (anatCoord$X1* transForm[1]) + (anatCoord$X1* transForm[3]) + transForm[5]
        anatCoord$y <-  (anatCoord$X2* transForm[2]) + (anatCoord$X2* transForm[4]) + transForm[6]
    } else if (grep('translate', transMatrix)) {
        transForm <- gsub('translate\\(|\\)', '', transMatrix)
        transForm <- as.numeric(strsplit(transForm, ",")[[1]])
         if(name =='leukocyte' & transForm[1]==103.63591) {
            transForm <- c(103.63591+4.5230265,-47.577078+11.586659)
        }
        anatCoord$x <-  anatCoord$X1 + transForm[1]
        anatCoord$y <-  anatCoord$X2 + transForm[2]
    }
    #anatCoord <- anatCoord[complete.cases(anatCoord),]
    if (name == 'bronchus') {
        if (min(anatCoord$y, na.rm=T) <25 ) {
            anatCoord$x <- NA
            anatCoord$y <- NA
        }
    }
    if( any(anatCoord[complete.cases(anatCoord),]$x < -5)) {
            anatCoord$x <- NA
            anatCoord$y <- NA
    }

    if( any(anatCoord[complete.cases(anatCoord),]$x > 150)) {
            anatCoord$x <- NA
            anatCoord$y <- NA
    }

    lastVal <- 0
    anatCoord$group <- 1
    for (j in 1:length(which(is.na(anatCoord$y)))) {
        curVal <- which(is.na(anatCoord$y))[j]
        anatCoord[c(lastVal:curVal),]$group <- paste0(i, '_', j)
        if (j < length(which(is.na(anatCoord$y)))) {
            lastVal <- curVal + 1
        } else if (j == length(which(is.na(anatCoord$y))) ) {
            anatCoord[c(curVal:length(anatCoord$y)),]$group <- paste0(i, '_',j+1)
        }
    }

    return(anatCoord)
}

####
#Male Human
####
hsMale <- read.table('homo_sapiens.male_coords.tsv', sep='\t', stringsAsFactors=F)
hgMale_list <- list()
for (i in 1:nrow(hsMale)) {
    df <- extractCoords(hsMale$V2[i], hsMale$V1[i],  hsMale$V3[i])

    hgMale_list[[i]] <- extractCoords(hsMale$V2[i], hsMale$V1[i],  hsMale$V3[i])
    hgMale_list[[i]]$id <- gsub(' ', '_', hgMale_list[[i]]$id)
    if ( (unique(hgMale_list[[i]]$id) == 'leukocyte') & (min(hgMale_list[[i]]$x, na.rm=T)<15) ) {
        hgMale_list[[i]]$x <- hgMale_list[[i]]$x-4.5230265
        hgMale_list[[i]]$y <- hgMale_list[[i]]$y -11.586659
    }
    names(hgMale_list)[i] <-  paste0(hsMale$V1[i],'-', i)
}
library(DescTools)
plot(hgMale_list[['LAYER_OUTLINE-384']]$x, hgMale_list[['LAYER_OUTLINE-384']]$y)
testis1 <- DrawEllipse(x=59.421902+ -9.6858637, y=168.88368+ -66.040746, radius.x=1, radius.y=2.6421267,  col='red')
hgMale_list[['testis-1']] <- data.frame(X1 = testis1$x, X2 = testis1$y, id='testis', x = testis1$x, y = testis1$y, group ='testis1', stringsAsFactors = FALSE)
testis2 <- DrawEllipse(x=66.046127+ -9.6858637, y=168.88368 + -66.040746, radius.x=1, radius.y=2.6421267,  col='red')
hgMale_list[['testis-2']] <- data.frame(X1 = testis2$x, X2 = testis2$y, id='testis', x = testis2$x, y = testis2$y, group ='testis2', stringsAsFactors = FALSE)

prostate <- DrawEllipse(x=52.737728, y= 93.289169, radius.x=3.3939276, radius.y=2.6712799,  col='red')
hgMale_list[['prostate-1']] <- data.frame(X1 = prostate$x, X2 = prostate$y, id='prostate', x = prostate$x, y = prostate$y, group ='prostate', stringsAsFactors = FALSE)

breast1 <- DrawEllipse(x=43.5, y=47, radius.x=6, radius.y=5, col='red')
breast1 <- data.frame(x = breast1$x, y = breast1$y)
hgMale_list[['breast-1']] <- data.frame(X1 = breast1$x, X2 = breast1$y, id='breast', x = breast1$x, y = breast1$y, group = 'breast1', stringsAsFactors = F)

breast2 <- DrawEllipse(x=62, y=47, radius.x=6, radius.y=5, col='red')
breast2 <- data.frame(x = breast2$x, y = breast2$y)
hgMale_list[['breast-2']] <- data.frame(X1 = breast2$x, X2 = breast2$y, id='breast', x = breast2$x, y = breast2$y, group = 'breast2', stringsAsFactors = F)

plot(hgMale_list[['LAYER_OUTLINE-384']]$x, hgMale_list[['LAYER_OUTLINE-384']]$y, cex=0.3)
putGland <- DrawEllipse(x=53, y=10, radius.x=0.8, radius.y=1.6, col='red')

hgMale_list[['pituitary gland-6']] <- data.frame(X1 = putGland$x, X2 = putGland$y, id='pituitary_gland', x = putGland$x, y = putGland$y, group ='pituitary_gland', stringsAsFactors = FALSE)
#lines(hgMale_list[['pituitary gland-6']]$X1, hgMale_list[['pituitary gland-6']]$X2)
names(hgMale_list) <- gsub('-.*', '', names(hgMale_list))
names(hgMale_list) <- gsub(' ', '_', names(hgMale_list) )




allAnatomy <- read.table('allOrgans.tsv', sep='\t', stringsAsFactors=F)
organColour <- data.frame(type = c('circulation', 'nerve', 'digestion', 'respiratory','other', 'reproductive'),
                            colour = c('red', 'purple', 'orange', 'steelblue',  '#41ab5d', '#d95f02'), stringsAsFactors=F)
allAnatomy$colour <- organColour[match(allAnatomy$V2, organColour$type),]$colour
colnames(allAnatomy) <- c('organ', 'type', 'colour')
allAnatomy[allAnatomy$type=='nerve',]$type <- 'nervous_system'
allAnatomy$organ <- gsub(' ', '_', allAnatomy$organ)
#allAnatomy <- allAnatomy[! allAnatomy$organ %in% c('amygdala','pituitary gland'),]
#allAnatomy <- allAnatomy[!allAnatomy$type %in% 'other',]
hgMale_key <- allAnatomy
hgMale_key$value <- runif(nrow(hgMale_key), 0, 20)

#gganatogram(data=allAnatomy, fillOutline='#a6bddb', organism='human', sex='male', fill="colour") +facet_wrap(~type, ncol=3) +theme_classic()
