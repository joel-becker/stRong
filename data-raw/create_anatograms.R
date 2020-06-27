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

    c[[1]][c(grep("M", c[[1]] )+1,grep("M", c[[1]] )+2)] <- NA

    c[[1]] <- c[[1]][grep("[[:alpha:]]", c[[1]], invert=TRUE)]

    anatCoord <- as.data.frame(lapply( c, function(u)
        matrix(as.numeric(unlist(strsplit(u, ","))),ncol=2,byrow=TRUE) ))
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
