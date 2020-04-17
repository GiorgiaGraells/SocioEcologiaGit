# PLS Path Modeling with R

library(plspm)
library(readr)
library(tidyverse)

# VALORIZACION #########################
Val <- read_csv("RespEnc_Val.csv")
Val <- Val %>%  dplyr::filter(!is.na(Ingreso_percap)) %>% dplyr::filter(!is.na(Beneficio_amb))

## Inner model matrix
foot_pathV <- read_csv("Path_matrix.csv")
Rownames <- foot_pathV$X1
foot_pathV <- foot_pathV %>% dplyr::select(-X1)
row.names(foot_pathV)<- Rownames
foot_pathV <- as.matrix(foot_pathV)

# plot the path matrix
innerplot(foot_pathV)

## Outer model list

# define list of indicators: what variables are associated with
# what latent variables

foot_blocksV <- list(c(4,5), 22, c(13, 20), 18, 16 )

# all latent variables are measured in a reflective way
foot_modesV = c("A", "A", "A", "A", "A")

# run plspm analysis

example_scaling = list(c("NUM", "NUM"),
                       c("NUM"),
                       c("NUM",  "NUM"),
                       c("NUM"),
                       c("NUM"))


#foot_plsV <- plspm(Val, foot_pathV, foot_blocksV, foot_modesV, scaling = example_scaling)
foot_plsV <- plspm(Val, foot_pathV, foot_blocksV, foot_modesV, scaling = example_scaling, boot.val = T, br = 1000)

###########################
# path coefficients       #
foot_plsV$path_coefs      #
                          #
# inner model             #
foot_plsV$inner_model     #
                          #
# summarized results      #
summary(foot_plsV)        #
                          #
###########################

# plotting results (inner model)
plot(foot_plsV)

# plotting loadings of the outer model
plot(foot_plsV, what = "loadings", arr.width = 0.1)
plot(foot_plsV, what = "weights")


######chequeando
# unidimensionality
foot_plsV$unidim

# loadings and communalities
foot_plsV$outer_model

# cross-loadings
foot_plsV$crossloadings

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(foot_plsV$crossloadings, id.vars = c("name", "block"),variable_name = "LV")

# bar-charts of crossloadings by block
ggplot(data = xloads,aes(x = name, y = value, fill = block)) +
# add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
# indicate the use of car-charts
  geom_bar(stat = 'identity', position = 'dodge') +
# panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
# tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),line = element_blank(),plot.title = element_text(size = 12)) +
# add titlegg
  title("Crossloadings")+ xlab("Nombre variables") +ylab("Valor de crossloading")


# BIENESTAR #########################
Bien <- read_csv("RespEnc_Bien.csv")
Bien <- Bien %>%  dplyr::filter(!is.na(Ingreso_percap))

## Inner model matrix
foot_pathB <- read_csv("Path_matrixB.csv")
Rownames <- foot_pathB$X1
foot_pathB <- foot_pathB %>% dplyr::select(-X1)
row.names(foot_pathB)<- Rownames
foot_pathB <- as.matrix(foot_pathB)

# plot the path matrix
innerplot(foot_pathB)

## Outer model list

# define list of indicators: what variables are associated with
# what latent variables
foot_blocksB <- list(c(4,5), 12,21, 13)

# all latent variables are measured in a reflective way
foot_modesB = c("A", "A", "B", "A")

# run plspm analysis

example_scaling = list(
                       c("NUM", "NUM"),
                       c("NUM"),
                       c("NUM"),
                       c("NUM"))

foot_plsB <- plspm(Bien, foot_pathB, foot_blocksB, foot_modesB, scaling = example_scaling,boot.val = T, br = 1000)

###########################
# path coefficients       #
foot_plsB$path_coefs      #
#
# inner model             #
foot_plsB$inner_model     #
#
# summarized results      #
summary(foot_plsB)        #
#
###########################

# plotting results (inner model)
plot(foot_plsB)

# plotting loadings of the outer model
plot(foot_plsB, what = "loadings", arr.width = 0.1)

######chequeando variables reflectivas
# unidimensionality
foot_plsB$unidim

# loadings and communalities
foot_plsB$outer_model

# cross-loadings
foot_plsB$crossloadings

#chequeando variables formativas
# select R2
foot_plsB$inner_summary[, "R2", drop = FALSE]

# gof index
foot_plsB$gof
