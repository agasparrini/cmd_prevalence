rm(list=ls())  
setwd("/Users/agasparrini/Documents/Projects/cmd prevalence/")

# load packages
library(curatedMetagenomicData)
library(ggplot2)
library(dplyr)
library(reshape2)
library(pheatmap)
library(ggrepel)
library(plotly)

# download all stool pathway abundances 

stool_pathabun <- curatedMetagenomicData("*pathabundance_relab.stool*", dryrun = FALSE)
stool_pathabun_merged <- mergeData(stool_pathabun)
stool_pathabun_exprs <- exprs(stool_pathabun_merged)
stool_pathabun_metadata <- pData(stool_pathabun_merged)

stool_pathabun_exprs <- stool_pathabun_exprs[!grepl("UNMAPPED",rownames(stool_pathabun_exprs)),]
stool_pathabun_exprs <- stool_pathabun_exprs[!grepl("UNINTEGRATED",rownames(stool_pathabun_exprs)),]


# metadata exploration
View(combined_metadata)

ggplot(combined_metadata, aes(reorder(study_condition, study_condition, function(x)-length(x)))) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(size=16), axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("study condition")

# get control stools
control_metadata <- filter(combined_metadata, study_condition=="control" && body_site =="stool")

# number of unique control individuals
length(table(control_metadata$subjectID))

# get first occurrence of each control individual
first_control_metadata <- control_metadata[match(unique(control_metadata$subjectID), control_metadata$subjectID),]

ggplot(first_control_metadata, aes(age)) +
  geom_histogram() +
  theme_classic() +
  theme(text = element_text(size=16))

ggplot(first_control_metadata, aes(age_category)) +
  geom_bar() +
  theme_classic() +
  theme(text = element_text(size=16))

ggplot(first_control_metadata, aes(reorder(country, country, function(x)-length(x)))) + 
  geom_bar() +
  theme_classic() +
  theme(text = element_text(size=16), axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Country")

ggplot(first_control_metadata, aes(reorder(body_site, body_site, function(x)-length(x)))) +
  geom_bar() + theme_classic( )

# get western adults 

filtered_metadata <- filter(first_control_metadata, 