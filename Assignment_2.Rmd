---
title: "Assignment 2"
subtitle: "Deconstruct, Reconstruct Web Report"
author: "Santosh Kumaravel Sundaravadivelu(s3729461)"
output: html_document
urlcolor: blue
---

```{r setup, include=FALSE}
# Do not change these settings!
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

### {.tabset} 

Click the **Original**, **Code** and **Reconstruction** tabs to read about the issues and how they were fixed. 

#### Original

<br>
<center>
<img src="C:/Users/santo/Desktop/Site usage - OS.png" width="80%">
</center>
<center>*Source: Site Wide Analysis of data.gov.au (2013-2020).*</center>
<br>

**Objective**

* The main objective of this visualization is to analyse the number of people visited the data.gov.au site from different Operating Systems across the 2013-2020 period.

* The target audience are the people looking for statistical information of a particular operating systems visit count on the website, developers who are handling the web page (to analyse the visitor's traffic), students who wish to learn about the web traffic of the data.gov.au website.

The visualization chosen had the following three main issues:

* Plot anatomy(Axis label): The graph does not have x-axis and y-axis labels which imply it will be hard to interpret the visualization. For instance, let us take the x-axis, we could assume that it could be a series of continuous numbers or maybe a year. similarly, the y-axis starts from 0 and ends in 100 which could imply either the percentage of visit count or the actual visit count.

* Colour Issues: It is really hard to understand the lower values in the bottom because of the color palette used in the visualzation is not color-blind friendly. Since the visualization is stacked one above the other it is very difficult to differentiate with the colors of different operating systems.

* Area and Size as Quantity(Deception): The chosen visualizations size/area among the operating system is quite misleading because of the stacked nature. For instance, Windows is 40-100(approx) percent if we consider the y-axis as percent, which tends to mislead the audience by making predictions about Windows and its visit count on the website.


**Reference**

* Site Wide Analysis of data.gov.au (2013-2020). *Number of visitors From different Operating Systems (2013-2020)* . Retrieved May 8, 2020, from Australian open data website: https://data.gov.au/site-usage

#### Code

The following code was used to fix the issues identified in the original. 

```{r}
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)

data <- read_csv("D:/Sem 4/DataVis/site-usage.csv")

new_OSdata1 <- separate_(data,col = "Period",into = c("Year", "Month"),sep = "-")

data_OS <- new_OSdata1[ which(new_OSdata1$Statistic=='Operating Systems'),]

new_OSdata2 <- data_OS %>% group_by(Year, Key) %>% summarise(Count=sum(Value))

new_OSdata3 <- new_OSdata2 %>% spread(key = Key, value = Count)
new_OSdata3 <- new_OSdata3 %>% gather(-c("Year","Windows","Macintosh","iOS","Android","Linux"), key = "Others", value = "value")
new_OSdata4 <- subset(new_OSdata3, select = -c(Others))

new_OSdata4 <- new_OSdata4 %>% group_by(Year) %>%   mutate(Others=sum(value,na.rm=TRUE))
new_OSdata4 <- subset(new_OSdata4, select = -c(value))

new_OSdata5 <- distinct(new_OSdata4,.keep_all= TRUE)
final_visdata <- new_OSdata5 %>% gather(c("Macintosh","Windows","iOS","Android","Linux","Others"), key = "Type", value = "Visits")

#Plotting grouped barchat
Visual <-ggplot(final_visdata, aes(Year, Visits)) + geom_bar(aes(fill = Type), 
   width = 0.9, position = position_dodge(width=0.9), stat="identity") + scale_fill_manual(values=c("#56B4E9" ,"#E69F00", "#F0E442","#999999","#D55E00","#0072B2")) + scale_y_continuous(expand=c(0.05,0), labels = scales::comma) +
  
#Titles  
labs(title = "Visit count of different Operating Systems of data.gov website",
y = "Visit Count",x = "Time(Year)") +

#Title theme
theme(plot.title = element_text(face="bold",size = 12, hjust=0.2,family = "calibre"),axis.title=element_text(size=10,face="bold")) +
  
#Panel theme
theme(
      panel.border = element_rect(color = "white", fill = NA),
      panel.grid.major  = element_line(color = "white"),
      axis.text = element_text(color = "black"),
      axis.ticks = element_line(color = "black"))  + 

#Legend theme  
guides(fill=guide_legend(title="Types of OS")) +
theme(
  legend.title = element_text(size = 10, face = "bold"),
  legend.key.width = unit(0.6, "cm"),
  legend.key.height = unit(0.6, "cm"),
  legend.position="right",
  legend.direction="vertical",
  legend.background = element_rect(size=0.6, linetype="solid",colour ="black"))  

```


**Data Reference**

* Site Wide Analysis of data.gov.au (2013-2020). *Number of visitors From different Operating Systems (2013-2020)* . Retrieved May 8, 2020, from Australian open data website: https://data.gov.au/site-usage

#### Reconstruction

The following plot fixes the main issues in the original.

```{r fig.align="center", echo = FALSE}

Visual

```

