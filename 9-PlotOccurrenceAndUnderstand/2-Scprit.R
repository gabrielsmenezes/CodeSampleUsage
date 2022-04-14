library(ggplot2)
library(scales)

typeOfFramework = "Framework"
frameworkName1 = "Android"
frameworkName2 = "AWS"
frameworkName3 = "Azure"
frameworkName4 = "Spring"
frameworkName5 = "Spring Cloud"

factorPositionMedianLabel = 1.7 

plotGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
    #scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + 
    xlab(typeOfFramework) + 
    ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 8) + 
    annotate("text", x = 2.1, y = framework4_median*factorPositionMedianLabel, label = round(framework4_median, 2), size = 8) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=18))  
  return(p1)
}

plotLogGraphic <- function ()  {
  p1 <- ggplot(all, aes) + 
    scale_y_continuous(trans=scales::pseudo_log_trans(base = 10)) +
    #scale_y_log10(labels = comma) +
    geom_violin(width=1, trim=TRUE, fill="#87CEFA") + 
    geom_boxplot(width=0.7,alpha=0.7) + ggtitle(title) + 
    xlab(typeOfFramework) + 
    ylab(verticalTitle) + 
    annotate("text", x = 1.1, y = framework1_median*factorPositionMedianLabel, label = round(framework1_median, 2), size = 8) +
    annotate("text", x = 2.1, y = framework4_median*factorPositionMedianLabel, label = round(framework4_median, 2), size = 8) +
    theme(plot.title=element_text(size=20,face="bold") ,axis.title=element_text(size=18),axis.text=element_text(size=18))  
  return(p1)
}

framework1=read.csv("android.csv", sep=",",header=T)
framework4=read.csv("spring.csv", sep=",",header=T)
all=rbind.data.frame(framework1, framework4)

################################################################################
#Number of duplicated lines
dataFramework1=framework1$total_duplicated_lines
dataFramework4=framework4$total_duplicated_lines

title = "Duplicate Lines"
verticalTitle = "Lines (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), total_duplicated_lines)

plotLogGraphic()
ggsave("duplicated_lines.pdf", width = 4.5, height = 4.5)

################################################################################
#Number of occorrences
dataFramework1=framework1$total_occurrences
dataFramework4=framework4$total_occurrences

title = "Occorrences"
verticalTitle = "Occorrences (log scale)"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), total_occurrences)

plotLogGraphic()
ggsave("occurrences.pdf", width = 4.5, height = 4.5)


################################################################################
#Number of percentage of duplicate lines
dataFramework1=framework1$percentage_duplicate_lines
dataFramework4=framework4$percentage_duplicate_lines

factorPositionMedianLabel = 2

title = "Relative Duplicate Lines"
verticalTitle = "Percentage"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), percentage_duplicate_lines)

plotGraphic()
ggsave("relative-duplicate-lines.pdf", width = 4.5, height = 4.5)
factorPositionMedianLabel = 1.7


################################################################################
#Number of percentage of duplicate lines of code
dataFramework1=framework1$percentage_duplicate_lines_of_code
dataFramework4=framework4$percentage_duplicate_lines_of_code

factorPositionMedianLabel = 2

title = "Relative Duplicate Lines of\nCode"
verticalTitle = "Percentage"
framework1_median =  median(unlist(dataFramework1), na.rm = TRUE)
framework4_median =  median(unlist(dataFramework4), na.rm = TRUE)
aes = aes(factor(framework,levels = c(frameworkName1, frameworkName4)), percentage_duplicate_lines_of_code)

plotGraphic()
ggsave("relative-duplicate-lines_of_code.pdf", width = 4.5, height = 4.5)
factorPositionMedianLabel = 1.7
