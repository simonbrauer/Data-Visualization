## ---- message = FALSE, warning = FALSE-----------------------------------
setwd("C:/Users/Simon/OneDrive/Documents/Github/Data-Visualization/")
library(foreign)
library(ggplot2)
library(GGally)
NCS <- read.dta("NCS.dta")
attach(NCS)

## ---- warning = FALSE----------------------------------------------------
head(NCS)

## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
###First, I graph the discrete variables with histograms
discrete.description <- c("Catholic parishes make up the majority of the NCS data, followed by Baptists. No other group has more than 500 congregations represented.",
                          "Clergy are overwhelmingly white in the NCS.",
                          "Nearly half of individuals are in a congregation that allows homosexual members.",
                          "Most individuals are in a congregation that restricts homosexuals from becoming leaders.",
                          "Most individuals are in congregations that are conservative or moderate. Relatively few are in liberal congregations.")

discrete.title <- c("Congregation's Religious Tradition", "Clergy's Race", "Congregation Allows Homosexual Members", "Congregation Allows Homosexual Leaders", "Congregation's Political Position")

## ---- warning = FALSE----------------------------------------------------
counter <- 1
angle1 <- c(15, 15, 0, 0, 15)
horz <- c(1, 1, 0.5, 0.5, 1)
for(i in list(DENCODE3, clerrace, mbrgay, ldrgay, libcon)){
  print(discrete.title[counter], quote = FALSE)
  print(discrete.description[counter], quote = FALSE)
  print(summary(i), quote = FALSE)
  
  temp.fig <- ggplot(data = NCS) + 
    geom_histogram(aes(x = i)) +
    ggtitle(discrete.title[counter]) + 
    ylab("") +
    xlab("") +
    theme(axis.text.x = element_text(angle = angle1[counter], hjust = horz[counter]))
  print(temp.fig)
  counter <- counter + 1
}

## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
###Next, I graph the continuous variables with density plots
continuous.description <- c("The average atendee has 776 adults that regularly attend their congregation. The median attendee has 250, indicating that the distribution is highly right-skewed.",
                            "The average attendee is in a congregation that brings in just under $1,000,000 a year, though the median attendee is in a congregation that brings in $300,000",
                            "While the average attendee's congregation is 1.4% made up of homosexual members, the median is 0, indicating that most attendees have no homosexual members in their congregation, or at least that the respondent did not know of or chose not to report any.")

continuous.title <- c("Logged number of regularly attending adults", "Logged total income", "Logged percentage attendees that are homosexual")

continuous.label <- c("ln(adults)", "ln(dollars)", "ln(percentage)")

## ---- warning = FALSE----------------------------------------------------
counter <- 1
for(i in list(numadlts, income, gaypct)){
  print(continuous.title[counter], quote = FALSE)
  print(continuous.description[counter], quote = FALSE)
  print(summary(i), quote = FALSE)
  
  temp.fig <- ggplot(data = NCS) + 
    geom_density(aes(x = log(i))) +
    ggtitle(continuous.title[counter]) + 
    ylab("") +
    xlab(continuous.label[counter])
  print(temp.fig)
  counter <- counter + 1
}

## ---- warning = FALSE----------------------------------------------------
NCS.log <- NCS
NCS.log$numadlts <- log(NCS$numadlts)
NCS.log$income <- log(NCS$income)
NCS.log$gaypct <- log(NCS$gaypct)

ggpairs(NCS.log, 
        upper = list(continuous = "smooth", combo = "box", discrete = "facetbar"),
        lower = list(continuous = "blank", combo = "blank", discrete = "blank"))

## ---- echo = FALSE, warning = FALSE--------------------------------------
ggplot(data = NCS.log) + 
  geom_density(aes(x = gaypct)) + 
  facet_wrap(~DENCODE3) +
  ggtitle("Logged percentage homosexual attendees across eleven religious traditions") +
  ylab("") +
  xlab("Ln(percent homosexual attendees)")

ggplot(data = NCS.log) + 
  geom_boxplot(aes(x = DENCODE3, y = gaypct)) + 
  coord_flip() +
  ggtitle("Logged percentage homosexual attendees across eleven religious traditions") +
  ylab("ln(percent homosexual attendees)") +
  xlab("")

