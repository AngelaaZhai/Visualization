library(readxl)
library(tidyverse)
library(lubridate)
library(sqldf)
library(zoo)
library(ggimage)
library(sentimentr)

#the main code to calculate sentiment score
visitsnotes_sentiment <- sentiment(Sentiment$VisitNotes) %>%
  group_by(element_id) %>% 
  summarise(sentiment_score = sum(sentiment)) %>%
  inner_join(tibble::rowid_to_column(Visits, "element_id"), by = "element_id")%>%
  select(zkfHiveID, DateVisit, sentiment_score)

## Main function starts from here
Buzz <- function(HiveID) {
  ##Get first visitID for each hive. 
  first_visit <- Visits %>%
    group_by(zkfHiveID) %>%
    summarise(min(zkpVisitID))
  min1 <- as.vector(first_visit$`min(zkpVisitID)`)
  missing_first <- Visits %>%
    filter(zkpVisitID %in% min1) %>%
    filter(is.na(FrameSidesOfBees))
  missing_first <- as.vector(missing_first$zkpVisitID)
  ## Imputation if frame sides is NA for first visit
  Visits$FrameSidesOfBees[Visits$zkpVisitID %in% missing_first] <- 0
  
  ## Imputation if hive condition is NA for first visit
  missing_condition <- Visits %>%
    filter(zkpVisitID %in% min1) %>%
    filter(is.na(HiveCondition))
  missing_condition <- as.vector(missing_condition$zkpVisitID)
  Visits$HiveCondition[Visits$zkpVisitID %in% missing_condition] <- "Unchecked"
  
  ## Data used to plot. Hive is selected from here
  plot <- Visits %>%
    filter(zkfHiveID == HiveID & !is.na(DateVisit)) %>%
    arrange(DateVisit, zkpVisitID) %>%
    mutate(FrameSidesOfBees_num = as.numeric(FrameSidesOfBees),
           FrameSidesOfBees_num = na.locf(FrameSidesOfBees_num),
           month=format(DateVisit, "%m"), year=format(DateVisit,"%Y"),
           TreatmentType = replace(TreatmentType, TreatmentType=="None", NA),
           TreatmentType = replace(TreatmentType, TreatmentType=="0", NA),
           TreatmentType = replace(TreatmentType, TreatmentType=="Treatment Type", NA),
           HiveCondition = replace(HiveCondition, HiveCondition=="Status", "Unchecked"),
           HiveCondition = replace(HiveCondition, HiveCondition=="Hive Condition", "Unchecked"),
           HiveCondition = na.locf(HiveCondition))
  Hivename <- paste0("Hive ", unique(plot$HiveName))
  
  ## Get the max value, used for decide where should we put the emojis and treatment indicators
  maxpoint <- plot %>%
    group_by(zkfHiveID) %>%
    summarise(max(FrameSidesOfBees_num)) %>%
    select(-zkfHiveID) 
  maxpoint <- as.vector(maxpoint$`max(FrameSidesOfBees_num)`)
  
  ## Data used for emojis
  emoji <- visitsnotes_sentiment %>%
    filter(zkfHiveID == HiveID  & !is.na(DateVisit)) %>%
    mutate(month=format(DateVisit, "%m"), year=format(DateVisit,"%Y")) %>%
    group_by(month, year) %>%
    mutate(score = mean(sentiment_score)) %>%
    select(-sentiment_score, -DateVisit) %>%
    distinct()
  emoji$plot <- NA
  emoji <- emoji %>%
    mutate(plot = replace(plot, score<=-0.5, "1f600"),
           plot = replace(plot, score>-0.5 & score<0.5, "1f610"),
           plot = replace(plot, score>=0.5, "1f635"))
  
  # Assuming previous value for NAs in Frame Sides of Bees and Hive Condition
  ggplot() +
    facet_wrap(~year, nrow=1) +
    scale_x_discrete(breaks=c("01","03","05","07","09","11"),
                     labels=c("Jan","Mar","May","Jul","Sep","Nov")) +
    geom_line(data=plot, aes(x=month, y=FrameSidesOfBees_num, group=year),
              size=1) +
    scale_color_manual(values = c("Dead"="#F8766D", "Excellent"="#00BA38", "Good"="#619CFF",
                                  "Troubled"="#F564E3", "Nonchart"="#B79F00",
                                  "Unchecked"="#00BFC4")) +
    labs(title = Hivename,
         subtitle = "Assume Status Unchanged From Previous Visit If NA and Same Population if NA",
         y="Frame Sides", x="Month") +
    geom_point(data=plot, aes(x=month, y=FrameSidesOfBees_num, group=year, color=HiveCondition),
               size=3) +
    geom_point(data=subset(plot, !is.na(TreatmentType)),
               aes(x=month, y=maxpoint+6, group=year),
               size=2.5, color="brown3", stroke=1, fill="white", shape=24) +
    geom_emoji(data=emoji, aes(x=month, y=maxpoint+15, group=year, image=plot), size=.09) +
    theme(text=element_text(size=8), legend.text = element_text(size=9)) +
    guides(color=guide_legend(title="Condition"))
}

## Change Hive ID here
Buzz()



