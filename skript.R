library(ggplot2)
# data ####
path = "~/Desktop/psy pandemic uni konstanz/corona_alltime.csv"
coro <- read.csv(path, sep=";")

# date
coro$year <- substring(as.character(coro$TIME_PAGE_INTRO),1,4)
coro$month <- substring(as.character(coro$TIME_PAGE_INTRO),5,6)
coro$day <- substring(as.character(coro$TIME_PAGE_INTRO),7,8)
coro$hour <- substring(as.character(coro$TIME_PAGE_INTRO),9,10)
coro$minute <- substring(as.character(coro$TIME_PAGE_INTRO),11,12)
coro$second <- substring(as.character(coro$TIME_PAGE_INTRO),13,14)
coro$date <- paste(coro$year,"-",coro$month,"-",coro$day,sep="")
coro$date <- as.Date(coro$date)
coro$datetime <- paste(coro$date," ",coro$hour,":",coro$minute,":",
                       coro$second,sep="")
coro$datetime <- as.POSIXct(coro$datetime, format = "%Y-%m-%d %H:%M:%S")

# quality ####
dim(coro)
na_count <- sapply(coro, function(y) sum(length(which(is.na(y)))))
count99 <- sapply(coro, function(y) sum(length(which(y==-99))))
count9 <- sapply(coro, function(y) sum(length(which(y==-9))))
count11 <- sapply(coro, function(y) sum(length(which(y==-11))))
count8 <- sapply(coro, function(y) sum(length(which(y==-8))))
count7 <- sapply(coro, function(y) sum(length(which(y==-7))))
countNA <- sapply(coro, function(y) sum(length(which(y==NA))))

myvar <- c('DIAGNOSED_SELF_SUFFERED_ANXIETY', 'ILC018', 'SATLIFE', 
           'VIRUSOPINION_LONELY', 'VIRUSOPINION_DEPRESSED', 
           'VIRUSOPINION_LIFE',
           'HHNRMEMB', 'HOUSEHOLDROOMS', 'HH_SELFISOLATION', 'COUNTRYR_API',
           'HH_PETS', 'TIME_PAGE_INTRO', 'NROFJOB1', 'JOBCORONA', 'HHCHILD',
           'CHLDHOME', 'HHGCHILD', 'CHLDHOMEVIRUS', 'VIRUSOPINION_CHLDHOME',
           'JOBCORONA_HOMEOFFICE')
na_count[myvar]
count99[myvar]

# visualizing the potential DVs ####
# DIAGNOSED_SELF_SUFFERED_ANXIETY
table(coro$DIAGNOSED_SELF_SUFFERED_ANXIETY)

# ILC018
table(coro$ILC018)
p1 <- ggplot(coro[coro$ILC018 > 0,], aes(x=ILC018)) +
  geom_histogram() +
  geom_vline(xintercept = mean(coro$ILC018[coro$ILC018 > 0],na.rm=TRUE),
             color = "red")
ggsave("/Users/jonasmiosga/Desktop/psy pandemic uni konstanz/ILC018.png", p1)

# satlife
table(coro$SATLIFE)
p2 <- ggplot(coro[coro$SATLIFE > 0,], aes(x=SATLIFE)) + 
  geom_histogram()+
  geom_vline(xintercept = mean(coro$SATLIFE[coro$SATLIFE > 0],na.rm=TRUE),
             color = "red")
ggsave("/Users/jonasmiosga/Desktop/psy pandemic uni konstanz/SATLIFE.png", p2)

# VIRUSOPINION_LONELY
table(coro$VIRUSOPINION_LONELY)
p3 <- ggplot(coro[coro$VIRUSOPINION_LONELY > 0,], aes(x=VIRUSOPINION_LONELY))+
  geom_histogram()+
  geom_vline(xintercept = 
               mean(coro$VIRUSOPINION_LONELY[coro$VIRUSOPINION_LONELY > 0]
                    ,na.rm=TRUE),
             color = "red")
ggsave("/Users/jonasmiosga/Desktop/psy pandemic uni konstanz/
       VIRUSOPINION_LONELY.png", p3)

# VIRUSOPINION_DEPRESSED
table(coro$VIRUSOPINION_DEPRESSED)
p4 <- ggplot(coro[coro$VIRUSOPINION_DEPRESSED > 0,], 
             aes(x=VIRUSOPINION_DEPRESSED)) + 
  geom_histogram()+
  geom_vline(xintercept = 
               mean(coro$VIRUSOPINION_DEPRESSED[coro$VIRUSOPINION_DEPRESSED > 0],
                    na.rm=TRUE),
             color = "red")
ggsave("/Users/jonasmiosga/Desktop/psy pandemic uni konstanz/
       VIRUSOPINION_DEPRESSED.png", p4)

# VIRUSOPINION_LIFE
table(coro$VIRUSOPINION_LIFE)
p5 <- ggplot(coro[coro$VIRUSOPINION_LIFE > 0,], aes(x=VIRUSOPINION_LIFE))+
  geom_histogram()+
  geom_vline(xintercept = 
               mean(coro$VIRUSOPINION_LIFE[coro$VIRUSOPINION_LIFE > 0],
                    na.rm=TRUE),
             color = "red")
ggsave("/Users/jonasmiosga/Desktop/psy pandemic uni konstanz/
       VIRUSOPINION_LIFE.png", p5)

# HHNRMEMB
table(coro$HHNRMEMB)
p6 <- ggplot(coro[coro$HHNRMEMB > 0,], aes(x=HHNRMEMB)) + 
  geom_histogram()+
  geom_vline(xintercept = mean(coro$HHNRMEMB[coro$HHNRMEMB > 0],na.rm=TRUE),
             color = "red")
ggsave("/Users/jonasmiosga/Desktop/psy pandemic uni konstanz/HHNRMEMB.png",
       p6)

# HOUSEHOLDROOMS
table(coro$HOUSEHOLDROOMS)
p7 <- ggplot(coro[coro$HOUSEHOLDROOMS > 0,], aes(x=HOUSEHOLDROOMS)) + 
  geom_histogram()+
  geom_vline(xintercept = mean(coro$HOUSEHOLDROOMS[coro$HOUSEHOLDROOMS > 0],
                               na.rm=TRUE),
             color = "red")
ggsave("/Users/jonasmiosga/Desktop/psy pandemic uni konstanz/
       HOUSEHOLDROOMS.png", p7)

# HH_SELFISOLATION
table(coro$HH_SELFISOLATION)

# COUNTRYR_API
table(coro$COUNTRYR_API)

# Analysis ####
## replace all negatives with NA
for (i in myvar) {
  coro[,i] <- replace(coro[,i], which(coro[,i] < 0), NA)
}

# hypotheses
h11 <- lm(SATLIFE ~ HHNRMEMB + HOUSEHOLDROOMS + HH_SELFISOLATION + 
            COUNTRYR_API, data = coro)
h12 <- lm(SATLIFE ~ HHNRMEMB + HOUSEHOLDROOMS + HH_SELFISOLATION + 
            COUNTRYR_API + HH_PETS, data = coro)
h13

h21 <- aov(NROFJOB1 ~ DIAGNOSED_SELF_SUFFERED_ANXIETY + ILC018 + SATLIFE +
             VIRUSOPINION_LONELY + VIRUSOPINION_DEPRESSED + 
             VIRUSOPINION_LIFE, data=coro[coro$JOBCORONA == 1,])
h22 <- aov(NROFJOB1 ~ DIAGNOSED_SELF_SUFFERED_ANXIETY + ILC018 + SATLIFE +
             VIRUSOPINION_LONELY + VIRUSOPINION_DEPRESSED + 
             VIRUSOPINION_LIFE + JOBCORONA_HOMEOFFICE,
           data=coro[coro$JOBCORONA == 1,])
h23

# visuals
h13_plot <- ggplot(coro, aes(x = datetime, y = SATLIFE)) +
    geom_line() +
    scale_x_datetime(date_labels = "%d", date_breaks = "7 day") +
    theme_classic()
h13_trend <- ggplot(coro, aes(x = datetime, y = SATLIFE)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, span = 0.6) +
  theme_classic()


