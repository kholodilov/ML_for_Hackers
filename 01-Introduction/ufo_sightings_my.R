library(ggplot2)
library(scales)

library(plyr)
library(doParallel)

ufo<-read.delim("data/ufo/ufo_awesome.tsv", 
                sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")
names(ufo)<-c("DateOccurred","DateReported","Location",
              "ShortDescription", "Duration","LongDescription")

#head(ufo[which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8),1])
#length(ufo[which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8),1])
good.rows<-ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8, 
                  FALSE, TRUE)
#length(which(!good.rows))
ufo<-ufo[good.rows,]
ufo$DateOccurred<-as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported, format="%Y%m%d")

get.location<-function(l) {
  split.location<-tryCatch(strsplit(l,",")[[1]],
                           error=function(e) return(c(NA,NA)))
  clean.location<-gsub("^ ", "", split.location)
  if (length(clean.location)>2) {
    return(c(NA,NA))
  }
  else {
    return(clean.location)
  }
}

city.state<-lapply(ufo$Location, get.location)
#head(city.state)
location.matrix<-do.call(rbind, city.state)
ufo<-transform(ufo, USCity=location.matrix[,1],
                    USState=tolower(location.matrix[,2]),
                    stringsAsFactors=FALSE)
us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia",
             "id","il", "in","ks","ky","la","ma","md","me","mi","mn","mo",
             "ms","mt","nc","nd","ne","nh", "nj","nm","nv","ny","oh","ok",
             "or","pa","ri","sc","sd","tn","tx","ut","va","vt", "wa","wi",
             "wv","wy")
ufo$USState<-us.states[match(ufo$USState,us.states)]
#head(ufo[is.na(ufo$USState), 3])
ufo$USCity[is.na(ufo$USState)]<-NA
#head(ufo[is.na(ufo$USCity), 3])
ufo.us<-subset(ufo, !is.na(USState) & 
                    DateOccurred>=as.Date("1990-01-01"))


#summary(ufo.us$DateOccurred)
ufo.date.occured.hist<-ggplot(ufo.us, aes(x=DateOccurred)) + 
                       geom_histogram() 
                       #scale_x_date(breaks="50 years")
# print(ufo.date.occured.hist)
# ggsave(plot=ufo.date.occured.hist, 
#        filename="images/ufo_date_occured_hist.png",
#        height=6, width=8)                       

ufo.us$YearMonth<-strftime(ufo.us$DateOccurred, format="%Y-%m")

nodes <- detectCores()
cl <- makeCluster(nodes)
registerDoParallel(cl)
ufo.sightings.counts<-ddply(ufo.us,.(USState, YearMonth), nrow, .parallel=TRUE)
stopCluster(cl)

# put zeroe in counts
ufo.dates.range<-seq.Date(from=min(ufo.us$DateOccurred),
                          to=max(ufo.us$DateOccurred), by="month")
ufo.dates.strings<-strftime(ufo.dates.range, "%Y-%m")
states.dates<-lapply(us.states, 
                     function(state) cbind(state, ufo.dates.strings))
states.dates.df<-data.frame(do.call(rbind, states.dates),
                            stringsAsFactors=FALSE)
all.sightings<-merge(states.dates.df, ufo.sightings.counts,
                     by.x=c("state", "ufo.dates.strings"),
                     by.y=c("USState", "YearMonth"),
                     all=TRUE)

names(all.sightings)<-c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
all.sightings$YearMonth<-as.Date(rep(ufo.dates.range, length(us.states)))
all.sightings$State<-as.factor(toupper(all.sightings$State))

# filter states with few sightings (by maximal value)
max.sightings.by.state<-cbind(by(all.sightings, all.sightings$State,
                                   function(df) max(df$Sightings))) # it is matrix
colnames(max.sightings.by.state)<-c("MaxSightings")
all.sightings.merged<-merge(all.sightings, max.sightings.by.state,
                            by.x=c("State"), by.y=c("row.names"))
all.sightings.filtered<-subset(all.sightings.merged, all.sightings.merged$MaxSightings >= 25)

state.plot <- ggplot(all.sightings.filtered, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format("%Y")) +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")
# print(state.plot)
# ggsave(plot=state.plot, 
#        filename="images/ufo_sightings.pdf",
#        width=14, height=8.5)
