######################################################################################################
#Self Instagram EDA with Shiny App
#Author: Yatin Kode
######################################################################################################

############################## Loading required Libraries ############################################
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(data.table)
library(scales)
library(reshape2)
#####################################################################################################

############################# Loading theme for ggplot ##############################################
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#####################################################################################################

##################################### Expolratory Data Analysis #####################################

#---------------------------------- Data about what I uploaded -------------------------------------#

#Loading media json file
media <- fromJSON("media.json", flatten=TRUE)
str(media)

#getting the pics upload info out of media
pics<-media$photos

pics<-separate(pics,taken_at,c("upload_date","upload_time"),sep="T")

pics$type="photo"    #Assigning the photo type to pics needed for further analysis

#Removing Location since it is not included in stories
pics<-pics[,-5]

#getting the videos upload info out of media
videos<-media$videos
videos<-separate(videos,taken_at,c("upload_date","upload_time"),sep="T")

videos$type="video" #Assigning the video type to video needed for further analysis

#Removing Location since it is not included in stories
videos<-videos[,-4]

stories<-media$stories

#Removing data of 2019
stories<-stories[-which(grepl("2019-", stories$taken_at, fixed=TRUE)),]

stories<-separate(stories,taken_at,c("upload_date","upload_time"),sep="T")

stories$type="story"  #Assigning the story type to stories needed for further analysis

#combining pics ,video and stories into sngle dataframe
uploads<-rbind(pics,videos,stories)
nrow(uploads)

table(uploads$type)
#photo story video 
#183    57     7 

sumfreq<-sum(as.data.frame(table(uploads$type))$Freq)

#get monthly uploads
uploads$upload_date<-as.Date(uploads$upload_date,"%Y-%m-%d")

#Uploads per month
uploads$month<-format(uploads$upload_date,"%Y-%m")

#Uploads per year
uploads$Year<-format(uploads$upload_date,"%Y")

#----------------------------------------------------------------------------------------------------#

#----------------------------- Data about my connections --------------------------------------------#

connections <- fromJSON("connections.json", flatten=TRUE)
str(connections)

#followers 

#tidying up the followers data frame
followers<-as.data.frame(connections$followers,header=F)
followers<-gather(followers,follower_id,time_followed , -1:-2)
followers[nrow(followers)+1,3]<-colnames(followers[1])
followers[nrow(followers),4]<-as.character(followers[1,1])
followers[nrow(followers)+1,3]<-colnames(followers[2])
followers[nrow(followers),4]<-as.character(followers[1,2])
followers<-followers[,c(3,4)]

#Removing data to 2019
followers<-followers[-which(grepl("2019-", followers$time_followed, fixed=TRUE)),]

#Convert followed time to date format
followers$time_followed<-as.Date(followers$time_followed,"%Y-%m-%d")


#followers per month
followers$month<-format(followers$time_followed,"%Y-%m")

#followers per year
followers$Year<-format(followers$time_followed,"%Y")


#Following

following<-as.data.frame(connections$following)

#tidying up following data frame to get correct data format
following<-gather(following,following_id,time_following , -1:-2)
following[nrow(following)+1,3]<-colnames(following[1])
following[nrow(following),4]<-as.character(following[1,1])
following[nrow(following)+1,3]<-colnames(following[2])
following[nrow(following),4]<-as.character(following[1,2])
following<-following[,c(3,4)]

#Removing data to 2019
following<-following[-which(grepl("2019-", following$time_following, fixed=TRUE)),]

#Convert following time to date format
following$time_following<-as.Date(following$time_following,"%Y-%m-%d")

#following per month
following$Month<-format(following$time_following,"%Y-%m")


#following per year
following$Year<-format(following$time_following,"%Y")


#------------------------------ Data about what I liked ---------------------------------------------#

likes <- fromJSON("likes.json", flatten=TRUE)
likes_df<-likes$media_likes

likes_df<-as.data.frame(likes_df)

#giving column names to the dataframe
names(likes_df)<-c("like_time","like_uid")

#Removing records of year 2019
likes_df<-likes_df[-which(grepl("2019-", likes_df$like_time, fixed=TRUE)),]

#Getting top 5 userids I liked
most_liked<-head(arrange(aggregate(likes_df$like_time, list(likes_df$like_uid), length),desc(x)),5)
most_liked$Group.1<-as.character(c("U1","U2","U3","U4","U5"))

#separate date and time
likes_df<-separate(likes_df,like_time,into=c("date","time"),sep="T")

#Separate hours out of time to further get hourly usage (likes done)
likes_df<-separate(likes_df,time,into=c("hour"),sep=":",remove=T)
#hourlyseries<-aggregate(likes_df$like_uid, list(likes_df$hour),FUN=length)

#ggplot(hourlyseries,aes(x=as.factor(hourlyseries$Group.1),y=hourlyseries$x))+geom_bar(stat = "identity",fill="steelblue")+theme_bw()+labs(x="Hours in a Day",y="Number of Likes",title = "Usage(Likes per hour)")



likes_df$date<-as.Date(likes_df$date,"%Y-%m-%d")

#Likes per month
likes_df$month<-format(likes_df$date,"%Y-%m")

#Converted to monthly series since it is lot of data to plot in Shiny apps make it unresponsive
monthlyseries<-likes_df[,c(4,3)]

monthlyseries<-aggregate(monthlyseries$like_uid, list(monthlyseries$month),FUN=length)

#Likes per year
likes_df$Year<-format(likes_df$date,"%Y")

yearlyseries<-likes_df[,c(5,3)]

yearlyseries<-aggregate(yearlyseries$like_uid, list(yearlyseries$Year),FUN=length)

############################################## Server plot part #######################################################

server <- function(input, output) {
  
  output$trendPlot <- renderPlotly({
    
    if(input$r1=="Monthly" && input$y=="Followers"){
      p<- ggplot(followers,aes(x=as.factor(followers$month),y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Followers")+
        ggtitle("Followers per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))
      
    }
    else if(input$r1=="Yearly" && input$y=="Followers"){
      
      p<- ggplot(followers,aes(x=as.factor(followers$Year),y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Followers")+
        ggtitle("Followers per Year")
      
    }
    else if(input$r1=="Monthly" && input$y=="Following"){
      
      p<- ggplot(following,aes(x=following$Month,y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Following")+
        ggtitle("Following per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))
      
    }
    else if(input$r1=="Yearly" && input$y=="Following"){
      p<- ggplot(following,aes(x=as.factor(following$Year),y=1))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Following")+
        ggtitle("Following per Year")
      
    }
    else if(input$r1=="Monthly" && input$y=="Likes Given"){
      p<-ggplot(monthlyseries,aes(x=as.factor(monthlyseries$Group.1),y=monthlyseries$x))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Month-Year")+ylab("Number of Likes Given")+
        ggtitle("Likes Given per Month")+theme(axis.text.x=element_text(angle=60, hjust=1))
    }  
    else{
      p<-ggplot(yearlyseries,aes(x=as.factor(yearlyseries$Group.1),y=yearlyseries$x))+
        geom_bar(stat = "identity",fill="steelblue")+xlab("Year")+ylab("Number of Likes Given")+
        ggtitle("Likes Given per Year")
    }
    
    ggplotly(p)
    
  })
  
  
  output$propPlot <- renderPlotly({
    
    if(input$r2=="Monthly" && input$z=="Uploads"){
      q<- ggplot(uploads,aes(x=as.factor(uploads$month),y=1,fill=as.factor(uploads$type)))+
        geom_bar(stat = "identity")+xlab("Month-Year")+ylab("Number of uploads")+
        ggtitle("Uploads per Month")+guides(fill=guide_legend(title="Post Type"))+
        theme(axis.text.x=element_text(angle=60, hjust=1))
    }
    else{
      q<-ggplot(uploads,aes(x=as.factor(uploads$Year),y=1,fill=as.factor(uploads$type)))+
        geom_bar(stat = "identity")+xlab("Year")+ylab("Number of uploads")+
        ggtitle("Uploads per year")+guides(fill=guide_legend(title="Post Type"))
      
    }
    ggplotly(q)
    
    
  })
  
}