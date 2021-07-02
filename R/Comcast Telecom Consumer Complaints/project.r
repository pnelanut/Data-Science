#importing packages
library(dplyr)
library(lubridate)
library(ggplot2)
Com_data <- read.csv("./Comcast Telecom Complaints data.csv",header = TRUE)
View(Com_data)
str(Com_data)

#checking if NA present
na_value <- is.na(Com_data)
length(na_value[na_value==TRUE])
head(Com_data$Date)
Com_data$Date_New<- dmy(Com_data$Date)
head(Com_data$Date_New)
#monthly and daily complaint count
Com_data$month = month(Com_data$Date_New)
monthly_count<- summarise(group_by(Com_data,month),Count = n())
daily_count<- summarise(group_by(Com_data,Date_New),Count =n())
monthly_count<-arrange(monthly_count,month)
daily_count
monthly_count
#count distribution - monthly plot.
ggplot(data = monthly_count, aes(x= month, y= Count))+
  geom_line(color="blue")+
  geom_point()+
  geom_text(label= monthly_count$Count, nudge_x = 0.2, nudge_y = 0.3, check_overlap = T)+
  scale_x_continuous(breaks = monthly_count$month)+
  labs(title = "Monthly Trend for the Number of Complaints",x= "Months",y ="No. of Complaints")

#from above plot it is determined that June has the highest number of complaints

#count distribution - daily plot
ggplot(data = daily_count, aes(x= as.POSIXct(Date_New), y= Count))+
  geom_line(color="blue")+
  geom_point()+
  scale_x_datetime(breaks = "1 weeks", date_labels = "%d/%m")+
  theme(axis.text.x = element_text(angle = 75))+
  labs(title = "Daily Trend for the Number of Complaints", x = "Date", y ="No. of Complaints")

#from above graph it is determined that June 21 has the highest number of complaints with 14 and 28 as nearby peaks.

# Complaint Type Processing
network_tickets<- contains(Com_data$Customer.Complaint,match = 'network',ignore.case = T)
internet_tickets<- contains(Com_data$Customer.Complaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(Com_data$Customer.Complaint,match = 'bill',ignore.case = T)
email_tickets<- contains(Com_data$Customer.Complaint,match = 'email',ignore.case = T)
charges_ticket<- contains(Com_data$Customer.Complaint,match = 'charge',ignore.case = T)
Com_data$ComplaintType[internet_tickets]<- "Internet"
Com_data$ComplaintType[network_tickets]<- "Network"
Com_data$ComplaintType[billing_tickets]<- "Billing"
Com_data$ComplaintType[email_tickets]<- "Email"
Com_data$ComplaintType[charges_ticket]<- "Charges"
Com_data$ComplaintType[-c(internet_tickets,network_tickets,
                              billing_tickets,charges_ticket,email_tickets)]<- "Others"
table(Com_data$ComplaintType)
#make a new categorical variable for Complaint Status.
open_complaints<-(Com_data$Status == 'Open' | Com_data$Status == 'Pending')
closed_complaints<-(Com_data$Status == 'Closed' | Com_data$Status == 'Solved')
Com_data$ComplaintStatus[open_complaints]<-'Open'
Com_data$ComplaintStatus[closed_complaints]<-'Closed'
stack<-table(Com_data$ComplaintStatus,Com_data$State)
stack
Com_data<- group_by(Com_data,State,ComplaintStatus)
chart_data<- summarise(Com_data,Count = n())

#Plotting on stacked bar chart
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "darkblue"),
        plot.title = element_text(hjust = 0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets", fill= "Status")

#which state has maximum unresolved complaints
Com_data %>% filter(ComplaintStatus=='Open') %>% group_by(State) %>% summarize(NumOfComplaints=n())
chart_data%>%
  filter(ComplaintStatus == "Open")->
  open_complaints
open_complaints[open_complaints$Count == max(open_complaints$Count),c(1,3)]

#Georgia has the highest number of open complaints

#the percentage of resolved complaints.
resolved_data <- group_by(Com_data,ComplaintStatus)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 
resolved_data <- group_by(Com_data,Received.Via,ComplaintStatus)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 

par(mfrow = c(1,2))
total<-ggplot(total_resloved,
              aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

# Pie Chart for Category wise Ticket Status
category<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(Received.Via,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
total

# from the chart it can be determined that 77% of complaints are closed and 23% are open.

category
#out of closed complaints 39% are taken from customer care calls, 38% are taken from Internet 
#out of open complaints 11% are taken from customer care calls, 12% are taken from Internet 
