library(highcharter)

bs.table = data.frame(
  Closing.Date = paste(2013:2017, 12, sep = "/"),
  Non.Current.Assets = c(13637344, 14075507, 14578093, 10911628, 10680998),
  Current.Assets = c(13078654, 12772388, 14226181, 10205708, 10950779),
  Non.Current.Liabilities = c(9376243, 8895126, 9715914, 9810157, 13493110),
  Current.Liabilities = c(5075985, 4963856, 5992229, 8859263, 4094183)
)



highchart() %>% 
  hc_chart(type = "column") %>%
  hc_plotOptions(column = list(stacking = "normal"))  %>%
  hc_add_series(data = mtcars,
                type = 'column',
                hcaes(x = rownames(mtcars), y = `gear`,group= `gear`,color=`gear`)) %>%
  hc_xAxis(categories = rownames(mtcars)) 


indata <- read.csv("~/Desktop/filereader/indata.csv")
unique(indata$`Subject.ID`)

library(dplyr)
indata<-indata %>%
  group_by(indata$`Subject.ID`) %>%
  mutate(kcount = n())
indata$kcount

colnames(indata)
indata$`Subject.ID`

hchart(indata, "column", hcaes(x =indata$`Subject.ID`, y = indata$`Site`,group=indata$`Sample.Status`,color=indata$`Sample.Status`))%>%
  hc_add_theme(hc_theme_ft())
library(plotly)
plot_ly(indata, x = ~indata$`Subject.ID`, y = ~indata$`Site`, group=~indata$`Sample.Status`,color =~indata$`Sample.Status` , type = 'bar')%>% layout(title = "Number of encounters",
                                                                                                                                                     barmode = 'group',
                                                                                                                                                     xaxis = list(title = "Months"),  ## Order & label the x-axis
                                                                                                                                                     yaxis = list(title = "Number of encounters"))
# I've tried to created a function using `JS`:

library(dplyr)
indata<-indata %>%
  group_by(indata$`Subject.ID`) %>%
  mutate(kcount = n())

# I've tried to created a function using `JS`:

easeOutBounce  <- JS("function (pos) {
  if ((pos) < (1 / 2.75)) {
    return (7.5625 * pos * pos);
  }
  if (pos < (2 / 2.75)) {
    return (7.5625 * (pos -= (1.5 / 2.75)) * pos + 0.75);
  }
  if (pos < (2.5 / 2.75)) {
    return (7.5625 * (pos -= (2.25 / 2.75)) * pos + 0.9375);
  }
  return (7.5625 * (pos -= (2.625 / 2.75)) * pos + 0.984375);
}")
highchart() %>% 
  hc_title(text = "Patient Status",
           style = list(fontSize = "15px", fontWeight = "bold")) %>% 
  hc_subtitle(text = "Frequency of Occurences") %>% 
  hc_chart(type = "column") %>%
  hc_plotOptions(column = list(stacking = "normal"))  %>%
  hc_add_series(data = indata,
                animatio = list(
                  duration = 1000,
                  easing = easeOutBounce
                ),
                type = 'column',
                hcaes(x = Subject.ID, y = kcount,group= Sample.Status,fill=Sample.Status)
) %>%
  hc_xAxis(title = list(text = "Subjects"),categories = indata$Subject.ID,
           tickmarkPlacement = "on",
           plotLines = list(
             list(label = list(
               rotation = 90))
           ))%>%
  hc_yAxis(title = list(text = "Frequency"),offset = 10) %>%
  # hc_tooltip(pointFormat = "<b> Freq: </b> {point.kcount}",shared = F)%>%
  hc_add_theme(hc_theme_google())%>%
  hc_legend(reversed = TRUE)%>%hc_exporting(
    enabled = TRUE, # always enabled
    filename = "Intellia_Patient_Status"
  )
# %>%hc_motion(enabled = TRUE)

highchart() %>% 
  hc_title(text = "Title") %>% 
  hc_subtitle(text = "Subtitle") %>% 
  hc_chart(type = "column", polar = F) %>%
  hc_plotOptions(column = list(dataLabels = list(enabled = F),stacking = "normal")) %>%
  hc_xAxis(categories = unique(indata$`Sample.Status`) )%>%
  hc_add_series(name="Sample Status",type = 'column',
                data = indata$`Sample.Status`
                ,stack = "4") %>%
  hc_add_series(name="Site",type = 'column',
                data = indata$`Visit.Name`
                ,stack = "4") %>%
  hc_add_theme(hc_theme_ft())      
# hc <- hchart(dfr(), type="bar
highchart() %>% 
  hc_chart(type = "column") %>%
  hc_plotOptions(column = list(stacking = "normal"))  %>%
  hc_add_series(data = indata,
                type = 'bar',
                hcaes(x =indata$`Subject.ID`, y = indata$`Sample.Status`,group= indata$`Sample.Status`,color=indata$`Sample.Status`)) %>%
  hc_xAxis(categories = indata$`Subject.ID`) 


plot(indata$Subject.ID,indata$Site)

t(mtcars)

%>%
  hc_xAxis(categories = bs.table$Closing.Date) %>%
  hc_add_series(name="Non Current Assets",
                data = bs.table$Non.Current.Assets,
                stack = "Assets") %>%
  hc_add_series(name="Current Assets",
                data = bs.table$Current.Assets,
                stack = "Assets") %>%
  hc_add_series(name="Non Current Liabilities",
                data = bs.table$Non.Current.Liabilities,
                stack = "Liabilities") %>%
  hc_add_series(name="Current Liabilities",
                data = bs.table$Current.Liabilities,
                stack = "Liabilities") %>%
  hc_add_theme(hc_theme_ft())

highchart() %>%
  hc_chart(type = "column") %>%
  hc_plotOptions(column = list(stacking = "normal")) %>%
  hc_xAxis(categories = bs.table$Closing.Date) %>%
  hc_add_series(name="Non Current Assets",
                data = bs.table$Non.Current.Assets,
                stack = "Assets") %>%
  hc_add_series(name="Current Assets",
                data = bs.table$Current.Assets,
                stack = "Assets") %>%
  hc_add_series(name="Non Current Liabilities",
                data = bs.table$Non.Current.Liabilities,
                stack = "Liabilities") %>%
  hc_add_series(name="Current Liabilities",
                data = bs.table$Current.Liabilities,
                stack = "Liabilities") %>%
  hc_add_theme(hc_theme_ft())
