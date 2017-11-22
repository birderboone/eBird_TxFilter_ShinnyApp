#Shiny Presence and Filter
library(shiny)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
###################################
presence<-read.csv('C:/Users/birde/Dropbox/ebird/eBird_TX_all.csv')
sub.t<-read.csv('C:/Users/birde/Dropbox/ebird/eBird_TX_filters_small.csv')
dfsample<-read.csv('C:/Users/birde/Dropbox/ebird/eBird_TX_sample.csv')
dfsample$county<-tolower(dfsample$county)
f.dates<-read.csv('C:/Users/birde/Dropbox/ebird/ebird_dates.csv')
map.county <- map_data('county')
map.county<-subset(map.county,region=='texas')
all.species<-as.character(sort(unique(presence$Species[presence$year.sum>=100])))
all.counties<-sort(unique(presence$county))
cdate<-as.character(f.dates$cdate)
ui<-shinyUI(navbarPage("Texas eBird",
                       
                       tabPanel("Presence",fluidPage(

                         wellPanel(
                           fluidRow(

                             column(4,
                                    sliderInput(inputId='maxsum',
                                                label='Maximum sightings',
                                                value=50,min=1,max=1000)),
                             column(4, selectInput(inputId='q.species',label='Species', choices=all.species))
                           )
                           ,
                           ###I need to built this slider manually
                           fluidRow(
                             column(4,
                                    sliderInput(inputId='max.p.sum',
                                                label='Maximum presence',
                                                value=.1,min=0,max=1)),
                             column(4,
                                    dateRangeInput("daterange1", "Date range:",
                                                   start  = paste0(as.POSIXlt(Sys.Date())$year+1900,"-01-01"),
                                                   end    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-12-31"),
                                                   min    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-01-01"),
                                                   max    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-12-31"),
                                                   format = "mm/dd",
                                                   separator = " to ")
                             )
                           )
                         ),
                         wellPanel(
                           plotOutput('triPlot'))
                       )),
                       
                       


####### county
          tabPanel('County Level',fluidPage(
            wellPanel(
            fluidRow(
              
              column(4,
                     sliderInput(inputId='smoother',
                                 label='Smoothing (0-1)',
                                 value=.4,min=0,max=1)),
              
              column(4,selectInput(inputId='county',label='County', choices=all.counties))),
            fluidRow(
              column(4,
                     sliderInput(inputId='p.limit',
                                 label='Presence limitation',
                                 value=.001,min=.0001,max=.1)),
              column(4,selectInput(inputId='q.species1',label='Species', choices=all.species)
              ),
              column(4,textOutput('samplesize'))
            ))
            ,
            wellPanel(
            plotOutput('timeGraph')
          ))),
####### table
    tabPanel('County Data',fluidPage(
      wellPanel(
        fluidRow(
          
          column(4,
        dateRangeInput("daterange4", "Date range:",
                       start  = paste0(as.POSIXlt(Sys.Date())$year+1900,"-01-01"),
                       end    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-12-31"),
                       min    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-01-01"),
                       max    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-12-31"),
                       format = "mm/dd",
                       separator = " to ")),
        column(4,
               selectInput(inputId='county3',label='County', choices=all.counties)),
        column(4,textOutput('listsize'))),
        dataTableOutput('table')
      ))),
###Texas summary
tabPanel('Birds v Filter',fluidPage(
  wellPanel(
    ###I need to built this slider manually
    fluidRow(
      column(4,
             dateRangeInput("daterange3", "Date range:",
                            start  = paste0(as.POSIXlt(Sys.Date())$year+1900,"-01-01"),
                            end    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-12-31"),
                            min    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-01-01"),
                            max    = paste0(as.POSIXlt(Sys.Date())$year+1900,"-12-31"),
                            format = "mm/dd",
                            separator = " to ")
      )
    )
  ),
  wellPanel(
    plotOutput('triPlot1'))
          )),
###Texas county Image
tabPanel('Texas County Map',fluidPage(
  img(src='https://upload.wikimedia.org/wikipedia/commons/5/5a/Texas_counties_map.png')
))
    )                                 
)         


server<-function(input, output){
  output$triPlot <- renderPlot({
    #q.species<-'Black-crested Titmouse'
    #max.sum<-50
    #max.p.sum<-.1
    #x1<-100
    #y1<-300
    #cdate<-paste0('X',rep(1:12,1,each=4),'.',1:4)
    q.species<-input$q.species
    max.sum<-input$maxsum
    max.p.sum<-input$max.p.sum
    x1<-as.POSIXlt(input$daterange1[1])$yday
    y1<-as.POSIXlt(input$daterange1[2])$yday
    if(y1<x1){y1<-x1}
    filter1<-f.dates$julian.start<=y1 & f.dates$julian.start>=x1
    presence$county<-tolower(presence$county)
    sub.p<-subset(presence, Species==q.species)
    df.nsample<- merge(sub.p[,c('county'),drop=F],dfsample,by='county',all.x=T)
    df.numbers<-round(sub.p[,cdate]    * df.nsample[,cdate])
    sub.p$year.sum<-apply(df.numbers[,filter1], 1, sum)
    sub.p$mean.pre<-apply(sub.p[,as.character(f.dates$cdate[filter1])], 1, mean)
    sub.p$county<-tolower(sub.p$county)
    map.county1<-merge(map.county, sub.p[,c('year.sum','county','mean.pre')],by.x='subregion',by.y='county',all.x=T)
    map.county1<-map.county1[order(map.county1$order,map.county1$group),]
    map.county1$year.sum[is.na(map.county1$year.sum)]<-0
    map.county1$year.sum[map.county1$year.sum>max.sum]<-max.sum
    map.county1$year.sum[map.county1$year.sum==0]<-NA
    
    a<-ggplot() + geom_polygon(data = map.county1, aes(x=long, y = lat, group =group,fill=year.sum),color='black') + coord_fixed(1.3) +ggtitle('Sightings on eBird') + scale_fill_continuous(name = "Total")
    #+guide_colorbar(title = 'Total')
    
    map.county1$mean.pre[is.na(map.county1$mean.pre)]<-0
    map.county1$mean.pre[map.county1$mean.pre>max.p.sum]<-max.p.sum
    map.county1$mean.pre[map.county1$mean.pre==0]<-NA
    
    b<-ggplot() + geom_polygon(data = map.county1, aes(x=long, y = lat, group =group,fill=mean.pre),color='black') + coord_fixed(1.3) +ggtitle('Presence on eBird')+ scale_fill_continuous(name = "Mean")
    
    #allspecies<-data.frame(species=sort(unique(sub.t$taxon)))
    sub.s<-sub.t[sub.t$taxon==q.species,]
    sub.s$COUNTY<-tolower(sub.s$COUNTY)
    
    
    {df.onfilter<-data.frame(county=tolower(unique(sub.s$COUNTY)), onfilter=as.numeric(sapply(unique(sub.s$COUNTY), function(x){
      sub.df<-subset(sub.s, COUNTY==x)
      fsum<-sum(sub.df$count[sub.df$dayOfYear<=y1 & sub.df$dayOfYear>=x1])
      if(is.na(fsum)| fsum==0){fsum<-F}else {fsum<-T}
      fsum
    })))
      
      df.onfilter<-df.onfilter[df.onfilter$onfilter==T,]
      map.county3<-merge(map.county, df.onfilter,by.x='subregion',by.y='county',all.x=T)
      map.county3<-map.county3[order(map.county3$order,map.county3$group),]
      c<-ggplot() + geom_polygon(data = map.county3, aes(x=long, y = lat, group =group,fill=onfilter),color='black') + coord_fixed(1.3) + theme(legend.position="none")+ ggtitle('Present on filter')}
    
    grid.arrange(a,b,c,nrow=1) 
    
  })
  output$triPlot1 <- renderPlot({
    #q.species<-'Black-crested Titmouse'
    #max.sum<-50
    #max.p.sum<-.1
    #x1<-100
    #y1<-190
    #cdate<-paste0('X',rep(1:12,1,each=4),'.',1:4)
    #maxcheck<-1000
    q.species<-input$q.species
    max.sum<-input$maxsum
    max.p.sum<-input$max.p.sum
    x1<-as.POSIXlt(input$daterange3[1])$yday
    y1<-as.POSIXlt(input$daterange3[2])$yday
    maxcheck<-input$maxcheck
    #if(x1>y1){y1<-as.POSIXlt(input$daterange3[1])$yday; x1<-as.POSIXlt(input$daterange3[2])$yday}
    filter1<-f.dates$julian.start<=y1 & f.dates$julian.start>=x1
    presence$county<-tolower(presence$county)
    sub.p<-presence[apply(presence[,cdate[filter1]],1,sum)>0,]
    df.nsample<- merge(sub.p[,c('county'),drop=F],dfsample,by='county',all.x=T)
    df.numbers<-round(sub.p[,cdate]    * df.nsample[,cdate])
    
    s.df<-data.frame(speciesD=tapply(sub.p$Species, sub.p$county,length), county=tapply(sub.p$county, sub.p$county,function(x)as.character(x[1])))
    
    p.df<-data.frame(totalCheck=apply(dfsample[,cdate][,filter1],1,sum), county=dfsample$county)
    
    sp.df<-merge(s.df,p.df,by='county')
    #sub.s<-sub.t
    sub.t$COUNTY<-tolower(sub.t$COUNTY)
    
    dd<-tapply(sub.t$dayOfYear,list(sub.t$COUNTY,sub.t$taxon) ,function(x){length(x[x<=y1 & x>=x1])>0})
    df.onfilter<-data.frame(county=row.names(dd),onfilter=apply(dd,1,sum,na.rm=T))
    # df.onfilter<-data.frame(county=tolower(unique(sub.s$COUNTY)), onfilter=as.numeric(sapply(unique(sub.s$COUNTY), function(x){
    #   #x<-'comal'
    #   sub.df<-subset(sub.s, COUNTY==x)
    #   length(unique(sub.df$taxon[sub.df$dayOfYear<=y1 & sub.df$dayOfYear>=x1]))
    # })))
    
    df1<-merge(sp.df, df.onfilter,by='county') 
    df1$ratio<-df1$speciesD/df1$onfilter
    map.county1<-merge(map.county, df1,by.x='subregion',by.y='county',all.x=T)
    ###plot time
    map.county1<-map.county1[order(map.county1$order,map.county1$group),]
    myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
    map.county1$totalCheck[map.county1$totalCheck>maxcheck]<-maxcheck
    ## filter numbers
    ###find mins and maxs
    fillmin<-min(c(map.county1$speciesD, map.county1$onfilter))
    fillmax<-max(c(map.county1$speciesD, map.county1$onfilter))
    bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
    re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
    ##plot
    a<-ggplot() + geom_polygon(data = map.county1, aes(x=long, y = lat, group =group,fill=speciesD),color='black') + coord_fixed(1.3) +ggtitle('Species seen on eBird') + scale_fill_gradientn(colours = myPalette(10),name='Total',lim=c(fillmin, fillmax))
    #b<-ggplot() + geom_polygon(data = map.county1, aes(x=long, y = lat, group =group,fill=totalCheck),color='black') + coord_fixed(1.3) +ggtitle('Total Checklists on eBird') + scale_fill_continuous(name = "Total")
    c<-ggplot() + geom_polygon(data = map.county1, aes(x=long, y = lat, group =group,fill=onfilter),color='black') + coord_fixed(1.3) +ggtitle('Species on Filter') + scale_fill_gradientn(colours = myPalette(10),name='Total', lim=c(fillmin, fillmax))
    d<-ggplot() + geom_polygon(data = map.county1, aes(x=long, y = lat, group =group,fill=ratio),color='black') + coord_fixed(1.3) +ggtitle('Ratio of birds seen/on filter') + scale_fill_gradientn(colours=c(bl,"white", re), na.value = "grey98",limits = c(0, 2)) 
    grid.arrange(a,c,d,nrow=1) 
  })
  output$timeGraph <- renderPlot({
    # q.species<-'American Kestrel'
    # county<-'Comal'
    # smoother<-0.6
    # p.limit<-0.001
    #cdate<-paste0('X',rep(1:12,1,each=4),'.',1:4)
    q.species<-input$q.species1
    smoother<-input$smoother
    p.limit<-input$p.limit
    county<-input$county
    presence$county<-tolower(presence$county)
    sub.p<-presence[presence$county==tolower(county) & presence$Species==q.species,]
    output$samplesize<-renderText({paste0('Total Sightings: ',ifelse(is.na(sub.p$year.sum),0,sub.p$year.sum))})
    if(nrow(sub.p)==0){sub.p1<-data.frame(matrix(rep(0,ncol(sub.p)),ncol=ncol(sub.p)))
    colnames(sub.p1)<-colnames(sub.p)
    sub.p<-sub.p1}
    #
    sub.p$mean.pre<-apply(sub.p[,cdate], 1, mean)
    species<-sub.p[,cdate]
    pmodel<-smooth.spline(t(species),spar=smoother)
    #unlist(species)
    pxmodel<-smooth.spline(data.frame(y=c(unlist(species[1,25:48]),unlist(species[1:24]))),spar=.6)
    pxdata<-data.frame(y1=predict(pxmodel,1:48)$y, realdate=c(25:48,1:24))
    pdata<-data.frame(y0=predict(pmodel,1:48)$y, realdate=c(1:48))
    new.pmodel<-merge(pdata,pxdata, 'realdate')
    new.pmodel$weights<-c(10000,1000,100,10,rep(0,40),10,100,1000,10000)
    new.pmodel$y2<-apply(new.pmodel,1,function(x)weighted.mean(c(x[2],x[3]),w=c(1,x[4])))
    new.pmodel$y2[new.pmodel$y2<0]<-0
    maxp<-max(new.pmodel$y2)
    if(maxp==0){maxp<-.5}
    red.df<-new.pmodel[new.pmodel$y2<p.limit,c('realdate')]
    poly.list<-lapply(red.df, function(x){
      x0<-c(x-.5)
      x1<-x+.5
      
      data.frame(x=c(x0,x1,x1,x0),y=c(0,0,maxp,maxp),t=as.character(x1))}
    )
    d<-do.call(rbind, poly.list)
    if(length(d)==0){d<-data.frame(x=c(0,0,0,0),y=c(0,0,0,0),t=as.character(1))}
    ggplot(new.pmodel, aes(x=realdate,y=y2)) + geom_point()+geom_polygon(data=d, mapping=aes(x=x, y=y,group=t,fill='red'),alpha=.2)+
      ggtitle(q.species)+scale_x_continuous(name='Date',breaks=1:48,labels=f.dates$name) +
      theme_bw()+theme(axis.title.x=element_text(size=17),axis.title.y=element_text(size=17),title=element_text(size=17),axis.text.x = element_text(angle = 60,vjust=.5,size=12))+
      ylab('Smoothed Presence')+ guides(fill=FALSE) + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    
    
    
  })
  output$table <- renderDataTable({
    # q.species<-'American Kestrel'
    # county<-'Comal'
    # smoother<-0.6
    # p.limit<-0.001
    #cdate<-paste0('X',rep(1:12,1,each=4),'.',1:4)
    # x1<-100
    # y1<-190
    county<-input$county3
    x1<-as.POSIXlt(input$daterange4[1])$yday
    y1<-as.POSIXlt(input$daterange4[2])$yday
    if(y1<x1){y1<-x1}
    filter1<-f.dates$julian.start<=y1 & f.dates$julian.start>=x1
    presence$county<-tolower(presence$county)
    sub.c<-presence[presence$county==tolower(county),]
    df.nsample<-c(t(dfsample[dfsample$county==tolower(county),colnames(dfsample)!='county']))
  
    listsize1<-sum(df.nsample[filter1])
    output$listsize<-renderText({paste0('Total Checklists: ',ifelse(is.na(listsize1),0,listsize1))})
    df.numbers<-round(sub.c[,cdate]    * df.nsample)
    sub.c$year.sum<-apply(df.numbers[,filter1], 1, sum)
    
    sub.c$mean.pre<-apply(sub.c[,as.character(f.dates$cdate[filter1])], 1, mean)
    sub.c$max.pre<-apply(sub.c[,as.character(f.dates$cdate[filter1])], 1, max)
    
    sub.ct<-subset(sub.t, tolower(COUNTY)==tolower(county))
    sub.cf<-data.frame(species=unique(c(as.character(sub.c$Species), as.character(sub.ct$taxon))))
    sub.cf<-merge(sub.cf, sub.c[,c('Species','year.sum', 'max.pre','mean.pre')],by.x='species',by.y='Species',all.x=T)
    sub.cf$filter<-sub.cf$species%in%unique(sub.ct$taxon)
    for(i in 2:ncol(sub.cf[,])){sub.cf[is.na(sub.cf[,i]),i]<-0}
    sub.cf<-sub.cf[order(-sub.cf$filter,sub.cf$year.sum),]
    sub.cf$filter<-ifelse(sub.cf$filter==1,'yes','no')
    colnames(sub.cf)<-c('Species','Total Sightings','Max Presence','Average Presence','On Filter')
    return(sub.cf)
  })

}

shinyApp(ui=ui,server=server)
