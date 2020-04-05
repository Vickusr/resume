#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(plotly)


profile_summary <- read_csv('profile_summary.csv')

get_heading_with_items <- function(df,heading_nbr){
    temp <- df %>% filter(ListHeadingNbr == heading_nbr)
}

build_line_item <- function(timeline, role, category, show_label = T){
    
    temp <- timeline %>% filter(Role == role, Category == category) %>% arrange(desc(ListHeadingNbr),ListItem)
    
    if(show_label == T){
        build_list <- list(timelineLabel(paste(temp$Role[1],sep='')))
    }else{
        build_list <- list()
    }
    
    
    total_headings <- unique(temp$ListHeadingNbr)
    
    
    k <- 2
    heading <- 2
    for(heading in total_headings){
        
        temp_heading_w_listitems <- get_heading_with_items(temp,heading_nbr = heading)
        #print(temp_heading_w_listitems$Content[1])
        
        if(nrow(temp_heading_w_listitems)>1){
            temp_listitems <- temp_heading_w_listitems %>% filter(ListItem != 0)
            temp_listitems <- tags$ol(
                map(temp_listitems$Content,tags$li)
            )
        }else{
            temp_listitems <- NULL
        }
        
        build_list[[k]] <- timelineItem(
            title = temp_heading_w_listitems$Content[1],
            icon = temp_heading_w_listitems$icon[1],
            temp_listitems
        )
        
        
        k <- k +1
        
        
    }
    #print(build_list)
    build_list
}



# Define UI for application that draws a histogram
ui <- dashboardPagePlus(title = 'Vickus Botha CV',skin = 'blue',
                        header = dashboardHeaderPlus(titleWidth = '400px',
                                                     title = '',disable = FALSE
                        ),
                        sidebar = dashboardSidebar(width = '400px', collapsed = T,disable = T,
                                                   sidebarMenu(
                                                       menuItem('Resume',tabName = 'resume', icon = icon('home'))
                                                   )), 
                        body = dashboardBody(
                            tabItems(
                                tabItem('resume',
                                        fluidRow(
                                            widgetUserBox(
                                                title = "Vickus Botha",
                                                subtitle = "Industrial Engineer",
                                                width = 6,
                                                type = 2,
                                                src = "profile2.jpg",
                                                color = "blue",
                                                profile_summary,
                                                footer = tagList(
                                                    tags$p( icon('birthday-cake')," : ",'1990-11-16 | 29 years old'),
                                                    tags$p( icon('at')," : ",'vickus.r.botha@gmail.com'),
                                                    tags$p(icon('phone-square')," : ",'(+27) 072 211 7017'),
                                                    tags$p(icon('home')," : ",'Agulhas Close, Loevenstein, Cape Town, 7530'),
                                                    socialButton(url = 'https://github.com/Vickusr',type = 'github'),
                                                    socialButton(url = 'https://www.linkedin.com/in/vickus-r-botha/',type = 'linkedin')
                                                    
                                                    
                                                )
                                            ),
                                            tabBox(title = '',
                                                   tabPanel(title = 'Skills',
                                                            plotlyOutput('skills')),
                                                   tabPanel(title = "Courses",
                                                            tags$ul('Udemy Courses :',
                                                                    tags$li(tags$a('Intro to Computer Science',href='CS.jpg')),
                                                                    tags$li(tags$a('R Data Science',href='R Data Science.jpg')),
                                                                    tags$li(tags$a('Database Design & Modeling',href='Database Modeling.jpg'))
                                                                    
                                                            )
                                                   ),
                                                   tabPanel(title = 'Projects',
                                                            tags$ul('R Personal Projects :',
                                                                    tags$li(tags$a('Mapify',href=''))
                                                                    
                                                                    
                                                            ),
                                                            tags$ul('R Freelance Projects :',
                                                                    tags$li(tags$a('Finland Map',href='')),
                                                                    tags$li(tags$a('Football Game',href=''))
                                                                    
                                                            ),
                                                            tags$ul('3D Printing Projects :',
                                                                    tags$li(tags$a('Radio Frequency Scanners',href='')),
                                                                    tags$li(tags$a('Mining Equipment Models',href='')),
                                                                    tags$li(tags$a('VESA mounts for schools',href=''))
                                                                    
                                                                    
                                                            )
                                                   ),
                                                   tabPanel(title = 'Hobbies & Interest',
                                                            dashboardLabel("3D printing", status = "primary"),
                                                            dashboardLabel("Programming", status = "primary"),
                                                            dashboardLabel("Gaming", status = "primary"),
                                                            dashboardLabel("Reading ", status = "primary")
                                                            
                                                            
                                                   )
                                                   
                                            ),
                                            box(
                                                title = "Employment History",
                                                status = "info",
                                                width = 12,
                                                
                                                uiOutput('employment_hist')
                                                # htmlOutput()
                                                
                                                
                                                
                                                
                                                
                                            ))
                                )
                            )
                            
                        )
                        
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    
    skills <- read_csv('skills.csv')
    
    employment_history <- read_csv('employment_history.csv')
    # employment_history <- read_csv('cv/employment_history.csv')
    
    employment_history <- employment_history %>% mutate(
        Role_Start_Date = paste(str_sub(Role_Start_Date,1,4),str_sub(Role_Start_Date,6,7),sep='-'),
        Role_End_Date = paste(str_sub(Role_End_Date,1,4),str_sub(Role_End_Date,6,7),sep='-')
        #Role_Start_Date = as.Date(Role_Start_Date,'%Y-%m')
        
    )
    
    roles <- unique(employment_history$Role)
    role <- roles[1]
    
    categories <- unique(employment_history$Category)
    cat <- categories[1]
    
    
    
    output$skills <- renderPlotly({
        
        p <- ggplot(skills, aes(Skills,Rating,fill=Skills)) + geom_col()
        ggplotly(p + coord_flip())
    })
    
    
    
    output$employment_hist <- renderUI({
        
        unique(employment_history$Role)
        unique(employment_history$Category)
        
        temp1 <- build_line_item(employment_history, "Candidate Engineer",'notable work')
        temp2 <- build_line_item(employment_history, "Reliability Engineer",'day to day',show_label = F)
        temp3 <- build_line_item(employment_history, "Reliability Engineer",'notable work',show_label = T)
        temp4 <- build_line_item(employment_history, "WMS Process Engineer",'day to day',show_label = F)
        temp5 <- build_line_item(employment_history, "WMS Process Engineer",'notable work',show_label = T)
        temp6 <- build_line_item(employment_history, "Supply Chain Analyst",'notable work',show_label = T)
        
        
        timelineBlock(
            timelineEnd(),
            temp6,
            temp5,
            temp4,
            temp3,
            temp2,
            temp1,
            
            timelineStart()
        )
        # temp <- 
        #temp[1]
        
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
