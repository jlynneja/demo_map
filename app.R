library(shiny)
library(leaflet)
library(tigris)
library(sf)
library(htmltools)
library(tidyverse)
library(shinyWidgets)
library(leaflet.extras)
library(tmaptools)
library(shinyalert)


#Load data and merge ACS data w/ shapefile

acs_tracts1 <- read_csv("data/acs_tracts1.csv")
bus_geo <- read_csv("data/bus_geo.csv")
#have to force geo ID to character to allow for joining
acs_tracts1 <- acs_tracts1 %>%
  mutate(geo_id_ct = as.character(geo_id_ct),
         fips = as.character(fips))

#read in spatial data
shapefile <- st_read("data/shapefile.shp")

#merge spatial data with ACS
map <- geo_join(shapefile, acs_tracts1, "GEOID", "fips")
map <- map %>%
  st_transform('+proj=longlat +datum=WGS84')




#set color palette for choropleth using median income
pal <- colorNumeric(
  palette = "YlGnBu",
  domain = map$hhincome,
  na.color = NA)

#set color palette for choropleth using total pop.
pop_pal <- colorNumeric(
  palette = "YlGnBu",
  domain = map$total_pop,
  na.color = NA)

#set color palette for choropleth using number of households
house_pal <- colorNumeric(
  palette = "YlGnBu",
  domain = map$households,
  na.color = NA)

#Popup for clicking on a census tract polygon
popup1 <- ifelse(is.na(map$hhincome),
                 paste0("<span class='popup'><strong>Census Tract</strong>: <span style='font-weight:normal;'>",
                        map$geo_id_ct, "</span><strong><br>",map$muni_nei,"</strong><br><br>",
                        "[No Census Data Available.]"),
                 paste0("<span class='muni'>",map$muni_nei,
                        "</span><br><span class='popup'><strong>Census Tract</strong>: <span style='font-weight:normal;'>",
                        map$geo_id_ct, "</span></span><br>",
                        "<br><strong>Population: </strong>",
                        prettyNum(map$total_pop, trim=T, big.mark=","),
                        "<br><strong>Households: </strong>",
                        prettyNum(map$households, trim=T, big.mark=","),
                        "<br><strong>Median Income: </strong>$",
                        prettyNum(map$hhincome, trim=T, big.mark=",")))


#icons for map markers
red_icons <- makeIcon(iconUrl="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
                     shadowUrl="https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
                     iconWidth=25, iconHeight=41,
                     shadowWidth=41, shadowHeight=41)


green_icon <- makeIcon(iconUrl="https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
                      shadowUrl="https://cdnjs.cloudflare.com/ajax/libs/leaflet/0.7.7/images/marker-shadow.png",
                      iconWidth=40, iconHeight=56,
                      shadowWidth=50, shadowHeight=50)




####USER INTERFACE####

ui <- bootstrapPage(
  
  #Head tags, call stylesheet
  tags$style(type = "text/css", "html, body {width:100%;height:100%;}"),
  tags$head( 
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    tags$link(rel = "shortcut icon", href = "icons/favicon.ico"),
    tags$title("Demographics Near You")
  ),
  
  #Main title, set as header
  titlePanel(
    h1("Demographics Near You", class="center")
  ),
  
  #subtitle
  h4("Where Are Your Customers? Who Are Your Neighbors?", class="center"),
  br(),
  #header banner
  p("Use", span("demographics", class="bold"), "& other key information to", 
    span("target your marketing", class="bold"), "and",
    span("inform your business decisions.", class="bold"),
    class="headerStrip"), #end p()
  br(),

 
  
  #Show map, fit to screen
  leafletOutput("map", width="100%", height="100%"),
  
  #Absolute panel for input selections
  absolutePanel(id="mainpanel",
                class="mainpanel",
                bottom=15, left=25,
                width="65%", height="auto",
                draggable=T,
                
                #Use fluidRow to divide into columns
                fluidRow(class="panel",
                         #Intro/instruction text for inputs
                         p(class="panelIntro",
                           "Select your characteristics of interest and then 
                             click on the map to view info by census tract."),
                         
                         #Make tabs to keep panel more organized, less cluttered
                         tabsetPanel(
                           #First demographic tab
                           tabPanel("Demographics", class="padded",
                                    #First column with dropdown selections
                                    column(3,
                                           p(class="show"),
                                           
                                           span(strong("See the percentage of the population by...")),
                                           br(),br(),
                                           
                                           #input to view/not view percent male/female
                                           prettyCheckbox(
                                             inputId = "sex",
                                             label = "Sex/Gender", 
                                             value = FALSE,
                                             status = "primary",
                                             outline = TRUE), #end prettyCheckbox
                                           
                                           #input for age ranges to view
                                           pickerInput("age",
                                                       label = "",
                                                       choices = colnames(sf::st_drop_geometry(map[,18:29])),
                                                       multiple = T,
                                                       options = list(`max-options` = 3,
                                                                      title="Age Group(s)")), #end pickerInput
                                  
                                           
                                           #input for education levels to view
                                           pickerInput("edu",
                                                       label = "",
                                                       choices = colnames(sf::st_drop_geometry(map[,42:47])),
                                                       multiple = T,
                                                       options = list(`max-options` = 3,
                                                                      title = "Education Level(s)")), #end pickerInput
                                           
                                           #input for race to view
                                           pickerInput("race",
                                                       label = "",
                                                       choices = colnames(sf::st_drop_geometry(map[,30:35])),
                                                       multiple = T,
                                                       options = list(`max-options` = 3,
                                                                      title = "Race(s)")) #end pickerInput
                                           
                                           
                                    ), #end column
                                    
                                    #second column with marker options
                                    column(5,
                                           p(class="show"),
                                           
                                           #search for an address on the map
                                           searchInput(
                                             inputId = "address",
                                             label = "Find your business on the map:", 
                                             placeholder = "Enter full address for best results",
                                             btnSearch = icon("search"),
                                             width = "100%"), #end searchInput
                                           
                                           br(),
                                           
                                           #show/don't show black-owned business markers
                                           prettyCheckbox(
                                             inputId = "bus",
                                             label = "Show Black-Owned Businesses", 
                                             value = FALSE,
                                             status = "success",
                                             outline = TRUE) #end prettyCheckbox
                                           
                                           ), #end column
                                    
                                    #third column with more options
                                    column(4,
                                           p(class="show"),
                                           #choose choropleth variable
                                           pickerInput("color_map",
                                                       label = "Color map based on:",
                                                       choices = c("Median Income",
                                                                   "Total Population",
                                                                   "Total Households"),
                                                       selected = "Median Income",
                                                       multiple = F), #end selectInput
                                           br(),
                                           
                                           #switch to toggle legend on/off
                                           span(class = "bold",
                                           materialSwitch(inputId = "legend",
                                                       label = "Show Legend",
                                                       value = T,
                                                       right = T,
                                                       status = "primary") #end materialSwitch
                                           ), #end span
                                           br(),
                                           br(),
                                           
                                           #action button to view "how to use this app" info
                                           actionBttn(inputId = "help",
                                                      label = tags$strong("Help Using This App"),
                                                      style = "gradient",
                                                      size = "sm",
                                                      color = "primary")
                                           
                                    ) #end column
                                    
                                    
                           ), #end tabPanel
                           
                           #Second tab with household characteristics info
                           tabPanel("Households & Employment", class="padded", 
                                    
                                    #First column with dropdown selections
                                    column(4,
                                           br(),
                                           #checkbox group to display employent info
                                           prettyCheckboxGroup(inputId = "employ",
                                                               label = "Display employment information:",
                                                               choices = c("Percent employed",
                                                                           "Percent unemployed"),
                                                               status = "primary",
                                                               inline = TRUE,
                                                               outline = TRUE), #end checkbox group
                                           prettyCheckbox(inputId = "lf",
                                                          label = "Show number in the labor force",
                                                          value = FALSE,
                                                          status = "success",
                                                          outline = TRUE), #end checkbox
                                           br(),
                                           
                                           #housing tenure
                                           prettyCheckboxGroup(inputId = "tenure",
                                                               label = "Display percent of housing units that are:",
                                                               choices = c("Owner-occupied",
                                                                           "Renter-occupied"),
                                                               status = "primary",
                                                               inline = TRUE,
                                                               outline = TRUE), #end checkbox group
                                           
                                           #Show vehicle information
                                           pickerInput("vehicles",
                                                       label = "",
                                                       choices = colnames(sf::st_drop_geometry(map[,58:60])),
                                                       multiple = T,
                                                       options = list(title="Vehicle Ownership (% of housholds with...)")), #end pickerInput
                                           
                                           #show transit info
                                           pickerInput("transit",
                                                       label = "",
                                                       choices = colnames(sf::st_drop_geometry(map[,55:57])),
                                                       multiple = T,
                                                       options = list(title="Transportation (% who get to work by...)")), #end pickerInput
                                           
                                    ), #end column
                                    
                                    #second column with marker options
                                    column(4,
                                           p(class="show"),
                                           useShinyalert(),
                                           
                                           #search for an address on the map
                                           searchInput(
                                             inputId = "address2",
                                             label = "Find your business on the map:", 
                                             placeholder = "Enter full address for best results",
                                             btnSearch = icon("search"),
                                             width = "100%"), #end searchInput
                                           
                                           br(),
                                           
                                           #show/don't show black-owned business markers
                                           prettyCheckbox(
                                             inputId = "bus2",
                                             label = "Show Black-Owned Businesses", 
                                             value = FALSE,
                                             status = "success",
                                             outline = TRUE) #end prettyCheckbox
                                           
                                    ), #end column
                                    
                                    #third column with more options
                                    column(4,
                                           p(class="show"),
                                           #choose choropleth variable
                                           pickerInput("color_map2",
                                                       label = "Color map based on:",
                                                       choices = c("Median Income",
                                                                   "Total Population",
                                                                   "Total Households"),
                                                       selected = "Median Income",
                                                       multiple = F), #end selectInput
                                           br(),
                                           
                                           #switch to toggle legend on/off
                                           span(class = "bold",
                                                materialSwitch(inputId = "legend2",
                                                               label = "Show Legend",
                                                               value = T,
                                                               right = T,
                                                               status = "primary") #end materialSwitch
                                           ), #end span
                                           br(),
                                           br(),
                                           
                                           #action button to view "how to use this app" info
                                           actionBttn(inputId = "help2",
                                                      label = tags$strong("Help Using This App"),
                                                      style = "gradient",
                                                      size = "sm",
                                                      color = "primary")
                                           
                                    ) #end column
                           ), #end tabPanel
                           
                           #Third tab with information about how this info is helpful
                           tabPanel("How Is This Useful?", class="padded",
                                    
                                    column(12,
                                           br(),
                                           p(class = "alertHead",
                                             "Potential Ways to Use This Information"),
                                           
                                           p(class="show",
                                             "Information about the people who live nearby is a 
                                             powerful tool for any small business owner. It can be used to
                                             better inform business decisions and
                                             to better target potential customers."),
                                          br(),
                                           span(class="tab3Text",
                                                "The US Census Bureau collects this
                                                information regularly, but it can be overly
                                                complicated and difficult to access.
                                                Some marketing firms charge a lot of money to
                                                make sense of this data for you."),
                                          span(class="tab3Bold",
                                                "The purpose of this app is to support Black
                                                business owners in Allegheny County by offering a free
                                                tool that makes Census data more accessible."),
                                          br(),
                                    
                                          #Buttons to show specific details
                                          br(), br(),br(), 
                                          
                                        
                                              actionBttn(
                                              inputId = "about",
                                              label = tags$strong("About the Data"),
                                              style = "minimal",
                                              icon = icon("chart-bar"),
                                              size = "sm",
                                              color = "primary"
                                            ), #end button
                                          
                                            actionBttn(
                                              inputId = "use_demo",
                                              label = tags$strong("Using Demographics"),
                                              style = "minimal",
                                              icon = icon("users"),
                                              size="sm",
                                              color = "success"
                                            ), #end button
                                          
                                            actionBttn(
                                              inputId = "use_house",
                                              label = tags$strong("Using Household/Employment Info"),
                                              style = "minimal",
                                              icon = icon("home"),
                                              size="sm",
                                              color = "success"
                                            ), #end button
                                          
                                            actionBttn(
                                              inputId = "resources",
                                              label = tags$strong("More Resources"),
                                              style = "minimal",
                                              icon = icon("link"),
                                              size="sm",
                                              color = "primary"
                                            ), #end button
                                          br(),br(),p("")
                                          
                                    ) #end column
                           ) # end tabPanel
                         ) #end tabSetPanel
                ) #end fluidRow
  ), #end absolutePanel
  
  #panel on the right side to display output
  absolutePanel(id = "panel",
                class = "panel",
                bottom="15", right="25",
                width="25%", height="auto",
                
                br(),
                span(#muni/neighborhood name
                     htmlOutput("click_name")) , #end span
                
                span(htmlOutput("click_tract"),
                     htmlOutput("sex"),
                     htmlOutput("age"),
                     htmlOutput("edu"),
                     htmlOutput("race"),
                     htmlOutput("employment"),
                     htmlOutput("labor"),
                     htmlOutput("tenure"),
                     htmlOutput("vehicles"),
                     htmlOutput("transit")
                     ) # end span
                ) #end absolutePanel
  
  ) #END UI
  
  
  
  
  ######SERVER#####
  server <- function(input, output, session) {
    
    
    #Render leaflet output
    output$map <- renderLeaflet({
      
      
      leaflet() %>%
        #Set base map tiles
        addProviderTiles(providers$CartoDB.Positron) %>%
        #Set center point and zoom
        setView(-79.97555171361572, 40.37548598793065, zoom=11) %>%
        
        #Add reset button
        addResetMapButton()
      
        
    }) #end renderLeaflet
    
    
    #popup alert when "help" button is activated
    observeEvent(input$help | input$help2, {
      shinyalert(
        title = "How to Use This App",
        text = "<span class = 'alertHead'>Get Started</span><br>
        <span class = 'alert'>Explore the data by clicking an area on the map and choosing one or more categories
        on the bottom left panel.<br><br>You'll see the output show up in the bottom right panel. When you click
        on a different map area, the output will automatically update. (Note that if you see 'NA'
        or 'NA%', this indicates that no census data is available for the selected characteristic
        for the selected tract.)</span><br>
        <span class = 'alertHead'>Controls</span><br> 
        <span class = 'alert'>Click and drag the map to move it. Use the left-side
        buttons to zoom in and out, or to reset the map.<br><br>
        You can also click and drag the bottom-left panel. Move it around the screen to a
        spot that works best for you. This panel is semi-transparent to let you see the map beneath it. Hover your mouse over
        it, and it becomes solid for easier reading.<br><br>
        Add markers to the map to show Black-owned businesses in the county, or you can use the
        search feature to find and mark your own business (or another location you care about).
        For best results, enter a complete address when searching.</span><br>
        <span class = 'alertHead'>About the Map</span><br> 
        <span class = 'alert'>The map shows Allegheny County, PA, though
        you can move it elsewhere. This map shows census tracts. Information listed is for the
        census tract on which you have clicked.<br><br>
        Municipalities or neighborhood names (for city of Pittsburgh) are listed. They are generally accurate,
        but may sometimes not be what you expect because a municipality/neighborhood may have more than one
        census tract and a census tract may include more than one municipality/neighborhood.<br><br>
        The map's coloring represents a tract's median income, total population, or number of households 
        (depending on your selection).</span>",
        html = TRUE,
        size="m",
        closeOnClickOutside = TRUE
      )
    }, ignoreInit = T) #end observeEvent
    
    
    
    #observe click event and reactively generate side panel with selected variables
    #for the clicked census tract
    observeEvent(input$map_shape_click, {
      
      ###CLICK EVENT###
      #store click event in an object
      click <- input$map_shape_click
      
      #drop geometry & make df that includes only row for current map click
      click_name <- sf::st_drop_geometry(map[map$NAME == click$id,])
    
      
      ####MAKE DATA FRAMES TO HOLD VALUES OF SELECTED VARIABLES FOR CLICKED POLYGON####
      
      #returns all columns in click_name (defined above) where the column name is included
      #in input$age
      age_range <- click_name[,colnames(click_name) %in% input$age]
      edu_level <- click_name[,colnames(click_name) %in% input$edu]
      race <- click_name[,colnames(click_name) %in% input$race]
      num_vehicles <- click_name[,colnames(click_name) %in% input$vehicles]
      transit_work <- click_name[,colnames(click_name) %in% input$transit]
      
      
      
      #####CREATE REACTIVE OBJECTS TO SHOW VARIABLE VALUES#####
      
      #create a reactive object to display percent of pop. male/female
      sex_gender <- reactive( {
        if(!input$sex) {
          req(input$sex)
        } else {
          return( HTML(paste0("<p class = 'paddingLeft'>", 
                              click_name$Female, "% <strong>Female</strong><br>", 
                              click_name$Male, "% <strong>Male</strong></p><br>")))
        } #end conditional
      }) #end reactive
      
      #create a reactive object to display percent of pop. in selected age range(s)
      #conditional based on number of age ranges selected
      selected_age <- reactive( {
        if(length(input$age) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$age, "</strong>: ", age_range, "%</p><br>")) )
        } else if(length(input$age) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$age[[1]], "</strong>: ", age_range[1], "% <br>",
                              "<strong>", input$age[[2]], "</strong>: ", age_range[2], "%</p><br>")) )
        } else if(length(input$age) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$age[[1]], "</strong>: ", age_range[1], "% <br>",
                              "<strong>", input$age[[2]], "</strong>: ", age_range[2], "% <br>",
                              "<strong>", input$age[[3]], "</strong>: ", age_range[3], "%</p><br>")) )
        } else {
          return("")
        } #end conditional
      }) #end reactive
      
      
      
      #create a reactive object to display percent of pop. in with selected edu levels
      #conditional based on number of edu levels selected
      selected_edu <- reactive( {
        if(length(input$edu) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$edu, "</strong>: ", edu_level, "%</p><br><br>")) )
        } else if(length(input$edu) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$edu[[1]], "</strong>: ", edu_level[1], "% <br>",
                              "<strong>", input$edu[[2]], "</strong>: ", edu_level[2], "%</p><br><br>")) )
        } else if(length(input$edu) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$edu[[1]], "</strong>: ", edu_level[1], "% <br>",
                              "<strong>", input$edu[[2]], "</strong>: ", edu_level[2], "% <br>",
                              "<strong>", input$edu[[3]], "</strong>: ", edu_level[3], "% </p><br>")) )
        } else {
          return("")
        } #end conditional
      }) #end reactive
      
      
      #create a reactive object to display percent of pop. of selected race(s)
      #conditional based on number of age races selected
      selected_race <- reactive( {
        if(length(input$race) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$race, "</strong>: ", race, "%</p><br>")) )
        } else if(length(input$race) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$race[[1]], "</strong>: ", race[1], "% <br>",
                              "<strong>", input$race[[2]], "</strong>: ", race[2], "%</p><br>")) )
        } else if(length(input$race) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$race[[1]], "</strong>: ", race[1], "% <br>",
                              "<strong>", input$race[[2]], "</strong>: ", race[2], "% <br>",
                              "<strong>", input$race[[3]], "</strong>: ", race[3], "%</p><br>")) )
        } else {
          return("")
        } #end conditional
      }) #end reactive
      
      
      #create a reactive object to display percent of who are employed/unemployed
      employment <- reactive( {
        if(is.null(input$employ)) {
          req(input$employ)
        } else if (length(input$employ) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'>
                               <strong>Employed</strong>: ", 
                              click_name$Employed, "%<br>", 
                              " <strong>Unemployed</strong>: ",click_name$Unemployed, "%</p><br>")))
        } else if (input$employ == "Percent employed") {
          return( HTML(paste0("<p class = 'paddingLeft'>
                               <strong>Employed</strong>: ", 
                               click_name$Employed, "%</p><br>")))
        } else if (input$employ == "Percent unemployed") {
          return( HTML(paste0("<p class = 'paddingLeft'>
                               <strong>Unemployed</strong>: ", 
                              click_name$Unemployed, "%</p><br>")))
        } #end conditional
      }) #end reactive
      
      
      #create a reactive object to display # of pop. in the labor force
      labor <- reactive( {
        if(!input$lf) {
          req(input$lf)
        } else {
          return( HTML(paste0("<p class = 'paddingLeft'>
                              <strong>Population in the Labor Force (aged 16+)</strong>: ", 
                              click_name$civ_lf_denom,"</p><br>")))
        } #end conditional
      }) #end reactive
      
      
      #create a reactive object to display percent of housing units that owner-/renter-occupied
      tenure <- reactive( {
        if(is.null(input$tenure)) {
          req(input$tenure)
        } else if (length(input$tenure) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'>
                               <strong>Owner-Occupied Housing Units</strong>: ", 
                              click_name$`Owner-Occupied Housing Units`, "%<br>", 
                              " <strong>Renter-Occupied Housing Units</strong>: ",
                              click_name$`Renter-Occupied Housing Units`, "%</p><br>")))
        } else if (input$tenure == "Owner-occupied") {
          return( HTML(paste0("<p class = 'paddingLeft'>
                               <strong>Owner-Occupied Housing Units</strong>: ", 
                              click_name$`Owner-Occupied Housing Units`, "%</p><br>")))
        } else if (input$tenure == "Renter-occupied") {
          return( HTML(paste0("<p class = 'paddingLeft'>
                               <strong>Renter-Occupied Housing Units</strong>: ", 
                              click_name$`Renter-Occupied Housing Units`, "%</p><br>")))
        } #end conditional
      }) #end reactive
      
      
      
      #create a reactive object to display percent of households w/ or w/o vehicles
      #conditional based on number of items selected
      vehicles <- reactive( {
        if(length(input$vehicles) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$vehicles, "</strong>: ", num_vehicles, "%</p><br>")) )
        } else if(length(input$vehicles) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$vehicles[[1]], "</strong>: ", num_vehicles[1], "% <br>",
                              "<strong>", input$vehicles[[2]], "</strong>: ", num_vehicles[2], "%</p><br>")) )
        } else if(length(input$vehicles) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$vehicles[[1]], "</strong>: ", num_vehicles[1], "% <br>",
                              "<strong>", input$vehicles[[2]], "</strong>: ", num_vehicles[2], "% <br>",
                              "<strong>", input$vehicles[[3]], "</strong>: ", num_vehicles[3], "%</p><br>")) )
        } else {
          return("")
        } #end conditional
      }) #end reactive
      
      
      #create a reactive object to display percent of workers using different forms of
      #transportation to work - conditional on number of items selected
      transit <- reactive( {
        if(length(input$transit) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$transit, "</strong>: ", transit_work, "%</p><br>")) )
        } else if(length(input$transit) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$transit[[1]], "</strong>: ", transit_work[1], "% <br>",
                              "<strong>", input$transit[[2]], "</strong>: ", transit_work[2], "%</p><br>")) )
        } else if(length(input$transit) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$transit[[1]], "</strong>: ", transit_work[1], "% <br>",
                              "<strong>", input$transit[[2]], "</strong>: ", transit_work[2], "% <br>",
                              "<strong>", input$transit[[3]], "</strong>: ", transit_work[3], "%</p><br>")) )
        } else {
          return("")
        } #end conditional
      }) #end reactive
      
      
      
      #####CREATE OUTPUTS W/ RENDERTEXT#####
      
      #using above-created object, renderText to output muni name and census tract
      output$click_name <- renderText( { HTML(paste0("<span class='muniOut'><center>", click_name$muni_nei,"</center></span>")) } )
      output$click_tract <- renderText( { HTML(paste0("<span class='tract'><center>(Census Tract ",click_name$geo_id_ct, ")</center></span><br>")) } )
      
      
      #renderText to output percent male/female if selected
      output$sex <- renderText( { sex_gender() } )
      
      #renderText using the selected_age function defined above, to output the values
      #of selected age columns
      output$age <- renderText( { selected_age() } )
      
      #renderText using selected_edu function to output values of education columns
      output$edu <- renderText( { selected_edu() } )
      
      #renderText using selected_edu function to output values of education columns
      output$race <- renderText( { selected_race() } )
      
      #renderText using employment function to output employed/unemployed values
      output$employment <- renderText( { employment() } )
      
      #renderText using labor function to output total pop. in the labor force
      output$labor <- renderText( { labor() })
      
      #renderText using tenure function to output % of housing units that are owner-/renter-occupied
      output$tenure <- renderText( { tenure() } )
      
      #renderText using vehicle ownership function for output
      output$vehicles <- renderText( { vehicles() } )

      #renderText to output selected values of transportation
      output$transit <- renderText( { transit() } )
      
      
      
    } ) #end observeEvent
    
    
    
    #####DUPLICATE OBSERVERS TO UPDATE OUTPUT WHEN SELECTIONS CHANGE, NOT JUST ON CLICK#####
    
    #event to display sex/gender when selected, even w/o new click event
    observeEvent(input$sex, {
      
      click <- input$map_shape_click
      
      #drop geometry & make df that includes only row for current map click
      click_name <- sf::st_drop_geometry(map[map$NAME == click$id,])
      
      #create a reactive object to display percent of pop. male/female
      sex_gender <- reactive( {
        if(!input$sex) {
          return("")
        } else {
          return( HTML(paste0("<p class = 'paddingLeft'>", 
                              click_name$Female, "% <strong>Female</strong><br>", 
                              click_name$Male, "% <strong>Male</strong></p><br><br>")))
        } #end conditional
      }) #end reactive
      
    }) #end observeEvent
    
    
      
    
    #similar to above age event but the event is a change in input$age selection
    #creating both will allow user to click the map first or to change the input first
    #without returning errors
    observeEvent(input$age, {  
      
      click <- input$map_shape_click
      
      #drop geometry & make df that includes only row for current map click
      click_name <- sf::st_drop_geometry(map[map$NAME == click$id,])
      
      
      #returns all columns in click_name (defined above) where the column name is included
      #in input$age
      age_range <- click_name[,colnames(click_name) %in% input$age]
      
      #create a reactive object to display percent of pop. in selected age range(s)
      #conditional based on number of age ranges selected
      selected_age <- reactive( {
        if(length(input$age) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$age, "</strong>: ", age_range, "%</p><br>")) )
        } else if(length(input$age) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$age[[1]], "</strong>: ", age_range[1], "% <br>",
                         "<strong>", input$age[[2]], "</strong>: ", age_range[2], "%</p><br>")) )
        } else if(length(input$age) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$age[[1]], "</strong>: ", age_range[1], "% <br>",
                         "<strong>", input$age[[2]], "</strong>: ", age_range[2], "% <br>",
                         "<strong>", input$age[[3]], "</strong>: ", age_range[3], "% </p><br>")) )
        } else {
          return("")
        } #end conditional
      })
      
      output$age <- renderText( { ifelse(!is.null(click), 
                                         selected_age(),
                                         "<span style='padding:5px; font-size:1.2em;'><strong><center>Click on a census tract to display age information.</center></strong></span>") } )
    }) #end observeEvent
    
    
    
    
    
    #event for education level
    observeEvent(input$edu, {  
      
      click <- input$map_shape_click
      
      #drop geometry & make df that includes only row for current map click
      click_name <- sf::st_drop_geometry(map[map$NAME == click$id,])
      
   
      #returns all columns in click_name (defined above) where the column name is included
      #in input$edu
      edu_level <- click_name[,colnames(click_name) %in% input$edu]
      
      #create a reactive object to display percent of pop. with selected edu. levels
      #conditional based on number of levels selected
      selected_edu <- reactive( {
        if(length(input$edu) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$edu, "</strong>: ", edu_level, "%</p><br>")) )
        } else if(length(input$edu) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$edu[[1]], "</strong>: ", edu_level[1], "% <br>",
                              "<strong>", input$edu[[2]], "</strong>: ", edu_level[2], "%</p><br>")) )
        } else if(length(input$edu) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$edu[[1]], "</strong>: ", edu_level[1], "% <br>",
                              "<strong>", input$edu[[2]], "</strong>: ", edu_level[2], "% <br>",
                              "<strong>", input$edu[[3]], "</strong>: ", edu_level[3], "% </p><br>")) )
        } else {
          return("")
        } #end conditional
      })
      
      output$edu <- renderText( { ifelse(!is.null(click), 
                                         selected_edu(),
                                         "<span style='padding:5px; font-size:1.2em;'><strong><center>Click on a census tract to display education information.</center></strong></span>") } )
      
      
    }) #end observeEvent
    
    
    
    
    #event for race 
    observeEvent(input$race, {  
      
      click <- input$map_shape_click
      
      #drop geometry & make df that includes only row for current map click
      click_name <- sf::st_drop_geometry(map[map$NAME == click$id,])
      
      
      #returns all columns in click_name (defined above) where the column name is included
      #in input$race
      race <- click_name[,colnames(click_name) %in% input$race]
      
      #create a reactive object to display percent of pop. of selected races
      #conditional based on number of races selected
      selected_race <- reactive( {
        if(length(input$race) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$race, "</strong>: ", race, "%</p><br>")) )
        } else if(length(input$race) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$race[[1]], "</strong>: ", race[1], "% <br>",
                              "<strong>", input$race[[2]], "</strong>: ", race[2], "% </p><br>")) )
        } else if(length(input$race) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$race[[1]], "</strong>: ", race[1], "% <br>",
                              "<strong>", input$race[[2]], "</strong>: ", race[2], "% <br>",
                              "<strong>", input$race[[3]], "</strong>: ", race[3], "% </p><br>")) )
        } else {
          return("")
        } #end conditional
      })
      
      
      #renderText to output selected values of race
      output$race <- renderText( { ifelse(!is.null(click), 
                                         selected_race(),
                                         "<span style='padding:5px; font-size:1.2em;'><strong><center>Click on a census tract to display racial information.</center></strong></span>") } )
      
      
    }) #end observeEvent

   
    #event for vehicle ownership
    observeEvent(input$vehicles, {  
      
      click <- input$map_shape_click
      
      #drop geometry & make df that includes only row for current map click
      click_name <- sf::st_drop_geometry(map[map$NAME == click$id,])
      
      
      #returns all columns in click_name (defined above) where the column name is included
      #in input$vehicles
      num_vehicles <- click_name[,colnames(click_name) %in% input$vehicles]
      
      vehicles <- reactive( {
        if(length(input$vehicles) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$vehicles, "</strong>: ", num_vehicles, "%</p><br>")) )
        } else if(length(input$vehicles) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$vehicles[[1]], "</strong>: ", num_vehicles[1], "% <br>",
                              "<strong>", input$vehicles[[2]], "</strong>: ", num_vehicles[2], "%</p><br>")) )
        } else if(length(input$vehicles) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$vehicles[[1]], "</strong>: ", num_vehicles[1], "% <br>",
                              "<strong>", input$vehicles[[2]], "</strong>: ", num_vehicles[2], "% <br>",
                              "<strong>", input$vehicles[[3]], "</strong>: ", num_vehicles[3], "%</p><br>")) )
        } else {
          return("")
        } #end conditional
      }) #end reactive
      
      
      #renderText to output selected values of race
      output$vehicles <- renderText( { ifelse(!is.null(click), 
                                          vehicles(),
                                          "<span style='padding:5px; font-size:1.2em;'><strong><center>Click on a census tract to display vehicle ownership information.</center></strong></span>") } )
      
      
    }) #end observeEvent
    
    
    #event for transportation to work
    observeEvent(input$transit, {  
      
      click <- input$map_shape_click
      
      #drop geometry & make df that includes only row for current map click
      click_name <- sf::st_drop_geometry(map[map$NAME == click$id,])
      
      
      #returns all columns in click_name (defined above) where the column name is included
      #in input$transit
      transit_work <- click_name[,colnames(click_name) %in% input$transit]
      
      transit <- reactive( {
        if(length(input$transit) == 1) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$transit, "</strong>: ", transit_work, "%</p><br>")) )
        } else if(length(input$transit) == 2) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$transit[[1]], "</strong>: ", transit_work[1], "% <br>",
                              "<strong>", input$transit[[2]], "</strong>: ", transit_work[2], "%</p><br>")) )
        } else if(length(input$transit) == 3) {
          return( HTML(paste0("<p class = 'paddingLeft'><strong>", input$transit[[1]], "</strong>: ", transit_work[1], "% <br>",
                              "<strong>", input$transit[[2]], "</strong>: ", transit_work[2], "% <br>",
                              "<strong>", input$transit[[3]], "</strong>: ", transit_work[3], "%</p><br>")) )
        } else {
          return("")
        } #end conditional
      }) #end reactive
      
      
      #renderText to output selected values of transportation type
      output$transit <- renderText( { ifelse(!is.null(click), 
                                              transit(),
                                              "<span style='padding:5px; font-size:1.2em;'><strong><center>Click on a census tract to display transportation information.</center></strong></span>") } )
      
      
    }) #end observeEvent
    
    
    
    
    
    
    #leafletProxy to turn legend on/off and switch variable to use
    observe( {
      proxy <- leafletProxy("map", data = map)
      
      
      proxy %>% clearControls()
      
      #if legend is toggled on...
      if (input$legend & input$color_map == "Median Income") {
        proxy %>% #Add legend for coloring by median income
          
          #Add Allegheny County census tract shapefile- fillColor- median income
          addPolygons(data=map,
                      fillColor = ~pal(hhincome),
                      color = "#000000",
                      fillOpacity = 0.5,
                      weight = 1,
                      smoothFactor = 0.8,
                      layerId = ~NAME,
                      label = ~muni_nei,
                      popup = popup1,
                      highlight = highlightOptions(stroke=T, color = "#ffffff",
                                                   opacity = 1,
                                                   weight = 4,bringToFront = TRUE)) %>%
        
          addLegend(pal = pal, 
                    values = map$hhincome,
                    position = "topright",
                    labFormat = labelFormat(prefix = "$"),
                    title = "Median Income")
        
      } else if (input$legend & input$color_map == "Total Population") {
        proxy %>% #Add legend for coloring by total pop.
          
          #Add Allegheny County census tract shapefile, fillColor- total pop.
          addPolygons(data=map,
                      fillColor = ~pop_pal(total_pop),
                      color = "#000000",
                      fillOpacity = 0.5,
                      weight = 1,
                      smoothFactor = 0.8,
                      layerId = ~NAME,
                      label = ~muni_nei,
                      popup = popup1,
                      highlight = highlightOptions(stroke=T, color = "#ffffff",
                                                   opacity = 1,
                                                   weight = 4,bringToFront = TRUE)) %>%
          
          addLegend(pal = pop_pal,
                    values = map$total_pop,
                    position = "topright",
                    title = "Population")
        
      } else if (input$legend & input$color_map == "Total Households") {
        proxy %>% #Add legend for coloring by total number of households
          
          #Add Allegheny County census tract shapefile, fillColor- households
          addPolygons(data=map,
                      fillColor = ~house_pal(households),
                      color = "#000000",
                      fillOpacity = 0.5,
                      weight = 1,
                      smoothFactor = 0.8,
                      layerId = ~NAME,
                      label = ~muni_nei,
                      popup = popup1,
                      highlight = highlightOptions(stroke=T, color = "#ffffff",
                                                   opacity = 1,
                                                   weight = 4,
                                                   bringToFront = TRUE)) %>%
          
          addLegend(pal = house_pal,
                    values = map$households,
                    position = "topright",
                    title = "Households")
        
        #Or, if legend is set to off
      } else if (!input$legend & input$color_map == "Median Income") {
        proxy %>% 
          
          #Add Allegheny County census tract shapefile- fillColor- median income
          #No legend
          addPolygons(data=map,
                      fillColor = ~pal(hhincome),
                      color = "#000000",
                      fillOpacity = 0.5,
                      weight = 1,
                      smoothFactor = 0.8,
                      layerId = ~NAME,
                      label = ~muni_nei,
                      popup = popup1,
                      highlight = highlightOptions(stroke=T, color = "#ffffff",
                                                   opacity = 1,
                                                   weight = 4,bringToFront = TRUE))
        
        
      } else if (!input$legend & input$color_map == "Total Population") {
        proxy %>%
          
          #Add Allegheny County census tract shapefile, fillColor- total pop.
          #No legend
          addPolygons(data=map,
                      fillColor = ~pop_pal(total_pop),
                      color = "#000000",
                      fillOpacity = 0.5,
                      weight = 1,
                      smoothFactor = 0.8,
                      layerId = ~NAME,
                      label = ~muni_nei,
                      popup = popup1,
                      highlight = highlightOptions(stroke=T, color = "#ffffff",
                                                   opacity = 1,
                                                   weight = 4,bringToFront = TRUE))
        
      } else if  (!input$legend & input$color_map == "Total Households") {
        proxy %>%
        
        #Add Allegheny County census tract shapefile, fillColor- households
        #No legend    
        addPolygons(data=map,
                    fillColor = ~house_pal(households),
                    color = "#000000",
                    fillOpacity = 0.5,
                    weight = 1,
                    smoothFactor = 0.8,
                    layerId = ~NAME,
                    label = ~muni_nei,
                    popup = popup1,
                    highlight = highlightOptions(stroke=T, color = "#ffffff",
                                                 opacity = 1,
                                                 weight = 4,
                                                 bringToFront = TRUE))
      } # end conditional
   
      
    }) #end observer
    
    
    #add Polylines to keep a tract highlighted after a click event even when mouseout
    observeEvent (input$map_shape_click, {
      
      click <- input$map_shape_click
      lines <- map[map$NAME %in% click$id,]
      
      
      if (is.null(click$id)) {
        req(click$id)
      } else {
        leafletProxy("map") %>%
          clearGroup("lines") %>%
          addMapPane("lines", zIndex = 500) %>%
          addPolylines(data = lines,
                       color = "#ffffff",
                       opacity = 1,
                       weight = 8,
                       group = "lines",
                       options = pathOptions(pane = "lines"))
      } #end conditional
      
    })  #end observeEvent
    
    
    
    #leafletProxy to show/hide markers for Black-owned businesses
    observe ({
      
      proxy <- leafletProxy("map") %>%
        clearGroup("bus")
      
      #Add markers using bus_geo df if input$bus is TRUE
      if (input$bus | input$bus2) {
        proxy %>% addMarkers(data = bus_geo,
                             lat = ~lat, lng = ~lng, #markers for businesses,
                             layerId = ~name,
                             icons <- red_icons,
                             group = "bus",
                             popup=paste0("<span style='font-size:1.3em; font-weight:bold;'>",
                                          bus_geo$name,"</span><br>",bus_geo$formatted_address),
                             options = markerOptions(riseOnHover = T, opacity = 0.8))
        
      } #end conditional
    }) #end observer
    
    
    ##for inputs that are duplicated on both demographic and household tabs, keep them
    ##in sync -- update each based on the other's value and then will only need code to run based on
    ##inputs on tab1
    
    #observers for "show black businesses" input:
    observeEvent (input$bus, {
      updatePrettyCheckbox(session=session,
                           inputId = "bus2",
                           value = input$bus)
    })
    observeEvent (input$bus2, {
      updatePrettyCheckbox(session=session,
                           inputId = "bus",
                           value = input$bus2)
    })
    
    #for address search input:
    observeEvent (input$address, {
      updateSearchInput(session=session,
                           inputId = "address2",
                           value = input$address)
    })
    observeEvent (input$address2, {
      updateSearchInput(session=session,
                           inputId = "address",
                           value = input$address2)
    })
    
    #for legend toggle
    observeEvent (input$legend, {
      updateMaterialSwitch(session=session,
                           inputId = "legend2",
                           value = input$legend)
    })
    observeEvent (input$legend2, {
      updatePrettyCheckbox(session=session,
                           inputId = "legend",
                           value = input$legend2)
    })
    
    #for choropleth picker
    observeEvent (input$color_map, {
      updatePrettyCheckbox(session=session,
                           inputId = "color_map2",
                           value = input$color_map)
    })
    observeEvent (input$color_map2, {
      updatePrettyCheckbox(session=session,
                           inputId = "color_map",
                           value = input$color_map2)
    })
    
    
    
    #observer for adding marker to address input
    observeEvent(input$address_search, {
      
      proxy <- leafletProxy("map")
      
      #geocode address input using OSM, store as a data frame
      search_address <- geocode_OSM(input$address, as.data.frame=T, details=T)
      
      #popup alert is address cannot be found
      if (is.null(search_address$lat)) {
        
        shinyalert(paste0("Oops, we can't find <em>",input$address,"</em>!"), "Sorry! OpenStreetMap can't seem to find the place
                   you entered.<br><br><strong>For best results, make sure you enter the full address: street
                   address, city, state, and zip code. It may be helpful to use the township name, even if the
                   mailing address is Pittsburgh.</strong><br><br>If you're not getting the results you expect,
                   try searching for a nearby address or just the municipality/neighborhood and state
                   (e.g., 'east liberty, pittsburgh, pa' or 'penn hills, pa').", 
                   type = "warning",
                   closeOnClickOutside = T, 
                   html=T)
        
      } #end conditional
      
    #if an address is queried, reset map view to center on the lat/lng and zoom in  
    if (length(search_address$query != 0)) {    
    proxy %>%
        clearGroup("searched") %>%
      setView(lat = search_address$lat, lng = search_address$lon, zoom = 15) %>%
      
      #add marker to address searched for
      addMarkers(data = search_address,
                 lat = ~lat,
                 lng = ~lon,
                 icon = green_icon,
                 group = "searched",
                 popup = paste0("<center><strong>You entered</strong>: <span style='font-style:italic'><br>", search_address$query, "</span><br>
                                  <br><strong><a href='https://www.openstreetmap.org/copyright' target='_blank' rel='noopener noreferrer'>OpenStreetMap</a> found</strong>: <span style='font-style:italic'><br>", search_address$display_name, "</span></center>"))
    }

      
    }) #end observer
    
      
      
      
    
    #observer for adding marker to address input (as above but for address2 input- on tab2)
    observeEvent(input$address2_search, {
      
      proxy <- leafletProxy("map")
      
      #geocode address input using OSM, store as a data frame
      search_address2 <- geocode_OSM(input$address2, as.data.frame=T, details=T)
      
      #popup alert is address cannot be found
      if (is.null(search_address2$lat)) {
        
        shinyalert(paste0("Oops, we can't find <em>",input$address2,"</em>!"), "Sorry! OpenStreetMap can't seem to find the place
                   you entered.<br><br><strong>For best results, make sure you enter the full address: street
                   address, city, state, and zip code. It may be helpful to use the township name, even if the
                   mailing address is Pittsburgh.</strong><br><br>If you're not getting the results you expect,
                   try searching for a nearby address or just the municipality/neighborhood and state
                   (e.g., 'east liberty, pittsburgh, pa' or 'penn hills, pa').", 
                   type = "warning",
                   closeOnClickOutside = T, 
                   html=T)
        
      } #end conditional
      
      #if an address is queried, reset map view to center on the lat/lng and zoom in  
      if (length(search_address2$query != 0)) {    
        proxy %>%
          clearGroup("searched") %>%
          setView(lat = search_address2$lat, lng = search_address2$lon, zoom = 15) %>%
          
          #add marker to address searched for
          addMarkers(data = search_address2,
                     lat = ~lat,
                     lng = ~lon,
                     icon = green_icon,
                     group = "searched",
                     popup = paste0("<center><strong>You entered</strong>: <span style='font-style:italic'><br>", search_address2$query, "</span><br>
                                  <br><strong><a href='https://www.openstreetmap.org/copyright' target='_blank' rel='noopener noreferrer'>OpenStreetMap</a> found</strong>: <span style='font-style:italic'><br>", search_address2$display_name, "</span></center>"))
      }
      
      
    }) #end observer
    
    
    #####HOW TO USE THIS INFO#####
    
    #popup for "about this data"
    observeEvent(input$about, {
        shinyalert(title = "<span class='useHead'><i class='far fa-chart-bar'></i> About the Data</span>",
                   "<span class='useText'>The data here comes from American Community Survey (ACS) 5-year 
                   estimates (2015-2019).</span><br><br>
                   <span class='useSub'>What is the ACS?</span>
                   <span class='useText'>The ACS is a survey done by the U.S. Census Bureau. While the Decennial Census (what
                   you probably know as \"the census\") happens only once every 10 years and aims to include
                   everyone living in the U.S., the ACS collects information continuously but only from a sample of
                   randomly selected households.<br><br>
                   The ACS asks questions about demographics, housing, household composition, employment, and
                   many other things. The Census Bureau makes this information available on a yearly basis.
                   <br><br>The Census Bureau uses the responses of its random sample to estimate the characteristics
                   of entire geographic areas. Estimates may be based on 1 year or 5 years of
                   data. (The Bureau used to provide 3-year estimates as well.)<br><br>
                   5-year estimates are more reliable but less current than 1-year estimates. The information
                   here is from 5-year estimates based on data collected between January 1, 2015 and December 31,
                   2019.
                   <br><br>
                   (For more information about the ACS, 
                   <a href='https://www.census.gov/programs-surveys/acs/about.html' target='_blank' rel='noopener noreferrer'>visit the 
                   Census Bureau's website</a>.)
                   <br><br></span>
                   <span class='useSub'>What are census tracts?</span>
                   <span class='useText'>The data here is provided at the census tract level. 
                   The Census Bureau divides U.S. counties into tracts. They define census tracts as \"small,
                   relatively permanent statistical subdivisions of a county.\" Tracts have between 1,200
                   and 8,000 residents (4,000 on average). They may be updated every 10 years.<br><br>
                   Due to the Census Bureau's use of tracts, they are commonly used as a geographical 
                   unit, but their borders are not typically the same as neighborhood boundaries.<br><br>
                   (For more information about census tracts, 
                   <a href=\"https://www.census.gov/data/academy/data-gems/2018/tract.html\" target='_blank' rel='noopener noreferrer'>
                   visit the Census Bureau's website.</a>)</span>",
                   closeOnClickOutside = TRUE,
                   size="m",
                   html=T)
    }, ignoreInit = T) # end observeEvent
    
    
    #popup for "using demographic info"
    observeEvent(input$use_demo, {
      shinyalert(title = "<span class='useHead'><i class='fas fa-users'></i> Using Demographic Info</span>",
                 "<span class='useText'>Demographics can be a powerful tool for business owners.
                 You can decide where and how to advertise and promote your business based on
                 an understanding of the characteristics of the people who live in different areas.
                 You might also take demographics into account when deciding where to
                 open a new business, open a new location, or relocate.</span><br><br>
                 <span class='useSub'>Sex/Gender</span>
                 <span class='useText'>You might know that your business tends to appeal to women more
                 than men (or vice versa) and so you could use demographics to find a nearby area that
                 has a higher proportion of women, and target that area.
                 </span><br><br>
                 <span class='useSub'>Age Groups</span>
                 <span class='useText'>Similarly, knowing age breakdowns for areas can be very helpful. For instance, if 
                 your business is a daycare, you can focus on areas with higher proportions of residents
                 under 5 years. If you know that your products appeal to older adults, you might
                 look for areas with higher proportions who are 65 to 74, 75 to 84, and/or 85 and older.
                 </span><br><br>
                 <span class='useSub'>Income, Population, Number of Households</span>
                 <span class='useText'>Knowing the median income of an area can be important in
                 making sure that you aren't marketing luxury products to people who can't afford them. It 
                 can also help you to figure out what kind of products <em>would</em> be
                 most useful to people in the area. Knowing the total population and number of
                 households in an area can also help with decision-making. For some marketing campaigns,
                 high population density might be a good thing, but not for others.<br><br><br>
                 Other demographic information can be used in much the same way. It can be helpful to know
                 the characteristics of the people most likely to use your business; in what areas
                 those people tend to live; and the characteristics of the people who do
                 live near to your business. Having these pieces of information when developing
                 a strategy for your business can be very valuable.",
                 closeOnClickOutside = T,
                 size="m",
                 html=T)
    }, ignoreInit = T) # end observeEvent
    
    
    #popup for "using household/employment info"
    observeEvent(input$use_house, {
      shinyalert(title = "<span class='useHead'><i class='fas fa-home'></i> Using Household/Employment Info</span>",
                 "<span class='useText'>Household and employment information
                 will also tell you a lot about the people who live near your
                 business.</span><br><br>
                 <span class='useSub'>Employment Information</span>
                 <span class='useText'>Knowing the unemployment rate in an area
                 can help you make decisions. Residents in
                 areas with higher unemployment rates might struggle to afford nonessential
                 items, but promotional discounts on products that people <em>need</em>
                 might pay off for you.<br><br>
                 <strong>These rates are pre-COVID</strong>. They can
                 help you get a sense of different areas, but they are not current, and some areas have been 
                 disproportionately impacted by COVID-related unemployment.<br><br>
                 \"Population in the labor force\" includes residents who were working or looking for work.
                 The percent of people employed/unemployed is a percentage of this number.</span><br><br>
                 <span class='useSub'>Owner-Occupied & Renter-Occupied Housing Units</span>
                 <span class='useText'>These percentages are based on the total
                 number of occupied housing units in a tract and are a percentage of total households.
                 <br><br>Understanding these characteristics can be useful to marketing. 
                 For instance, home improvement products may sell better
                 when targeted to homeowners versus renters.</span><br><br>
                 <span class='useSub'>Vehicle Ownership & Transportation to Work</span>
                 <span class='useText'>These characteristics give you information about how people
                 get around the area in which they live and beyond. Vehicle ownership displays the
                 percentage of <em>households</em> in the area with zero, one, or more than one vehicle. 
                 Transportation to work tells about people who get to work without a personal vehicle: via public transit 
                 (including taxis and ride-sharing services like
                 Uber), biking, or walking. This information might help you determine how much foot
                 traffic an area might have, how far you might expect
                 people to travel for your business, or what areas you should focus on when adding a 
                 delivery service.",
                 closeOnClickOutside = T,
                 size="m",
                 html=T)
    }, ignoreInit = T) #end observeEvent
    
    
    #popup for "more resources"
    observeEvent(input$resources, {
      shinyalert(title = "<span class='useHead'><i class='fas fa-link'></i> More Resources</span>",
                 "<br>
                 <span class='useText'>Below are several additional resources that business owners
                 might find helpful. They are provided here for your reference, but inclusion
                 does not constitute an endorsement.<br><br><br>
                 <ul class='list'>
                 <li><a href='https://www.sba.gov/funding-programs/loans/covid-19-relief-options' target='_blank' rel='noopener noreferrer'>
                 COVID-19 Relief Options</a> (Small Business Administration)</li>
                 <li><a href='https://www.ura.org/pages/businesses-entrepreneurs' target='_blank' rel='noopener noreferrer'>Business and Entrepreneur
                 Resources</a> (URA of Pittsburgh)</li>
                 <li><a href='https://www.usa.gov/business/' target='_blank' rel='noopener noreferrer'>Small Business Resources</a> (Federal government)</li>
                 <li><a href='https://dced.pa.gov/program/' target='_blank' rel='noopener noreferrer'>Programs and Funding Tool</a> (PA Department of 
                 Community and Economic Development)</li>
                 <li><a href='https://www.riversidecenterforinnovation.com/dbrc/home' target='_blank' rel='noopener noreferrer'>Diversity Business
                 Resource Center</a></li>
                 <li><a href='https://aaccwp.com/' target='_blank' rel='noopener noreferrer'>African American Chamber of Commerce of Western PA</a></li>
                 <li><a href='https://learn.sba.gov/dashboard' target='_blank' rel='noopener noreferrer'>Business Learning Center</a> (Small
                 Business Administration)</li>
                 <li><a href='https://www.carnegielibrary.org/services/for-businesses/entrepreneurs-and-small-businesses/' target='_blank' rel='noopener noreferrer'>
                 Entrepreneurs and Small Businesses</a> (Carnegie Library of Pittsburgh)</li>
                 <li><a href='https://blog.hootsuite.com/social-media-tips-for-small-business-owners/' target='_blank' rel='noopener noreferrer'>How to Use Social
                 Media for Small Business</a> (Hootsuite)</li>
                 <li><a href='https://www.sba.gov/business-guide/manage-your-business/marketing-sales' target='_blank' rel='noopener noreferrer'>
                 Marketing and Sales</a> (Small Business Administration)</li>
                 <li>Black-Owned Marketing Companies: <a href='https://www.anicoletay.com/' target='_blank' rel='noopener noreferrer'>ANICOLETAY</a>,
                 <a href='https://www.jirehmobile.com/' target='_blank' rel='noopener noreferrer'>Jireh Mobile</a>, 
                 <a href='https://lifeexperiencesolutions.com/pittsburgh-marketing-agency/' target='_blank' rel='noopener noreferrer'>LES Pittsburgh Marketing Agency</a></li>
                 </ul></span>",
                 closeOnClickOutside = T,
                 html=T)
    }, ignoreInit = T) #end observeEvent
    
    
  } #end server
  
  
####RUN APP####
shinyApp(ui = ui, server = server)  
    
    
  