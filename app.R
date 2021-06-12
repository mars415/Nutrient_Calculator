library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)

# load files from Canada Nutrient File

nutr_files <- list.files(pattern = "*.rda")
lapply(nutr_files,load,.GlobalEnv)
# format quantities for ingredients

# take everything before parenthesis
ca_measure_name$units <- regmatches(ca_measure_name$MeasureDescription, regexpr("^([^(]+)", ca_measure_name$MeasureDescription, perl = T))
# only take what is in parenthesis
ca_measure_name$description <- gsub("^([^(]+)", "", ca_measure_name$MeasureDescription, perl = T)
# extract numeric values
r <- regexpr("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", ca_measure_name$units)
out <- rep(NA,nrow(ca_measure_name))
out[r!=-1] <- regmatches(ca_measure_name$units, r)
ca_measure_name$numeric <- out
# convert fractions to decimal
fractions <- grep("\\/", ca_measure_name$numeric)
ca_measure_name$numeric[fractions] <- sapply(ca_measure_name$numeric[fractions], function(x) eval(parse(text=x)))
# fill in blank numeric values
ca_measure_name$numeric[is.na(ca_measure_name$numeric)] <- 1
# everything numberic
ca_measure_name$numeric <- round(as.numeric(ca_measure_name$numeric), 5)
# now remove numbers from units
ca_measure_name$units <- gsub("^[[:digit:]]+[\\/\\.]*[[:digit:]]*", "", ca_measure_name$units)


# format ingredient choices
ca_food_choices <- ca_food_name$FoodID
names(ca_food_choices) <- ca_food_name$FoodDescription


# format daily values

daily_value <- read.table("daily_values.txt", sep = "\t", header=T, stringsAsFactors = F)

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Nutrient & Health"),
                    dashboardSidebar(sidebarMenu(
                      menuItem("Nutrient Calculator", tabName = "calculator", icon = icon("calculator")),
                      menuItem("Health Calculator", tabName = "widgets", icon = icon("weight"))
                    )
                    ),
                    dashboardBody(tabItems(
                      # First tab content
                      tabItem(tabName = "calculator",
                              selectizeInput(
                                'food_id', '1. Ingredient', choices = ca_food_choices,
                                options = list(
                                  placeholder = 'Type to search for ingredient',
                                  onInitialize = I('function() { this.setValue(""); }')
                                )
                              ),
                              conditionalPanel('input.food_id != ""', 
                                               selectizeInput('measure_unit', '2. Measure Unit', choices = c("Select an ingredient" = "")),
                                               numericInput('quantity', '3. Quantity', value = 1, min = 0, step = 1)),
                              actionButton("add", "Add ingredient"),
                              actionButton("remove", "Remove ingredient"),
                              numericInput("serving", "Number of servings contained", min = 0.01, step = 1, value = 1),
                              tags$p("Note: All nutrient information is based on the Canadian Nutrient File. Nutrient amounts do not account for variation in nutrient retention and yield losses of ingredients during preparation. % daily values (DV) are taken from the Table of Daily Values from the Government of Canada. This data should not be used for nutritional labeling."),
                              fluidRow(
                                box(title = "Ingredients",
                                    solidHeader = T,
                                    width = 4,
                                    collapsible = T,
                                    div(DT::DTOutput("ing_df"), style = "font-size: 70%;")),
                                box(title = "Macronutrients", solidHeader = T,
                                    width = 8, collapsible = T,
                                    plotlyOutput("macro_plot"))
                              )
                      ),
                      
                      # Second tab content
                      tabItem(tabName = "widgets",
                              tabsetPanel(
                                tabPanel("Metric",
                                         # Input values
                                         sidebarPanel( width = 6,
                                           HTML("<h3>Input</h3>"),
                                           
                                           numericInput("weight", 
                                                       label = "Weight  (kg)", 
                                                       value = 0, 
                                                       min = 0, 
                                                       max = 650),
                                           
                                           numericInput("height", 
                                                        label = "Height (cm)", 
                                                        value = 0, 
                                                        min = 0, 
                                                        max = 250),
                                           
                                           numericInput("age", 
                                                        label = "Age (year)", 
                                                        value = 0, 
                                                        min = 0, 
                                                        max = 110),
                                           
                                           radioButtons("gender", label = "Gender",
                                                        choices = list("Male" = 1, "Female" = 2),
                                                        selected = 1),
                                           
                                           selectInput("lifestyle", label = "Lifestyle", 
                                                       choices = list("Sedentary (little or no exercise)" = 1, "Lightly Active (light exercise/sports 1-3 days/week)" = 2,
                                                                      "Moderately Active (moderate exercise/sports 3-5 days/week)" = 3, "Very Active (hard exercise/sports 6-7 days/week)"= 4, "Extra Active (very hard daily exercise/sports & physical job or 2X day training)"=5), selected = 0),
                                           
                                           actionButton("submitbutton", 
                                                        "Compute BMI", 
                                                        class = "btn btn-primary")
                                         
                                         
                                          ),
                                         
                                         mainPanel( width =6,
                                           tags$label(h3('Output')), # Status/Output Text Box
                                           verbatimTextOutput('contents'),
                                           tableOutput('tabledataBmi'), # Results table
                                           tableOutput('tabledataBmr'), # Results table
                                           tableOutput('tabledataCal'), # Results table
                                           textOutput("displayWeight"),
                                           textOutput("displayHeight")
                                         ) # mainPanel()
                                         
                                ),
                                tabPanel("Imperial",
                                         # Input values
                                         sidebarPanel(width = 6,
                                           HTML("<h3>Input</h3>"),
                                           
                                           numericInput("weightLbs", 
                                                       label = "Weight (lbs)", 
                                                       value = 0, 
                                                       min = 0, 
                                                       max = 550),
                                           
                                           numericInput("ft", 
                                                        label = "Feet",
                                                        value = 0,
                                                        min = 0,
                                                        max = 8,
                                                        width = '50%'),
                                           numericInput("inch", 
                                                        label = "Inch",
                                                        value = 0,
                                                        min = 0,
                                                        max = 11,
                                                        width = '50%'),
                                           numericInput("age2", 
                                                        label = "Age (year)", 
                                                        value = 0, 
                                                        min = 0, 
                                                        max = 110),
                                           
                                           radioButtons("gender2", label = "Gender",
                                                        choices = list("Male" = 1, "Female" = 2),
                                                        selected = 1),
                                           selectInput("lifestyle2", label = "Lifestyle", 
                                                       choices = list("Sedentary (little or no exercise)" = 1, "Lightly Active (light exercise/sports 1-3 days/week)" = 2,
                                                                      "Moderately Active (moderate exercise/sports 3-5 days/week)" = 3, "Very Active (hard exercise/sports 6-7 days/week)"= 4, "Extra Active (very hard daily exercise/sports & physical job or 2X day training)"=5), selected = 0),
                                           
                                           
                                           actionButton("submitbutton2", 
                                                        "Compute BMI", 
                                                        class = "btn btn-primary")
                                         ),
                                         
                                         mainPanel(width = 6,
                                           tags$label(h3('Output')), # Status/Output Text Box
                                           verbatimTextOutput('contents2'),
                                           tableOutput('tabledataBmi2'), # Results table
                                           tableOutput('tabledataBmr2'), # Results table
                                           tableOutput('tabledataCal2'), # Results table
                                           textOutput("displayWeight2"),
                                           textOutput("displayHeight2")
                                         ) # mainPanel()
                                         
                                )
                                
                              ) # navbarPage()
                      )
                    )
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # make reactive to store ingredients
  ing_df <- shiny::reactiveValues()
  ing_df$df <- data.frame("quantity" = numeric(), 
                          "units" = character(), 
                          "ingredient_name" = character(), 
                          "FoodID" = numeric(), 
                          stringsAsFactors = F)
  ing_df$measure <- data.frame("numeric" = numeric(),
                               "units" = character(),
                               "description" = character(),
                               "ConversionFactorValue" = numeric(),
                               "MeasureID" = numeric(),
                               "FoodID" = numeric(),
                               stringsAsFactors = F)
  
  # step 1 get singular ingredient
  measure_df <- eventReactive(input$food_id,{
    measure_df <- ca_food_name[ca_food_name$FoodID==input$food_id, "FoodID"] %>% 
      left_join(ca_conversion_factor) %>% 
      left_join(ca_measure_name) %>% 
      select(numeric, units, description, ConversionFactorValue, MeasureID, FoodID) 
    # measure_df <- rbind(measure_df, c(numeric = 100, units = "g", description = "", ConversionFactorValue = 1, MeasureID = ""))
    measure_df
  })
  
  # step 2 update the measure unit for singular ingredient
  observe({
    units <- unique(paste(measure_df()$units, measure_df()$description))
    updateSelectInput(session, "measure_unit", "2. Measure Unit", choices = units)
  })
  
  # step 3 update the ingredient dataframe
  observeEvent(input$remove, {
    isolate(ing_df$df<-ing_df$df[-(nrow(ing_df$df)),])
    isolate(ing_df$measure <- ing_df$measure[-nrow(ing_df$measure),])
  })
  observeEvent(input$add, {
    isolate(ing_df$df[nrow(ing_df$df) + 1,] <- c(input$quantity,
                                                 input$measure_unit, 
                                                 names(ca_food_choices[ca_food_choices == input$food_id]), 
                                                 as.numeric(input$food_id)))
    # get actual working ingredient dataframe for dplyr
    input_measure <- measure_df()
    input_measure <- input_measure[paste(measure_df()$units, measure_df()$description) == input$measure_unit, ]
    if(nrow(input_measure) > 1){
      input_measure <- input_measure[which(abs(input_measure$numeric-input$quantity)==min(abs(input_measure$numeric-input$quantity))),]
    }
    isolate(ing_df$measure[nrow(ing_df$measure) + 1, ] <- input_measure)
    # update choices
    updateNumericInput(session, 'quantity', '3. Quantity', 1)
    updateSelectizeInput(session, 'measure_unit', '2. Measure Unit')
    updateSelectInput(session, 'food_id', '1. Ingredient', choices = ca_food_choices)
  })
  
  # main nutrition data frame
  nutrition_df <- reactive({
    measure_food_df <- ing_df$measure
    ing_quant <- ing_df$df
    measure_food_df$quantity <- ing_quant$quantity
    measure_food_df <- measure_food_df %>%
      left_join(ca_nutrient_amount) %>%
      left_join(ca_nutrient_name) %>%
      # filter(NutrientID %in% select_nutrients) %>%
      mutate(NutrientName = tolower(NutrientName)) %>%
      mutate(NutrientValue = as.numeric(NutrientValue) * as.numeric(ConversionFactorValue) * as.numeric(quantity) / as.numeric(numeric) / input$serving) %>%
    select(NutrientName, NutrientValue, NutrientID, NutrientUnit, ConversionFactorValue, quantity, FoodID) %>% 
      group_by(NutrientName) %>% 
      summarize(Value = round(sum(NutrientValue, na.rm = T),2),
                Unit = first(NutrientUnit),
                NutrientID = first(NutrientID))
    
    measure_food_df
  })

  # df with dv%
  dv_df <- reactive({
    dv_df <- daily_value %>% left_join(nutrition_df())
    # hack for total sat fats and trans fats
    dv_df$Value[2] <- sum(nutrition_df()$Value[nutrition_df()$NutrientID %in% c(605, 606)], na.rm = T)
    dv_df$Unit[2] <- "g"
    dv_df$pct_dv <- round(dv_df$Value / dv_df$DV, 3) * 100
    dv_df
  })
  
  output$macro_plot <- renderPlotly({
    df_macro <- dv_df() %>% filter(Group == "macronutrients")
    plot_macro <- ggplot(df_macro) + 
      geom_col(aes(x = Nutrient, y = pct_dv, fill = pct_dv)) +
      labs(x = "Nutrient", y = "% Daily Value") + 
      theme_gray() + 
      ylim(0, NA) +
      geom_hline(yintercept = 100) +
      scale_fill_gradient(low = "green",
                          high = "red", 
                          limits = c(0, 100),
                          na.value = "darkred",
                          name = "% Daily Value") +
      theme(panel.background = element_rect(fill = "mintcream"), 
            legend.position = "none") +
      scale_x_discrete(labels = c("Cholesterol", "Fat", "Fibre", "Sodium",  "Sugars", "Saturated and \n Trans Fats"))
    ggplotly(plot_macro)
  })
  
  # dt indicator for ingredients
  output$ing_df <- DT::renderDataTable(ing_df$df[,1:3], 
                                      # colnames = c("Quantity", "Units", "Ingredient"), 
                                       rownames=F, options = list(pageLength = 5))
  output$nutrient_table <- DT::renderDataTable(nutrient_table())
  
  ####### FIRST TAB######
  # Input Data
  datasetInputBmi <- reactive({  
    
    bmi <- input$weight/( (input$height/100) * (input$height/100) )
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    print(bmi)
    
  })
  
  datasetInputBmr <- reactive({
    if(input$gender == 1){
      bmr <- (10 * input$weight) + (6.25*input$height) - (5 * input$age)+5
    }
    else{
      bmr <- (10 * input$weight) + (6.25*input$height) - (5 * input$age)-161
    }
    bmr <- data.frame(bmr)
    names(bmr) <- "BMR"
    print(bmr)
  })
  
  datasetInputCal <- reactive({
    if(input$gender == 1){
      bmr <- (10 * input$weight) + (6.25*input$height) - (5 * input$age)+5
    }
    else{
      bmr <- (10 * input$weight) + (6.25*input$height) - (5 * input$age)-161
    }
    bmr <- data.frame(bmr)
    if(input$lifestyle == 1){
      calorie <- bmr*1.2
    } else if(input$lifestyle == 2){
      calorie <- bmr*1.375
    } else if(input$lifestyle == 3){
      calorie <- bmr*1.55
    } else if(input$lifestyle ==4){
      calorie <- bmr*1.725
    } else{
      calorie <- bmr*1.9
    }
    calorie <- data.frame(calorie)
    names(calorie) <- "Daily Calorie Requirement"
    print(calorie)
  })
  
  #Reactive Display for Height and Weight
  output$displayWeight <- renderText({ 
    paste("Your weight is ",
          input$weight," kg")
  })
  
  output$displayHeight <- renderText({ 
    paste("Your height is ",
          input$height," cm")
  })
  
  # Prediction results table
  output$tabledataBmi <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInputBmi()) 
    } 
  })
  
  # Prediction results table
  output$tabledataBmr <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInputBmr()) 
    } 
  })
  
  # Prediction results table
  output$tabledataCal <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInputCal()) 
    } 
  })
  
  
  ####### SECOND TAB######
  # Input Data
  datasetInputBmi2 <- reactive({  
    convertedHeight <- (input$ft * 12) + input$inch
    bmi2 <- (input$weightLbs/( convertedHeight * convertedHeight ))* 703
    bmi2 <- data.frame(bmi2)
    names(bmi2) <- "BMI"
    print(bmi2)
  })
  
  datasetInputBmr2 <- reactive({
    convertedHeight <- (input$ft * 12) + input$inch
    if(input$gender ==1){
      bmr2 <- (4.536 * input$weightLbs) + (15.88*convertedHeight) - (5 * input$age2)+5
    } else{
      bmr2 <- (4.536 * input$weightLbs) + (15.88*convertedHeight) - (5 * input$age2)-161
    }
    bmr2 <- data.frame(bmr2)
    names(bmr2) <- "BMR"
    print(bmr2)
  })
  
  datasetInputCal2 <- reactive({
    convertedHeight <- (input$ft * 12) + input$inch
    if(input$gender ==1){
      bmr2 <- (4.536 * input$weightLbs) + (15.88*convertedHeight) - (5 * input$age2)+5
    } else{
      bmr2 <- (4.536 * input$weightLbs) + (15.88*convertedHeight) - (5 * input$age2)-161
    }
    bmr2 <- data.frame(bmr2)
    if(input$lifestyle2 == 1){
      calorie2 <- bmr2*1.2
    } else if(input$lifestyle == 2){
      calorie2 <- bmr2*1.375
    } else if(input$lifestyle == 3){
      calorie2 <- bmr2*1.55
    } else if(input$lifestyle ==4){
      calorie2 <- bmr2*1.725
    } else{
      calorie2 <- bmr2*1.9
    }
    calorie2 <- data.frame(calorie2)
    names(calorie2) <- "Daily Calorie Requirement"
    print(calorie2)
  })
  
  # Prediction results table
  output$tabledataBmi2 <- renderTable({
    if (input$submitbutton2>0) { 
      isolate(datasetInputBmi2()) 
    } 
  })
  
  output$tabledataBmr2 <- renderTable({
    if (input$submitbutton2>0) { 
      isolate(datasetInputBmr2()) 
    } 
  })
  
  output$tabledataCal2 <- renderTable({
    if (input$submitbutton2>0) { 
      isolate(datasetInputCal2()) 
    } 
  })
  
  #Reactive Display for Height and Weight
  output$displayWeight2 <- renderText({ 
    paste("Your weight is ",
          input$weightLbs," lbs (pounds)")
  })
  
  output$displayHeight2 <- renderText({ 
    paste("Your height is ",
          input$ft," ft, ", input$inch, " in")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
  
