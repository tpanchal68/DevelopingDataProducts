# This is the user-interface definition of a Shiny web application.
#

library(shiny)
library(ggplot2)

shinyUI(
        fluidPage(
                titlePanel("College Comparison for Prospective Students"),
                br(),
                # Create a new Row in the UI for selectInputs
                sidebarPanel(
                        h4("User Input Area", align="center"),
                        helpText("Provide following information to compare up to 3 colleges"),
                        hr(),
                        helpText("Click Help tab if you need additional help with the program."),
                        hr(),
                        selectizeInput(
                                "college1In",
                                "College 1:",
                                choices = c(unique(as.character(CollegeScorecardData$INSTNM))),
                                options = list(
                                        placeholder = 'Please select an option below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                )
                        ),
                        selectizeInput(
                                "college2In",
                                "College 2:",
                                choices = c(unique(as.character(CollegeScorecardData$INSTNM))),
                                options = list(
                                        placeholder = 'Please select an option below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                )
                        ),
                        selectizeInput(
                                "college3In",
                                "College 3:",
                                choices = c(unique(as.character(CollegeScorecardData$INSTNM))),
                                options = list(
                                        placeholder = 'Please select an option below',
                                        onInitialize = I('function() { this.setValue(""); }')
                                )
                        ),
                        submitButton("Compare")
                ),
                # Create tabs view for each group of the table.
                mainPanel(
                        h4("Display Comparison", align="center"),
                        br(),
                        tabsetPanel(
                                tabPanel(
                                        p(icon("table"), "About College"),
                                        dataTableOutput("AboutCollege")
                                ),
                                tabPanel(
                                        p(icon("table"), "SAT/ACT Details"),
                                        helpText("Displays SAT/ACT Details for entered College(s)."),
                                        dataTableOutput("SatAct")
                                ),
                                tabPanel(
                                        p(icon("table"), "Fields Offered"),
                                        helpText("Displays Fields being offered by entered College(s)."),
                                        h5("0 = Program NOT Offered"),
                                        h5("1 = Program Offered"),
                                        h5("2 = Distance Learning"),
                                        br(),
                                        dataTableOutput("FieldsOffered")
                                ),
                                tabPanel(
                                        p(icon("bar-chart-o"), "Graduation by Field"),
                                        helpText("Displays graduation rate by field chart for entered College(s)."),
                                        plotOutput("GradByField", width = "130%", height = "500px")
                                ),
                                tabPanel(
                                        p(icon("bar-chart-o"), "Student Body"),
                                        helpText("Displays student distribution by ethnicity chart for entered College(s)."),
                                        plotOutput("StudentBody", width = "100%")
                                ),
                                tabPanel(
                                        p(icon("bar-chart-o"), "Degree Completion"),
                                        helpText("Displays degree completion by ethnicity chart for entered College(s)."),
                                        plotOutput("Completion", width = "100%")
                                ),
                                tabPanel(
                                        p(icon("table"), "Costs"),
                                        helpText("Displays costs for entered College(s)."),
                                        dataTableOutput("Costs")
                                ),
                                tabPanel(
                                        p(icon("list-alt"), "Help"),
                                        includeMarkdown("help.md")
                                )
                        )
                )
        )
)
