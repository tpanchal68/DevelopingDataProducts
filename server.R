# This is the server logic for a Shiny web application.

library(shiny)
library(ggplot2)
library(data.table)
require(reshape2)

# Define server logic for output
shinyServer(function(input, output, session) {
        
        ### College Info ###
        AboutCollegeInfo <- reactive({
                college_data <- subset(CollegeScorecardData, INSTNM == input$college1In | INSTNM == input$college2In | INSTNM == input$college3In)
                AboutCollege_table <- data.frame(
                        "College Name" = college_data$INSTNM,
                        "City" = college_data$CITY,
                        "State" = college_data$STABBR,
                        "Zipcode" = college_data$ZIP,
                        "Accreditor for institution" = college_data$AccredAgency,
                        "URL for institution's homepage" = college_data$INSTURL,
                        "URL for institution's net price calculator" = college_data$NPCURL
                )
                # Remove column with NA values
                AboutCollege_table <- AboutCollege_table[,colSums(is.na(AboutCollege_table))<nrow(AboutCollege_table)]
                return(AboutCollege_table)
        })
        output$AboutCollege = renderDataTable({
                AboutCollegeInfo()
        })
        
        ### SatAct Info ###
        SatActInfo <- reactive({
                college_data <- subset(CollegeScorecardData, INSTNM == input$college1In | INSTNM == input$college2In | INSTNM == input$college3In)
                SatAct_table <- data.frame(
                        "College Name" = college_data$INSTNM,
                        "Admission Rate" = paste0(round(as.numeric(as.character(college_data$ADM_RATE)) * 100),"%"),
                        "SAT Average" = as.numeric(as.character(college_data$SAT_AVG)),
                        "SAT Critical Reading 25 Percentile" = as.numeric(as.character(college_data$SATVR25)),
                        "SAT Critical Reading 75 Percentile" = as.numeric(as.character(college_data$SATVR75)),
                        "SAT Math 25 Percentile" = as.numeric(as.character(college_data$SATMT25)),
                        "SAT Math 75 Percentile" = as.numeric(as.character(college_data$SATMT75)),
                        "SAT Writing 25 Percentile" = as.numeric(as.character(college_data$SATWR25)),
                        "SAT Writing 75 Percentile" = as.numeric(as.character(college_data$SATWR75)),
                        "ACT cumulative 25 Percentile" = as.numeric(as.character(college_data$ACTCM25)),
                        "ACT cumulative 75 Percentile" = as.numeric(as.character(college_data$ACTCM75)),
                        "ACT English 25 Percentile" = as.numeric(as.character(college_data$ACTEN25)),
                        "ACT English 75 Percentile" = as.numeric(as.character(college_data$ACTEN75)),
                        "ACT Math 25 Percentile" = as.numeric(as.character(college_data$ACTMT25)),
                        "ACT Math 75 Percentile" = as.numeric(as.character(college_data$ACTMT75)),
                        "ACT Writing 25 Percentile" = as.numeric(as.character(college_data$ACTWR25)),
                        "ACT Writing 75 Percentile" = as.numeric(as.character(college_data$ACTCMMID))
                )
                # Remove column with NA values
                #SatAct_table <- SatAct_table[,colSums(is.na(SatAct_table))<nrow(SatAct_table)]
                return(SatAct_table)
        })
        output$SatAct = renderDataTable({
                SatActInfo()
        })
        
        ### Field of Study Info ###
        FieldsOfferedInfo <- reactive({
                college_data <- subset(CollegeScorecardData, INSTNM == input$college1In | INSTNM == input$college2In | INSTNM == input$college3In)
                FieldsOffered_table <- data.frame(
                        "College Name" = college_data$INSTNM,
                        "Associate in Agriculture" = as.numeric(as.character(college_data$CIP01ASSOC)),
                        "BS in Agriculture" = as.numeric(as.character(college_data$CIP01BACHL)),
                        "Associate in Natural Resources" = as.numeric(as.character(college_data$CIP03ASSOC)),
                        "BS in Natural Resources" = as.numeric(as.character(college_data$CIP03BACHL)),
                        "Associate in Architecture" = as.numeric(as.character(college_data$CIP04ASSOC)),
                        "BS in Architecture" = as.numeric(as.character(college_data$CIP04BACHL)),
                        "Associate in Ethnicity and Cultural" = as.numeric(as.character(college_data$CIP05ASSOC)),
                        "BS in Ethnicity and Cultural" = as.numeric(as.character(college_data$CIP05BACHL)),
                        "Associate in Communication and Journalism" = as.numeric(as.character(college_data$CIP09ASSOC)),
                        "BS in Communication and Journalism" = as.numeric(as.character(college_data$CIP09BACHL)),
                        "Associate in Communications Technologies" = as.numeric(as.character(college_data$CIP10ASSOC)),
                        "BS in Communications Technologies" = as.numeric(as.character(college_data$CIP10BACHL)),
                        "Associate in Computer and Information Sciences" = as.numeric(as.character(college_data$CIP11ASSOC)),
                        "BS in Computer and Information Sciences" = as.numeric(as.character(college_data$CIP11BACHL)),
                        "Associate in Personal and Culinary" = as.numeric(as.character(college_data$CIP12ASSOC)),
                        "BS in Personal and Culinary" = as.numeric(as.character(college_data$CIP12BACHL)),
                        "Associate in Education" = as.numeric(as.character(college_data$CIP13ASSOC)),
                        "BS in Education" = as.numeric(as.character(college_data$CIP13BACHL)),
                        "Associate in Engineering" = as.numeric(as.character(college_data$CIP14ASSOC)),
                        "BS in Engineering" = as.numeric(as.character(college_data$CIP14BACHL)),
                        "Associate in Engineering Technologies" = as.numeric(as.character(college_data$CIP15ASSOC)),
                        "BS in Engineering Technologies" = as.numeric(as.character(college_data$CIP15BACHL)),
                        "Associate in Foreign Languages and Literatures" = as.numeric(as.character(college_data$CIP16ASSOC)),
                        "BS in Foreign Languages and Literatures" = as.numeric(as.character(college_data$CIP16BACHL)),
                        "Associate in Family and Consumer Sciences" = as.numeric(as.character(college_data$CIP19ASSOC)),
                        "BS in Family and Consumer Sciences" = as.numeric(as.character(college_data$CIP19BACHL)),
                        "Associate in Legal" = as.numeric(as.character(college_data$CIP22ASSOC)),
                        "BS in Legal" = as.numeric(as.character(college_data$CIP22BACHL)),
                        "Associate in English Language and Literature" = as.numeric(as.character(college_data$CIP23ASSOC)),
                        "BS in English Language and Literature" = as.numeric(as.character(college_data$CIP23BACHL)),
                        "Associate in Liberal Arts and Sciences" = as.numeric(as.character(college_data$CIP24ASSOC)),
                        "BS in Liberal Arts and Sciences" = as.numeric(as.character(college_data$CIP24BACHL)),
                        "Associate in Library Science" = as.numeric(as.character(college_data$CIP25ASSOC)),
                        "BS in Library Science" = as.numeric(as.character(college_data$CIP25BACHL)),
                        "Associate in Biological and Biomedical" = as.numeric(as.character(college_data$CIP26ASSOC)),
                        "BS in Biological and Biomedical" = as.numeric(as.character(college_data$CIP26BACHL)),
                        "Associate in Mathematics and Statistics" = as.numeric(as.character(college_data$CIP27ASSOC)),
                        "BS in Mathematics and Statistics" = as.numeric(as.character(college_data$CIP27BACHL)),
                        "Associate in Military Technologies" = as.numeric(as.character(college_data$CIP29ASSOC)),
                        "BS in Military Technologies" = as.numeric(as.character(college_data$CIP29BACHL)),
                        "Associate in Interdisciplinary Studies" = as.numeric(as.character(college_data$CIP30ASSOC)),
                        "BS in Interdisciplinary Studies" = as.numeric(as.character(college_data$CIP30BACHL)),
                        "Associate in Parks and Recreation" = as.numeric(as.character(college_data$CIP31ASSOC)),
                        "BS in Parks and Recreation" = as.numeric(as.character(college_data$CIP31BACHL)),
                        "Associate in Philosophy" = as.numeric(as.character(college_data$CIP38ASSOC)),
                        "BS in Philosophy" = as.numeric(as.character(college_data$CIP38BACHL)),
                        "Associate in Theology" = as.numeric(as.character(college_data$CIP39ASSOC)),
                        "BS in Theology" = as.numeric(as.character(college_data$CIP39BACHL)),
                        "Associate in Physical Sciences" = as.numeric(as.character(college_data$CIP40ASSOC)),
                        "BS in Physical Sciences" = as.numeric(as.character(college_data$CIP40BACHL)),
                        "Associate in Science Technologies" = as.numeric(as.character(college_data$CIP41ASSOC)),
                        "BS in Science Technologies" = as.numeric(as.character(college_data$CIP41BACHL)),
                        "Associate in Psychology" = as.numeric(as.character(college_data$CIP42ASSOC)),
                        "BS in Psychology" = as.numeric(as.character(college_data$CIP42BACHL)),
                        "Associate in Homeland Security, Law Enforcement" = as.numeric(as.character(college_data$CIP43ASSOC)),
                        "BS in Homeland Security, Law Enforcement" = as.numeric(as.character(college_data$CIP43BACHL)),
                        "Associate in Public Administration" = as.numeric(as.character(college_data$CIP44ASSOC)),
                        "BS in Public Administration" = as.numeric(as.character(college_data$CIP44BACHL)),
                        "Associate in Social Sciences" = as.numeric(as.character(college_data$CIP45ASSOC)),
                        "BS in Social Sciences" = as.numeric(as.character(college_data$CIP45BACHL)),
                        "Associate in Construction Trades" = as.numeric(as.character(college_data$CIP46ASSOC)),
                        "BS in Construction Trades" = as.numeric(as.character(college_data$CIP46BACHL)),
                        "Associate in Mechanic and Repair" = as.numeric(as.character(college_data$CIP47ASSOC)),
                        "BS in Mechanic and Repair" = as.numeric(as.character(college_data$CIP47BACHL)),
                        "Associate in Precision Production" = as.numeric(as.character(college_data$CIP48ASSOC)),
                        "BS in Precision Production" = as.numeric(as.character(college_data$CIP48BACHL)),
                        "Associate in Transportation and Materials Moving" = as.numeric(as.character(college_data$CIP49ASSOC)),
                        "BS in Transportation and Materials Moving" = as.numeric(as.character(college_data$CIP49BACHL)),
                        "Associate in Visual and Performing Arts" = as.numeric(as.character(college_data$CIP50ASSOC)),
                        "BS in Visual and Performing Arts" = as.numeric(as.character(college_data$CIP50BACHL)),
                        "Associate in Health Professions" = as.numeric(as.character(college_data$CIP51ASSOC)),
                        "BS in Health Professions" = as.numeric(as.character(college_data$CIP51BACHL)),
                        "Associate in Business, Management" = as.numeric(as.character(college_data$CIP52ASSOC)),
                        "BS in Business, Management" = as.numeric(as.character(college_data$CIP52BACHL)),
                        "Associate in History" = as.numeric(as.character(college_data$CIP54ASSOC)),
                        "BS in History" = as.numeric(as.character(college_data$CIP54BACHL))
                )
                # Remove column with NA values and if all elements are zero
                FieldsOffered_table <- FieldsOffered_table[,colSums(is.na(FieldsOffered_table))<nrow(FieldsOffered_table)]
                FieldsOffered_table <- FieldsOffered_table[, colSums(FieldsOffered_table != 0) > 0]
                return(FieldsOffered_table)
        })
        output$FieldsOffered = renderDataTable({
                FieldsOffered_table <- FieldsOfferedInfo()
        })
        
        ### Graduation Rate by Field Info ###
        GradByFieldInfo <- reactive({
                college_data <- subset(CollegeScorecardData, INSTNM == input$college1In | INSTNM == input$college2In | INSTNM == input$college3In)
                GradByField_table <- data.frame(
                        "College Name" = college_data$INSTNM,
                        "Agriculture" = as.numeric(as.character(college_data$PCIP01)),
                        "Natural Resources" = as.numeric(as.character(college_data$PCIP03)),
                        "Architecture" = as.numeric(as.character(college_data$PCIP04)),
                        "Ethnicity and Cultural" = as.numeric(as.character(college_data$PCIP05)),
                        "Communication and Journalism" = as.numeric(as.character(college_data$PCIP09)),
                        "Communications Technologies" = as.numeric(as.character(college_data$PCIP10)),
                        "Computer and Information Sciences" = as.numeric(as.character(college_data$PCIP11)),
                        "Personal and Culinary" = as.numeric(as.character(college_data$PCIP12)),
                        "Education" = as.numeric(as.character(college_data$PCIP13)),
                        "Engineering" = as.numeric(as.character(college_data$PCIP14)),
                        "Engineering Technologies" = as.numeric(as.character(college_data$PCIP15)),
                        "Foreign Languages and Literatures" = as.numeric(as.character(college_data$PCIP16)),
                        "Family and Consumer Sciences" = as.numeric(as.character(college_data$PCIP19)),
                        "Legal" = as.numeric(as.character(college_data$PCIP22)),
                        "English Language and Literature" = as.numeric(as.character(college_data$PCIP23)),
                        "Liberal Arts and Sciences" = as.numeric(as.character(college_data$PCIP24)),
                        "Library Science" = as.numeric(as.character(college_data$PCIP25)),
                        "Biological and Biomedical" = as.numeric(as.character(college_data$PCIP26)),
                        "Mathematics and Statistics" = as.numeric(as.character(college_data$PCIP27)),
                        "Military Technologies" = as.numeric(as.character(college_data$PCIP29)),
                        "Interdisciplinary Studies" = as.numeric(as.character(college_data$PCIP30)),
                        "Parks and Recreation" = as.numeric(as.character(college_data$PCIP31)),
                        "Philosophy" = as.numeric(as.character(college_data$PCIP38)),
                        "Theology" = as.numeric(as.character(college_data$PCIP39)),
                        "Physical Sciences" = as.numeric(as.character(college_data$PCIP40)),
                        "Science Technologies" = as.numeric(as.character(college_data$PCIP41)),
                        "Psychology" = as.numeric(as.character(college_data$PCIP42)),
                        "Homeland Security, Law Enforcement" = as.numeric(as.character(college_data$PCIP43)),
                        "Public Administration" = as.numeric(as.character(college_data$PCIP44)),
                        "Social Sciences" = as.numeric(as.character(college_data$PCIP45)),
                        "Construction Trades" = as.numeric(as.character(college_data$PCIP46)),
                        "Mechanic and Repair" = as.numeric(as.character(college_data$PCIP47)),
                        "Precision Production" = as.numeric(as.character(college_data$PCIP48)),
                        "Transportation and Materials Moving" = as.numeric(as.character(college_data$PCIP49)),
                        "Visual and Performing Arts" = as.numeric(as.character(college_data$PCIP50)),
                        "Health Professions" = as.numeric(as.character(college_data$PCIP51)),
                        "Business, Management" = as.numeric(as.character(college_data$PCIP52)),
                        "History" = as.numeric(as.character(college_data$PCIP54))
                )
                # Remove column with NA values and if all elements are zero
                GradByField_table <- GradByField_table[,colSums(is.na(GradByField_table))<nrow(GradByField_table)]
                GradByField_table <- GradByField_table[, colSums(GradByField_table != 0) > 0]
                return(GradByField_table)
        })
        output$GradByField = renderPlot({
                GradByField_table <- GradByFieldInfo()
                
                # Reshape data to all the headers in one column and data in other
                GradByField_table_reshape <- melt(GradByField_table, "College.Name")
                
                # Convert values into decimal
                GradByField_table_reshape$value <- round(GradByField_table_reshape$value * 100)
                
                # Change Column Names
                colnames(GradByField_table_reshape) <- c("College", "Field", "Percentage")
                
                # After reshape and rounding, some data has all percentage as zero.  Let's remove them
                uniqueVal <- unique(GradByField_table_reshape$Field)
                for(i in 1:length(uniqueVal)) {
                        tempVar <- uniqueVal[i]
                        # print(tempVar)
                        temp_dt <- subset(GradByField_table_reshape, GradByField_table_reshape$Field == tempVar)
                        print(temp_dt)
                        if (sum(temp_dt$Percentage) == 0){
                                GradByField_table_reshape <- GradByField_table_reshape[GradByField_table_reshape$Field != tempVar | GradByField_table_reshape$Percentage != 0, ]
                        }
                }
                
                #Create plot
                GradByFieldPlot <- ggplot(
                        data = GradByField_table_reshape,
                        aes(
                                x=Field,
                                y=Percentage,
                                fill = factor(College)
                        )
                )
                GradByFieldPlot <- GradByFieldPlot + 
                        geom_bar(stat="identity", position=position_dodge()) +
                        geom_text(aes(y=Percentage + 0.2, ymax=Percentage + 10, label=Percentage), position= position_dodge(width=0.9), hjust=0, vjust=0, color="black", size = 4, check_overlap = TRUE) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5), colour = "blue")) +
                        scale_y_continuous("% of Total Graduates") +
                        # coord_flip() +
                        ggtitle("Graduates by Field of Education")
                
                print(GradByFieldPlot)                 
        })
        
        ### Cost Info ###
        CostsInfo <- reactive({
                college_data <- subset(CollegeScorecardData, INSTNM == input$college1In | INSTNM == input$college2In | INSTNM == input$college3In)
                Costs_table <- data.frame(
                        "College Name" = college_data$INSTNM,
                        "Academic Year Cost" = as.numeric(as.character(college_data$COSTT4_A)),
                        "Program Year Cost" = as.numeric(as.character(college_data$COSTT4_P)),
                        "In State Tuition" = as.numeric(as.character(college_data$TUITIONFEE_IN)),
                        "Out of State Tuition" = as.numeric(as.character(college_data$TUITIONFEE_OUT)),
                        "Program Year Tuition and fees" = as.numeric(as.character(college_data$TUITIONFEE_PROG))
                )
                # Remove column with NA values
                Costs_table <- Costs_table[,colSums(is.na(Costs_table))<nrow(Costs_table)]
                return(Costs_table)
        })
        output$Costs = renderDataTable({
                CostsInfo()
        })
        
        ### Student Body Info ###
        StudentBodyInfo <- reactive({
                college_data <- subset(CollegeScorecardData, INSTNM == input$college1In | INSTNM == input$college2In | INSTNM == input$college3In)
                StudentBody_table <- data.frame(
                        "College Name" = college_data$INSTNM,
                        "WHITE" = as.numeric(as.character(college_data$UGDS_WHITE)),
                        "BLACK" = as.numeric(as.character(college_data$UGDS_BLACK)),
                        "HISP" = as.numeric(as.character(college_data$UGDS_HISP)),
                        "ASIAN" = as.numeric(as.character(college_data$UGDS_ASIAN)),
                        "AIAN" = as.numeric(as.character(college_data$UGDS_AIAN)),
                        "NHPI" = as.numeric(as.character(college_data$UGDS_NHPI)),
                        "2MOR" = as.numeric(as.character(college_data$UGDS_2MOR)),
                        "NRA" = as.numeric(as.character(college_data$UGDS_NRA)),
                        "Unknown" = as.numeric(as.character(college_data$UGDS_UNKN)),
                        "WHITENH" = as.numeric(as.character(college_data$UGDS_WHITENH)),
                        "BLACKNH" = as.numeric(as.character(college_data$UGDS_BLACKNH)),
                        "API" = as.numeric(as.character(college_data$UGDS_API)),
                        "AIANOld" = as.numeric(as.character(college_data$UGDS_AIANOld)),
                        "HISPOld" = as.numeric(as.character(college_data$UGDS_HISPOld))
                )
                # Remove column with NA values and if all elements are zero
                StudentBody_table <- StudentBody_table[,colSums(is.na(StudentBody_table))<nrow(StudentBody_table)]
                StudentBody_table <- StudentBody_table[, colSums(StudentBody_table != 0) > 0]
                return(StudentBody_table)
        })
        output$StudentBody = renderPlot({
                StudentBody_table <- StudentBodyInfo()
                
                # Reshape data to all the headers in one column and data in other
                StudentBody_table_reshape <- melt(StudentBody_table, "College.Name")
                
                # Convert values into decimal
                StudentBody_table_reshape$value <- round(StudentBody_table_reshape$value * 100)
                
                # Change Column Names
                colnames(StudentBody_table_reshape) <- c("College", "StudentBodyByRace", "Percentage")
                
                # After reshape and rounding, some data has all percentage as zero.  Let's remove them
                uniqueVal <- unique(StudentBody_table_reshape$StudentBodyByRace)
                for(i in 1:length(uniqueVal)) {
                        tempVar <- uniqueVal[i]
                        # print(tempVar)
                        temp_dt <- subset(StudentBody_table_reshape, StudentBody_table_reshape$StudentBodyByRace == tempVar)
                        print(temp_dt)
                        if (sum(temp_dt$Percentage) == 0){
                                StudentBody_table_reshape <- StudentBody_table_reshape[StudentBody_table_reshape$StudentBodyByRace != tempVar | StudentBody_table_reshape$Percentage != 0, ]
                        }
                }
                
                #Create plot
                StudentBodyPlot <- ggplot(
                        data = StudentBody_table_reshape,
                        aes(
                                x=StudentBodyByRace,
                                y=Percentage,
                                fill = factor(College)
                        )
                )
                StudentBodyPlot <- StudentBodyPlot + 
                        geom_bar(stat="identity", position=position_dodge()) +
                        geom_text(aes(y=Percentage + 0.2, ymax=Percentage + 10, label=Percentage), position= position_dodge(width=0.9), hjust=0, vjust=0, color="black", size = 4, check_overlap = TRUE) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5), colour = "blue")) +
                        scale_y_continuous("Student Distribution %") +
                        ggtitle("Student Distribution by Ethnicity")
                
                print(StudentBodyPlot)
        })
        
        ### College Completion Info ###
        CompletionInfo <- reactive({
                college_data <- subset(CollegeScorecardData, INSTNM == input$college1In | INSTNM == input$college2In | INSTNM == input$college3In)
                Completion_table <- data.frame(
                        "College Name" = college_data$INSTNM,
                        "NRA" = as.numeric(as.character(college_data$C150_4_NRA)),
                        "API" = as.numeric(as.character(college_data$C150_4_API)),
                        "HISP" = as.numeric(as.character(college_data$C150_4_HISP)),
                        "AIAN" = as.numeric(as.character(college_data$C150_4_AIAN)),
                        "NHPI" = as.numeric(as.character(college_data$C150_4_NHPI)),
                        "2MOR" = as.numeric(as.character(college_data$C150_4_2MOR)),
                        "UNKN" = as.numeric(as.character(college_data$C150_4_UNKN)),
                        "CC NRA" = as.numeric(as.character(college_data$C150_L4_NRA)),
                        "CC API" = as.numeric(as.character(college_data$C150_L4_API)),
                        "WHITE" = as.numeric(as.character(college_data$C150_4_WHITE)),
                        "BLACK" = as.numeric(as.character(college_data$C150_4_BLACK)),
                        "ASIAN" = as.numeric(as.character(college_data$C150_4_ASIAN)),
                        "CC HISP" = as.numeric(as.character(college_data$C150_L4_HISP)),
                        "CC AIAN" = as.numeric(as.character(college_data$C150_L4_AIAN)),
                        "CC NHPI" = as.numeric(as.character(college_data$C150_L4_NHPI)),
                        "CC 2MOR" = as.numeric(as.character(college_data$C150_L4_2MOR)),
                        "CC UNKN" = as.numeric(as.character(college_data$C150_L4_UNKN)),
                        "POOLED" = as.numeric(as.character(college_data$C150_4_POOLED)),
                        "CC WHITE" = as.numeric(as.character(college_data$C150_L4_WHITE)),
                        "CC BLACK" = as.numeric(as.character(college_data$C150_L4_BLACK)),
                        "CC ASIAN" = as.numeric(as.character(college_data$C150_L4_ASIAN)),
                        "CC POOLED" = as.numeric(as.character(college_data$C150_L4_POOLED)),
                        "WHITENH" = as.numeric(as.character(college_data$C150_4_WHITENH)),
                        "BLACKNH" = as.numeric(as.character(college_data$C150_4_BLACKNH)),
                        "AIANOld" = as.numeric(as.character(college_data$C150_4_AIANOld)),
                        "HISPOld" = as.numeric(as.character(college_data$C150_4_HISPOld)),
                        "CC WHITENH" = as.numeric(as.character(college_data$C150_L4_WHITENH)),
                        "CC BLACKNH" = as.numeric(as.character(college_data$C150_L4_BLACKNH)),
                        "CC AIANOld" = as.numeric(as.character(college_data$C150_L4_AIANOld)),
                        "CC HISPOld" = as.numeric(as.character(college_data$C150_L4_HISPOld)),
                        "POOLED_SUPP" = as.numeric(as.character(college_data$C150_4_POOLED_SUPP)),
                        "CC POOLED_SUPP" = as.numeric(as.character(college_data$C150_L4_POOLED_SUPP))
                )
                # Remove column with NA values and if all elements are zero
                Completion_table <- Completion_table[,colSums(is.na(Completion_table))<nrow(Completion_table)]
                # If we still have NA left, replace it with 0
                Completion_table[is.na(Completion_table)] <- 0
                return(Completion_table)
        })
        output$Completion = renderPlot({
                Completion_table <- CompletionInfo()
                
                # Reshape data to all the headers in one column and data in other
                Completion_table_reshape <- melt(Completion_table, "College.Name")
                
                # Convert values into decimal
                Completion_table_reshape$value <- round(Completion_table_reshape$value * 100)
                
                # Change Column Names
                colnames(Completion_table_reshape) <- c("College", "StudentBodyByRace", "Percentage")
                
                # After reshape and rounding, some data has all percentage as zero.  Let's remove them
                uniqueVal <- unique(Completion_table_reshape$StudentBodyByRace)
                for(i in 1:length(uniqueVal)) {
                        tempVar <- uniqueVal[i]
                        # print(tempVar)
                        temp_dt <- subset(Completion_table_reshape, Completion_table_reshape$StudentBodyByRace == tempVar)
                        print(temp_dt)
                        if (sum(temp_dt$Percentage) == 0){
                                Completion_table_reshape <- Completion_table_reshape[Completion_table_reshape$StudentBodyByRace != tempVar | Completion_table_reshape$Percentage != 0, ]
                        }
                }
                
                #Create plot
                CompletionPlot <- ggplot(
                        data = Completion_table_reshape,
                        aes(
                                x=StudentBodyByRace,
                                y=Percentage,
                                fill = factor(College)
                        )
                )
                CompletionPlot <- CompletionPlot + 
                        geom_bar(stat="identity", position=position_dodge()) +
                        geom_text(aes(y=Percentage + 0.2, ymax=Percentage + 10, label=Percentage), position= position_dodge(width=0.9), hjust=0, vjust=0, color="black", size = 4, check_overlap = TRUE) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(1.5), colour = "blue")) +
                        scale_y_continuous("Degree Completion %") +
                        ggtitle("Degree Completion by Ethnicity")
                
                print(CompletionPlot)           
        })
        
})