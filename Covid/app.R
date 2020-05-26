source("global.R")

ui <- dashboardPage(skin = "black",
    dashboardHeader(title = "Covid Colombia"),
    dashboardSidebar(
        sliderInput("Edad", "Selecciona tu Edad:",
                    min = 0, max = 100, value = 20, step = 1
        ),
        selectInput("sexo",label = "Selecciona tu genero:",choices = c("Hombre","Mujer"),selected = "Mujer"),
        selectInput("depto",label = "Selecciona tu Departamento de residencia:",choices = unique(datos_inter3$municipio)),
        selectInput("ciudad",label = "Selecciona tu Ciudad de residencia:",choices = NULL),
        selectInput("enfermedad",label = "Selecciona las enfermedades que actualmente tienes:",choices = c("Todas",unique(enfermedad$enfermedad),"Ninguna"),multiple = T,selected = "Ninguna"),
        br(),
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard"))
    ),
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                    shiny::fluidRow(
                        valueBoxOutput("casos",width = 3),
                        valueBoxOutput("recuperados",width = 3),
                        valueBoxOutput("fallecidos",width = 3),
                        valueBoxOutput("perfil",width = 3)
                    ),
                    shiny::fluidRow(
                        box(
                            width = 8, status = "primary", solidHeader = TRUE,
                            title = "Ubicación",plotlyOutput("mapa_1")
                        ),
                        uiOutput("texto")
                    )
            )
        )
    )
)

server <- function(input, output, session){
    
    observeEvent(input$depto,{
        
        updateSelectInput(session,inputId = "ciudad", 
                          choices = unique(datos_inter3$Ciudad[datos_inter3$municipio==input$depto]))  

        })
    
    
    
    datos_sum <- reactive({
        
        if(input$sexo=="Mujer"){
            
            data = datos_inter3 %>% filter(Edad == input$Edad,
                                           Sexo == "F",
                                           Ciudad == input$ciudad)}
        else{
            data = datos_inter3 %>% filter(Edad == input$Edad,
                                           Sexo == "M",
                                           Ciudad == input$ciudad)
        }
        data %>% group_by(atencion) %>% summarise(conteo = n(),
                                                  dias_recu = mean(dias_recup, na.rm = T),
                                                  dias_diag = mean(dias_diag_recup, na.rm = T))
        

    })
    
    total = reactive({
        
        datos_sum() %>% summarize(total = sum(conteo, na.rm = T))
        
    })
    
    output$casos <- renderValueBox({
        valueBox(
            value = total()$total,
            subtitle = "Casos a la Fecha",
            icon = icon("user"),
            width = 3,
            color = "teal")

    })
    
    recup = reactive({
        
        muestra = datos_sum() %>% filter(atencion=="recuperado") %>% select(conteo)
        
        if(nrow(muestra)>0) {
         casos = muestra
         casos = round(casos/total()$total*100,2)
        }
        else{
            casos = 0
            
            }
        casos
    })
    
    output$recuperados <- renderValueBox({
        valueBox(value = paste0(recup(),"%"),
                 subtitle = "Porcentaje Recuperados",
                 icon = icon("laugh-beam"),
                 width = 3,
                 color = "teal")
        
    })
    
    falle = reactive({
        
        muestra = datos_sum() %>% filter(atencion=="fallecido") %>% select(conteo)
        
        if(nrow(muestra)>0) {
            casos = muestra
            casos = round(casos/total()$total*100,2)
        }
        else{
            casos = 0 
        }
        casos
    })
        
    
    output$fallecidos <- renderValueBox({
        valueBox(value = paste0(falle(),"%"),
                 subtitle = "Porcentaje Fallecidos",
                 icon = icon("dizzy"),
                 width = 3,
                 color = "teal")
        
    })
    
    perfil = reactive({
        
        if(input$enfermedad == "Todas") {
            enfe = enfermedad
            enfe = sum(enfe$riesgo)
        }
        else if(input$enfermedad == "Ninguna"){
            enfe = 0
        }
        else{
        
            enfe = enfermedad %>% filter(enfermedad %in% input$enfermedad)
            enfe = sum(enfe$riesgo)
        }
        
        perfil = ifelse(recup()>falle()&enfe<0.15,"Riesgo Bajo",
                        ifelse(recup()>falle()&enfe>0.15,"Riesgo Medio","Riesgo Alto"))
        perfil
    })
    
    output$perfil <- renderValueBox({
        valueBox(width = 3,
                 value = perfil(),
                 subtitle = "Perfil de Riesgo",
                 icon = icon("address-book"),
                 color = "teal"
        )
    })
    
    
    mapa_read <- reactive({
        
        mapa %>% filter(municipio %in% input$depto)
        
    })
    
    output$mapa_1 <- renderPlotly({
        
        mapColmun(mapa_read())
    })
    
    
    bueno_malo = reactive({
        
        if(total()$total<30){
            
            p("Sin embargo, la cantidad de casos reportados en tu Ciudad sigue siendo bajo.")
        }
        else{
            p("Sin embargo, la cantidad de casos reportados en tu Ciudad es considerablemente alto.")
        }
        
    })
    
    output$texto <- renderUI({
       
        box(
            width = 4, status = "primary",solidHeader = TRUE,
            title = "Detalle",
            p("Según tus datos y el último reporte disponible del INS (Instituto Nacional de Salud) tienes un ",a(perfil(),target="_blank"),
              "de morir por COVID-19.",a(bueno_malo(),target="_blank"),".")
        )
    
    }) 
    
    
}

shiny::shinyApp(ui = ui, server = server)
