options(shiny.maxRequestSize = 50*1024^2)
options(shiny.http.response.timeout = 600)

server <- function(input, output, session) {
  
  shinyalert(
    title = "Welcome to the sphereML v0.1.0",
    text = "
    If you found sphereML useful please: <br><br>
    <b> <a href = https://data.mendeley.com/datasets/88d7m2fv7p/1> Cite the original paper </a></b><br><br>

    Additional servers are now available: <br><br>
    <a href = https://santosoph.shinyapps.io/sphereML/> Link 1 </a></b><br>
    ",
    size = "s",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = T,
    type = "success",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  
  get("demographic")
  get("literacy")
  get("physicsidentity")
  get("teachersjudgment")
  get("FCI")
  get("FMCE")
  get("RRMCS")
  get("FMCI")
  get("MWCS")
  get("TCE")
  get("STPFASL")
  get("SAAR")
  get("CLASS")
  
  get("FCIcontentvalidity")
  get("FMCEcontentvalidity")
  get("RRMCScontentvalidity")
  get("FMCIcontentvalidity")
  get("MWCScontentvalidity")
  get("TCEcontentvalidity")
  get("STPFASLcontentvalidity")
  get("SAARcontentvalidity")
  get("CLASScontentvalidity")
  
  get("FCIkey")
  get("FMCEkey")
  get("RRMCSkey")
  get("FMCIkey")
  get("MWCSkey")
  get("TCEkey")
  get("STPFASLkey")
  
  output$table1 <- renderDT(datatable(demographic, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table2 <- renderDT(datatable(literacy, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table3 <- renderDT(datatable(physicsidentity, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table4 <- renderDT(datatable(teachersjudgment, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table5 <- renderDT(datatable(FCI, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table6 <- renderDT(datatable(FMCE, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table7 <- renderDT(datatable(RRMCS, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table8 <- renderDT(datatable(FMCI, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table9 <- renderDT(datatable(MWCS, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table10 <- renderDT(datatable(TCE, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table11 <- renderDT(datatable(STPFASL, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table12 <- renderDT(datatable(SAAR, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  output$table13 <- renderDT(datatable(CLASS, class = "display nowrap compact", rownames = FALSE, options = list(scrollX = TRUE, lengthChange = TRUE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all")))))
  
  calc_conval <- function(data,index){
    if (index == "aikenV") {
      spheredata::aikenV(data[,-1])
    } else if (index == "lawshecvr") {
      spheredata::lawsheCVR(data[,-1])
    } 
  }
  
  conval_data <- reactive({
    if (input$convaldata_choice == ""){
    return(NULL)
    }
    switch(input$convaldata_choice,
           "table14" = FCIcontentvalidity,
           "table15" = FMCEcontentvalidity,
           "table16" = RRMCScontentvalidity,
           "table17" = FMCIcontentvalidity,
           "table18" = MWCScontentvalidity,
           "table19" = TCEcontentvalidity,
           "table20" = STPFASLcontentvalidity,
           "table21" = SAARcontentvalidity,
           "table22" = CLASScontentvalidity)
  })
  
  output$output_table_conval <- renderDataTable({
    df_conval <- conval_data()
    conval <- input$conval_choice

    if (is.null(df_conval) || conval == "") {
      return (data.frame(Message = "You should choose a data and a content validity index first."))
    }

    conval_result <- calc_conval(df_conval, conval)
    return(conval_result)}, 
    
    options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  

  factan_data <- reactive({
    if (input$factandata_choice == ""){
      return(NULL)
    }
    switch(input$factandata_choice,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey),
           "table12" = SAAR,
           "table13" = CLASS)
  })

  output$factan_var_select_ui <- renderUI({
    req(factan_data())
    selectInput("factan_selected_vars", "Select Items for CFA:", 
                choices = names(factan_data()), 
                selected = names(factan_data()), 
                multiple = TRUE, width = 500)
  })
  
  factan_spec_output <- observeEvent(input$factandata_choice, {
    req(factan_data())
    
    
    updateTextAreaInput(session, "factan_spec",
                        value = switch(input$factandata_choice,

    "table23" = paste(deparse(
        c("efa(\"efa\")*f1 =~ FCI1 + FCI2 + FCI3 + FCI4 + FCI5 + FCI6 + FCI7 + FCI8 + FCI9 + FCI10 + FCI11 + FCI12 + FCI13 + FCI14 + FCI15 + FCI16 + FCI17 + FCI18 + FCI19 + FCI20 + FCI21 + FCI22 + FCI23 + FCI24 + FCI25 + FCI26 + FCI27 + FCI28 + FCI29 + FCI30", 
          "efa(\"efa\")*f2 =~ FCI1 + FCI2 + FCI3 + FCI4 + FCI5 + FCI6 + FCI7 + FCI8 + FCI9 + FCI10 + FCI11 + FCI12 + FCI13 + FCI14 + FCI15 + FCI16 + FCI17 + FCI18 + FCI19 + FCI20 + FCI21 + FCI22 + FCI23 + FCI24 + FCI25 + FCI26 + FCI27 + FCI28 + FCI29 + FCI30", 
          "efa(\"efa\")*f3 =~ FCI1 + FCI2 + FCI3 + FCI4 + FCI5 + FCI6 + FCI7 + FCI8 + FCI9 + FCI10 + FCI11 + FCI12 + FCI13 + FCI14 + FCI15 + FCI16 + FCI17 + FCI18 + FCI19 + FCI20 + FCI21 + FCI22 + FCI23 + FCI24 + FCI25 + FCI26 + FCI27 + FCI28 + FCI29 + FCI30", 
          "efa(\"efa\")*f4 =~ FCI1 + FCI2 + FCI3 + FCI4 + FCI5 + FCI6 + FCI7 + FCI8 + FCI9 + FCI10 + FCI11 + FCI12 + FCI13 + FCI14 + FCI15 + FCI16 + FCI17 + FCI18 + FCI19 + FCI20 + FCI21 + FCI22 + FCI23 + FCI24 + FCI25 + FCI26 + FCI27 + FCI28 + FCI29 + FCI30", 
          "efa(\"efa\")*f5 =~ FCI1 + FCI2 + FCI3 + FCI4 + FCI5 + FCI6 + FCI7 + FCI8 + FCI9 + FCI10 + FCI11 + FCI12 + FCI13 + FCI14 + FCI15 + FCI16 + FCI17 + FCI18 + FCI19 + FCI20 + FCI21 + FCI22 + FCI23 + FCI24 + FCI25 + FCI26 + FCI27 + FCI28 + FCI29 + FCI30")), collapse = "\n"),
      
    "table24" = paste(deparse(
        c("efa(\"efa\")*f1 =~ FMCE1 + FMCE2 + FMCE3 + FMCE4 + FMCE5 + FMCE6 + FMCE7 + FMCE8 + FMCE9 + FMCE10 + FMCE11 + FMCE12 + FMCE13 + FMCE14 + FMCE15 + FMCE16 + FMCE17 + FMCE18 + FMCE19 + FMCE20 + FMCE21 + FMCE22 + FMCE23 + FMCE24 + FMCE25 + FMCE26 + FMCE27 + FMCE28 + FMCE29 + FMCE30 + FMCE31 + FMCE32 + FMCE33 + FMCE34 + FMCE35 + FMCE36 + FMCE37 + FMCE38 + FMCE39 + FMCE40 + FMCE41 + FMCE42 + FMCE43 + FMCE44 + FMCE45 + FMCE46 + FMCE47", 
          "efa(\"efa\")*f2 =~ FMCE1 + FMCE2 + FMCE3 + FMCE4 + FMCE5 + FMCE6 + FMCE7 + FMCE8 + FMCE9 + FMCE10 + FMCE11 + FMCE12 + FMCE13 + FMCE14 + FMCE15 + FMCE16 + FMCE17 + FMCE18 + FMCE19 + FMCE20 + FMCE21 + FMCE22 + FMCE23 + FMCE24 + FMCE25 + FMCE26 + FMCE27 + FMCE28 + FMCE29 + FMCE30 + FMCE31 + FMCE32 + FMCE33 + FMCE34 + FMCE35 + FMCE36 + FMCE37 + FMCE38 + FMCE39 + FMCE40 + FMCE41 + FMCE42 + FMCE43 + FMCE44 + FMCE45 + FMCE46 + FMCE47", 
          "efa(\"efa\")*f3 =~ FMCE1 + FMCE2 + FMCE3 + FMCE4 + FMCE5 + FMCE6 + FMCE7 + FMCE8 + FMCE9 + FMCE10 + FMCE11 + FMCE12 + FMCE13 + FMCE14 + FMCE15 + FMCE16 + FMCE17 + FMCE18 + FMCE19 + FMCE20 + FMCE21 + FMCE22 + FMCE23 + FMCE24 + FMCE25 + FMCE26 + FMCE27 + FMCE28 + FMCE29 + FMCE30 + FMCE31 + FMCE32 + FMCE33 + FMCE34 + FMCE35 + FMCE36 + FMCE37 + FMCE38 + FMCE39 + FMCE40 + FMCE41 + FMCE42 + FMCE43 + FMCE44 + FMCE45 + FMCE46 + FMCE47", 
          "efa(\"efa\")*f4 =~ FMCE1 + FMCE2 + FMCE3 + FMCE4 + FMCE5 + FMCE6 + FMCE7 + FMCE8 + FMCE9 + FMCE10 + FMCE11 + FMCE12 + FMCE13 + FMCE14 + FMCE15 + FMCE16 + FMCE17 + FMCE18 + FMCE19 + FMCE20 + FMCE21 + FMCE22 + FMCE23 + FMCE24 + FMCE25 + FMCE26 + FMCE27 + FMCE28 + FMCE29 + FMCE30 + FMCE31 + FMCE32 + FMCE33 + FMCE34 + FMCE35 + FMCE36 + FMCE37 + FMCE38 + FMCE39 + FMCE40 + FMCE41 + FMCE42 + FMCE43 + FMCE44 + FMCE45 + FMCE46 + FMCE47")), collapse = "\n"),
      
    "table25" = paste(deparse(
        c("efa(\"efa\")*f1 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30", 
          "efa(\"efa\")*f2 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30", 
          "efa(\"efa\")*f3 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30", 
          "efa(\"efa\")*f4 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30", 
          "efa(\"efa\")*f5 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30", 
          "efa(\"efa\")*f6 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30", 
          "efa(\"efa\")*f7 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30", 
          "efa(\"efa\")*f8 =~ RRMCS1 + RRMCS2 + RRMCS3 + RRMCS4 + RRMCS5 + RRMCS6 + RRMCS7 + RRMCS8 + RRMCS9 + RRMCS10 + RRMCS11 + RRMCS12 + RRMCS13 + RRMCS14 + RRMCS15 + RRMCS16 + RRMCS17 + RRMCS18 + RRMCS19 + RRMCS20 + RRMCS21 + RRMCS22 + RRMCS23 + RRMCS24 + RRMCS25 + RRMCS26 + RRMCS27 + RRMCS28 + RRMCS29 + RRMCS30")), collapse = "\n"),
      
    "table26" = paste(deparse(
        c("efa(\"efa\")*f1 =~ FMCI3 + FMCI4 + FMCI5 + FMCI6 + FMCI7 + FMCI8 + FMCI9 + FMCI10 + FMCI11 + FMCI12 + FMCI13 + FMCI14 + FMCI15 + FMCI16 + FMCI17 + FMCI18 + FMCI19 + FMCI20 + FMCI21 + FMCI22 + FMCI23 + FMCI24 + FMCI25 + FMCI26 + FMCI27 + FMCI28 + FMCI29 + FMCI30 + FMCI31 + FMCI32", 
          "efa(\"efa\")*f2 =~ FMCI3 + FMCI4 + FMCI5 + FMCI6 + FMCI7 + FMCI8 + FMCI9 + FMCI10 + FMCI11 + FMCI12 + FMCI13 + FMCI14 + FMCI15 + FMCI16 + FMCI17 + FMCI18 + FMCI19 + FMCI20 + FMCI21 + FMCI22 + FMCI23 + FMCI24 + FMCI25 + FMCI26 + FMCI27 + FMCI28 + FMCI29 + FMCI30 + FMCI31 + FMCI32", 
          "efa(\"efa\")*f3 =~ FMCI3 + FMCI4 + FMCI5 + FMCI6 + FMCI7 + FMCI8 + FMCI9 + FMCI10 + FMCI11 + FMCI12 + FMCI13 + FMCI14 + FMCI15 + FMCI16 + FMCI17 + FMCI18 + FMCI19 + FMCI20 + FMCI21 + FMCI22 + FMCI23 + FMCI24 + FMCI25 + FMCI26 + FMCI27 + FMCI28 + FMCI29 + FMCI30 + FMCI31 + FMCI32", 
          "efa(\"efa\")*f4 =~ FMCI3 + FMCI4 + FMCI5 + FMCI6 + FMCI7 + FMCI8 + FMCI9 + FMCI10 + FMCI11 + FMCI12 + FMCI13 + FMCI14 + FMCI15 + FMCI16 + FMCI17 + FMCI18 + FMCI19 + FMCI20 + FMCI21 + FMCI22 + FMCI23 + FMCI24 + FMCI25 + FMCI26 + FMCI27 + FMCI28 + FMCI29 + FMCI30 + FMCI31 + FMCI32", 
          "efa(\"efa\")*f5 =~ FMCI3 + FMCI4 + FMCI5 + FMCI6 + FMCI7 + FMCI8 + FMCI9 + FMCI10 + FMCI11 + FMCI12 + FMCI13 + FMCI14 + FMCI15 + FMCI16 + FMCI17 + FMCI18 + FMCI19 + FMCI20 + FMCI21 + FMCI22 + FMCI23 + FMCI24 + FMCI25 + FMCI26 + FMCI27 + FMCI28 + FMCI29 + FMCI30 + FMCI31 + FMCI32", 
          "efa(\"efa\")*f6 =~ FMCI3 + FMCI4 + FMCI5 + FMCI6 + FMCI7 + FMCI8 + FMCI9 + FMCI10 + FMCI11 + FMCI12 + FMCI13 + FMCI14 + FMCI15 + FMCI16 + FMCI17 + FMCI18 + FMCI19 + FMCI20 + FMCI21 + FMCI22 + FMCI23 + FMCI24 + FMCI25 + FMCI26 + FMCI27 + FMCI28 + FMCI29 + FMCI30 + FMCI31 + FMCI32")), collapse = "\n"),
      
    "table27" = paste(deparse(
        c("efa(\"efa\")*f1 =~ MWCS1 + MWCS2 + MWCS3 + MWCS4 + MWCS5 + MWCS6 + MWCS7 + MWCS8 + MWCS9 + MWCS10 + MWCS11 + MWCS12 + MWCS13 + MWCS14 + MWCS15 + MWCS16 + MWCS17 + MWCS18 + MWCS19 + MWCS20 + MWCS21 + MWCS22", 
          "efa(\"efa\")*f2 =~ MWCS1 + MWCS2 + MWCS3 + MWCS4 + MWCS5 + MWCS6 + MWCS7 + MWCS8 + MWCS9 + MWCS10 + MWCS11 + MWCS12 + MWCS13 + MWCS14 + MWCS15 + MWCS16 + MWCS17 + MWCS18 + MWCS19 + MWCS20 + MWCS21 + MWCS22", 
          "efa(\"efa\")*f3 =~ MWCS1 + MWCS2 + MWCS3 + MWCS4 + MWCS5 + MWCS6 + MWCS7 + MWCS8 + MWCS9 + MWCS10 + MWCS11 + MWCS12 + MWCS13 + MWCS14 + MWCS15 + MWCS16 + MWCS17 + MWCS18 + MWCS19 + MWCS20 + MWCS21 + MWCS22", 
          "efa(\"efa\")*f4 =~ MWCS1 + MWCS2 + MWCS3 + MWCS4 + MWCS5 + MWCS6 + MWCS7 + MWCS8 + MWCS9 + MWCS10 + MWCS11 + MWCS12 + MWCS13 + MWCS14 + MWCS15 + MWCS16 + MWCS17 + MWCS18 + MWCS19 + MWCS20 + MWCS21 + MWCS22")), collapse = "\n"),
      
    "table28" = paste(deparse(
        c("efa(\"efa\")*f1 =~ TCE1 + TCE2 + TCE3 + TCE4 + TCE5 + TCE6 + TCE7 + TCE8 + TCE9 + TCE10 + TCE11 + TCE12 + TCE13 + TCE14 + TCE15 + TCE16 + TCE17 + TCE18 + TCE19 + TCE20 + TCE21 + TCE22 + TCE23 + TCE24 + TCE25 + TCE26", 
          "efa(\"efa\")*f2 =~ TCE1 + TCE2 + TCE3 + TCE4 + TCE5 + TCE6 + TCE7 + TCE8 + TCE9 + TCE10 + TCE11 + TCE12 + TCE13 + TCE14 + TCE15 + TCE16 + TCE17 + TCE18 + TCE19 + TCE20 + TCE21 + TCE22 + TCE23 + TCE24 + TCE25 + TCE26", 
          "efa(\"efa\")*f3 =~ TCE1 + TCE2 + TCE3 + TCE4 + TCE5 + TCE6 + TCE7 + TCE8 + TCE9 + TCE10 + TCE11 + TCE12 + TCE13 + TCE14 + TCE15 + TCE16 + TCE17 + TCE18 + TCE19 + TCE20 + TCE21 + TCE22 + TCE23 + TCE24 + TCE25 + TCE26", 
          "efa(\"efa\")*f4 =~ TCE1 + TCE2 + TCE3 + TCE4 + TCE5 + TCE6 + TCE7 + TCE8 + TCE9 + TCE10 + TCE11 + TCE12 + TCE13 + TCE14 + TCE15 + TCE16 + TCE17 + TCE18 + TCE19 + TCE20 + TCE21 + TCE22 + TCE23 + TCE24 + TCE25 + TCE26")), collapse = "\n"),
      
    "table29" = paste(deparse(
        c("efa(\"efa\")*f1 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33", 
          "efa(\"efa\")*f2 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33", 
          "efa(\"efa\")*f3 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33", 
          "efa(\"efa\")*f4 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33", 
          "efa(\"efa\")*f5 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33", 
          "efa(\"efa\")*f6 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33", 
          "efa(\"efa\")*f7 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33", 
          "efa(\"efa\")*f8 =~ STPFASL1 + STPFASL2 + STPFASL3 + STPFASL4 + STPFASL5 + STPFASL6 + STPFASL7 + STPFASL8 + STPFASL9 + STPFASL10 + STPFASL11 + STPFASL12 + STPFASL13 + STPFASL14 + STPFASL15 + STPFASL16 + STPFASL17 + STPFASL18 + STPFASL19 + STPFASL20 + STPFASL21 + STPFASL22 + STPFASL23 + STPFASL24 + STPFASL25 + STPFASL26 + STPFASL27 + STPFASL28 + STPFASL29 + STPFASL30 + STPFASL31 + STPFASL32 + STPFASL33")), collapse = "\n"),
      
    "table12" = paste(deparse(
        c("efa(\"efa\")*f1 =~ SAARB1 + SAARB2 + SAARB3 + SAARB4 + SAARB5 + SAARB6 + SAARB7 + SAARB8 + SAARB9 + SAARF10 + SAARF11 + SAARG12 + SAARG13 + SAARG14 + SAARG15 + SAARG16", 
          "efa(\"efa\")*f2 =~ SAARB1 + SAARB2 + SAARB3 + SAARB4 + SAARB5 + SAARB6 + SAARB7 + SAARB8 + SAARB9 + SAARF10 + SAARF11 + SAARG12 + SAARG13 + SAARG14 + SAARG15 + SAARG16", 
          "efa(\"efa\")*f3 =~ SAARB1 + SAARB2 + SAARB3 + SAARB4 + SAARB5 + SAARB6 + SAARB7 + SAARB8 + SAARB9 + SAARF10 + SAARF11 + SAARG12 + SAARG13 + SAARG14 + SAARG15 + SAARG16", 
          "efa(\"efa\")*f4 =~ SAARB1 + SAARB2 + SAARB3 + SAARB4 + SAARB5 + SAARB6 + SAARB7 + SAARB8 + SAARB9 + SAARF10 + SAARF11 + SAARG12 + SAARG13 + SAARG14 + SAARG15 + SAARG16", 
          "efa(\"efa\")*f5 =~ SAARB1 + SAARB2 + SAARB3 + SAARB4 + SAARB5 + SAARB6 + SAARB7 + SAARB8 + SAARB9 + SAARF10 + SAARF11 + SAARG12 + SAARG13 + SAARG14 + SAARG15 + SAARG16", 
          "efa(\"efa\")*f6 =~ SAARB1 + SAARB2 + SAARB3 + SAARB4 + SAARB5 + SAARB6 + SAARB7 + SAARB8 + SAARB9 + SAARF10 + SAARF11 + SAARG12 + SAARG13 + SAARG14 + SAARG15 + SAARG16")), collapse = "\n"),
      
    "table13" = paste(deparse(
        c("efa(\"efa\")*f1 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42", 
          "efa(\"efa\")*f2 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42", 
          "efa(\"efa\")*f3 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42", 
          "efa(\"efa\")*f4 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42", 
          "efa(\"efa\")*f5 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42", 
          "efa(\"efa\")*f6 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42", 
          "efa(\"efa\")*f7 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42", 
          "efa(\"efa\")*f8 =~ CLASS1 + CLASS2 + CLASS3 + CLASS5 + CLASS6 + CLASS8 + CLASS10 + CLASS11 + CLASS12 + CLASS13 + CLASS14 + CLASS15 + CLASS16 + CLASS17 + CLASS18 + CLASS19 + CLASS20 + CLASS21 + CLASS22 + CLASS23 + CLASS24 + CLASS25 + CLASS26 + CLASS27 + CLASS28 + CLASS29 + CLASS30 + CLASS32 + CLASS34 + CLASS35 + CLASS36 + CLASS37 + CLASS38 + CLASS39 + CLASS40 + CLASS42")), collapse = "\n")
    )
    )
  })
  
  # Reactive expression to run the CFA when button is clicked
  factan_model_results <- observeEvent(input$factan_run_model, {
    req(factan_data(), input$factan_spec)
    
    withProgress(message = 'Running Confirmatory Factor Analysis...', value = 0, {
    setProgress(value = 0.1, detail = "Starting EFA...")
    
    cfa_model <- lavaan::cfa(model = eval(parse(text = input$factan_spec)), data = factan_data()[, input$factan_selected_vars],
                             sample.nobs = NULL, rotation = "varimax", rotation.args = list(), bounds = "pos.var", model.type = "sem", int.ov.free = TRUE, 
                             int.lv.free = FALSE, auto.fix.first = TRUE, auto.fix.single = TRUE, auto.var = TRUE, auto.cov.lv.x = TRUE, auto.cov.y = TRUE, 
                             auto.th = TRUE, auto.delta = TRUE, auto.efa = TRUE)
    cfa_result <- summary(cfa_model, fit.measures = TRUE, standardized = TRUE)
    cfa_fit <- fitMeasures(cfa_model)
    
    setProgress(value = 1, detail = "Completed.")
    })
    
    # Display CFA results
    output$factan_modelfit <- renderDataTable({
      data.frame("Fit_Measures" = round(cfa_fit,3))
    }, options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
    
    output$factan_modelunstandard <- renderDataTable({
      cfa_result$pe[c(1,2,3,5,6,8)]
    }, options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
    
    output$factan_modelstandard <- renderDataTable({
      cfa_result$pe[c(1,2,3,10,8)]
    }, options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
    
    
    output$factan_modindices <- renderDataTable({
      lavaan::modindices(cfa_model, sort. = TRUE)
    }, options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
    
    output$factan_plot <- renderPlot(
      semPaths(cfa_model, "std", whatLabels = "std", rotation = 2, edge.label.cex = 0.5, curvature = 3, style = "lisrel",
               layout = "tree", nCharNodes = 5, edge.width = 0.3, edge.color = "black", color = list(man = "lightblue", lat = "lightpink"),
               sizeMan = 3, sizeLat = 3, mar = c(0.5,25,0.5,8), node.height = 0.8)
    )
    
  })
  
  ctt_results_1 <- reactive({
    if (input$iteman_choice_ctt_1 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_ctt_1,
           "table30" = CTT::itemAnalysis(spheredata::binary(FCI,FCIkey))$itemReport,
           "table31" = CTT::itemAnalysis(spheredata::binary(FMCE,FMCEkey))$itemReport,
           "table32" = CTT::itemAnalysis(spheredata::binary(RRMCS,RRMCSkey))$itemReport,
           "table33" = CTT::itemAnalysis(spheredata::binary(FMCI,FMCIkey))$itemReport,
           "table34" = CTT::itemAnalysis(spheredata::binary(MWCS,MWCSkey))$itemReport,
           "table35" = CTT::itemAnalysis(spheredata::binary(TCE,TCEkey))$itemReport,
           "table36" = CTT::itemAnalysis(spheredata::binary(STPFASL,STPFASLkey))$itemReport)
  })
  
  output$iteman_ctt_1 <- renderDataTable({
    ctt_results_1()[,1:4]
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  ctt_results_2 <- reactive({
    if (input$iteman_choice_ctt_2 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_ctt_2,
           "table37" = CTT::distractorAnalysis(FCI,FCIkey),
           "table38" = CTT::distractorAnalysis(FMCE,FMCEkey),
           "table39" = CTT::distractorAnalysis(RRMCS,RRMCSkey),
           "table40" = CTT::distractorAnalysis(FMCI,FMCIkey, nGroups = 3),
           "table41" = CTT::distractorAnalysis(MWCS,MWCSkey),
           "table42" = CTT::distractorAnalysis(TCE,TCEkey),
           "table43" = CTT::distractorAnalysis(STPFASL,STPFASLkey))
  })
  
  output$ctt_column_select_ui <- renderUI({
    req(ctt_results_2())
    selectInput("ctt_column_select", "Choose an item:", choices = c("", names(ctt_results_2())), width = 150)
  })
  
  output$iteman_ctt_2 <- renderDataTable({
    req(ctt_results_2(), input$ctt_column_select)
    ctt_results_2()[[input$ctt_column_select]]
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  ctt_results_3 <- reactive({
    if (input$iteman_choice_ctt_3 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_ctt_3,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey))
  })
  
  output$ctt_plot_select_ui <- renderUI({
    req(ctt_results_3())
    selectInput("ctt_plot_select", "Choose an item:", choices = c("", names(ctt_results_3())), width = 150)
  })
  
  output$iteman_ctt_3 <- renderPlot({
    req(ctt_results_3(), input$ctt_plot_select)
    
    CTT::cttICC(rowSums(ctt_results_3()), ctt_results_3()[,input$ctt_plot_select], cex=1.5)
  })
  
  ctt_results_4 <- reactive({
    if (input$iteman_choice_ctt_4 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_ctt_4,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey))
  }) 
  
  output$iteman_ctt_4 <- renderPlot({
    req(ctt_results_4())
    
    hist(scale(rowSums(ctt_results_4())), main = "Histogram", xlab = "Standardized score (z transformation)")
  })
  
  irt_results_1 <- reactive({
    if (input$iteman_choice_irt_1 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_irt_1,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey))
  })
  
  irt_modelfit_1 <- observeEvent(input$run_irt_modelfit_ui, {
    req(irt_results_1())
    
    mod.1PL<-mirt(irt_results_1(), model=1, itemtype="Rasch", SE=T)
    mod.2PL<-mirt(irt_results_1(), model=1, itemtype="2PL", SE=T)
    mod.3PL<-mirt(irt_results_1(), model=1, itemtype="3PL", SE=T)
    
    output$iteman_irt_1 <- renderDataTable({
      data.frame(model = c("1PL", "2PL", "3PL"),
                 AIC = c(mod.1PL@Fit$AIC, mod.2PL@Fit$AIC, mod.3PL@Fit$AIC),
                 BIC = c(mod.1PL@Fit$BIC, mod.2PL@Fit$BIC, mod.3PL@Fit$BIC),
                 SABIC = c(mod.1PL@Fit$SABIC, mod.2PL@Fit$SABIC, mod.3PL@Fit$SABIC))
    },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
    
  })
  
  irt_results_2 <- reactive({
    if (input$iteman_choice_irt_2 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_irt_2,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey))
  })
  
  irt_modelchoice_2 <- reactive({
    req(irt_results_2())
    
    if (input$iteman_model_irt_2 == ""){
      return(NULL)
    }
    switch(input$iteman_model_irt_2,
           "table44" = mirt(irt_results_2(), model=1, itemtype="Rasch", SE=T),
           "table45" = mirt(irt_results_2(), model=1, itemtype="2PL", SE=T),
           "table46" = mirt(irt_results_2(), model=1, itemtype="3PL", SE=T))
  })
  
  output$iteman_irt_2 <- renderDataTable({
    req(irt_modelchoice_2())
    
    data.frame(coef(irt_modelchoice_2(), IRTpars=TRUE, simplify = TRUE)$items)
    
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  irt_results_3 <- reactive({
    if (input$iteman_choice_irt_3 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_irt_3,
           "table5" = spheredata::FCI,
           "table6" = spheredata::FMCE,
           "table7" = spheredata::RRMCS,
           "table8" = spheredata::FMCI,
           "table9" = spheredata::MWCS,
           "table10" = spheredata::TCE,
           "table11" = spheredata::STPFASL)
  })
  
  output$irt_distractor_select_ui <- renderUI({
    req(irt_results_3())
    
    selectInput("irt_distractor_select", "Choose an item:", choices = c("", names(irt_results_3())), width = 150)
  })
  
  output$iteman_irt_3 <- renderPlot({
    req(irt_results_3(), input$irt_distractor_select)
    
    df_num <- data.frame(lapply(irt_results_3(), function(x) {
      if (is.character(x) || is.factor(x)) {
        as.numeric(factor(x, levels = unique(x)))
      } else {
        x  
      }
    }))
    
    mirt::itemplot(mirt(df_num, model = 1, itemtype = "nominal", lwd = 2), input$irt_distractor_select)
  })
  
  irt_results_4 <- reactive({
    if (input$iteman_choice_irt_4 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_irt_4,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey))
  })
  
  irt_modelchoice_4 <- reactive({
    req(irt_results_4())
    
    if (input$iteman_model_irt_4 == ""){
      return(NULL)
    }
    switch(input$iteman_model_irt_4,
           "table44" = mirt(irt_results_4(), model=1, itemtype="Rasch", SE=T),
           "table45" = mirt(irt_results_4(), model=1, itemtype="2PL", SE=T),
           "table46" = mirt(irt_results_4(), model=1, itemtype="3PL", SE=T))
  })
  
  output$irt_plot_select_ui <- renderUI({
    req(irt_results_4(), irt_modelchoice_4())
    
    selectInput("irt_plot_select", "Choose an item:", choices = c("", names(irt_results_4())), width = 150)
  })
  
  output$iteman_irt_4 <- renderPlot({
    req(irt_results_4(), input$irt_plot_select)
    
    mirt::itemplot(irt_modelchoice_4(), input$irt_plot_select)
  })
  
  irt_results_5 <- reactive({
    if (input$iteman_choice_irt_5 == ""){
      return(NULL)
    }
    switch(input$iteman_choice_irt_5,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey))
  })
  
  irt_modelchoice_5 <- reactive({
    req(irt_results_5())
    
    if (input$iteman_model_irt_5 == ""){
      return(NULL)
    }
    switch(input$iteman_model_irt_5,
           "table44" = mirt(irt_results_5(), model=1, itemtype="Rasch", SE=T),
           "table45" = mirt(irt_results_5(), model=1, itemtype="2PL", SE=T),
           "table46" = mirt(irt_results_5(), model=1, itemtype="3PL", SE=T))
  })
  
  output$iteman_irt_5 <- renderPlot({
    req(irt_results_5(), irt_modelchoice_5())
    
    hist(fscores(irt_modelchoice_5(), method="EAP", full.scores=T, full.scores.SE = T), 
         main = "Histogram", xlab = "Ability (Theta)")
    
    })
  
  reliability_data <- reactive({
    if (input$reliabilitydata_choice == ""){
      return(NULL)
    }
    switch(input$reliabilitydata_choice,
           "table23" = spheredata::binary(FCI,FCIkey),
           "table24" = spheredata::binary(FMCE,FMCEkey),
           "table25" = spheredata::binary(RRMCS,RRMCSkey),
           "table26" = spheredata::binary(FMCI,FMCIkey),
           "table27" = spheredata::binary(MWCS,MWCSkey),
           "table28" = spheredata::binary(TCE,TCEkey),
           "table29" = spheredata::binary(STPFASL,STPFASLkey),
           "table12" = SAAR,
           "table13" = CLASS)

  })
  
  output$reli_var_select_ui <- renderUI({
    req(reliability_data())
    selectInput("reli_selected_vars", "Selected Items:", 
                choices = names(reliability_data()), 
                selected = names(reliability_data()), 
                multiple = TRUE, width = 300)
  })
  
  reliability_results <- observeEvent(input$reliability_run, {
    req(reliability_data(), input$reli_selected_vars)
    calc_reliability <- psych::alpha(reliability_data()[, input$reli_selected_vars])
    
    output$reliability_alpha <- renderDataTable({
      t(calc_reliability$total)
    },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
    
    output$item_drop_alpha <- renderDataTable({
      calc_reliability$alpha.drop
    },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
    
  })
  
  load(file = "data/df_numeric.rda")
  load(file = "data/numeric_d.rda")
  load(file = "data/numeric_p.rda")
  load(file = "data/par_numeric_d.rda")
  load(file = "data/par_numeric_p.rda")
  
  df_MOO_numeric <- data.frame(Features = names(numeric_d), 
                               a_d = scale(par_numeric_d$a), 
                               b_d = scale(par_numeric_d$b), 
                               g_d = scale(par_numeric_d$g), 
                               a_p = scale(par_numeric_p$a))
  df_MOO_numeric$MOO <- df_MOO_numeric$a_d + df_MOO_numeric$b_d - df_MOO_numeric$g_d + df_MOO_numeric$a_p
  df_MOO_numeric <- df_MOO_numeric[order(df_MOO_numeric$MOO, decreasing = TRUE),]
  
  output$fselect_data <- renderDataTable({
    round(df_numeric, 4)
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  output$dichotomous_data <- renderDataTable({
    numeric_d
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  output$polytomous_data <- renderDataTable({
    numeric_p
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  output$IRT_fselect <- renderDataTable({
    data.frame(Features = df_MOO_numeric$Features, round(df_MOO_numeric[,2:6], 4))
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  

  load(file = "data/df_categoric.rda")
  load(file = "data/par_categoric_p.rda")
  load("data/info_gain_categoric.rda")
  
  df_MOO_categoric <- data.frame(Features = rownames(par_categoric_p), 
                                 a_p = scale(par_categoric_p$a),
                                 information_gain = scale(info_gain_categoric$importance))
  df_MOO_categoric$MOO <- df_MOO_categoric$a_p + df_MOO_categoric$information_gain
  df_MOO_categoric <- df_MOO_categoric[order(df_MOO_categoric$MOO, decreasing = TRUE),]
  
  output$fselect_data_cat <- renderDataTable({
    df_categoric
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  output$IRT_fselect_data_cat <- renderDataTable({
    data.frame(Features = df_MOO_categoric$Features, round(df_MOO_categoric[,2:4], 4))
  },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  
  df <- data.frame(df_numeric, df_categoric)
  
  # define data types
  df$SCH <- as.factor(demographic$SCH)
  df$GDR <- as.factor(demographic$GDR)
  df$FATHOCC <- as.factor(demographic$FATHOCC)
  df$MOTHOCC <- as.factor(demographic$MOTHOCC)
  df$FATHEDU <- as.factor(demographic$FATHEDU)
  df$MOTHEDU <- as.factor(demographic$MOTHEDU)
  df$FATHINC <- as.factor(demographic$FATHINC)
  df$MOTHINC <- as.factor(demographic$MOTHINC)
  df$SIBL <- as.factor(demographic$SIBL)
  df$DOM <- as.factor(demographic$DOM)
  df$LIT1 <- as.factor(literacy$LIT1)
  df$LIT2 <- as.factor(literacy$LIT2)
  df$PHYIDE1 <- as.factor(physicsidentity$PHYIDE1)
  df$PHYIDE2 <- as.factor(physicsidentity$PHYIDE2)
  df$Target <- as.factor(df$Target)
  
  output$GA_var_select_ui <- renderUI({
    selectInput("GA_selected_vars", "Selected Features:", 
                choices = c(df_MOO_numeric$Features, df_MOO_categoric$Features), 
                selected = c("FMCI_f1", "FMCE_f4", "STPFASL_f2", "TCE_f2", "FMCI_f4"), 
                multiple = TRUE, width = 300)
  })
  
  
  observeEvent(input$runGA, {
    start_time <- Sys.time()
    
    set.seed(86)
    split = sample.split(df[, c(input$GA_selected_vars, "Target")]$Target, SplitRatio = 0.7)
    df.Train = subset(df[, c(input$GA_selected_vars, "Target")], split == 1)
    df.Test = subset(df[, c(input$GA_selected_vars, "Target")], split == 0)
    
    withProgress(message = "Running GA ...", value = 0, {
      set.seed(86)
      fit_rf=function(chromosome)
      {
        ntree_bin = chromosome[1:10]
        mtry_bin = chromosome[11:13]
        sampsize_bin = chromosome[14:22]
        nodesize_bin = chromosome[23:26]
        maxnodes_bin = chromosome[27:32]
        
        ntree <- min(max(binary2decimal(ntree_bin), 50), 1000)
        mtry <- min(max(binary2decimal(mtry_bin), 1), length(input$GA_selected_vars))
        sampsize <- min(max(binary2decimal(sampsize_bin), 100), 300)
        nodesize <- min(max(binary2decimal(nodesize_bin), 1), 10)
        maxnodes <- min(max(binary2decimal(maxnodes_bin), 10), 50)
        
        set.seed(86)
        model <- randomForest(Target ~ ., 
                              data = df.Train, 
                              ntree = ntree, 
                              mtry = mtry, 
                              sampsize = sampsize, 
                              nodesize = nodesize, 
                              maxnodes = maxnodes)
        
        Pred <- predict(model, df.Test)
        cf_table <- confusionMatrix(Pred, df.Test$Target)
        
        metric <- switch(input$objective,
                         "Accuracy" = cf_table$overall['Accuracy'],
                         "Kappa" = cf_table$overall['Kappa'],
                         "Sensitivity" = cf_table$byClass['Sensitivity'],
                         "Specificity" = cf_table$byClass['Specificity'],
                         "Recall" = cf_table$byClass['Recall'],
                         "F1" = cf_table$byClass['F1'])
        
        return(as.numeric(metric)) # optimization
      }
      
      set.seed(86)
      ga_result = ga(type='binary', 
                     fitness=fit_rf, 
                     nBits=32,
                     maxiter=input$maxiter,
                     run = 25,
                     popSize=input$popSize,
                     elitism=TRUE,
                     pcrossover = input$pcrossover,
                     pmutation = input$pmutation,
                     seed=86,
                     keepBest=TRUE,
                     monitor = function(obj) {
                       elapsed_time <- Sys.time() - start_time
                       total_seconds <- as.numeric(elapsed_time, units = "secs")
                       
                       if (total_seconds > 60) {
                         minutes <- as.integer(total_seconds %/% 60)
                         seconds <- round(total_seconds %% 60, 2)
                         time_display <- paste(minutes, "minutes", seconds, "seconds")
                       } else {
                         time_display <- paste(round(total_seconds, 2), "seconds")
                       }
                       
                       setProgress(
                         value = obj@iter / input$maxiter, 
                         detail = paste("Generation:", obj@iter, sep = "\n", "- Elapsed time:", time_display))
                     })
    })
    
    output$summary_gaResults <- renderPrint({
      summary(ga_result)
    })
    
    output$plot_gaResults <- renderPlot(
      plot(ga_result)
    )
    
    output$hype_gaResults <- renderDataTable({
      ga_rf_fit = as.data.frame(ga_result@solution)
      
      data.frame(
        ntree = apply(ga_rf_fit[, 1:10], 1, binary2decimal),
        mtry = apply(ga_rf_fit[, 11:13], 1, binary2decimal),
        sampsize = apply(ga_rf_fit[, 14:22], 1, binary2decimal),
        nodesize = apply(ga_rf_fit[, 23:26], 1, binary2decimal),
        maxnodes = apply(ga_rf_fit[, 27:32], 1, binary2decimal)
      )
    },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  
  output$RF_var_select_ui <- renderUI({
    selectInput("RF_selected_vars", "Selected Features:", 
                choices = c(df_MOO_numeric$Features, df_MOO_categoric$Features), 
                selected = c("FMCI_f1", "FMCE_f4", "STPFASL_f2", "TCE_f2", "FMCI_f4"), 
                multiple = TRUE, width = 300)
  })
  
  observeEvent(input$train, {
    selected_features_rf <- input$RF_selected_vars
    df_selected_rf <- df[, c(selected_features_rf, "Target")]
    
    set.seed(86)
    rf_split = sample.split(df_selected_rf$Target, SplitRatio = 0.7)
    df.Train_rf = subset(df_selected_rf, rf_split == 1)
    df.Test_rf = subset(df_selected_rf, rf_split == 0)
    
    set.seed(86)
    model_rf <- reactive({
                randomForest(Target ~ ., 
                             data = df.Train_rf,
                             ntree = input$rf_ntree,
                             mtry = input$rf_mtry,
                             sampsize = input$rf_sampsize,
                             nodesize = input$rf_nodesize,
                             maxnodes = input$rf_maxnodes,
                             importance = TRUE)
    })

    Pred_rf <- predict(model_rf(), df.Test_rf)
    cf_table_rf <- confusionMatrix(Pred_rf, df.Test_rf$Target)

    Pred.prob_rf <- predict(model_rf(), newdata = df.Test_rf, type = 'prob')
    roc_rf <- roc(df.Test_rf$Target, Pred.prob_rf[,2])

    output$summary_rfResults <- renderPrint({
      model_rf()
    })

    output$train_rfResults <- renderPrint({
      cf_table_rf
    })
    
    output$plot_roc_rf <- renderPlot({
      Pred.prob <- predict(model_rf(), newdata = df.Test, type = 'prob')
      roc <- roc(df.Test$Target, Pred.prob[,2])
      plot.roc(roc, print.thres = F, print.auc = T, legacy.axes = T)
    })
    
    output$plot_error_rf <- renderPlot({
      plot(model_rf(), main = "Out-of-Bag error")
      legend("topright", 
             legend = c("Total Error", "Error Class 1", "Error Class 0"),
             col = c("black", "red", "green"), lty = 1,  cex = 0.8)
    })
    
    output$saveML <- downloadHandler(
      filename = function() {
        paste0("rf_", input$rf_ntree, "_", input$rf_mtry, "_", input$rf_sampsize, "_", input$rf_nodesize, "_", input$rf_maxnodes, ".RDS")
      },
      content = function(file) {
        saveRDS(model_rf(), file)
      }
    )
    
  })
  
  data_LA <- reactive({
    req(input$datafile)
    file <- input$datafile$datapath
    read_xlsx(file)
  })
  
  model_LA <- reactive({
    req(input$modelfile)
    file <- input$modelfile$datapath
    readRDS(file)
  })
  
  
  observeEvent(input$open_raw_LA,{
    req(data_LA())
    
    output$raw_LA_data <- renderDataTable({
      data_LA()
    },options = list(scrollX = TRUE, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = "dt-left", targets = "_all"))))
  })
  
  observeEvent(input$predict_LA,{
    req(data_LA(), model_LA())
    pred_result <- predict(model_LA(), scale(data_LA()[,-1]), type = "class")
    
    output$predict_LA_results <- renderDT({
      datatable(data.frame(data_LA(), Prediction = pred_result), 
                options = list(scrollX = TRUE,
                               lengthChange = FALSE,
                               paging = TRUE,
                               searching = FALSE,
                               columnDefs = list(
                                 list(
                                   targets = 7,
                                   createdCell = JS(
                                     "function(td, cellData, rowData, row, col) {
                                      if (cellData == 1) {
                                        $(td).css({'background-color': 'green', 'color': 'white'});
                                      } else if (cellData == 0) {
                                        $(td).css({'background-color': 'orange', 'color': 'white'});
                                      }
                                    }"
                                   )
                                 )
                               )
                )
      )
      
    })
  })
  
  
  
  
  
  
  
  
}
