appserver <- function(input, output, session) {

	# Read STS list (once every user visit)
	STS <- readSTS()

	output$setControls <- renderUI({
	    selectInput("dataset", "Time Series", names(STS), width="50%")
	})

	output$yearControls <- renderUI({
	    if(!is.null(input$dataset)) {
		years <- sort(unlist(STS[[input$dataset]][,"year"]))
		print(as.character(years))
		selectInput("year", "Year", as.character(years), width="50%")
	    }
	})

	observe({
		if(input$allYearSelectBox == FALSE) {
			session$sendCustomMessage(type="allYearSelect", FALSE)
		} else {
			session$sendCustomMessage(type="allYearSelect", TRUE)
		}
	})

	output$downloadData <- downloadHandler(
	    filename = function() {
		if(input$allYearSelectBox) {
			allY <- sort(unlist(STS[[input$dataset]][,"year"]))
			paste0(input$dataset, "_", paste0(head(allY, 1), "-", tail(allY, 1)), ".zip", sep = "")
		} else {
			paste0(input$dataset, "_", input$year, ".zip", sep = "")
		}
	    },
	    content = function(file) {
		session$sendCustomMessage("startDownload", "")
		p <- Progress$new()
		p$set(value = NULL, message = "Preparing data", detail="\nThis might take a while, please wait...")
		prepDataset <- input$dataset
		if(input$allYearSelectBox)
			prepYear <- sort(unlist(STS[[prepDataset]][,"year"]))
		else
			prepYear <- input$year
		future({
		    res <- prepareData(STS, prepDataset, prepYear)
		    res
		}) %...>% (function(result) {
		setwd(result$outDir)
		if(result$warning != "") {
			showModal(modalDialog(
				title = "Important message",
				result$warning,
				easyClose = TRUE
			))
		}
		}) %...>% 
		{ p$set(message = "Zipping all data", detail="\nAlmost done, please wait...") } %...>% 
		{ 
		zip(zipfile=file, flags = "-r9Xm", files=".") 
		getwd()
		} %...>%
		print() %>%
		finally(~{p$close()
			session$sendCustomMessage("finishDownload", "")
		})
	    },
	    contentType = "application/zip"
	)

        # CHECK Portion
        data <- reactive({

            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.

            req(input$file1)

            output <- checkDataOne(input$file1$datapath, input$checkboxIgnore)

            output
        })

		output$summary <- renderTable({

			rec <- data()
			if(is.null(rec[[1]])){
				""
			} else {
				rec[[1]][["Status"]] <- ifelse(
					rec[[1]][["Status"]] == TRUE,
					as.character(tags$i(
						class = "fas fa-2x fa-check-circle",
						style = "color: green"
					)),
					rec[[1]][["Status"]]
				)
				rec[[1]][["Status"]] <- ifelse(
					rec[[1]][["Status"]] == FALSE,
					as.character(tags$i(
						class = "fas fa-2x fa-times-circle",
						style = "color: red"
					)),
					rec[[1]][["Status"]]
				)
				rec[[1]]
			}
		}, sanitize.text.function = function(x) x)

		output$abundance <- renderTable({

			rec <- data()
			if(is.null(rec[[2]])){
				""
			} else {
				rec[[2]]
			}
		})

		output$log <- renderPrint({

			rec <- data()
			if(is.null(rec[[3]])){
				""
			} else {
				rec[[3]]
			}
		})

}

