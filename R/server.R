appserver <- function(input, output, session) {

	# Read STS list (once every user visit)
	STS <- readSTS()

	output$setControls <- renderUI({
	    selectInput("dataset", "Time Series", names(STS))
	})

	output$yearControls <- renderUI({
	    if(!is.null(input$dataset)) {
		years <- sort(unlist(STS[[input$dataset]][,"year"]))
		print(as.character(years))
		selectInput("year", "Year", as.character(years))
	    }
	})

	output$downloadData <- downloadHandler(
	    filename = function() {
		paste0(input$dataset, "_", input$year, ".zip", sep = "")
	    },
	    content = function(file) {
		session$sendCustomMessage("startDownload", "")
		p <- Progress$new()
		p$set(value = NULL, message = "Preparing data", detail="\nThis might take a while, please wait...")
		a1 <- input$dataset
		a2 <- input$year
		future({
		    res <- prepareData(STS, a1, a2)
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
}

