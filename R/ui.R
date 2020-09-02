appui <- fluidPage(
    titlePanel("Survey time series downloader"),
    tabsetPanel(
        tabPanel("Download",
            h3("Pick a time series:"),
            uiOutput("setControls"),
            uiOutput("yearControls"),
            checkboxInput("allYearSelectBox", "Download all years", FALSE),
            downloadButton("downloadData", "Download ZIP file"),
            hr(),
            singleton(
                tags$head(tags$script('Shiny.addCustomMessageHandler("startDownload", \
          				function(message) { \
					        $(\'#downloadData\').attr("disabled", "true").attr("onclick", "return false;");\
					        $(\'#downloadData i:first-child\' ).removeClass("fa-download").addClass("fa-circle-o-notch fa-spin");\
            					$(\'#downloadData\').css(\'color\', "gray");\
          				} \
				        );\
				        Shiny.addCustomMessageHandler("finishDownload", \
          				function(message) { \
					        $(\'#downloadData\').removeAttr("disabled").removeAttr("onclick");\
            					$(\'#downloadData\').css(\'color\', "");\
					        $(\'#downloadData i:first-child\').removeClass("fa-circle-o-notch fa-spin").addClass("fa-download");\
          				} \
				        );\
				        Shiny.addCustomMessageHandler("allYearSelect", \
				        function(message) { \
					        var yr = $(\"#year\");\
					        var yr0 = yr[0];\
					        if(yr0) { \
						        if(!message) {\
							        yr0.selectize.enable(); \
							        yr.prop(\"disabled\",false); \
						        } else { \
							        yr[0].selectize.disable(); \
							        $(\"#year\").prop(\"disabled\",true); \
						        }\
					        }\
				        } \
				        );'
	        ))
            )
        ),
        tabPanel("Check",
            h3("Check a time series:"),
            # Input: Select a file ----
            fileInput("file1", "Choose a StoX project file",
                    multiple = FALSE,
                    accept = c("text/xml", "application/xml", ".xml")),
            # Horizontal line ----
            hr(),
            # Output: Data file ----
            verbatimTextOutput("contents")
        )
    ),
    hr(),
    print("\u00A9 2019-2020 Havforskningsinstituttet. A part of REDUS project and SEA2DATA project. Developer: Ibrahim Umar.")
)

