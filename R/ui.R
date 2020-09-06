appui <- fluidPage(
    h1("Survey time series downloader and checker"),
	hr(),
	tags$div("Please select a function below:"),
    tabsetPanel(
        tabPanel(HTML("<i class=\"fas fa-2x fa-cloud-download-alt\"></i> DOWNLOADER"),
            h3("Select time series"),
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
					)
				)
            )
        ),
        tabPanel(HTML("<i class=\"fas fa-2x fa-check\"></i> CHECKER"),
            h3("Check a project xml file"),
			br(),
            # Input: Select a file ----
            fileInput("file1", "Choose and upload a StoX project file",
					width = "80%",
                    multiple = FALSE,
                    accept = c("text/xml", "application/xml", ".xml")),
			checkboxInput("checkboxIgnore", label = "Ignore missing biotic file snapshots", value = FALSE),
			tags$div(tags$em(HTML("NOTE: Ignoring biotic snapshots option is for debugging purposes only.
				A submission of <code>project.xml</code> file that didn't pass all the checks, including missing biotic snapshots, will not be accepted into NMD Dataset Explorer."))
			),
            # Horizontal line ----
            hr(),
			h3("Detailed check report"),
			tabsetPanel(
				tabPanel("Summary",
					shinycssloaders::withSpinner(tableOutput("summary"))),
				tabPanel("Result",
					shinycssloaders::withSpinner(tableOutput("abundance"))),
				tabPanel("Log",
					shinycssloaders::withSpinner(verbatimTextOutput("log")))
			)
		)
    ),
    hr(),
    div("\u00A9 2019-2020 Havforskningsinstituttet. A part of REDUS project and SEA2DATA project. Developer: Ibrahim Umar.")
)

