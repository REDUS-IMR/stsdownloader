appui <- fluidPage(
    titlePanel("Survey time series downloader"),
    hr(),
    uiOutput("setControls"),
    uiOutput("yearControls"),
    downloadButton("downloadData", "Download ZIP file"),
    hr(),
    print("\u00A9 2019 Havforskningsinstituttet. A part of REDUS project and SEA2DATA project. Developer: Ibrahim Umar."),
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
				);'
	))
    )
)

