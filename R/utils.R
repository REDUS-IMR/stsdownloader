checkSnapshot <- function(z) {
    # Get the snapshot (accommodate wrong separator)
    date <- tail(unlist(strsplit(tools::file_path_sans_ext(z), "_")), 1)
    d <- try(as.Date(date))
    if (class(d) == "try-error" || is.na(d)) {
    date <- paste0(tail(unlist(strsplit(tools::file_path_sans_ext(z), "_")), 3), collapse = "-")
    d <- try(as.Date(date))
    if (class(d) == "try-error" || is.na(d)) return(NA)
    else return(date)
    }
    else return(date)
}

downloadEchosounder <- function(z, targetDir) {
    print(z)
    # Get cruise and ship name
    sp <- unlist(strsplit(tools::file_path_sans_ext(z), "_"))
    cruiseNo <- tail(head(sp, 3), 1)
    shipName <- tail(head(sp, 4), 1)

    # For non-standard cruise number
    if (nchar(cruiseNo) != 7) {
        cruiseNo <- paste0(tail(head(sp, 6), 4), collapse = "_")
        shipName <- tail(head(sp, 7), 1)
    }

    # Fix for broken filename (e.g., ...G+O+Sars (1))
    shipName <- gsub("\\s+\\([0-9]+\\)", "", shipName)

    # Fix for GO SARS
    if (shipName == "G+O+Sars")
    shipName <- "G.O.Sars"

    # Fix for M.+Ytterstad
    if (shipName == "M++Ytterstad")
    shipName <- "M.+Ytterstad"

    url <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/echosounder/v1/find?cruisenr=", cruiseNo, "&shipname=", shipName)
    doc <- read_xml(URLencode(url))
    url <- xml_text(xml_find_all(doc, "//*[local-name() = 'element'][@key='url']"))[1]
    download.file(URLencode(url), paste0(targetDir, "/input/acoustic/", z))

	return(z)
}

appendSnapshot <- function(z, targetDir) {
    snapshot <- checkSnapshot(z)

    print(z)
    # Get cruise and ship name
    sp <- unlist(strsplit(tools::file_path_sans_ext(z), "_"))
    cruiseNo <- tail(head(sp, 3), 1)
    shipName <- tail(head(sp, 4), 1)

    # For non-standard cruise number
    if (nchar(cruiseNo) != 7) {
        cruiseNo <- paste0(tail(head(sp, 6), 4), collapse = "_")
        shipName <- tail(head(sp, 7), 1)
    }

    dbShipName <- shipName

    # Fix for broken filename (e.g., ...G+O+Sars (1))
    shipName <- gsub("\\s+\\([0-9]+\\)", "", shipName)

    # Fix for GO SARS
    if (shipName == "G+O+Sars")
    shipName <- "G.O.Sars"

    # Fix for M.+Ytterstad
    if (shipName == "M++Ytterstad")
    shipName <- "M.+Ytterstad"

    url <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3?type=findByCruise&shipname=", shipName, "&cruisenr=", cruiseNo)
    print(url)
    doc <- read_xml(URLencode(url))

    path <- xml_text(xml_find_all(doc, "//*[local-name() = 'element'][@name='path']"))
    print(path)

    if (is.na(snapshot)) {
    url <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/", path, "/snapshot?version=3.0")
    print(url)
    doc <- read_xml(URLencode(url))

    snapshots <- xml_text(xml_find_all(doc, "//*[local-name() = 'element'][@name='snapshot time']"))
    print(snapshots)
    snapshots <- snapshots[snapshots != "latest"]
    snapshots <- snapshots[order(as.Date(snapshots), decreasing = TRUE)]
    print(snapshots)
    snapshot <- snapshots[1]
    }

    # Construct the correct file name with snapshot
    correctFile <- paste0("biotic_cruiseNumber_", cruiseNo, "_", dbShipName, "_", snapshot, ".xml")

    # Download latest file
    url <- paste0("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/", path, "/snapshot/", snapshot, "?version=3.0")
    download.file(URLencode(url), paste0(targetDir, "/input/biotic/", correctFile))

    return(correctFile)
}

createTempDir <- function() {
	# Create dir structure
    target <- paste0(tempdir(), "/zip/", sample(10000000:99999999, 1))
    
    # Repeat random direname until doesn't exists
    while(dir.exists(target)){
        target <- paste0(tempdir(), "/", sample(10000000:99999999, 1))
    }

    structure <- c("input/biotic", "input/acoustic", "process", "output")
    lapply(paste0(target, "/", structure), dir.create, recursive = TRUE)

	return(target)

}

prepareInputFiles <- function(doc, target) {
    # Get acoustic data filenames
    acousticFiles <- basename(xml_text(xml_find_all(doc, "//*[local-name() = 'process'][@name='ReadAcousticXML']//*[local-name() = 'parameter'][@name]")))
    acousticOK <- unlist(lapply(acousticFiles, function(i) try(downloadEchosounder(i, target), TRUE)))

    # Get biotic data filenames
    bioticXML <- xml_find_all(doc, "//*[local-name() = 'process'][@name='ReadBioticXML']//*[local-name() = 'parameter'][@name]")
    bioticFiles <- basename(xml_text(bioticXML))

    # Download and append snapshot
    bioticOK <- unlist(lapply(bioticFiles, function(i) try(appendSnapshot(i, target), TRUE)))

	return(list(acousticFiles=acousticFiles, acousticOK=acousticOK, bioticFiles=bioticFiles, bioticXML=bioticXML, bioticOK=bioticOK))
}

#' @import xml2 data.table Rstox
checkDataOne <- function(file, ignoreSnapshots) {

	# Additional notes for detailed errors
	noteRstox <-	"<br/><br/><i class=\"fas fa-2x fa-exclamation-circle\" style=\"color: orange\"></i> <strong>Some tips:</strong><br/>
						<ol>
							<li>Make sure that the <code>project.xml</code> file is pointing to the correct input file names.</li>
							<li>Double check the <b>UseProcessData</b> parameters in the <code>project.xml</code> file. Usually these should be set as <b>true</b>.<br/>
								Setting this as <b>false</b> can cause Rstox's baseline process to break.
							</li>
							<li>Double check the <code>&lt;stratumpolygon&gt;</code> section in the <code>project.xml</code> file.<br/>
								Sometimes all or most of the <b>includeintotal</b> parameters are accicentally set to <b>false</b>.<br/>
								This has been known to be the common cause of the empty abundance result.
							</li>
						</ol>
					"
	noteInput <- 	"<br/><br/><i class=\"fas fa-2x fa-exclamation-circle\" style=\"color: orange\"></i> <strong>Some tips:</strong><br/>
						<ol>
							<li>Double check the file names for errors (e.g., no trailing <b>... (1).xml</b>).</li>
							<li>Ensure there is a valid snapshot time appended to the file names.<br/>
								As an example, always download biotic files from the list of snapshots
								available, such as <a href=\"https://datasetexplorer.hi.no/apps/datasetexplorer/v2/Cruises/Forskningsfartøy/2020/Johan%20Hjort_LDGJ/2020203/datatype/biotic\" target=\"_blank\">here</a>.
							</li>
							<li>Make sure that the <code>project.xml</code> file is pointing to the correct input files (i.e., ship name and cruise number combinations are valid).</li>
						</ol>
					"

	makeTable <- function(statusOutput, detailOutput) {
		x <- cbind(as.data.frame(names(statusOutput)), t(as.data.frame(statusOutput)), t(as.data.frame(detailOutput)))
		colnames(x) <- c("Checklists", "Status", "Details")
		return(x)
	}

	# Output
	statusOutput <- list("Project file" = FALSE,
				"Acoustic Files" = FALSE,
				"Biotic Files" = FALSE,
				"StoX Process" = FALSE
				)
	detailOutput <- statusOutput
	stdout <- vector('character')
	report <- NA

	# Get temp directory
    target <- createTempDir()

	xmlFile <- paste0(target, "/process/project.xml")
    file.copy(file, xmlFile)

	doc <- try(read_xml(xmlFile), TRUE)

	# Check Project XML
	if(inherits(doc, "try-error")) {
		detailOutput[["Project file"]] <- as.character(doc)
		return(list(makeTable(statusOutput, detailOutput), report, stdout))
	} else {
		statusOutput[["Project file"]] <- TRUE
		detailOutput[["Project file"]] <- ""
	}

	# Prepare all input files
	inputFiles <- prepareInputFiles(doc, target)

	# Check biotic files
	if (!all(inputFiles$bioticFiles == inputFiles$bioticOK)) {
		detailOutput[["Biotic Files"]] <- paste0("<strong>Problematic files:</strong><br/><code>", paste(inputFiles$bioticFiles[which(inputFiles$bioticFiles != inputFiles$bioticOK)], collapse = "<br/>"), "</code>", noteInput)

		# If user decides to ignore biotic snapshots
		if(ignoreSnapshots == TRUE) {

			# Update biotic filenames in XML
			xml_text(inputFiles$bioticXML) <- paste0("input/biotic/", inputFiles$bioticOK)

			# Update timestamp
			xml_attr(doc, "lastmodified") <- format(Sys.time(), "%d/%m/%y %H:%M")

			# Write back xml file
			unlink(xmlFile)
			write_xml(doc, xmlFile)

			# Use a yellow status
			statusOutput[["Biotic Files"]] <- "<i class=\"fas fa-2x fa-exclamation-triangle\" style=\"color: orange\"></i>"

			# Update information
			detailOutput[["Biotic Files"]] <- paste0("<strong>Files added as replacement:</strong><br/><code>", paste(inputFiles$bioticOK[which(inputFiles$bioticFiles != inputFiles$bioticOK)], collapse = "<br/>"), "</code><br/>", detailOutput[["Biotic Files"]])
		}
	} else {
		detailOutput[["Biotic Files"]] <- ""
		statusOutput[["Biotic Files"]] <- TRUE
	}

	# Check acoustic files
	if (!all(inputFiles$acousticFiles == inputFiles$acousticOK )) {
		detailOutput[["Acoustic Files"]] <- paste0("<strong>Problematic files:</strong><br/><code>", paste(inputFiles$acousticFiles[which(inputFiles$acousticFiles != inputFiles$acousticOK)], collapse = "<br/>"), "</code>", noteInput)
	} else {
		detailOutput[["Acoustic Files"]] <- ""
		statusOutput[["Acoustic Files"]] <- TRUE
	}

	# Check Rstox
	stdout <- NULL
	rp <- list()
	detailOutput[["StoX Process"]] <- ""
	report <- data.frame(NULL)
	if (requireNamespace("Rstox", quietly = FALSE)) {

		# Run Rstox
		args <- paste0("-e 'target <-\"", target, "\";",
				"library(Rstox);",
				"g <- try(getBaseline(target), TRUE);",
				"bs <- try(runBootstrap(target, nboot=5, cores=1, seed=1, acousticMethod=PSU~Stratum, bioticMethod=EDSU~Stratum), TRUE);",
				"im <- try(imputeByAge(target), TRUE);",
				"rp <- try(getReports(target), TRUE);",
				"saveRDS(rp, paste0(target, \"/result.rds\"))",
				"'")

		stdout <- system2("Rscript", args = args, stdout = TRUE, stderr = TRUE)
		print(stdout)

		# Get report result
		rp <- readRDS(paste0(target, "/result.rds"))

		# Gather report error
		if(inherits(rp, "try-error")) {
			detailOutput[["StoX Process"]] <- as.character(rp)
		} else {
			if(!is.null(rp$bootstrapImpute) && nrow(rp$bootstrapImpute$abnd) > 0) {
				report <- rp$bootstrapImpute$abnd
				statusOutput[["StoX Process"]] <- TRUE
			} else {
				detailOutput[["StoX Process"]] <- paste0("<strong>No valid abundance table is produced after <code>bootstrapImpute()</code> is called. Something is seriously wrong!</strong>", noteRstox)
			}
		}
	} else {
		detailOutput[["StoX Process"]] <- "No Rstox found in R. Disabling Rstox test."
	}

	# Clean up
	unlink(target, recursive = TRUE)

	return(list(makeTable(statusOutput, detailOutput), report, stdout))
}

prepareDataOne <- function(yr, stsList, stsName) {

    # Get data
    id <- stsList[[stsName]][year==as.numeric(yr), c("id")]
    print(id)

    prefix <- "http://tomcat7.imr.no:8080/apis/nmdapi/stox/v1/"

	# Get temp directory
    target <- createTempDir()

    oldFile <- paste0(target, "/process/project.xml")
    download.file(URLencode(paste0(prefix, id)), oldFile)

    doc <- read_xml(oldFile)
    #xml_ns_strip(doc)
    print(doc)

	# Prepare all input files
	inputFiles <- prepareInputFiles(doc, target)

    # Getting last modified
    currentTime <- Sys.time()

    xmlProject <- xml_find_all(doc, "//*[local-name() = 'project']")

    lastModified <- xml_attr(xmlProject, "lastmodified")
    if (is.na(lastModified))
    lastModified <- format(as.Date("1970-01-01"), "%Y-%m-%dT%H.%M.%SZ")
    else
    lastModified <- format(as.Date(lastModified, format="%d/%m/%y %H:%M"), "%Y-%m-%dT%H.%M.%SZ")

    warningMsg = ""

    # Check whether we modify any of the biotic filenames without snapshot
    if (!all(inputFiles$bioticFiles == inputFiles$bioticOK)) {

	    warningMsg <- "The original project XML file contains biotic filename(s) without snapshot and therefore has been modified to use snapshots. The original project XML file have been saved in the /process directory."

	    # Update biotic filenames in XML
	    xml_text(inputFiles$bioticXML) <- paste0("input/biotic/", inputFiles$bioticOK)

	    # Update timestamp
	    xml_attr(xmlProject, "lastmodified") <- format(currentTime, "%d/%m/%y %H:%M")

	    # Backup old doc
	    # Rename old file using timestamp (if any)
	    file.rename(oldFile, paste0(target, "/process/stox_", stsName, "_", yr, "_", lastModified, ".xml"))

	    # Save XML
	    #write_xml(doc, paste0(target, "/process/stox_", stsName, "_", yr, "_", format(currentTime, "%Y-%m-%dT%H.%M.%SZ"), ".xml"))
	    write_xml(doc, paste0(target, "/process/project.xml"))
    }

    # Compose result
    return(list(outDir=target, warning=warningMsg))
}

prepareData <- function(stsList, stsName, yr) {

	if(length(yr) == 1) {
		return(prepareDataOne(yr, stsList, stsName))
	} else {
		print("Download multiple years:")
		print(yr)
		combined <- lapply(yr, prepareDataOne, stsList, stsName)

		# Create dir structure
		target <- paste0(tempdir(), "/zip/", sample(10000000:99999999, 1))

		# Repeat random direname until doesn't exists
		while(dir.exists(target)){
			target <- paste0(tempdir(), "/", sample(10000000:99999999, 1))
		}
		dir.create(target)

		# Copy dirs and process msgs
		msgs <- list()
		structure <- c("input", "process", "output")
		for(curYr in 1:length(yr)) {
			sour <- combined[[curYr]][["outDir"]]
			dest <- paste0(target, "/", stsName, "_", yr[[curYr]])
			dir.create(dest)
			lapply(paste0(sour, "/", structure), file.copy, dest, recursive = TRUE)
			unlink(sour, recursive = TRUE)

			# Process message
			msg <- combined[[curYr]][["warning"]]
			if(nchar(msg) > 0)
				msgs[[curYr]] <- yr[[curYr]]
		}

		print(list.files(dest, all.files = TRUE, recursive = TRUE))

		# Combine warning
		warningMsg <- "The original project XML file contains biotic filename(s) without snapshot and therefore has been modified to use snapshots. The original project XML file have been saved in the /process directory."
		warningMsg <- paste(warningMsg, "Affected years:", paste(msgs, collapse=", "))

		# Compose result
		return(list(outDir=target, warning=warningMsg))
	}
}


readSTS <- function() {

src <- read_xml("http://tomcat7.imr.no:8080/apis/nmdapi/reference/v2/dataset/surveytimeseries?version=2.0")
xml_ns_strip(src)

stsName <- xml_text(xml_find_all(src, "//row/name"))
stsSamples <- xml_find_all(src, "//row/cruiseSeries/cruiseSeries/samples")

tmp <- lapply(stsSamples, function(x) lapply(xml_children(x), function(y) { z <- lapply(xml_children(y), xml_text); names(z) <- xml_name(xml_children(y)); return(z) }))

parsedSts <- lapply(tmp, function(x) rbindlist(lapply(x, function(y) list(year=y[["sampleTime"]], id=y[["stoxProject"]]))))
names(parsedSts) <- stsName

return(parsedSts)

}

