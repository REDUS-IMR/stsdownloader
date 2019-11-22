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

    dbShipName <- shipName

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


prepareData <- function(stsList, stsName, yr) {

    # Get data
    id <- stsList[[stsName]][year==as.numeric(yr), c("id")]
    print(id)

    prefix <- "http://tomcat7.imr.no:8080/apis/nmdapi/stox/v1/"

    # Create dir structure
    target <- paste0(tempdir(), "/zip/", sample(10000000:99999999, 1))
    
    # Repeat random direname until doesn't exists
    while(dir.exists(target)){
        target <- paste0(tempdir(), "/", sample(10000000:99999999, 1))
    }

    structure <- c("input/biotic", "input/acoustic", "process", "output")
    lapply(paste0(target, "/", structure), dir.create, recursive = TRUE)

    oldFile <- paste0(target, "/process/project.xml")
    download.file(URLencode(paste0(prefix, id)), oldFile)

    doc <- read_xml(oldFile)
    #xml_ns_strip(doc)
    print(doc)

    # Get acoustic data filenames
    acousticFiles <- basename(xml_text(xml_find_all(doc, "//*[local-name() = 'process'][@name='ReadAcousticXML']//*[local-name() = 'parameter'][@name]")))
    acousticOK <- unlist(lapply(acousticFiles, downloadEchosounder, target))

    # Get biotic data filenames
    bioticXML <- xml_find_all(doc, "//*[local-name() = 'process'][@name='ReadBioticXML']//*[local-name() = 'parameter'][@name]")
    bioticFiles <- basename(xml_text(bioticXML))

    # Append snapshot
    bioticOK <- unlist(lapply(bioticFiles, appendSnapshot, target))

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
    if (!all(bioticFiles == bioticOK)) {

	    warningMsg <- "The original project XML file contains biotic filename(s) without snapshot and therefore has been modified to use snapshots. The original project XML file have been saved in the /process directory."

	    # Update biotic filenames in XML
	    xml_text(bioticXML) <- paste0("input/biotic/", bioticOK)

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

