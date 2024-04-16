# All functions needed to hack rgbif to scrape data from gbif-uat.org (test)

occ_data_testurl <- function(taxonKey=NULL,
                     scientificName=NULL, 
                     country=NULL,
                     publishingCountry=NULL,
                     hasCoordinate=NULL, 
                     typeStatus=NULL,
                     recordNumber=NULL,
                     lastInterpreted=NULL,
                     continent=NULL,
                     geometry=NULL,
                     geom_big="asis",
                     geom_size=40,
                     geom_n=10,
                     recordedBy=NULL,
                     recordedByID=NULL,
                     identifiedByID=NULL,
                     basisOfRecord=NULL,
                     datasetKey=NULL,
                     eventDate=NULL,
                     catalogNumber=NULL,
                     year=NULL,
                     month=NULL,
                     decimalLatitude=NULL,
                     decimalLongitude=NULL,
                     elevation=NULL,
                     depth=NULL,
                     institutionCode=NULL,
                     collectionCode=NULL,
                     hasGeospatialIssue=NULL,
                     issue=NULL,
                     search=NULL,
                     mediaType=NULL,
                     subgenusKey = NULL,
                     repatriated = NULL,
                     phylumKey = NULL,
                     kingdomKey = NULL,
                     classKey = NULL,
                     orderKey = NULL,
                     familyKey = NULL,
                     genusKey = NULL,
                     speciesKey = NULL,
                     establishmentMeans = NULL,
                     degreeOfEstablishment = NULL,
                     protocol = NULL,
                     license = NULL,
                     organismId = NULL,
                     publishingOrg = NULL,
                     stateProvince = NULL,
                     waterBody = NULL,
                     locality = NULL,
                     limit=500,
                     start=0,
                     skip_validate = TRUE,
                     occurrenceStatus = 'PRESENT',
                     gadmGid = NULL,
                     coordinateUncertaintyInMeters = NULL,
                     verbatimScientificName = NULL,
                     eventId = NULL,
                     identifiedBy = NULL,
                     networkKey = NULL,
                     verbatimTaxonId = NULL,
                     occurrenceId = NULL,
                     organismQuantity = NULL,
                     organismQuantityType = NULL,
                     relativeOrganismQuantity = NULL,
                     iucnRedListCategory = NULL,
                     lifeStage = NULL,
                     isInCluster = NULL,
                     distanceFromCentroidInMeters = NULL,
                     curlopts = list(http_version=2)) {
  
  geometry <- geometry_handler(geometry, geom_big, geom_size, geom_n)
  
  # url <- paste0(gbif_base(), '/occurrence/search')
  url <- paste0("https://api.gbif-uat.org/v1", '/occurrence/search')
  argscoll <- NULL
  
  .get_occ_data <- function(x=NULL, itervar=NULL, curlopts = list()) {
    if (!is.null(x)) {
      assign(itervar, x)
    }
    
    # check that wkt is proper format and of 1 of 4 allowed types
    geometry <- check_wkt(geometry, skip_validate = skip_validate)
    
    # check limit and start params
    check_vals(limit, "limit")
    check_vals(start, "start")
    
    # Make arg list
    args <- rgbif_compact(
      list(
        hasCoordinate = hasCoordinate,
        hasGeospatialIssue = hasGeospatialIssue,
        occurrenceStatus = occurrenceStatus,
        q = search,
        repatriated = repatriated,
        limit = check_limit(as.integer(limit)),
        isInCluster = isInCluster,
        offset = check_limit(as.integer(start))
      )
    )
    args <- c(
      args,
      parse_issues(issue),
      convmany(lastInterpreted),
      convmany(decimalLatitude),
      convmany(decimalLongitude),
      convmany(elevation),
      convmany(depth), 
      convmany(eventDate),
      convmany(month),
      convmany(year),
      convmany(coordinateUncertaintyInMeters),
      convmany(organismQuantity),
      convmany(organismQuantityType),
      convmany(relativeOrganismQuantity),
      convmany(taxonKey),
      convmany(scientificName),
      convmany(country),
      convmany(publishingCountry),
      convmany(datasetKey),
      convmany(typeStatus),
      convmany(recordNumber),
      convmany(continent),
      convmany(recordedBy),
      convmany(recordedByID),
      convmany(identifiedByID),
      convmany(catalogNumber),
      convmany(institutionCode),
      convmany(collectionCode),
      convmany(geometry),
      convmany(mediaType),
      convmany(subgenusKey),
      convmany(phylumKey),
      convmany(kingdomKey),
      convmany(classKey),
      convmany(orderKey),
      convmany(familyKey),
      convmany(genusKey),
      convmany(speciesKey),
      convmany(establishmentMeans),
      convmany(degreeOfEstablishment),
      convmany(protocol),
      convmany(license),
      convmany(organismId),
      convmany(publishingOrg),
      convmany(stateProvince),
      convmany(waterBody),
      convmany(locality),
      convmany(basisOfRecord),
      convmany(gadmGid),
      convmany(verbatimScientificName),
      convmany(eventId),
      convmany(identifiedBy),
      convmany(networkKey),
      convmany(occurrenceId),
      convmany(iucnRedListCategory),
      convmany(lifeStage),
      convmany(distanceFromCentroidInMeters)
    )
    argscoll <<- args
    
    if (limit >= 300) {
      ### loop route for limit>0
      iter <- 0
      sumreturned <- 0
      outout <- list()
      while (sumreturned < limit) {
        iter <- iter + 1
        tt <- gbif_GET(url, args, TRUE, curlopts)
        
        # if no results, assign count var with 0
        if (identical(tt$results, list())) tt$count <- 0
        
        numreturned <- NROW(tt$results)
        sumreturned <- sumreturned + numreturned
        
        if (tt$count < limit) {
          limit <- tt$count
        }
        
        if (sumreturned < limit) {
          args$limit <- limit - sumreturned
          args$offset <- sumreturned + start
        }
        outout[[iter]] <- tt
      }
    } else {
      ### loop route for limit<300
      outout <- list(gbif_GET(url, args, TRUE, curlopts))
    }
    
    meta <- outout[[length(outout)]][c('offset', 'limit', 'endOfRecords',
                                       'count')]
    data <- lapply(outout, "[[", "results")
    
    if (identical(data[[1]], list())) {
      data <- NULL
    } else {
      data <- lapply(data, clean_data)
      data <- data.table::setDF(data.table::rbindlist(data, use.names = TRUE,
                                                      fill = TRUE))
      data <- tibble::as_tibble(prune_result(data))
    }
    
    list(meta = meta, data = data)
  }
  
  params <- list(
    taxonKey=taxonKey,
    scientificName=scientificName,
    datasetKey=datasetKey,
    catalogNumber=catalogNumber,
    recordedBy=recordedBy,
    recordedByID=recordedByID,
    identifiedByID=identifiedByID,
    geometry=geometry,
    country=country,
    publishingCountry=publishingCountry,
    recordNumber=recordNumber,
    q=search,
    institutionCode=institutionCode,
    collectionCode=collectionCode,
    continent=continent,
    decimalLatitude=decimalLatitude,
    decimalLongitude=decimalLongitude,
    depth=depth,
    year=year,
    typeStatus=typeStatus,
    lastInterpreted=lastInterpreted,
    mediaType=mediaType,
    subgenusKey=subgenusKey,
    repatriated=repatriated,
    phylumKey=phylumKey,
    kingdomKey=kingdomKey,
    classKey=classKey,
    orderKey=orderKey,
    familyKey=familyKey,
    genusKey=genusKey,
    speciesKey=speciesKey,
    establishmentMeans=establishmentMeans,
    protocol=protocol,
    license=license,
    organismId=organismId,
    publishingOrg=publishingOrg,
    stateProvince=stateProvince,
    waterBody=waterBody,
    locality=locality,
    limit=limit, 
    basisOfRecord=basisOfRecord, 
    occurrenceStatus=occurrenceStatus,
    gadmGid=gadmGid,
    verbatimScientificName=verbatimScientificName,
    eventId=eventId,
    identifiedBy=identifiedBy,
    networkKey=networkKey,
    occurrenceId=occurrenceId,
    organismQuantity=organismQuantity,
    organismQuantityType=organismQuantityType,
    relativeOrganismQuantity=relativeOrganismQuantity,
    iucnRedListCategory=iucnRedListCategory,
    lifeStage=lifeStage,
    coordinateUncertaintyInMeters=coordinateUncertaintyInMeters,
    distanceFromCentroidInMeters=distanceFromCentroidInMeters
  )
  if (!any(sapply(params, length) > 0)) {
    stop(sprintf("At least one of the parmaters must have a value:\n%s",
                 possparams()),
         call. = FALSE)
  }
  iter <- params[which(sapply(params, length) > 1)]
  if (length(names(iter)) > 1) {
    stop(sprintf("You can have multiple values for only one of:\n%s",
                 possparams()),
         call. = FALSE)
  }
  
  if (length(iter) == 0) {
    out <- .get_occ_data(curlopts = curlopts)
  } else {
    out <- lapply(iter[[1]], .get_occ_data, itervar = names(iter), 
                  curlopts = curlopts)
    names(out) <- transform_names(iter[[1]])
  }
  
  if (any(names(argscoll) %in% names(iter))) {
    argscoll[[names(iter)]] <- iter[[names(iter)]]
  }
  
  structure(out, args = argscoll, class = "gbif_data",
            type = if (length(iter) == 0) "single" else "many")
}


geometry_handler <- function(x, geom_big = "asis", size = 40, n = 10, verbose = TRUE) {
  gbopts <- c('asis', 'bbox', 'axe')
  if (!geom_big %in% gbopts) {
    stop('geom_big must be one of: ', paste0(gbopts, collapse = ", "))
  }
  
  assert(size, c("numeric", "integer"))
  assert(n, c("numeric", "integer"))
  if (size <= 0) stop("geom_size must be > 0")
  if (n <= 0) stop("geom_n must be > 0")
  
  if (!is.null(x)) {
    if (!is.character(x)) {
      return(gbif_bbox2wkt(bbox = x))
    }
    
    out <- c()
    for (i in seq_along(x)) {
      out[[i]] <- switch(geom_big,
                         asis = {
                           x[i]
                         },
                         bbox = {
                           if (nchar(x[i]) > 1500) {
                             if (verbose) message("geometry is big, querying BBOX, then pruning results to polygon")
                             # set run time option so that we know to prune result once it returns
                             options(rgbif.geometry.original = x)
                             gbif_bbox2wkt(bbox = gbif_wkt2bbox(x[i]))
                           } else {
                             x[i]
                           }
                         },
                         axe = {
                           check_for_a_pkg("sf")
                           xsf <- sf::st_as_sfc(x[i])
                           gt <- sf::st_make_grid(xsf, cellsize = rep(size, 2), n = rep(n, 2))
                           res <- sf::st_intersection(xsf, gt)
                           unlist(lapply(res, function(w) {
                             if (inherits(w, "MULTIPOLYGON")) mp2wkt(w) else sf::st_as_text(w)
                           }))
                         }
      )
      
    }
    unlist(out, recursive = FALSE)
    
  } else {
    return(x)
  }
}

mp2wkt <- function(x) {
  z <- lapply(x, sf::st_polygon)
  vapply(z, sf::st_as_text, "")
}

prune_result <- function(x) {
  geom_orig <- getOption('rgbif.geometry.original')
  if (!is.null(geom_orig)) {
    check_for_a_pkg("sf")
    poly_orig <- sf::st_as_sf(data.frame(wkt=geom_orig),wkt="wkt",crs = "+proj=longlat +datum=WGS84")
    xx <- sf::st_as_sf(x, coords = c("decimalLongitude", "decimalLatitude"),crs = "+proj=longlat +datum=WGS84")
    clipped <- unlist(sf::st_intersects(xx,poly_orig,sparse=FALSE)) == 1
    clipped[is.na(clipped)] <- FALSE
    x <- x[clipped, ]
  }
  # set rgbif.geometry.original to NULL on return
  options(rgbif.geometry.original = NULL)
  return(x)
}

is_integer <- function(x){
  !grepl("[^[:digit:]]", format(x,  digits = 20, scientific = FALSE))
}



get_hier <- function(x, h1, h2){
  name <- data.frame(
    t(data.frame(x[names(x) %in% h1], stringsAsFactors=FALSE)),
    stringsAsFactors=FALSE)
  if (nrow(name)==0){
    data.frame(name=NA_character_, key=NA_character_, rank=NA_character_,
               row.names=NULL, stringsAsFactors=FALSE)
  } else {
    name$ranks <- row.names(name)
    name <- name[order(match(name$ranks, h1)), ]
    tt <- as.matrix(name[,1])
    row.names(tt) <- name[,2]
    name <- tt
    key <- t(data.frame(x[names(x) %in% h2], stringsAsFactors=FALSE))
    data.frame(name=name, key=as.character(key), rank=row.names(name),
               row.names=NULL, stringsAsFactors=FALSE)
  }
}

# Parser for gbif data
# param: input A list
# @param: fields (character) Default ("minimal") will return just taxon name, key, latitude, and
#    longitute. "all" returns all fields. Or specify each field you want returned by name, e.g.
#    fields = c('name','latitude','altitude').
gbifparser <- function(input, fields= "minimal") {
  parse <- function(x){
    x[sapply(x, length) == 0] <- "none"
    h1 <- c('kingdom','phylum','class','order','family','genus',"species")
    h2 <- c('kingdomKey','phylumKey','classKey','orderKey','familyKey',
            'genusKey','speciesKey')
    hier <- get_hier(x, h1, h2)
    
    # issues
    x[names(x) %in% "issues"] <- collapse_issues(x)
    # networkKeys
    x[names(x) %in% "networkKeys"] <- sapply(x[names(x) %in% "networkKeys"], function(x) paste(unlist(x),collapse=","))
    
    # media
    if ("media" %in% names(x)) {
      media <- x[names(x) %in% "media"]
      media <- lapply(media$media, as.list)
      media2 <- list()
      iter <- seq(1, length(media), 2)
      for(i in iter){
        media2[[i]] <- as.list(unlist(c(media[i], media[i+1])))
      }
      media2 <- rgbif_compact(media2)
      media2$key <- as.character(x$key)
      media2$species <- x$species
      media2$decimalLatitude <- x$decimalLatitude
      media2$decimalLongitude <- x$decimalLongitude
      media2$country <- x$country
      media2 <- list(media2)
      names(media2) <- x$key
      x <- x[!names(x) %in% "media"] # remove images
    } else { media2 <- list() }
    
    # extensions is a PITA, just remove
    x$extensions <- NULL
    
    # remove any fields > length 1
    lgts <- unname(sapply(x, length))
    if (any(lgts > 1)) x[lgts > 1] <- NULL
    
    # all other data
    x <- data.frame(x, stringsAsFactors=FALSE)
    if (any(fields == "minimal")) {
      if (all(c("decimalLatitude","decimalLongitude") %in% names(x))) {
        x <-
          x[c("key","datasetKey", "scientificName", "decimalLatitude",
              "decimalLongitude", "issues")]
      } else {
        x <- data.frame(x["key"], x["scientificName"],
                        decimalLatitude = NA, decimalLongitude = NA,
                        x["issues"], stringsAsFactors = FALSE)
      }
    } else if(any(fields == "all")) {
      # rearrange columns
      firstnames <- c("key", "scientificName", "decimalLatitude",
                      "decimalLongitude", "issues")
      x <- x[c(firstnames[firstnames %in% names(x)],
               names(x)[-unlist(rgbif_compact(sapply(firstnames, function(z) {
                 tmp <- grep(z, names(x)); if(!length(tmp) == 0) tmp
               }, USE.NAMES = FALSE)))])]
      # add name column, duplicate of scientificName, to not break downstream code
      if ("scientificName" %in% names(x)) {
        x$name <- x$scientificName
      }
    } else {
      x <- x[names(x) %in% fields]
    }
    # make key and gbifID character class
    if ("key" %in% names(x)) x$key <- as.character(x$key)
    if ("gbifID" %in% names(x)) x$gbifID <- as.character(x$gbifID)
    list(hierarchy = hier, media = media2, data = x)
  }
  if (is.numeric(input[[1]])) {
    parse(input)
  } else {
    lapply(input, parse)
  }
}

clean_data <- function(x){
  # collapse issues
  if (!identical(x, list())) x[names(x) %in% "issues"] <- collapse_issues_vec(x)
  
  # drop media, facts, relations, etc.
  x$media <- x$facts <- x$relations <- x$identifiers <-
    x$extensions <- x$gadm <- x$recordedByIDs <- x$identifiedByIDs <- NULL
  
  # drop any new columns that GBIF adds of which each element is not length 1
  x[sapply(x, is.data.frame)] <- NULL
  
  # add name column, duplicate of scientificName, to not break downstream code
  if ("scientificName" %in% names(x)) {
    x$name <- x$scientificName
  }
  
  # move columns
  x <- move_col(x, "issues")
  x <- move_col(x, "decimalLongitude")
  x <- move_col(x, "decimalLatitude")
  x <- move_col(x, "scientificName")
  x <- move_col(x, "key")
  
  # make key and gbifID character class
  if ("key" %in% names(x)) x$key <- as.character(x$key)
  if ("gbifID" %in% names(x)) x$gbifID <- as.character(x$gbifID)
  
  return(x)
}

# Parser for gbif data
# param: input A list
# param: fields (character) Default ("minimal") will return just taxon name,
#    key, decimalLatitude, and decimalLongitute. "all" returns all fields. Or
#    specify each field you want returned by name, e.g. fields =
#    c('name','decimalLatitude','altitude').
gbifparser_verbatim <- function(input, fields="minimal") {
  parse <- function(x) {
    nn <- vapply(names(x), function(z) {
      tmp <- strsplit(z, "/")[[1]]
      tmp[length(tmp)]
    }, "", USE.NAMES = FALSE)
    
    names(x) <- nn
    
    if (any(fields == "minimal")) {
      if (all(c("decimalLatitude","decimalLongitude") %in% names(x))) {
        x <- x[c("key", "scientificName", "decimalLatitude", "decimalLongitude")]
      } else {
        x <- list(key = x[["key"]], scientificName = x[["scientificName"]],
                  decimalLatitude = NA, decimalLongitude = NA, stringsAsFactors = FALSE)
      }
    } else if (any(fields == "all")) {
      x[vapply(x, length, 0) == 0] <- "none"
      if ("extensions" %in% names(x)) {
        if (length(x$extensions) == 0) {
          x$extensions <- NULL
        } else if (identical(x$extensions[[1]], "none")) {
          x$extensions <- NULL
        } else {
          m <- list()
          for (i in seq_along(x$extensions)) {
            z <- x$extensions[[i]]
            names(z) <- sprintf("extensions_%s_%s",
                                basename(names(x$extensions)[i]), names(z))
            m[[i]] <- as.list(z)
          }
          
          x$extensions <- NULL
          x <- c(x, as.list(unlist(m)))
        }
      }
    } else {
      x[vapply(x, length, 0) == 0] <- "none"
      x <- x[names(x) %in% fields]
    }
    if ("key" %in% names(x)) x$key <- as.character(x$key)
    if ("gbifID" %in% names(x)) x$gbifID <- as.character(x$gbifID)
    return(x)
  }
  
  if (is.numeric(input[[1]])) {
    data.frame(parse(input), stringsAsFactors = FALSE)
  } else {
    setdfrbind(lapply(input, function(w) data.frame(parse(w),
                                                    stringsAsFactors = FALSE)))
  }
}


ldfast <- function(x, convertvec=FALSE){
  if (convertvec) {
    setdfrbind(lapply(x, convert2df))
  } else {
    setdfrbind(x)
  }
}

ldfast_names <- function(x, convertvec=FALSE){
  for (i in seq_along(x)) {
    x[[i]] <- data.frame(.id = names(x)[i], x[[i]], stringsAsFactors = FALSE)
  }
  ldfast(x, convertvec)
}

convert2df <- function(x){
  if (!inherits(x, "data.frame")) {
    tibble::as_tibble(rbind(x))
  } else {
    tibble::as_tibble(x)
  }
}

rbind_rows <- function(x, by) {
  tmp <- data.frame(names(x), count = unname(unlist(x)),
                    stringsAsFactors = FALSE)
  row.names(tmp) <- NULL
  names(tmp)[1] <- by
  return(tmp)
}

#' Custom ggplot2 theme
#' @export
#' @keywords internal
blanktheme <- function(){
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        plot.margin = u_nit())
}

u_nit <- function() {
  structure(c(0, 0, 0, 0), unit = c("null", "null", "null", "null"
  ), valid.unit = c(5L, 5L, 5L, 5L), class = "unit")
}


# Capitalize the first letter of a character string.
gbif_capwords <- function(s, strict = FALSE, onlyfirst = FALSE) {
  cap <- function(s) paste(toupper(substring(s,1,1)),
                           {s <- substring(s,2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
  if(!onlyfirst){
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  } else {
    sapply(s, function(x)
      paste(toupper(substring(x,1,1)),
            tolower(substring(x,2)),
            sep="", collapse=" "), USE.NAMES=F)
  }
}

namelkupparser <- function(x){
  tmp <- x[ !names(x) %in% c("descriptions", "vernacularNames", "higherClassificationMap") ]
  tmp <- lapply(tmp, function(x) {
    if (length(x) == 0) {
      NA
    } else if (length(x) > 1 || inherits(x, "list")) {
      paste0(x, collapse = ", ")
    } else {
      x
    }
  })
  df <- data.frame(tmp, stringsAsFactors = FALSE)
  movecols(df, c("key", "scientificName"))
}

namelkupcleaner <- function(x){
  tmp <- x[ !names(x) %in% c("descriptions", "vernacularNames", "higherClassificationMap") ]
  lapply(tmp, function(x) {
    if (length(x) == 0) {
      NA
    } else if (length(x) > 1 || inherits(x, "list")) {
      paste0(x, collapse = ", ")
    } else {
      x
    }
  })
}

nameusageparser <- function(z){
  tomove <- c("key", "scientificName")
  tmp <- lapply(z, function(y) {
    if (length(y) == 0) NA else y
  })
  # reduce multiple element slots to comma sep
  if ("issues" %in% names(tmp)) {
    tmp[names(tmp) %in% "issues"] <- collapse_issues(tmp)
  }
  df <- tibble::as_tibble(tmp)
  if (all(tomove %in% names(df))) {
    movecols(df, tomove)
  } else {
    df
  }
}

backbone_parser <- function(x){
  tmp <- lapply(x, function(x) if (length(x) == 0) NA else x)
  data.frame(tmp, stringsAsFactors = FALSE)
}

is_null_or_na <- function(x) {
  if (is.environment(x)) return(FALSE)
  is.null(x) || all(is.na(x))
}

# allows all elements in a list, except two things, which are removed:
# - NULL
# - NA
# while detecting environments and passing on them
rgbif_compact <- function(l) Filter(Negate(is_null_or_na), l)
rc <- rgbif_compact

compact_null <- function(l){
  tmp <- rgbif_compact(l)
  if (length(tmp) == 0) NULL else tmp
}

# REST helpers ---------------------------------------
rgbif_ua <- function() {
  versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
                paste0("crul/", utils::packageVersion("crul")),
                sprintf("rOpenSci(rgbif/%s)", utils::packageVersion("rgbif")))
  paste0(versions, collapse = " ")
}

rgbif_ual <- list(`User-Agent` = rgbif_ua(), `X-USER-AGENT` = rgbif_ua())

gbif_GET <- function(url, args, parse=FALSE, curlopts = list(), mssg = NULL) {
  cli <- crul::HttpClient$new(url = url, headers = rgbif_ual, opts = curlopts)
  temp <- cli$get(query = args)
  if (temp$status_code == 204)
    stop("Status: 204 - not found ", mssg, call. = FALSE)
  if (temp$status_code > 200) {
    mssg <- temp$parse("UTF-8")
    if (grepl("html", mssg)) {
      stop("500 - Server error", call. = FALSE)
    }
    if (length(mssg) == 0 || nchar(mssg) == 0) {
      mssg <- temp$status_http()$message
    }
    if (temp$status_code == 503) mssg <- temp$status_http()$message
    stop(mssg, call. = FALSE)
  }
  # check content type
  stopifnot(temp$response_headers$`content-type` == 'application/json')
  # parse JSON
  json <- jsonlite::fromJSON(temp$parse("UTF-8"), parse)
  return(json)
}

gbif_GET_content <- function(url, args, curlopts = list()) {
  cli <- crul::HttpClient$new(url = url, headers = rgbif_ual, opts = curlopts)
  temp <- cli$get(query = args)
  if (temp$status_code > 200) stop(temp$parse("UTF-8"), call. = FALSE)
  stopifnot(temp$response_headers$`content-type` == 'application/json')
  temp$parse("UTF-8")
}

# other helpers --------------------
cn <- function(x) if (length(x) == 0) NULL else x

# gbif_base <- function() 'https://api.gbif.org/v1'
gbif_allowed_urls <- c(
  "https://api.gbif.org/v1",
  "https://api.gbif-uat.org/v1"
)
gbif_base <- function() {
  x <- Sys.getenv("RGBIF_BASE_URL", "")
  if (identical(x, "")) {
    x <- gbif_allowed_urls[1]
  }
  if (!x %in% gbif_allowed_urls) {
    stop("the RGBIF_BASE_URL environment variable must be in set:\n",
         paste0(gbif_allowed_urls, collapse = "  \n"))
  }
  return(x)
}

as_log <- function(x){
  stopifnot(is.logical(x) || is.null(x))
  if (is.null(x)) NULL else if (x) 'true' else 'false'
}

noNA <- function(x) {
  !(any(is.na(x)))
}

strextract <- function(str, pattern) regmatches(str, regexpr(pattern, str))
strextracta <- function(str, pattern) regmatches(str, gregexpr(pattern, str))[[1]]

strtrim <- function(str) gsub("^\\s+|\\s+$", "", str)

move_col <- function(x, y) {
  if (y %in% names(x)) {
    x[ c(y, names(x)[-grep(y, names(x))]) ]
  } else {
    x
  }
}

movecols <- function(x, cols){
  other <- names(x)[ !names(x) %in% cols ]
  x[ , c(cols, other) ]
}

transform_names <- function(x) {
  if (all(grepl("POLYGON", x)) && any(nchar(x) > 50)) {
    paste0("geom", seq_along(x))
  } else {
    x
  }
}

get_meta <- function(x){
  if ('endOfRecords' %in% names(x)) {
    data.frame(x[!names(x) == 'results'], stringsAsFactors = FALSE)
  } else {
    NULL
  }
}

parse_results <- function(x, y){
  if (!is.null(y)) {
    if ('endOfRecords' %in% names(x)) {
      tmp <- x[ !names(x) %in% c('offset','limit','endOfRecords','count') ]
    } else {
      tmp <- x
    }
  } else {
    tmp <- x$results
  }
  if (inherits(tmp, "data.frame")) {
    out <- tryCatch(tibble::as_tibble(tmp), error = function(e) e)
    if (inherits(out, "error")) tmp else out
  } else {
    tmp
  }
}

check_gbif_arg_set <- function(x) {
  facnms <- c('facet', 'facetMincount', 'facetMultiselect',
              'facetOffset', 'facetLimit')
  if (!all(grepl(paste0(facnms, collapse = "|"), names(x)))) {
    stop("some param names not allowed: ", names(x), call. = FALSE)
  }
}

# pull out args passed in ... that should be combined with
# GET request to GBIF
yank_args <- function(...) {
  dots <- list(...)
  #for (i in seq_along(dots)) cat(names(dots)[i], "  ", dots[[i]])
  # filter out request objects
  dots <- Filter(function(z) !inherits(z, "request"), dots)
  # check that args are in a acceptable set
  check_gbif_arg_set(dots)
  dots
}

`%||%` <- function(x, y) if (is.null(x)) y else x

rgbif_wrap <- function(..., indent = 0, width = getOption("width")) {
  x <- paste0(..., collapse = "")
  wrapped <- strwrap(x, indent = indent, exdent = indent + 5, width = width)
  paste0(wrapped, collapse = "\n")
}

as_many_args <- function(x) {
  if (!is.null(x)) {
    names(x) <- rep(deparse(substitute(x)), length(x))
    return(x)
  } else {
    NULL
  }
}

convmany <- function(x) {
  if (is.null(x)) return(x)
  nms <- deparse(substitute(x))
  if (inherits(x, "character")) {
    if (length(x) == 1) {
      if (grepl(";", x)) {
        x <- strtrim(strsplit(x, ";")[[1]])
      }
    }
  }
  x <- stats::setNames(x, rep(nms, length(x)))
  return(x)
}

convmany_rename <- function(x,y) { 
  if (is.null(x)) return(x) 
  stats::setNames(convmany(x),rep(y,length(x))) 
}

check_vals <- function(x, y){
  if (is.na(x) || is.null(x))
    stop(sprintf("%s can not be NA or NULL", y), call. = FALSE)
  if (length(x) > 1)
    stop(sprintf("%s has to be length 1", y), call. = FALSE)
}

check_for_a_pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  } else {
    invisible(TRUE)
  }
}

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

# check correctness issues and their type (type: "name" or "code")
check_issues  <- function(type , ...) {
  types <- c("occurrence", "name")
  if (!length(dots(...)) == 0) {
    filters <- parse_input(...)
    iss <- c(filters$neg, filters$pos)
    if (any(!iss %in% gbifissues$code)) {
      stop("One or more invalid issues.")
    }
    if (any(!iss %in%
            gbifissues$code[which(gbifissues$type == type)])) {
      stop(paste("Impossible to filter",
                 paste0(type, "s"), "by",
                 types[which(type == types) %% 2 + 1],
                 "related issues."))
    }
  }
}

parse_input <- function(...) {
  x <- as.character(dots(...))
  neg <- gsub('-', '', x[grepl("-", x)])
  pos <- x[!grepl("-", x)]
  list(neg = neg, pos = pos)
}

dots <- function(...){
  eval(substitute(alist(...)))
}

parse_issues <- function(x){
  sapply(x, function(y) list(issue = y), USE.NAMES = FALSE)
}

handle_issues <- function(.data, is_occ, ..., mutate = NULL) {
  if ("data" %in% names(.data)) {
    tmp <- .data$data
  } else {
    many <- FALSE
    if (attr(.data, "type") == "many") {
      many <- TRUE
      tmp <- data.table::setDF(
        data.table::rbindlist(lapply(.data, "[[", "data"),
                              fill = TRUE, use.names = TRUE, idcol = "ind"))
    } else {
      tmp <- .data
    }
  }
  
  # handle downloads data
  is_dload <- FALSE
  if (
    all(c("issue", "accessRights", "accrualMethod") %in% names(tmp)) &&
    !"issues" %in% names(tmp)
  ) {
    is_dload <- TRUE
    
    # convert long issue names to short ones
    issstr <- tmp[names(tmp) %in% "issue"][[1]]
    tmp$issues <- unlist(lapply(issstr, function(z) {
      if (identical(z, "")) return (character(1))
      paste(gbifissues[ grepl(gsub(";", "|", z), gbifissues$issue), "code" ],
            collapse = ",")
    }))
  }
  
  if (!length(dots(...)) == 0) {
    filters <- parse_input(...)
    if (length(filters$neg) > 0) {
      tmp <- tmp[ !grepl(paste(filters$neg, collapse = "|"), tmp$issues), ]
    }
    if (length(filters$pos) > 0) {
      tmp <- tmp[ grepl(paste(filters$pos, collapse = "|"), tmp$issues), ]
    }
  }
  
  if (!is.null(mutate)) {
    if (mutate == 'split') {
      tmp <- split_iss(tmp, is_occ, is_dload)
    } else if (mutate == 'split_expand') {
      tmp <- mutate_iss(tmp)
      tmp <- split_iss(tmp, is_occ, is_dload)
    } else if (mutate == 'expand') {
      tmp <- mutate_iss(tmp)
    }
  }
  
  tmp <- tibble::as_tibble(tmp)
  
  if ("data" %in% names(.data)) {
    .data$data <- tmp
    return( .data )
  } else {
    # add same class of input data: "gbif" or "gbif_data"
    class(tmp) <- class(.data)
    return( tmp )
  }
}

mutate_iss <- function(w) {
  w$issues <- sapply(strsplit(w$issues, split = ","), function(z) {
    paste(gbifissues[ gbifissues$code %in% z, "issue" ], collapse = ",")
  })
  return( w )
}

split_iss <- function(m, is_occ, is_dload) {
  unq <- unique(unlist(strsplit(m$issues, split = ",")))
  if (length(unq) == 0) return(data.frame())
  df <- data.table::setDF(
    data.table::rbindlist(
      lapply(strsplit(m$issues, split = ","), function(b) {
        v <- unq %in% b
        data.frame(rbind(ifelse(v, "y", "n")), stringsAsFactors = FALSE)
      })
    )
  )
  names(df) <- unq
  m$issues <- NULL
  if (is_occ) {
    first_search <- c('name','key','decimalLatitude','decimalLongitude')
    first_dload <- c('scientificName', 'taxonKey',
                     'decimalLatitude', 'decimalLongitude')
  } else{
    first_search <- c('scientificName', 'key', 'nubKey',
                      'rank', 'taxonomicStatus')
  }
  first <- if (is_dload) first_dload else first_search
  tibble::as_tibble(data.frame(m[, first], df, m[, !names(m) %in% first],
                               stringsAsFactors = FALSE))
}

setdfrbind <- function(x) {
  (data.table::setDF(
    data.table::rbindlist(x, use.names = TRUE, fill = TRUE)))
}

asl <- function(z) {
  if (is.null(z)) return(z)
  if (
    is.logical(z) || tolower(z) == "true" || tolower(z) == "false"
  ) {
    if (z) {
      return("true")
    } else {
      return("false")
    }
  } else {
    return(z)
  }
}

last <- function(x) x[length(x)]

mssg <- function(v, ...) if (v) message(...)

pchk <- function(from, fun, pkg_version = "v3.0.0") {
  assert(deparse(substitute(from)), "character")
  assert(pkg_version, "character")
  param_mssg <- "`%s` param in `%s` function is defunct as of rgbif %s, and is ignored"
  parms_help <- "\nSee `?rgbif` for more information."
  mssg <- c(sprintf(param_mssg, deparse(substitute(from)), fun, pkg_version),
            parms_help)
  if (!is.null(from)) warning(mssg)
}

tryDefault <- function(expr, default, quiet = FALSE) {
  result <- default
  if (quiet) {
    tryCatch(result <- expr, error = function(e) NULL)
  }
  else {
    try(result <- expr)
  }
  result
}
fail_with <- function(default = NULL, f, quiet = FALSE) {
  f <- match.fun(f)
  function(...) tryDefault(f(...), default, quiet = quiet)
}

cat_n <- function(..., sep = "") cat(..., "\n", sep = sep)

pc <- function(..., collapse = "") paste0(..., collapse = collapse)

check_user <- function(x) {
  z <- if (is.null(x)) Sys.getenv("GBIF_USER", "") else x
  if (z == "") getOption("gbif_user", stop("supply a username")) else z
}

check_pwd <- function(x) {
  z <- if (is.null(x)) Sys.getenv("GBIF_PWD", "") else x
  if (z == "") getOption("gbif_pwd", stop("supply a password")) else z
}

check_email <- function(x) {
  z <- if (is.null(x)) Sys.getenv("GBIF_EMAIL", "") else x
  if (z == "") getOption("gbif_email", stop("supply an email address")) else z
}

check_inputs <- function(x) {
  if (is.character(x)) {
    # replace newlines
    x <- gsub("\n|\r", "", x)
    # validate
    tmp <- jsonlite::validate(x)
    if (!tmp) stop(attr(tmp, "err"))
    x
  } else {
    jsonlite::toJSON(x)
  }
}

is_download_key <- function(x) ifelse(!is.null(x),grepl("^[0-9]{7}-[0-9]{15}$",x),FALSE)

is_uuid <- function(x) ifelse(!is.null(x),grepl("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}",x),FALSE)

is_empty <- function(x) length(x) == 0

bind_rows <- function(x) tibble::as_tibble(data.table::rbindlist(x,fill=TRUE))

getsize <- function(x) {
  round(x/10 ^ 6, 2)
}

prep_output <- function(x) {
  list(
    meta = data.frame(offset = x$offset, limit = x$limit,
                      endofrecords = x$endOfRecords, count = x$count,
                      stringsAsFactors = FALSE),
    results = tibble::as_tibble(x$results)
  )
}

collargs <- function(x){
  outlist <- list()
  for (i in seq_along(x)) {
    outlist[[i]] <- makemultiargs(x[[i]])
  }
  as.list(unlist(rgbif_compact(outlist)))
}

makemultiargs <- function(x){
  value <- get(x, envir = parent.frame(n = 2))
  if ( length(value) == 0 ) {
    NULL
  } else {
    if ( any(sapply(value, is.na)) ) {
      NULL
    } else {
      if ( !is.character(value) ) {
        value <- as.character(value)
      }
      names(value) <- rep(x, length(value))
      value
    }
  }
}

to_camel <- function(x) {
  gsub("(_)([a-z])", "\\U\\2", tolower(x), perl = TRUE)
}


# helpers -------------------------
check_limit <- function(x){
  if (x > 1000000L) {
    stop("Maximum request size is 1 million. Please use occ_download() for large requests.")
  } else {
    x
  }
}

possparams <- function(){
  "taxonKey, speciesKey, scientificName, datasetKey, catalogNumber, recordedBy,
  recordedByID, identifiedByID, geometry, country, publishingCountry,
  recordNumber, search, institutionCode, collectionCode, decimalLatitude,
  decimalLongitude, depth, year, typeStatus, lastInterpreted, occurrenceStatus,
  continent, gadmGid, verbatimScientificName, eventId, identifiedBy, networkKey, 
  occurrenceId, iucnRedListCategory, lifeStage, degreeOfEstablishment, 
  distanceFromCentroidInMeters, or mediatype"
}







#' List all GBIF issues and their codes.
#'
#' Returns a data.frame of all GBIF issues with the following columns:
#' - `code`: issue short code, e.g. `gass84`
#' - `code`: issue full name, e.g. `GEODETIC_DATUM_ASSUMED_WGS84`
#' - `description`: issue description
#' - `type`: issue type, either related to `occurrence` or `name`
#'
#' @export
#' @usage gbif_issues()
#' @source
#' https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/OccurrenceIssue.html
#' https://gbif.github.io/gbif-api/apidocs/org/gbif/api/vocabulary/NameUsageIssue.html
#'
gbif_issues <- function() gbifissues

gbifissues <- structure(list(
  code = c(
    # occurrence issues
    "bri", "ccm", "cdc", "conti", "cdiv",
    "cdout", "cdrep", "cdrepf", "cdreps", "cdround", "cucdmis", "cudc",
    "cuiv", "cum", "depmms", "depnn", "depnmet", "depunl", "elmms",
    "elnn", "elnmet", "elunl", "gass84", "gdativ", "iddativ", "iddatunl",
    "mdativ", "mdatunl", "muldativ", "muluriiv", "preneglat",
    "preneglon", "preswcd", "rdativ", "rdatm", "rdatunl", "refuriiv",
    "txmatfuz", "txmathi", "txmatnon", "typstativ", "zerocd",
    "cdpi", "cdumi", "indci", "interr", "iccos", "osiic", "osu",
    "geodi","geodu", "ambcol", "ambinst", "colmafu", "colmano",
    "incomis", "inmafu", "inmano", "osifbor", "diffown", "taxmatagg",
    "fpsrsinv","fpwktinv",
    # name issues
    "anm", "annu", "anuidi", "aitidinv", "bbmn", "basauthm",
    "bibrinv", "chsun", "clasna", "clasroi", "conbascomb", "desinv",
    "disinv", "hom", "minv", "npm", "ns","nsinv", "onder", "onnu",
    "onuidinv", "ov", "pc", "pnnu", "pnuidinv", "pp","pbg","rankinv",
    "relmiss", "scina", "spprinv", "taxstinv", "taxstmis", "unpars",
    "vernnameinv", "backmatagg"),
  issue = c(
    # occurrence issues
    "BASIS_OF_RECORD_INVALID",
    "CONTINENT_COUNTRY_MISMATCH", "CONTINENT_DERIVED_FROM_COORDINATES",
    "CONTINENT_INVALID", "COORDINATE_INVALID", "COORDINATE_OUT_OF_RANGE",
    "COORDINATE_REPROJECTED", "COORDINATE_REPROJECTION_FAILED", "COORDINATE_REPROJECTION_SUSPICIOUS",
    "COORDINATE_ROUNDED", "COUNTRY_COORDINATE_MISMATCH", "COUNTRY_DERIVED_FROM_COORDINATES",
    "COUNTRY_INVALID", "COUNTRY_MISMATCH", "DEPTH_MIN_MAX_SWAPPED",
    "DEPTH_NON_NUMERIC", "DEPTH_NOT_METRIC", "DEPTH_UNLIKELY", "ELEVATION_MIN_MAX_SWAPPED",
    "ELEVATION_NON_NUMERIC", "ELEVATION_NOT_METRIC", "ELEVATION_UNLIKELY",
    "GEODETIC_DATUM_ASSUMED_WGS84", "GEODETIC_DATUM_INVALID", "IDENTIFIED_DATE_INVALID",
    "IDENTIFIED_DATE_UNLIKELY", "MODIFIED_DATE_INVALID", "MODIFIED_DATE_UNLIKELY",
    "MULTIMEDIA_DATE_INVALID", "MULTIMEDIA_URI_INVALID", "PRESUMED_NEGATED_LATITUDE",
    "PRESUMED_NEGATED_LONGITUDE", "PRESUMED_SWAPPED_COORDINATE",
    "RECORDED_DATE_INVALID", "RECORDED_DATE_MISMATCH", "RECORDED_DATE_UNLIKELY",
    "REFERENCES_URI_INVALID", "TAXON_MATCH_FUZZY", "TAXON_MATCH_HIGHERRANK",
    "TAXON_MATCH_NONE", "TYPE_STATUS_INVALID", "ZERO_COORDINATE",
    "COORDINATE_PRECISION_INVALID", "COORDINATE_UNCERTAINTY_METERS_INVALID",
    "INDIVIDUAL_COUNT_INVALID", "INTERPRETATION_ERROR",
    "INDIVIDUAL_COUNT_CONFLICTS_WITH_OCCURRENCE_STATUS", "OCCURRENCE_STATUS_INFERRED_FROM_INDIVIDUAL_COUNT", "OCCURRENCE_STATUS_UNPARSABLE",
    "GEOREFERENCED_DATE_INVALID","GEOREFERENCED_DATE_UNLIKELY", "AMBIGUOUS_COLLECTION",
    "AMBIGUOUS_INSTITUTION", "COLLECTION_MATCH_FUZZY", "COLLECTION_MATCH_NONE",
    "INSTITUTION_COLLECTION_MISMATCH", "INSTITUTION_MATCH_FUZZY",
    "INSTITUTION_MATCH_NONE", "OCCURRENCE_STATUS_INFERRED_FROM_BASIS_OF_RECORD",
    "DIFFERENT_OWNER_INSTITUTION","TAXON_MATCH_AGGREGATE",
    "FOOTPRINT_SRS_INVALID","FOOTPRINT_WKT_INVALID",
    # name issues
    "ACCEPTED_NAME_MISSING", "ACCEPTED_NAME_NOT_UNIQUE", "ACCEPTED_NAME_USAGE_ID_INVALID",
    "ALT_IDENTIFIER_INVALID", "BACKBONE_MATCH_NONE",
    "BASIONYM_AUTHOR_MISMATCH", "BIB_REFERENCE_INVALID", "CHAINED_SYNOYM",
    "CLASSIFICATION_NOT_APPLIED", "CLASSIFICATION_RANK_ORDER_INVALID",
    "CONFLICTING_BASIONYM_COMBINATION", "DESCRIPTION_INVALID",
    "DISTRIBUTION_INVALID", "HOMONYM", "MULTIMEDIA_INVALID", "NAME_PARENT_MISMATCH",
    "NO_SPECIES", "NOMENCLATURAL_STATUS_INVALID", "ORIGINAL_NAME_DERIVED",
    "ORIGINAL_NAME_NOT_UNIQUE", "ORIGINAL_NAME_USAGE_ID_INVALID", "ORTHOGRAPHIC_VARIANT",
    "PARENT_CYCLE", "PARENT_NAME_NOT_UNIQUE", "PARENT_NAME_USAGE_ID_INVALID",
    "PARTIALLY_PARSABLE", "PUBLISHED_BEFORE_GENUS", "RANK_INVALID",
    "RELATIONSHIP_MISSING", "SCIENTIFIC_NAME_ASSEMBLED", "SPECIES_PROFILE_INVALID",
    "TAXONOMIC_STATUS_INVALID", "TAXONOMIC_STATUS_MISMATCH", "UNPARSABLE",
    "VERNACULAR_NAME_INVALID", "BACKBONE_MATCH_AGGREGATE"),
  description = c(
    # occurrence issues
    "The given basis of record is impossible to interpret or seriously different from the recommended vocabulary.",
    "The interpreted continent and country do not match up.",
    "The interpreted continent is based on the coordinates, not the verbatim string information.",
    "Uninterpretable continent values found.", "Coordinate value given in some form but GBIF is unable to interpret it.",
    "Coordinate has invalid lat/lon values out of their decimal max range.",
    "The original coordinate was successfully reprojected from a different geodetic datum to WGS84.",
    "The given decimal latitude and longitude could not be reprojected to WGS84 based on the provided datum.",
    "Indicates successful coordinate reprojection according to provided datum, but which results in a datum shift larger than 0.1 decimal degrees.",
    "Original coordinate modified by rounding to 5 decimals.",
    "The interpreted occurrence coordinates fall outside of the indicated country.",
    "The interpreted country is based on the coordinates, not the verbatim string information.",
    "Uninterpretable country values found.", "Interpreted country for dwc:country and dwc:countryCode contradict each other.",
    "Set if supplied min>max", "Set if depth is a non numeric value",
    "Set if supplied depth is not given in the metric system, for example using feet instead of meters",
    "Set if depth is larger than 11.000m or negative.", "Set if supplied min > max elevation",
    "Set if elevation is a non numeric value", "Set if supplied elevation is not given in the metric system, for example using feet instead of meters",
    "Set if elevation is above the troposphere (17km) or below 11km (Mariana Trench).",
    "Indicating that the interpreted coordinates assume they are based on WGS84 datum as the datum was either not indicated or interpretable.",
    "The geodetic datum given could not be interpreted.", "The date given for dwc:dateIdentified is invalid and cant be interpreted at all.",
    "The date given for dwc:dateIdentified is in the future or before Linnean times (1700).",
    "A (partial) invalid date is given for dc:modified, such as a non existing date, invalid zero month, etc.",
    "The date given for dc:modified is in the future or predates unix time (1970).",
    "An invalid date is given for dc:created of a multimedia object.",
    "An invalid uri is given for a multimedia object.", "Latitude appears to be negated, e.g. 32.3 instead of -32.3",
    "Longitude appears to be negated, e.g. 32.3 instead of -32.3",
    "Latitude and longitude appear to be swapped.", "A (partial) invalid date is given, such as a non existing date, invalid zero month, etc.",
    "The recording date specified as the eventDate string and the individual year, month, day are contradicting.",
    "The recording date is highly unlikely, falling either into the future or represents a very old date before 1600 that predates modern taxonomy.",
    "An invalid uri is given for dc:references.", "Matching to the taxonomic backbone can only be done using a fuzzy, non exact match.",
    "Matching to the taxonomic backbone can only be done on a higher rank and not the scientific name.",
    "Matching to the taxonomic backbone cannot be done cause there was no match at all or several matches with too little information to keep them apart (homonyms).",
    "The given type status is impossible to interpret or seriously different from the recommended vocabulary.",
    "Coordinate is the exact 0/0 coordinate, often indicating a bad null coordinate.",
    "Indicates an invalid or very unlikely coordinatePrecision",
    "Indicates an invalid or very unlikely dwc:uncertaintyInMeters.",
    "Individual count value not parsable into an integer.",
    "An error occurred during interpretation, leaving the record interpretation incomplete.",
    "Example: individual count value > 0, but occurrence status is absent and etc.",
    "Occurrence status was inferred from the individual count value",
    "Occurrence status value can't be assigned to OccurrenceStatus",
    "The date given for dwc:georeferencedDate is invalid and can't be interpreted at all.",
    "The date given for dwc:georeferencedDate is in the future or before Linnean times (1700).",
    "The given collection matches with more than 1 GrSciColl collection.",
    "The given institution matches with more than 1 GrSciColl institution.",
    "The given collection was fuzzily matched to a GrSciColl collection.",
    "The given collection couldn't be matched with any GrSciColl collection.",
    "The collection matched doesn't belong to the institution matched.",
    "The given institution was fuzzily matched to a GrSciColl institution.",
    "The given institution couldn't be matched with any GrSciColl institution.",
    "Occurrence status was inferred from basis of records",
    "The given owner institution is different than the given institution. Therefore we assume it doesn't belong to the institution and we don't link it to the occurrence.",
    "Matching to the taxonomic backbone can only be done on a species level, but the occurrence was in fact considered a broader species aggregate/complex.",
    "The Footprint Spatial Reference System given could not be interpreted",
    "The Footprint Well-Known-Text given could not be interpreted",
    # name issues
    "Synonym lacking an accepted name.",
    "Synonym has a verbatim accepted name which is not unique and refers to several records.",
    "The value for dwc:acceptedNameUsageID could not be resolved.",
    "At least one alternative identifier extension record attached to this name usage is invalid.",
    "Name usage could not be matched to the GBIF backbone.",
    "The authorship of the original name does not match the authorship in brackets of the actual name.",
    "At least one bibliographic reference extension record attached to this name usage is invalid.",
    "If a synonym points to another synonym as its accepted taxon the chain is resolved.",
    "The denormalized classification could not be applied to the name usage.",
    "The given ranks of the names in the classification hierarchy do not follow the hierarchy of ranks.",
    "There have been more than one accepted name in a homotypical basionym group of names.",
    "At least one description extension record attached to this name usage is invalid.",
    "At least one distribution extension record attached to this name usage is invalid.",
    "A not synonymized homonym exists for this name in some other backbone source which have been ignored at build time.",
    "At least one multimedia extension record attached to this name usage is invalid.",
    "The (accepted) bi/trinomial name does not match the parent name and should be recombined into the parent genus/species.",
    "The group (currently only genera are tested) are lacking any accepted species GBIF backbone specific issue.",
    "dwc:nomenclaturalStatus could not be interpreted",
    "Record has a original name (basionym) relationship which was derived from name & authorship comparison, but did not exist explicitly in the data.",
    "Record has a verbatim original name (basionym) which is not unique and refers to several records.",
    "The value for dwc:originalNameUsageID could not be resolved.",
    "A potential orthographic variant exists in the backbone.",
    "The child parent classification resulted into a cycle that needed to be resolved/cut.",
    "Record has a verbatim parent name which is not unique and refers to several records.",
    "The value for dwc:parentNameUsageID could not be resolved.",
    "The beginning of the scientific name string was parsed, but there is additional information in the string that was not understood.",
    "A bi/trinomial name published earlier than the parent genus was published.",
    "dwc:taxonRank could not be interpreted",
    "There were problems representing all name usage relationships, i.e.",
    "The scientific name was assembled from the individual name parts and not given as a whole string.",
    "At least one species profile extension record attached to this name usage is invalid.",
    "dwc:taxonomicStatus could not be interpreted",
    "no description",
    "The scientific name string could not be parsed at all, but appears to be a parsable name type, i.e.",
    "At least one vernacular name extension record attached to this name usage is invalid.",
    "Name usage could only be matched to a GBIF backbone species, but was in fact a broader species aggregate/complex."),
  type <- c(rep("occurrence", 63), rep("name", 36)
  )), .Names = c("code", "issue", "description", "type"), class = "data.frame", row.names = c(NA, -99L))

collapse_issues <- function(x, issue_col = "issues") {
  tmp <- x[names(x) %in% issue_col][[1]]
  tmp <- gbifissues[ gbifissues$issue %in% tmp, "code" ]
  paste(tmp, collapse = ",")
}

collapse_issues_vec <- function(x, issue_col = "issues") {
  tmp <- x[names(x) %in% issue_col][[1]]
  unlist(lapply(tmp, function(z) paste(gbifissues[ gbifissues$issue %in% z, "code" ], collapse = ",")))
}
