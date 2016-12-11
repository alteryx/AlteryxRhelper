extractNodeInfo <- function(yxmd){
  doc <- xmlTreeParse(yxmd)
  r <- xmlRoot(doc)
  g <- getNodeSet(r, '//Node')
  d2 <- ldply(g, function(x){
    d = xmlToList(x)
    annotationText = getNodeSet(x, '//Properties//Annotation//AnnotationText')
    if (length(annotationText) > 0){
      annotation = strsplit(
        xmlValue(getNodeSet(x, '//Properties//Annotation//AnnotationText')[[1]]),
        "---"
      )[[1]]
    } else {
      annotation = ""
    }
    name = if (is.null(d$Properties$Annotation$Name)) {
      paste("Tool", d$.attrs[['ToolID']])
    } else {
      d$Properties$Annotation$Name
    }
    if (length(annotation) > 1){
      name = paste0(name, LETTERS[seq_along(annotation)])
    }
    data.frame(
      x = as.numeric(d$GuiSettings[[1]]),
      y = as.numeric(d$GuiSettings[[2]]),
      id = d$.attrs[['ToolID']],
      name = name,
      type = if (is.null(d$GuiSettings$.attrs[['Plugin']])) "" else d$GuiSettings$.attrs[['Plugin']],
      annotation = sapply(annotation, commonmark::markdown_html),
      annotation_md = annotation
    )
  })
  d2$x <- as.numeric(d2$x)
  d2$y <- as.numeric(d2$y)
  d3 <- d2[!grepl('TextBox', d2$type),]
  d3 <- d2
  d3 <- arrange(d3, x, y)
  d3$name = as.character(d3$name)
  return(d3)
}



yxmd <- 'macro/temp.yxmd'

createStarterKitData <- function(yxmd, docx, extract_media = TRUE){
  d3 <- extractNodeInfo(yxmd)
  opts <- c("--atx-headers")
  if (extract_media){
    opts <- c(opts, "--extract-media=.")
  }
  foo <- pandoc_convert(
    input = docx,
    to = 'markdown',
    from = 'docx',
    output = sub("docx", "md", docx),
    options = opts
  )
  tf <- tempfile(fileext = '.html')
  pandoc_convert(
    input = sub("docx", "md", docx), to = 'html', output = tf, options = c("--section-divs")
  )
  doc <- read_html(tf)
  
  annotation <- doc %>% html_nodes("div.section")
  name <- doc %>% html_nodes("h2") %>% html_text()
  
  main = annotation[1]
  title = main %>% html_node("h2") %>% html_text()
  auth_desc = main %>% html_nodes("p") %>% html_text()
  
  g4 <- data.frame(
    name = as.character(name),
    annotation2 = as.character(annotation)
  )
  
  g5 <- join(g4, d3, by = 'name', type = 'full')
  
  g6 <- data.frame(
    x = g5$x,
    y = g5$y,
    id = g5$id,
    annotation = g5$annotation2,
    type = g5$type
  )
  list(
    #data = g6[-c(1:4),], 
    data = g6[-1,],
    description = auth_desc[2],
    author = auth_desc[1],
    title = title
    #description = doc %>% html_node("#description p") %>% html_text(),
    #title = doc %>% html_node("#title p") %>% html_text(),
    #author = doc %>% html_node("#author p") %>% html_text()
  )
}

renderStarterKit <- function(kit, workflow = './img/workflow.png', 
    header = './img/header.png', assetDir = NULL){
  tf <- "__temp__.html"; on.exit(unlink(tf))
  if (is.null(assetDir)){
    assetDir = system.file("starterkit", package = 'AlteryxRhelper')
  }
  file.copy(
    file.path(assetDir, 'assets', 'tpl', 'index.html'), tf, overwrite = TRUE
  )
  x <- htmltools::htmlTemplate(
    filename = '__temp__.html',
    description = kit$description,
    title = kit$title,
    author = kit$author,
    data = kit$data,
    workflow = workflow,
    header = header,
    assetDir = assetDir
  )
  cat(as.character(x), file = 'index.html')
  #htmlwidgets:::pandoc_self_contained_html('index.html', 'index.html')
}

gdoc2kit <- function(docx_id, yxmd, remove_docx = TRUE){
  require(driver)
  d <- file_metadata(docx_id)
  download_file(
    d, 
    'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
    '__story__.docx'
  )
  docx2kit('__story__.docx', yxmd)
  if (remove_docx){
    unlink('__story__.docx')
    unlink('__story__.md')
  }
}

docx2kit <- function(docx, yxmd, extract_media = TRUE, ...){
  kit <- createStarterKitData(yxmd, docx, extract_media = extract_media)
  renderStarterKit(kit, ...)
}


createScreenshotWorkflow <- function(yxmd, output){
  d <- paste(readLines(yxmd), collapse = '\n')
  d2 <- gsub(
    'TextColor name="Black"', 
    'TextColor name="White"',
    d
  )
  d2 <- gsub(
    'TextColor name="Gray"',
    'TextColor name="White"',
    d2
  )
  cat(d2, file = output)
}

#' Make Tutorial
#' 
#' 
#' @export
makeTutorial <- function(docx, title = "Tutorial", preview = FALSE, standalone = FALSE){
  require(rvest)
  tf <- tempfile(fileext = '.html')
  rmarkdown::pandoc_convert(
    input = docx, to = 'html', output = tf, options = c("--section-divs", "--mathml", "--html-q-tags")
  )
  doc <- read_html(tf)
  
  annotation <- doc %>% html_nodes("div.section")
  name <- doc %>% html_nodes("h2") %>% html_text()
  
  g4 <- data.frame(
    step_title = as.character(name[-1]),
    step_content = as.character(annotation[-1])
  )
  
  tutorial <- list(
    title = as.character(name[1]),
    steps = as.character(annotation[-1]),
    steptitles = as.character(name[-1])
  )
  
  library(whisker)
  tplFile <- system.file('starterkit', 'tutorial', 'tpl.html', package = 'AlteryxRhelper')
  tpl <- paste(readLines(tplFile), collapse = '\n')
  d <- whisker.render(
    tpl,
    list(
      tutorial = jsonlite::toJSON(tutorial, auto_unbox = TRUE),
      assetDir = system.file('starterkit', 'tutorial', package = 'AlteryxRhelper'),
      title = title
    )
  )
  cat(as.character(d), file = 'index.html')
  if (preview){
    app <- servr::httw(daemon = TRUE)
    return(app)
  }
  if (standalone){
    htmlwidgets:::pandoc_self_contained_html('index.html', 'index_self.html')
  }
}

# makeTutorial <- function(docx, preview = FALSE, standalone = FALSE){
#   require(rvest)
#   tf <- tempfile(fileext = '.html')
#   rmarkdown::pandoc_convert(
#     input = docx, to = 'html', output = tf, options = c("--section-divs", "--mathml")
#   )
#   doc <- read_html(tf)
#   
#   annotation <- doc %>% html_nodes("div.section")
#   name <- doc %>% html_nodes("h2") %>% html_text()
#   
#   g4 <- data.frame(
#     step_title = as.character(name[-1]),
#     step_content = as.character(annotation[-1])
#   )
#   
#   tutorial <- list(
#     title = as.character(name[1]),
#     steps = as.character(annotation[-1]),
#     steptitles = as.character(name[-1])
#   )
#   
#   library(htmltools)
#   d = htmlTemplate(
#     system.file('starterkit', 'tutorial', 'tpl.html', package = 'AlteryxRhelper'),
#     assetDir = system.file('starterkit', 'tutorial', package = 'AlteryxRhelper'),
#     tutorial = tutorial
#   )
#   cat(as.character(d), file = 'index.html')
#   if (preview){
#     app <- servr::httw(daemon = TRUE)
#     return(app)
#   }
#   if (standalone){
#     htmlwidgets:::pandoc_self_contained_html('index.html', 'index_self.html')
#   }
# }
