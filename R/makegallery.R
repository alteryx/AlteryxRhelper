takeShot = function(x){
  cwd = getwd(); on.exit(setwd(cwd))
  setwd(dirname(x))
  imgfile = gsub("\\.html", "\\.png", basename(x))
  if (isOlder(imgfile, basename(x))){
    message("Taking screenshot...")
    webshot::webshot(basename(x), file = imgfile, vwidth = 1200, vheight = 750,
      delay = 0.2
    )
  } else {
    message('Thumbnail for ', x, ' is up to date')
  }
}
makeThumbNail = function(x){
  tags$div(class = 'col-md-4', 
    tags$a(href = x, class = 'thumbnail',
      tags$img(
        src = paste0(tools::file_path_sans_ext(x), ".png"), 
        style='height:200px;'
      ),
      tags$div(class = 'caption', tags$h3(tools::file_path_sans_ext(basename(x))))
    )         
  )
}

#' @export
makeGallery = function(files){
  tags$div(class = 'row',
     do.call(tagList, lapply(files, function(x){
       if (basename(x) == 'index.html')
         return(NULL)
       else
         takeShot(x)
         makeThumbNail(x)
     }))         
  )
}
