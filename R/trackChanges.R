trackChanges <- function(path, wait = 1){

  # when was the file at path last changed?
  last.modified <- file.info(path)$mtime
  # initialize changes
  tmpDirPath <- initializeChanges(path)
  # filename of copy of the file in our directory
  tmpfilePath <- file.path(tmpDirPath, basename(path))

  # get initial html
  refreshDiff(origPath = tmpfilePath, newPath = path,
              outDir = tmpDirPath)

  # load the html in the viewer

  servr::httw(dir = tmpDirPath, daemon = T)

  daemons <- servr::daemon_list()
  daemonID <- daemons[length(daemons)]

  # set up checking of tmp dir

  check <- function(){
    if(last.modified != file.info(path)$mtime ){
      refreshDiff(origPath = tmpfilePath, newPath = path,
                  outDir = tmpDirPath)
      last.modified <<- file.info(path)$mtime
    }
    if (daemonID %in% servr::daemon_list()){
      later::later(check, wait)
    } else {

      applyChanges(path, tmpfilePath)

      }

  }
  later::later(check, wait)
}



initializeChanges <- function(path){

  #step 0
  tmpDirPath <- checkTmpPath(tempdir())

  # Step 1: create tmp dir
  dir.create(tmpDirPath, showWarnings = FALSE)

  # Step 2: copy the user's file (path)

  copypath <- file.path(tmpDirPath, basename(path))

  file.copy(from = path, to = copypath)

  return(tmpDirPath)
}

refreshDiff <- function(origPath, newPath, outDir) {
  orig <- readr::read_file(origPath)
  new <- readr::read_file(newPath)
  diff <- trackmd:::diff_to_markup(orig, new)

  render_changes(text = diff, output_dir = outDir, output_file = "index.html", quiet = TRUE)

}

applyChanges <- function(origPath, newPath){
  new <- readr::read_file(origPath)
  old <- readr::read_file(newPath)
  diff <- diff_to_markup(old, new)

  readr::write_file(diff, path = origPath)

  # delete temp file
  unlink(tmpfilePath, recursive = T)
}

checkTmpPath <- function(tmpfilePath){
  fixedPath <- gsub(pattern = "//", replace = "/" , tmpfilePath, fixed = T)
  return(fixedPath)
}
