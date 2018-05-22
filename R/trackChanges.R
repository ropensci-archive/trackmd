#' Start tracking changes on a file
#' 
#' Start tracking all changes to a file and preview changes in the viewer.
#' The changes will be added to the original file as markup when done.
#' 
#' @param path The path to the file to track.
#' @param wait The number of seconds between each check for changes
#' 
#' @examples
#' trackChanges("my_file.txt")
#' 
#' @export
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


#' Initialize track changes tmp dir
#' 
#' Initialize track changes tmp dir
#' 
#' @param path The path to the users original file to track.
#' 
#' @keywords internal
initializeChanges <- function(path){
  # Create temporary directory where the diff is hosted
  tmpDirPath <- checkTmpPath(tempdir())
  dir.create(tmpDirPath, showWarnings = FALSE)

  # Step 2: copy the user's file (path)
  copypath <- file.path(tmpDirPath, basename(path))
  file.copy(from = path, to = copypath)

  return(tmpDirPath)
}

#' Create a HTML of the diff
#' 
#' Create a HTML rendering of the diff 
#' 
#' @param origPath The path to the file being edited
#' @param newPath The path to the copy of the inital state of the file.
#' @param outDir Where to save the "index.html" file with the rendered diff.
#' 
#' @keywords internal
refreshDiff <- function(origPath, newPath, outDir) {
  orig <- readr::read_file(origPath)
  new <- readr::read_file(newPath)
  diff <- trackmd:::diff_to_markup(orig, new)

  render_changes(text = diff, output_dir = outDir, output_file = "index.html", quiet = TRUE)

}

#' Replace user's file with diff markup 
#' 
#' Replace user's file with diff markup 
#' 
#' @param origPath The path to the file being edited
#' @param newPath The path to the copy of the inital state of the file.
#' 
#' @keywords internal
applyChanges <- function(origPath, newPath){
  # Create diff
  new <- readr::read_file(origPath)
  old <- readr::read_file(newPath)
  diff <- diff_to_markup(old, new)
  
  # Replace user's file 
  readr::write_file(diff, path = origPath)
}

#' Fix problems with temp dir path
#' 
#' Fix problems with temp dir path
#' 
#' @param tmpfilePath The path to fix
#' 
#' @keywords internal
checkTmpPath <- function(tmpfilePath){
  fixedPath <- gsub(pattern = "//", replace = "/" , tmpfilePath, fixed = T)
  return(fixedPath)
}
