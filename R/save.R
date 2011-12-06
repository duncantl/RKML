#
# save a KMLDoc object to a file, but check if it refers to 
# local resources and put them into a KMZ file.
#



setMethod("saveXML", c("KMLDoc", "character"),
function (doc, file = NULL, compression = 0, indent = TRUE, prefix = "<?xml version=\"1.0\"?>\n", 
    doctype = NULL, encoding = "", ...)   
{
  if(is(file, "AsIs"))
    callNextMethod()
  else if(!require("Rcompression")) {
     #XXX Can move this into makeKMZArchive and have it use an external zip if we want.
    warning("Rcompression is not available, so not attempting to build a KMZ file")
    callNextMethod()
  } else
    makeKMZArchive(doc, file, force = FALSE, ...)  
})

makeKMZArchive =
  #
  # Intended to bring all the local external resources together into
  # a single KMZ file if necessary, otherwise a .kml file
  # with the intent of making the document stand-alone on another file system.
  # It can referenc URLs http:...

  #
  # Use a temporary directory no matter what to avoid writing doc.kml.
  #
  # Check for extension
  #
  # Some of the gymnastics done here is also done in the ROOXML and Rcompression
  # packages. And we should add facilities to Rcompression to do this in memory and avoid
  # files.  Done now (Feb 2010).
  #
  #
function(doc, file = docName(doc), ..., force = TRUE,
          clone = TRUE, useKMZ = NA, baseDir = getwd(),  # was system.file(package = "RKML"), but also had basedir that was not used.
          xpathQuery = "//Icon/href|//Icon/text()|//image[@src]", fixHTMLRefs = TRUE)
{
      # see if we are explicitly asked for a KMZ file.
  if(!missing(file)) {
    if(missing(useKMZ))
      useKMZ = length(grep("\\.kmz$", file)) > 0
  }


  newFiles = character()  # the mapping from the local files to names in the zip archive.


  urls = unlist(getExternalFileNames(doc))

          # Fix them up if they are names of local files.
  if(length(urls) || force) {

         # If there are any that are not external references, but local references,
         # we'll have to use a kmz file.
     i = isLocalFile(urls) 
     if(any(i)) {
        nonURLs = urls[i]

            # we will change the names of these files when we put them into the archive
        newFiles = translateFileNames(nonURLs, baseDir)
          

        if(!all(e <- file.exists(names(newFiles)))) {
          warning("not creating KMZ archive: ", sum(!e), " of the ", length(newFiles), " files could not be found")
          useKMZ = FALSE
        } else {
          # Change the urls to be local. Just remove the /
          #XXX Note that we are changing the document so the caller will see the changes.
          # Should we duplicate the document. See clone()
          if(clone) {
             doc = xmlClone(doc)
          }
          
          nodes = getNodeSet(doc, xpathQuery)
          mapply(replaceExtFileName, 
                   nodes[i], newFiles)
              # XXX fix references in html
          if(fixHTMLRefs)
             fixHTMLExtRefs(doc, newFiles)
          useKMZ = TRUE
        }
      }
   }
  

  # Make this into an XMLInternalDocument to avoid recursively calling
  # same saveXML() method.
  # make certain to restore the object since it is an external pointer
  # we are changing the attributes on the single instance.
  oclass = class(doc)
  on.exit(class(doc) <- oclass) 
  kdoc = structure(doc, class = setdiff(class(doc), "KMLDoc"))
  if(is.na(useKMZ)) {
        # check the size and if > 10K, then elect to use a KMZ file. 
    tmp = saveXML(kdoc)
    useKMZ = nchar(tmp, "bytes") > 10 * 1024
  }


  if(useKMZ) {
      file = normalizePath(path.expand(file), mustWork = FALSE)
      file = gsub("\\.km[lz]$", "", file)
      file = paste(file, ".kmz", sep = "")
        # Need the full file name since we may have changed directory.
        # But have to be careful to honor a specific name.
      if(length(grep("^/", file)) == 0)
         file = paste(normalizePath(path.expand(getwd()), mustWork = FALSE), file, sep = .Platform$file.sep)

      entries = list(doc.kml = I(saveXML(kdoc)))
      if(length(newFiles))
              # add the files that are local to this machine into the zip file
              # using the new names. 
          entries[newFiles] = lapply(names(newFiles), readBinaryFile)
      zip(file, entries, append = FALSE, ...)
 
      file
   } else {
      saveXML(kdoc, file = if(useKMZ) "doc.kml" else file, ...)
   }
}

fixHTMLExtRefs =
function(doc, fileMap, xpath = "//description")
{
   xpathApply(doc, xpath, fixHTMLNodeExtRef, fileMap)
   doc
}

fixHTMLNodeExtRef =
  #
  #
  # See updateHTMLFileName() below this one.
  #
function(node, fileMap)
{

  if(xmlSize(node) == 0)
      return()

  
  isCData = is(node[[1]], 'XMLInternalCDataNode')

  if(isCData) {
     hdoc = htmlParse(xmlValue(node[[1]]), asText = TRUE)
  } else
     hdoc = node

  xpathApply(hdoc, "//img[@src]|//a[@href]", updateHTMLFileName, fileMap)

  if(isCData) {
     txt = paste(xmlSApply(xmlRoot(hdoc)[["body"]], saveXML), collapse = "")
     replaceNodes(node[[1]], newXMLCDataNode(txt))
  }

  node  
}

updateHTMLFileName =
function(node, fileMap)
{
   at = if(xmlName(node) == "a")
           "href"
        else
           "src"
   f = xmlGetAttr(node, at)
   if(!isLocalFile(f))
     return(node)
   
   i = match(f, names(fileMap))
   if(is.na(i)) 
     warning("Didn't find ", f, " in file map")
   else
      xmlAttrs(node) = structure(fileMap[i], names = at)

   node
}

replaceExtFileName =
  #
  # Not for HTML, but KML nodes.
  #
function(node, newFilename)
{
  if(is.null(node))
    return()

  tag = xmlName(node)

  if(tag == "image" || tag == "img") {
    xmlAttrs(node) = c(src = newFilename)
    return(node)
  }
  
     # shouldn't need the next 3 lines as we get the text() from an Icon w/o href.
  h = getNodeSet(node, "./href")

  if(length(h))
    node = h[[1]]
  xmlValue(node) = newFilename #gsub("^/", "", xmlValue(node)),

  node
}  


getExternalFileNameFromNode =
function(node)
{
  tag = xmlName(node)

  if(tag == "image" || tag == "img")
    xmlGetAttr(node, "src")
  else
    xmlValue(node)
  
}


getExternalFileNames =
function(doc, xpathQuery = "//Icon/href|//Icon/text()|//image[@src]", checkHTML = TRUE)
{
  ans = xpathSApply(doc, xpathQuery, getExternalFileNameFromNode)
  if(checkHTML)
    c(ans,  getHTMLLocalFileRefs(doc))
  else
    ans
}
