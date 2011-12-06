if(FALSE) # no longer used.
translateFileNames =
  #
  # The idea is to take each of the baseDirs as prefixes that we want to remove.
  # So for example if we have files named  /a/b/c, /a/x/y/z, /o/p and baseDirs
  #  /a/b  and /a/x, then we would want to be left with
  #   c, y/z and /o/p
function(files, baseDirs)
{
   if(length(baseDirs) == 0 || all(baseDirs == ""))
     return(files)

      # ensure a / is at the end of each baseDirs
   baseDirs = paste(gsub("/$", "", baseDirs), "/", sep = "")
  
   left = rep(TRUE, length(files))

   ans = structure(files, names = files)
   left = files
   ans = character()

   for(dir in baseDirs) {
     rx = sprintf("^%s", dir)
     i = grep(rx, left)

     if(length(i)) {
       ans = c(ans, structure(gsub(rx, "", left[i]), names = left[i]))
       left = left[-i]

       if(length(left) == 0)
         break       
     }
   }

   if(length(left))
     ans = c(ans, structure(basename(left), names = left))
  
   ans
}


translateFileNames =
  #
  #
  # This uses a simple strategy. Basically,
  # relative files stay the same, and fully qualified
  # path names (i.e. starting with / or c:\\ etc).
  # are prefixed with prefix. This is to avoid conflicts
  # between the local files relative to the current directory
  # and the chance of having something with a fully qualified name
  # that maps to the same name as  a local file name.
  # e.g.
  #    tmp/foo.png
  # and
  #    /tmp/foo.png
  # So the latter gets mapped to root/tmp/foo.png
  # and the former stays as is.
  #
  #  We can also remove paths.
  #
  #
function(files, baseDirs, prefix = "root", pattern = "^(/|[A-Za-c]:\\\\)",
          drop = character())
{
   ids = files

   if(length(drop))
      files = gsub(sprintf("^(%s)", paste(drop, collapse = "|")), "", files)
   
   fullq = grepl(pattern, files)
   
   files[fullq] = sprintf("%s/%s", prefix, gsub("^[\\/]", "", files[fullq]))

   structure(files, names = ids)
}
