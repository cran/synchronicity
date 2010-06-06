.onLoad <- function(libname, pkgname)
{
 
  isNamespaceLoaded <- function(name)
  { !is.null(.Internal(getRegisteredNamespace(as.name(name)))) }
  
  if (!isNamespaceLoaded('bigmemory')) {
      setGeneric('describe', function(x) standardGeneric('describe'))
  }

  setMethod('describe', signature(x='boost.mutex'),
    function(x)
    {
      return(new('boost.mutex.descriptor', 
        description=list(shared.name=shared.name(x), 
        timeout=timeout(x))))
    })

  if (!isGeneric('attach.mutex'))
  {
    setGeneric('attach.mutex', function(obj, ...) 
      standardGeneric('attach.mutex'))

    setMethod('attach.mutex', signature(obj='character'),
      function(obj, ...)
      {
        path = match.call()[['path']]
        if (is.null(path))
        {
          path <- '.'
        }
        path <- path.expand(path)
        if (basename(obj) != obj)
        {

            warning(paste("Two paths were specified in attach.mutex",
              "The one associated with the file will be used.", sep="  "))
          path <- dirname(obj)
          obj <- basename(obj)
        }
        fileWithPath <- file.path(path, obj)
        fi = file.info(fileWithPath)
        print(dir())
        if (is.na(fi$isdir))
          stop( paste("The file", fileWithPath, "could not be found") )
        if (fi$isdir)
          stop( fileWithPath, "is a directory" )
        info <- dget(fileWithPath)
        return(attach.mutex(info, path=path))
      })
  }
  setMethod('attach.mutex', signature(obj='boost.mutex.descriptor'),
    function(obj, ...)
    {
      desc = description(obj)
      return(boost.mutex(sharedName = desc$shared.name, 
        timeout = desc$timeout))
    })
  library.dynam('synchronicity', pkgname, libname);
}

.onUnload <- function(libpath)
{
  library.dynam.unload('synchronicity', libpath);
}
