# MUTEXES

setClass('mutex')
setGeneric('lock', function(m, ...) standardGeneric('lock'))
setGeneric('lock.shared', function(m, ...) standardGeneric('lock.shared'))
setGeneric('unlock', function(m, ...) standardGeneric('unlock'))

setClass('boost.mutex', contains='mutex', 
  representation(isRead='logical', mutexInfoAddr='externalptr'))

setGeneric('read', function(m) standardGeneric('read'))
setMethod('read', signature(m='boost.mutex'), function(m) 
  return(.Call("IsRead", m@mutexInfoAddr)))

setMethod('lock', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=FALSE
    if (!is.logical(block)) stop('The block argument should be logical')
    return(
      .Call(ifelse(block, 'boost_try_lock', 'boost_lock'), m@mutexInfoAddr))
  })
setMethod('lock.shared', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=FALSE
    if (!is.logical(block)) stop('The block argument should be logical')
    return( 
      .Call(ifelse(block, 'boost_try_lock_shared', 'boost_lock_shared'),
        m@mutexInfoAddr))
  })
setMethod('unlock', signature(m='boost.mutex'),
  function(m, ...)
  {
    return(
      .Call( ifelse(read(m), 'boost_unlock_shared', 'boost_unlock'),
        m@mutexInfoAddr) )
  })

setGeneric('shared.name', function(m) standardGeneric('shared.name'))

setMethod('shared.name', signature(m='boost.mutex'), 
  function(m) 
  {
    return(.Call('GetResourceName', m@mutexInfoAddr))
  })

setGeneric('timeout', function(m) standardGeneric('timeout'))

setMethod('timeout', signature(m='boost.mutex'),
  function(m)
  {
    return(.Call('GetTimeout', m@mutexInfoAddr))
#    ret = .Call('GetTimeout', m@mutexInfoAddr)
#    if (length(ret) == 0) ret = Inf
#    return(ret)
  })

setGeneric('is.timed', function(m) standardGeneric('is.timed'))
setMethod('is.timed', signature(m='boost.mutex'),
  function(m)
  {
    return(!is.null(timeout(m)))
  })

# The constructor for a boost.mutex
boost.mutex=function(sharedName=NULL, timeout=NULL)
{
  isRead = TRUE
  if (is.null(sharedName)) 
  {
    sharedName = uuid()
  }
  if (!is.null(timeout) && !is.numeric(timeout))
  {
    stop("The timeout parameter must be numeric.")
  }
  if (is.numeric(timeout) && timeout <= 0)
  {
    stop("You must specify a timeout greater than zero.")
  }
  mutexInfoAddr=.Call('CreateBoostMutexInfo', sharedName, as.double(timeout))
  return(new('boost.mutex', isRead=isRead, mutexInfoAddr=mutexInfoAddr))
}


setClass('descriptor', representation(description='list'))
setGeneric('description', function(x) standardGeneric('description'))
setMethod('description', signature(x='descriptor'),
  function(x) return(x@description))

setClass('boost.mutex.descriptor', contains='descriptor')

