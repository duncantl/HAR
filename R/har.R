readHAR =
function(file, d = fromJSON(file))
{
    e = d$log$entries
    els = lapply(e, mkEntryDF)
    do.call(rbind, els)
}


EntryFields = c(
                 "cache", "pageref", "request", "response", "startedDateTime", "time", "timings",
                # following are not in every request
                "_securityState", "serverIPAddress", "connection" 
              )

EntryFields = c(startedDateTime = "character", request = "list", response = "list", 
                cache = "list", timings = "numeric", time = "numeric", `_securityState` = "character", 
                serverIPAddress = "character", connection = "character", pageref = "character"
                )


asDF1 =
function(x)    
{
    structure(as.list(x), row.names = 1L, class = "data.frame")
}


mkEntryDF = mkEntryDF0 = mkEntryDF00 =
function(x)
{
    scalar = c("startedDateTime", "_securityState", "serverIPAddress", "connection", "pageref", "time")
    tmp = lapply(x[scalar], orNA)
    names(tmp) = scalar

    #    ans = as.data.frame(tmp)
    ans = asDF1(tmp)

    # timings
    if(length(x$timings))
        ans[names(x$timings)] = asDF1(as.list(x$timings)) # as.data.frame(as.list(x$timings))
    else
        ans = cbind(ans, data.frame("blocked" = NA,  "dns" = NA, connect = NA, "ssl" = NA, "send" = NA, "wait" = NA, "receive" = NA))
    
    # request, response, cache
    # For now, ignore cache as I have that turned off.

    r = mkRequest(x$request)
    ans[names(r)] = r

    r = mkResponse(x$response)
    ans[names(r)] = r    

    ans
}




mkEntryDF = mkEntryDF0 =
function(x)
{
    scalar = c("startedDateTime", "_securityState", "serverIPAddress", "connection", "pageref", "time")
    tmp = lapply(x[scalar], orNA)
    names(tmp) = scalar

    #    ans = as.data.frame(tmp)
    ans = tmp

    # timings
    if(length(x$timings))
        ans = c(ans, as.list(x$timings)) # as.data.frame(as.list(x$timings))
    else
        ans = c(ans, list("blocked" = NA,  "dns" = NA, connect = NA, "ssl" = NA, "send" = NA, "wait" = NA, "receive" = NA))
    
    # request, response, cache
    # For now, ignore cache as I have that turned off.

    r1 = mkRequest(x$request)
    ans[names(r1)] = r1

    r2 = mkResponse(x$response)
    ans[names(r2)] = r2

    # cbind(asDF1(ans), r1, r2)

    ans
}


orNA =
function(x)
    if(length(x) == 0) NA else x


queryString = queryString0 =
    # convert the list of name value vectors into a single vector with the values
    # as the elements and the field name as the names() vector.
function(x)
{

    # w/o this check for length(x), get warnings of the form
    # In structure(v[i + 1L], names = v[i]) :
    #  Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    #  Consider 'structure(list(), *)' instead.
    if(length(x) == 0)
        return(character())
    
    v =  unlist(x)
    i = seq(1, by = 2, length = length(x))
    structure(v[ i + 1L], names = v[i])
    # Which is faster - structure() or ans = ; names(ans) = ; ans
    #ans = v[ i + 1L]
    #names(ans) = v[i]
    #ans
}

queryString = 
    # convert the list of name value vectors into a single vector with the values
    # as the elements and the field name as the names() vector.
function(x)
{

    # w/o this check for length(x), get warnings of the form
    # In structure(v[i + 1L], names = v[i]) :
    #  Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    #  Consider 'structure(list(), *)' instead.
    if(length(x) == 0)
        return(character())
    
    tmp = matrix(unlist(x), , 2, byrow = TRUE)
    z = tmp[,2]
    names(z) = tmp[,1]
    z
}



# This version is slower than the above.
queryString.slower = 
    # convert the list of name value vectors into a single vector with the values
    # as the elements and the field name as the names() vector.
function(x)
{

    # w/o this check for length(x), get warnings of the form
    # In structure(v[i + 1L], names = v[i]) :
    #  Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
    #  Consider 'structure(list(), *)' instead.
    if(length(x) == 0)
        return(character())
    
    tmp = matrix(unlist(x), , 2, byrow = TRUE)
    structure(tmp[,2], names = tmp[,1])
}



# This version creates the entire list and then calls asDF1
# Check gives same result
mkRequest =
function(x)
{
    # scalars
    s = c("bodySize", "method", "url", "httpVersion", "headersSize")
    ans = lapply(x[s], orNA) 

    ans$requestHeaders = list(queryString(x$headers))
    ans$requestCookies = list(queryString(x$cookies))
    ans$queryString = list(queryString(x$queryString))
    ans$postData = list(queryString(x$postData))

    asDF1(ans)
#    ans
}



mkResponse = 
function(x)
{
    # scalars
    s = c("status", "statusText", "httpVersion", "redirectURL", "headersSize", "bodySize")
    ans = lapply(x[s], orNA)
    # added this when did not get the same answer as mkResponse0
    names(ans) = s
    

    ans$responseHeaders = list(queryString(x$headers))
    ans$responseCookies = list(queryString(x$cookies))
    ans$content = orNA(x$content$text)
    ans$mimeType = orNA(x$content$mimeType)
    
    asDF1(ans)
}
