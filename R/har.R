readHAR =
function(file, d = fromJSON(file))
{
    e = d$log$entries
    els = lapply(e, mkEntryDF)
    do.call(rbind, els)
}


EntryFields = c(
                 "cache", "pageref", "request", "response", "startedDateTime", "time", "timings",
                # following are not in all.
                "_securityState", "serverIPAddress", "connection" 
              )

EntryFields = c(startedDateTime = "character", request = "list", response = "list", 
                cache = "list", timings = "numeric", time = "numeric", `_securityState` = "character", 
                serverIPAddress = "character", connection = "character", pageref = "character"
                )


mkEntryDF =
function(x)
{
    scalar = c("startedDateTime", "_securityState", "serverIPAddress", "connection", "pageref", "time")
    tmp = lapply(x[scalar], orNA)

    ans = as.data.frame(tmp)
    names(ans) = scalar

    # timings
    if(length(x$timings))
        ans[names(x$timings)] = as.data.frame(as.list(x$timings))
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

orNA =
function(x)
    if(length(x) == 0) NA else x


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
    
    v =  unlist(x)
    i = seq(1, by = 2, length = length(x))
    structure(v[ i + 1L], names = v[i])
}


mkRequest =
function(x)
{
    # scalars
    s = c("bodySize", "method", "url", "httpVersion", "headersSize")
    ans = as.data.frame(lapply(x[s], orNA))

    ans$requestHeaders = list(queryString(x$headers))
    ans$requestCookies = list(queryString(x$cookies))
    ans$queryString = list(queryString(x$queryString))
    ans$postData = list(queryString(x$postData))
    ans
}

mkResponse =
function(x)
{
    # scalars
    s = c("status", "statusText", "httpVersion", "redirectURL", "headersSize", "bodySize")
    ans = as.data.frame(lapply(x[s], orNA))

    ans$responseHeaders = list(queryString(x$headers))
    ans$responseCookies = list(queryString(x$cookies))
    ans$content = orNA(x$content$text)
    ans$mimeType = orNA(x$content$mimeType)
    
    ans
}
