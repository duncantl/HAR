readHAR =
function(file, d = fromJSON(file))
{
    e = d$log$entries
    els = lapply(e, mkEntryDF)
    return(fix2(els))
#    tmp = do.call(rbind, els)
#    fix(tmp)
}

fix =
    # See fix2.R
function(x)    
{
    df = as.data.frame(x)

    num = c("time", "blocked", "dns", "connect", "ssl", "send", "wait", 
            "receive", "bodySize", "headersSize", "responseStatus",
            "responseHttpVersion", "responseHeadersSize", "responseBodySize")

    char = c("startedDateTime", "_securityState", "serverIPAddress", "connection", 
             "pageref", "method", "url", "httpVersion", "responseStatusText", "responseRedirectURL", 
             "content", "mimeType", "responseEncoding")

    df[c(num, char)] = lapply(df[c(num, char)], unlist, recursive = FALSE)

    df$postData = unlist(df$postData, recursive = FALSE)
    df
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
    #  structure(as.list(x), row.names = 1L, class = "data.frame")
  structure(x, row.names = 1L, class = "data.frame")    
}


mkEntryDF = 
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





# This version creates the entire list and then calls asDF1
# Check gives same result
mkRequest =
function(x)
{
    # scalars
    s = c("bodySize", "method", "url", "httpVersion", "headersSize")
    ans = lapply(x[s], orNA) 

    ans$requestHeaders = queryString(x$headers)
    ans$requestCookies = queryString(x$cookies)
    ans$queryString = queryString(x$queryString)
    ans$postData = list(x$postData) # had queryString() but the postData is a named list not an unnamed list of name-value pairs.

    asDF1(ans)
}



mkResponse = 
function(x)
{
    # scalars
    s = c("status", "statusText", "httpVersion", "redirectURL", "headersSize", "bodySize")
    ans = lapply(x[s], orNA)
    # added this when did not get the same answer as mkResponse0
    names(ans) = paste0("response", capitalize(s))
    

    ans$responseHeaders = queryString(x$headers)
    ans$responseCookies = queryString(x$cookies)
    ans$content = orNA(x$content$text)
    ans$mimeType = orNA(x$content$mimeType)
    ans$responseEncoding = orNA(x$content$encoding)    
    
    asDF1(ans)
}

capitalize =
function(x)
    paste0(toupper(substring(x, 1, 1)), substring(x, 2))
