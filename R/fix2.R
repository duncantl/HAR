asIs = c("_workerFetchStart", "_workerReady", "_workerRespondWithSettled", 
         "_workerStart", "postData", "queryString", "requestCookies", 
         "requestHeaders", "responseCookies", "responseHeaders")

colTypes = list(character = c("connection", "content", "httpVersion", "method", 
                              "mimeType", "pageref", "responseEncoding", "responseHttpVersion", 
                              "responseRedirectURL", "responseStatusText", "serverIPAddress", 
                              "startedDateTime", "url"),
                list = c("_workerFetchStart", "_workerReady", 
                         "_workerRespondWithSettled", "_workerStart", "postData", "queryString", 
                         "requestCookies", "requestHeaders", "responseCookies", "responseHeaders"
                         ),
                logical = "_securityState",
                numeric = c("_blocked_queueing", 
                            "blocked", "bodySize", "connect", "dns", "headersSize", "receive", 
                            "responseBodySize", "responseHeadersSize", "responseStatus", 
                            "send", "ssl", "time", "wait"))

fix2 =
function(els)
{
    varNames = unlist(colTypes)
    ans = lapply(varNames, function(var) lapply(els, function(x) if(var %in% names(x)) orNA(x[[var]]) else NA))
    names(ans) = varNames
    
    w = names(colTypes) != "list"
    v = unlist(colTypes[w])

    ans[v] = lapply(ans[v], unlist)
    ans2 = as.data.frame(ans[v])
    o = colTypes$list
    ans2[o] = ans[o]
    ans2
}

