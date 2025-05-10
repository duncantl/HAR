# send or evaluate a request

send =
function(req, cookie = character(), postData = character(), curl = getCurlHandle(),
         headers = req$headers, dropEncoding = TRUE,
         response = NULL, ...)
{
    if("request" %in% names(req)) {
        response = req$response
        req = req$request
    }
    
    op = req$method

    url = req$url
    
    qry = req$queryString
    if(length(qry)) 
        url = sprintf("%s?%s", url, combine(qry, collapse = "&"))


    cookie = if(length(cookie))
                 cookie
             else
                 combine(req$cookies, collapse = "; ")
    
    url = gsub(" ", "%20", url)

    if(is.list(headers))
        headers = queryString(headers)

    if(dropEncoding)
        headers = dropEncoding(headers)
    
    browser()
    getURLContent(url = url, cookie = cookie, curl = curl, customrequest = op, httpheader = headers, ...)
}

combine =
function(x, sep = "=", collapse = "&")
{
    x = queryString(x)
    paste(names(x), x, sep = sep, collapse = collapse)
}

dropEncoding =
function(x)    
{
    i = match("Accept-Encoding", names(x))
    if(!is.na(i))
        x = x[ -i ]

    x
}
