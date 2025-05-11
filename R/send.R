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
    
    ans = getURLContent(url = url, binary = TRUE, cookie = cookie, curl = curl, customrequest = op, httpheader = headers, header = TRUE, ...)

    cvtBody(ans, ans$header)
}


# https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Headers/Content-Encoding
cvtBody =
function(ans, header = ans$header)    
{
    b = ans$body$body #XX
    enc = header["content-encoding"]
    b2 = switch(enc,
                br = brotli::brotli_decompress(b),
                gzip = Rcompression::gunzip(b),
                b
                )

    b2 = cvtContentType(b2, header["content-type"])

    attr(b2, "header") = ans$header
    b2
}

cvtContentType =
function(data, ct)    
{
    els = strsplit(ct, ";")[[1]]
    if(els[1] %in% TextContentTypes)
        rawToChar(data)
    else
        data
}

TextContentTypes =
c("text/javascript", "application/json", "application/javascript", 
  "text/css", "text/plain", "text/html", 
   "image/svg+xml" 
  )

# "font/woff2", 
# "image/x-icon", "image/vnd.microsoft.icon"
# "font/ttf",
# "image/png", "image/gif", "image/jpeg", "binary/octet-stream", 
# "image/webp",
    

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
