mt2 = mimeType2 =
    # Get the second part of the general/specific type
function(type)
{    
    gsub("^[^/]+/", "", sapply(strsplit(type, ";"), `[`, 1))
}
