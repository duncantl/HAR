if(FALSE) {
    p = list(a = list(d = c(x = 1, y = 2)), b = list(z = list(t = 2, h = "abc"),
                                                     ff = list(e = 5, gh = 7)))
    find(p, "x")
    find(p, "h")
    find(p, "gh")        
}

find =
function(x, el, cur = character(), idx = integer())
{
    if(el %in% names(x)) {
        return(list(path = cur, index = idx))
    }


    if(!is.list(x))
        return(NULL)
    
#    print(idx)
#    print(names(x))

    # If we don't have names, the mapply() doesn't do anything as names(x) has length 0 and it makes seq(along = x)
    if(length(names(x)) == 0)
        names(x) = rep("", length(x))
    
#    if(length(idx) ==3 && all(idx == c(1, 3, 39))) browser()
#    if(length(names(x)) == 0) recover()
    ans =mapply(function(v, i) {
        #        browser()
        find(x[[i]], el, cur = c(cur, v), idx = c(idx, i))
                 },
                names(x), seq(along = x),
                SIMPLIFY = FALSE)

    #    ans[!sapply(ans, is.null)]
        ans[sapply(ans, length) > 0 ]
}
