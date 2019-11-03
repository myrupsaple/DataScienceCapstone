flip <- function(news, blogs, twitter){
        n = dim(news)[1]
        t = dim(twitter)[1]
        bl = dim(blogs)[1]
        a = length(intersect(intersect(news[, 1], twitter[, 1]), blogs[, 1]))
        b = length(intersect(twitter[, 1], blogs[, 1]))
        c = length(intersect(twitter[, 1], news[, 1]))
        d = length(intersect(news[, 1], blogs[, 1]))
        
        a = a
        b = b - a
        c = c - a
        d = d - a
        n = n - a - c - d
        t = t - a - b - c
        bl = bl - a - b - d
        total == a + b + c + d + n + bl + t
}