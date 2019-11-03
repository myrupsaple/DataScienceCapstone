consoleInput <- function(){
        funcDir <- 'C:/DS Capstone/DataScienceCapstone/DataScienceCapstone/'
        dataDir <- 'C:/DS Capstone/DataScienceCapstone/data/en_US/'
        setwd(funcDir)
        
        source('filterSpecial.R')
        source('countWords.R')
        source('countTwo.R')
        source('countMany.R')
        
        ## Initial Reads of Datasets ##
        
        # Initial Read of Blog Data
        ptm <- proc.time()
        data <- readLines(paste0(dataDir, 'en_US.blogs.txt'))
        fdata <- filterSpecial(data, n = 5)
        gdata <- countWords(fdata)
        write.csv(gdata, 'Blogs Single Word Counts.csv', row.names = FALSE)
        gdata <- countTwo(fdata)
        write.csv(gdata, 'Blogs Two Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 3)
        write.csv(gdata, 'Blogs Three Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 4)
        write.csv(gdata, 'Blogs Four Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 5)
        write.csv(gdata, 'Blogs Five Word Counts.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Intial read of blog data complete. Total Time: ', time))
        
        # Initial Read of News Data
        ptm <- proc.time()
        data <- readLines(paste0(dataDir, 'en_US.news.txt'))
        fdata <- filterSpecial(data, n = 5)
        gdata <- countWords(fdata)
        write.csv(gdata, 'News Single Word Counts.csv', row.names = FALSE)
        gdata <- countTwo(fdata)
        write.csv(gdata, 'News Two Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 3)
        write.csv(gdata, 'News Three Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 4)
        write.csv(gdata, 'News Four Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 5)
        write.csv(gdata, 'News Five Word Counts.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Intial read of news data complete. Total Time: ', time))
        
        # Initial Read of Twitter Data
        ptm <- proc.time()
        data <- readLines(paste0(dataDir, 'en_US.twitter.txt'))
        fdata <- filterSpecial(data, n = 8)
        gdata <- countWords(fdata)
        write.csv(gdata, 'Twitter Single Word Counts.csv', row.names = FALSE)
        gdata <- countTwo(fdata)
        write.csv(gdata, 'Twitter Two Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 3)
        write.csv(gdata, 'Twitter Three Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 4)
        write.csv(gdata, 'Twitter Four Word Counts.csv', row.names = FALSE)
        gdata <- countMany(fdata, n = 5)
        write.csv(gdata, 'Twitter Five Word Counts.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Intial read of Twitter data complete. Total Time: ', time))
        readline(prompt = "Press Enter to continue...")
        
        source('filter.R')
        
        ## Initial Cleaning of Datasets ##
        
        # Initial Cleaning of Blogs Data
        ptm <- proc.time()
        data <- read.csv('Blogs Single Word Counts.csv', colClasses = c('character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Single Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Two Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Three Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Four Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Five Word Counts.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Intial cleaning of blog data complete. Total Time: ', time))
        
        # Initial Cleaning of News Data
        ptm <- proc.time()
        data <- read.csv('News Single Word Counts.csv', colClasses = c('character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Single Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Two Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Three Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Four Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Five Word Counts.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Intial cleaning of news data complete. Total Time: ', time))
        
        # Initial Cleaning of Twitter Data
        ptm <- proc.time()
        data <- read.csv('Twitter Single Word Counts.csv', colClasses = c('character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Single Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Two Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, )
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Four Word Counts.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Five Word Counts.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Intial cleaning of Twitter data complete. Total Time: ', time))
        readline(prompt = "Press Enter to continue...")
        
        
        
        ## (Optional) Top Three Sorting of Separate Data ##
        # These sets are created in case we want to customize suggestions based
        # on the type of application being used (news prose will be different
        # than Twitter prose)
        
        source('topThree.R')
        
        # Top Three Sorting of Blogs Data
        ptm <- proc.time()
        data <- read.csv('Blogs Single Word Counts.csv', colClasses = c('character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Blogs Single Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Blogs Two Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Blogs Three Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Blogs Four Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Blogs Five Word Counts2.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Top Three sorting of blog data complete. Total Time: ', time))
        
        # Top Three Sorting of News Data
        ptm <- proc.time()
        data <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'News Two Word Counts2.csv', row.names = FALSE)
        data <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'News Three Word Counts2.csv', row.names = FALSE)
        data <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'News Four Word Counts2.csv', row.names = FALSE)
        data <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'News Five Word Counts2.csv', row.names = FALSE)
        
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Top Three sorting of news data complete. Total Time: ', time))
        
        # Top Three Sorting of Twitter Data
        ptm <- proc.time()
        data <- read.csv('Twitter Single Word Counts.csv', colClasses = c('character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Twitter Single Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Twitter Two Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Twitter Three Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Twitter Four Word Counts2.csv', row.names = FALSE)
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- filter(data)
        write.csv(cleaner, 'Twitter Five Word Counts2.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Top Three sorting of Twitter data complete. Total Time: ', time))
        readline(prompt = "Press Enter to continue...")
        
        ## Merge Datasets of the Same Word Count Number ##
        
        source('mergeSetsSingles.R')
        source('mergeSets.R')
        
        # This uses the datasets output directly from the filter function, and
        # not the 'top three' datasets generated above
        ptm <- proc.time()
        blogs <- read.csv('Blogs Single Word Counts.csv', colClasses = c('character', 'numeric'))
        news <- read.csv('News Single Word Counts.csv', colClasses = c('character', 'numeric'))
        twitter <- read.csv('Twitter Single Word Counts.csv', colClasses = c('character', 'numeric'))
        merged <- mergeSetsSingles(blogs, news, twitter)
        write.csv(merged, 'Master Single Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Merging of single word data complete. Total Time: ', time))
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        merged <- mergeSets(blogs, news, twitter)
        write.csv(merged, 'Master Two Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Merging of two word data complete. Total Time: ', time))
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        merged <- mergeSets(blogs, news, twitter)
        write.csv(merged, 'Master Three Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Merging of three word data complete. Total Time: ', time))
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        merged <- mergeSets(blogs, news, twitter)
        write.csv(merged, 'Master Four Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Merging of four word data complete. Total Time: ', time))
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        merged <- mergeSets(blogs, news, twitter)
        write.csv(merged, 'Master Five Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Merging of five word data complete. Total Time: ', time))
        readline(prompt = "Press Enter to continue...")
        
        ## Find Top Three of Master Datasets ##
        
        ptm <- proc.time()
        data <- read.csv('Master Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Master Two Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Top Three Sorting of two word data complete. Total Time: ', time))
        
        ptm <- proc.time()
        data <- read.csv('Master Tjree Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Master Three Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Top Three Sorting of three word data complete. Total Time: ', time))
        
        ptm <- proc.time()
        data <- read.csv('Master Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Master Four Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Top Three Sorting of four word data complete. Total Time: ', time))
        
        ptm <- proc.time()
        data <- read.csv('Master Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Master Five Word Counts.csv', row.names = FALSE)
        time <- proc.time() - ptm
        time <- round(time[[1]], 0)
        print(paste0('Top Three Sorting of five word data complete. Total Time: ', time))
        readline(prompt = "Press Enter to continue...")
        
}