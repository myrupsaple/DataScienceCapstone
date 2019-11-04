consoleInput <- function(){
        debugMode = TRUE
        # n1 = Blogs, n2 = News, n3 = Twitter (argument n for filterSpecial)
        if(debugMode){
                n1 = 2000
                n2 = 2000
                n3 = 2000
        }
        else{

                n1 = 20
                n2 = 20
                n3 = 8
        }
        mode = 'PC'
        ### One of the directory sets must be enabled for this script to work!!!
        ## PC Directories
        if(mode == 'PC'){
                funcDir <- 'C:/DS Capstone/DataScienceCapstone/DataScienceCapstone/'
                dataDir <- 'C:/DS Capstone/DataScienceCapstone/data/en_US/'
        }
        ## Mac Directories
        else if(mode == 'Mac'){
                funcDir <- '/Users/Riley 1/Documents/Data Science/John Hopkins/Course 10 - Capstone/DataScienceCapstone/DataScienceCapstone'
                dataDir <- '/Users/Riley 1/Documents/Data Science/John Hopkins/Course 10 - Capstone/DataScienceCapstone/data/en_US/'
        }
        setwd(funcDir)
        
        print(paste0('Debug Mode: ', debugMode, ' | OS Mode: ', mode))
        print('Starting up...')
        Sys.sleep(3)
        
        times <- data.frame(Process = character(), Time = numeric(), 
                            stringsAsFactors = FALSE)
        index <- 1
        
        start <- proc.time()
        
        source('filterSpecial.R')
        source('countWords.R')
        source('countTwo.R')
        source('countMany.R')
        
        ## Initial Reads of Datasets ##
        startSection <- proc.time()
        
        # Initial Read of Blog Data
        ptm <- proc.time()
        print('Reading Blogs Data...')
        data <- readLines(paste0(dataDir, 'en_US.blogs.txt'))
        print('Filtering Blogs Data...')
        fdata <- filterSpecial(data, n = n1)
        rm(data)
        print('Finding Blogs Single Word Counts...')
        gdata <- countWords(fdata)
        write.csv(gdata, '01_PreProcessed/Output/Blogs Single Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Blogs Two Word Counts...')
        gdata <- countTwo(fdata)
        write.csv(gdata, '01_PreProcessed/Output/Output/Blogs Two Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Blogs Three Word Counts...')
        gdata <- countMany(fdata, n = 3)
        write.csv(gdata, '01_PreProcessed/Output/Output/Blogs Three Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Blogs Four Word Counts...')
        gdata <- countMany(fdata, n = 4)
        write.csv(gdata, '01_PreProcessed/Output/Output/Blogs Four Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Blogs Five Word Counts...')
        gdata <- countMany(fdata, n = 5)
        write.csv(gdata, '01_PreProcessed/Output/Output/Blogs Five Word Counts.csv', row.names = FALSE)
        rm(fdata, gdata)
        
        time <- proc.time() - ptm
        print(paste0('Intial read of blog data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '01_Blogs'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        # Initial Read of News Data
        ptm <- proc.time()
        print('Reading News Data...')
        data <- readLines(paste0(dataDir, 'en_US.news.txt'))
        print('Filtering News Data...')
        fdata <- filterSpecial(data, n = n2)
        rm(data)
        print('Finding News Single Word Counts...')
        gdata <- countWords(fdata)
        write.csv(gdata, 'News Single Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding News Two Word Counts...')
        gdata <- countTwo(fdata)
        write.csv(gdata, 'News Two Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding News Three Word Counts...')
        gdata <- countMany(fdata, n = 3)
        write.csv(gdata, 'News Three Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding News Four Word Counts...')
        gdata <- countMany(fdata, n = 4)
        write.csv(gdata, 'News Four Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding News Five Word Counts...')
        gdata <- countMany(fdata, n = 5)
        write.csv(gdata, 'News Five Word Counts.csv', row.names = FALSE)
        rm(fdata, gdata)
        
        time <- proc.time() - ptm
        print(paste0('Intial read of news data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '01_News'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        # Initial Read of Twitter Data
        ptm <- proc.time()
        print('Reading Twitter Data...')
        data <- readLines(paste0(dataDir, 'en_US.twitter.txt'))
        print('Filtering Twitter Data...')
        fdata <- filterSpecial(data, n = n3)
        rm(data)
        print('Finding Twitter Single Word Counts...')
        gdata <- countWords(fdata)
        write.csv(gdata, 'Twitter Single Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Twitter Two Word Counts...')
        gdata <- countTwo(fdata)
        write.csv(gdata, 'Twitter Two Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Twitter Three Word Counts...')
        gdata <- countMany(fdata, n = 3)
        write.csv(gdata, 'Twitter Three Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Twitter Four Word Counts...')
        gdata <- countMany(fdata, n = 4)
        write.csv(gdata, 'Twitter Four Word Counts.csv', row.names = FALSE)
        rm(gdata)
        print('Finding Twitter Five Word Counts...')
        gdata <- countMany(fdata, n = 5)
        write.csv(gdata, 'Twitter Five Word Counts.csv', row.names = FALSE)
        rm(fdata, gdata)
        
        time <- proc.time() - ptm
        readTime <- proc.time() - startSection
        print(paste0('Intial read of Twitter data complete. Total time: ', round(time[[1]], 0), 's'))
        print(paste0('Total read time: ', round(readTime[[1]], 0), 's'))
        times[index, 1] <- '01_Twitter'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        times[index, 1] <- '01_Total'
        times[index, 2] <- round(readTime[[1]], 2)
        index = index + 1
        readline(prompt = "Press Enter to continue...")
        
        source('filter.R')
        
        ## Initial Cleaning of Datasets ##
        startSection <- proc.time()
        
        # Initial Cleaning of Blogs Data
        ptm <- proc.time()
        print('Cleaning Blogs Single Word Counts...')
        data <- read.csv('Blogs Single Word Counts.csv', colClasses = c('character', 'numeric'))
        clean <- filter(data, twoOrMoreWords = FALSE)
        write.csv(clean, 'Blogs Single Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Blogs Two Word Counts...')
        data <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Two Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Blogs Three Word Counts...')
        data <- read.csv('Blogs Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Three Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Blogs Four Word Counts...')
        data <- read.csv('Blogs Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Four Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Blogs Five Word Counts...')
        data <- read.csv('Blogs Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Blogs Five Word Counts.csv', row.names = FALSE)
        rm(data, clean)
        
        time <- proc.time() - ptm
        print(paste0('Intial cleaning of blog data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '02_Blogs'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        # Initial Cleaning of News Data
        ptm <- proc.time()
        print('Cleaning News Single Word Counts...')
        data <- read.csv('News Single Word Counts.csv', colClasses = c('character', 'numeric'))
        clean <- filter(data, twoOrMoreWords = FALSE)
        write.csv(clean, 'News Single Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning News Two Word Counts...')
        data <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Two Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning News Three Word Counts...')
        data <- read.csv('News Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Three Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning News Four Word Counts...')
        data <- read.csv('News Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Four Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning News Five Word Counts...')
        data <- read.csv('News Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'News Five Word Counts.csv', row.names = FALSE)
        rm(data, clean)
        
        time <- proc.time() - ptm
        print(paste0('Intial cleaning of news data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '02_News'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        # Initial Cleaning of Twitter Data
        ptm <- proc.time()
        print('Cleaning Twitter Single Word Counts...')
        data <- read.csv('Twitter Single Word Counts.csv', colClasses = c('character', 'numeric'))
        clean <- filter(data, twoOrMoreWords = FALSE)
        write.csv(clean, 'Twitter Single Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Twitter Two Word Counts...')
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Two Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Twitter Three Word Counts...')
        data <- read.csv('Twitter Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Three Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Twitter Four Word Counts...')
        data <- read.csv('Twitter Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Four Word Counts.csv', row.names = FALSE)
        rm(clean)
        print('Cleaning Twitter Five Word Counts...')
        data <- read.csv('Twitter Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        clean <- filter(data)
        write.csv(clean, 'Twitter Five Word Counts.csv', row.names = FALSE)
        rm(data, clean)
        
        time <- proc.time() - ptm
        cleanTime <- proc.time() - startSection
        print(paste0('Intial cleaning of Twitter data complete. Total time: ', round(time[[1]], 0), 's'))
        print(paste0('Total cleaning time: ', round(cleanTime[[1]], 0), 's'))
        times[index, 1] <- '02_Twitter'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        times[index, 1] <- '02_Total'
        times[index, 2] <- round(cleanTime[[1]], 2)
        index = index + 1
        readline(prompt = "Press Enter to continue...")
        
        ## (Optional) Top Three Sorting of Separate Data ##
        # These sets are created in case we want to customize suggestions based
        # on the type of application being used (news prose will be different
        # than Twitter prose)
        startSection <- proc.time()
        
        source('topThree.R')
        
        # Top Three Sorting of Blogs Data
        ptm <- proc.time()
        print('Top Three Sorting Blogs Two Word Counts...')
        data <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Blogs Two Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting Blogs Three Word Counts...')
        data <- read.csv('Blogs Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Blogs Three Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting Blogs Four Word Counts...')
        data <- read.csv('Blogs Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Blogs Four Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting Blogs Five Word Counts...')
        data <- read.csv('Blogs Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Blogs Five Word Counts2.csv', row.names = FALSE)
        rm(data, cleaner)
        
        time <- proc.time() - ptm
        print(paste0('Top Three sorting of blog data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '03_Blogs'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        # Top Three Sorting of News Data
        ptm <- proc.time()
        print('Top Three Sorting News Two Word Counts...')
        data <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'News Two Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting News Three Word Counts...')
        data <- read.csv('News Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'News Three Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting News Four Word Counts...')
        data <- read.csv('News Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'News Four Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting News Five Word Counts...')
        data <- read.csv('News Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'News Five Word Counts2.csv', row.names = FALSE)
        rm(data, cleaner)
        
        time <- proc.time() - ptm
        print(paste0('Top Three sorting of news data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '03_News'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        # Top Three Sorting of Twitter Data
        ptm <- proc.time()
        print('Top Three Sorting Twitter Two Word Counts...')
        data <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Twitter Two Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting Twitter Three Word Counts...')
        data <- read.csv('Twitter Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Twitter Three Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting Twitter Four Word Counts...')
        data <- read.csv('Twitter Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Twitter Four Word Counts2.csv', row.names = FALSE)
        rm(cleaner)
        print('Top Three Sorting Twitter Five Word Counts...')
        data <- read.csv('Twitter Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        cleaner <- topThree(data)
        write.csv(cleaner, 'Twitter Five Word Counts2.csv', row.names = FALSE)
        rm(data, cleaner)
        
        time <- proc.time() - ptm
        sortTime <- proc.time() - startSection
        print(paste0('Top Three sorting of Twitter data complete. Total time: ', round(time[[1]], 0), 's'))
        print(paste0('Total sorting time: ', round(sortTime[[1]], 0), 's'))
        times[index, 1] <- '03_Twitter'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        times[index, 1] <- '03_Total'
        times[index, 2] <- round(sortTime[[1]], 2)
        index = index + 1
        readline(prompt = "Press Enter to continue...")
        
        ## Merge Datasets of the Same Word Count Number ##
        startSection <- proc.time()
        
        source('mergeSetsSingles.R')
        source('mergeSets.R')
        
        # This uses the datasets output directly from the filter function, and
        # not the 'top three' datasets generated above
        ptm <- proc.time()
        blogs <- read.csv('Blogs Single Word Counts.csv', colClasses = c('character', 'numeric'))
        news <- read.csv('News Single Word Counts.csv', colClasses = c('character', 'numeric'))
        twitter <- read.csv('Twitter Single Word Counts.csv', colClasses = c('character', 'numeric'))
        print('Merging Single Word Counts...')
        merged <- mergeSetsSingles(blogs, news, twitter)
        rm(blogs, news, twitter)
        write.csv(merged, 'Master Single Word Counts.csv', row.names = FALSE)
        rm(merged)
        
        time <- proc.time() - ptm
        print(paste0('Merging of single word data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '04_Singles'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Merging Two Word Counts...')
        merged <- mergeSets(blogs, news, twitter)
        rm(blogs, news, twitter)
        write.csv(merged, 'Master Two Word Counts.csv', row.names = FALSE)
        rm(merged)
        
        time <- proc.time() - ptm
        print(paste0('Merging of two word data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '04_Twos'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Merging Three Word Counts...')
        merged <- mergeSets(blogs, news, twitter)
        rm(blogs, news, twitter)
        write.csv(merged, 'Master Three Word Counts.csv', row.names = FALSE)
        rm(merged)
        
        time <- proc.time() - ptm
        print(paste0('Merging of three word data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '04_Threes'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Merging Four Word Counts...')
        merged <- mergeSets(blogs, news, twitter)
        rm(blogs, news, twitter)
        write.csv(merged, 'Master Four Word Counts.csv', row.names = FALSE)
        rm(merged)
        
        time <- proc.time() - ptm
        print(paste0('Merging of four word data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '04_Fours'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        ptm <- proc.time()
        blogs <- read.csv('Blogs Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        news <- read.csv('News Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        twitter <- read.csv('Twitter Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Merging Five Word Counts...')
        merged <- mergeSets(blogs, news, twitter)
        rm(blogs, news, twitter)
        write.csv(merged, 'Master Five Word Counts.csv', row.names = FALSE)
        rm(merged)
        
        time <- proc.time() - ptm
        mergeTime <- proc.time() - startSection
        print(paste0('Merging of five word data complete. Total time: ', round(time[[1]], 0), 's'))
        print(paste0('Total merge time: ', round(mergeTime[[1]], 0), 's'))
        times[index, 1] <- '04_Fives'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        times[index, 1] <- '04_Totals'
        times[index, 2] <- round(mergeTime[[1]], 2)
        index = index + 1
        readline(prompt = "Press Enter to continue...")
        
        ## Find Top Three of Master Datasets ##
        startSection <- proc.time()
        
        ptm <- proc.time()
        data <- read.csv('Master Two Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Top Three Sorting Two Word Counts...')
        cleaner <- topThree(data)
        rm(data)
        write.csv(cleaner, 'Master Two Word Counts.csv', row.names = FALSE)
        rm(cleaner)
        
        time <- proc.time() - ptm
        print(paste0('Top Three Sorting of two word data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '05_Twos'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        ptm <- proc.time()
        data <- read.csv('Master Three Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Top Three Sorting Three Word Counts...')
        cleaner <- topThree(data)
        rm(data)
        write.csv(cleaner, 'Master Three Word Counts.csv', row.names = FALSE)
        rm(cleaner)
        
        time <- proc.time() - ptm
        print(paste0('Top Three Sorting of three word data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '05_Threes'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        ptm <- proc.time()
        data <- read.csv('Master Four Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Top Three Sorting Four Word Counts...')
        cleaner <- topThree(data)
        rm(data)
        write.csv(cleaner, 'Master Four Word Counts.csv', row.names = FALSE)
        rm(cleaner)
        
        time <- proc.time() - ptm
        print(paste0('Top Three Sorting of four word data complete. Total time: ', round(time[[1]], 0), 's'))
        times[index, 1] <- '05_Fours'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        
        ptm <- proc.time()
        data <- read.csv('Master Five Word Counts.csv', colClasses = c('character', 'character', 'numeric'))
        print('Top Three Sorting Five Word Counts...')
        cleaner <- topThree(data)
        rm(data)
        write.csv(cleaner, 'Master Five Word Counts.csv', row.names = FALSE)
        rm(cleaner)
        
        time <- proc.time() - ptm
        sortTime <- proc.time() - startSection
        print(paste0('Top Three Sorting of five word data complete. Total time: ', round(time[[1]], 0), 's'))
        print(paste0('Total sorting time: ', round(sortTime[[1]], 0), 's'))
        times[index, 1] <- '05_Fives'
        times[index, 2] <- round(time[[1]], 2)
        index = index + 1
        times[index, 1] <- '05_Total'
        times[index, 2] <- round(sortTime[[1]], 2)
        index = index + 1
        readline(prompt = "Press Enter to continue...")
        
        ########################################################################
        
        totalTime <- proc.time() - start
        print(paste0('Total runtime: ', round(totalTime[[1]], 0), 's'))
        times[index, 1] <- 'Total'
        times[index, 2] <- round(totalTime[[1]], 2)
        index = index + 1
        
        times
}