Next word prediction application
========================================================
author: Pisarev I.K.
date: 2018.09.09
autosize: true

Data Science Course Capstone Project

Project overview
========================================================
Goal of the project is to build predictive text model and create a web application with user interface for work with this model.

- Training dataset is HC Corpora, collected from publicly available sources by a web crawler.  
  The data is available here: [Coursera-SwiftKey.zip](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)   
  Total size of english three files (Blogs, News and Twitter) is about 830Mb, so we used a cumulative set of small samples
- The preparation of the text is an important part of building a predictive model.  
  The key features of the preparation were:  
  - remove all punctuation except punctuation of the end of sentence - they were replace to special keyword. All digits were replace to keyword too. This keywords were take a part in the predictive model.
  - stopwords were not remove, stemming also were not made, because this could reduce the predictive power of the model.

Alhorithm
========================================================

- We create a small sample from source files, clear and prepare the text
- Create tables of frequency for 1-gramms, 2-gramms, 3-gramms, 4-gramms, 5-gramms and 6-gramms, connect in a common table
- Repeat previous two steps enough, every time add n-gramms frequency to cumulative table
- Save final table of n-gramms frequency
- For predict next words we find 10 most frequency words, finishing n-gramms equal to input text, begin from 6-gramms, and if necessary go to 5-gramms, 4-gramms and so on


Model quality assessment
========================================================
For assessment of predictive power of the model, we split every small sample to training (900 lines) and testing (100 lines) sets.  
After adding n-gramms from current sample, we predicted every word in test set, and save ratio of successfully predicted words to total number of words.  
We can see increase of predictive power with increase of n-gramms table size:
![Test of the model](https://raw.githubusercontent.com/Pisarev-I/DS-Capstone/master/20iterations.png)

Application
========================================================

You can [run the application](https://pisarevivan.shinyapps.io/nextwordprediction)  

For get 10 predicted next words you can type some text (or not, in this case you just get 10 most frequency words in source corpus) and press Submi button.  

![Interface](https://raw.githubusercontent.com/Pisarev-I/DS-Capstone/master/Interface.png)

And you can see source at the [repository](https://github.com/Pisarev-I/DS-Capstone)
