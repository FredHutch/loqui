# Loqui: Shiny app for Creating Automated Courses with ari

Loqui is an open source web application that enables the creation of automated courses using ari, an R package for generating videos from text and images. Loqui takes as input a Google Slides URL, extracts the speaker notes from the slides, and converts them into an audio file. Then, it converts the Google Slides to images and ultimately, generates an mp4 video file where each image is presented with its corresponding audio.

The functionality of Loqui relies on two R packages, namely ari and text2speech, which run in the background. Although it is certainly possible to go directly to these packages and run their functions for course generation, we realize that not everyone feels comfortable programming in R. This web application offers an intuitive and user-friendly interface allowing individuals to effortlessly create automated courses without the need for programming skills.

## Getting Help

If you are confused, please open a [GitHub issue](https://github.com/FredHutch/loqui/issues/new) and let us know what you are struggling with. You can ask us questions such as "Could you provide step-by-step instructions for navigating the app?", "Where exactly is my data stored?" or "Is it possible to access additional voice options?"




