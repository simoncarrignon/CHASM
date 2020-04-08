Collection of various scripts to explore and analyse data from twitter:
- `botdetect.R`: use botornot to check if a timeline as been generated byy a bot
- `wordscount.R`: Generate distance matrix using words usage frequencies
- `getTimeSeries.R`: Download timeline fromw twitter API and transform it into timeseries
- `rwteet/`:  should link to  my fork of [rtweet](https://github.com/ropensci/rtweet) package to integrade the `counts.json` api endpoint used to generate timeseries.
The `counts` branch should be checked out (`cd rtweet/ ; git checkout counts ; cd ..`)
When cloning this repo for the first time simply one simply does:
    ```bash
        git submodule init
        git submodule update
        cd rtweet/ ; git checkout counts ; cd ..
    ```



You can download IRA dataset [here](http://www.nimbios.org/~simon/bd4ss/data/ira_data.tgz)
