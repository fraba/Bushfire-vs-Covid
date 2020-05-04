Bushfire vs Covid v1
================
Francesco Bailo
2020-05-04

  - [Bushfire](#bushfire)
      - [Sources](#sources)
  - [Coronavirus](#coronavirus)
      - [Sources](#sources-1)

``` r
library(ggplot2)
library(dplyr)
library(knitr)
```

Media Cloud data (mediacloud.org) collected on 4 May 2020.

# Bushfire

``` r
raw_bush.df <- 
  read.csv("bushfire-all-story-urls-20200504060053.csv",
           stringsAsFactors = F)
raw_bush.df$posix <- 
  as.POSIXct(raw_bush.df$publish_date, tz = "CET")
```

  - Records: 58457
  - Search term: `bushfire`
  - Media Cloud collections:
      - `Australia - National`
        <https://sources.mediacloud.org/#/collections/34412282>
      - `Australia - State & Local`
        <https://sources.mediacloud.org/#/collections/38378024>
  - From: 2019-11-01 00:18:00
  - To: 2020-04-01 23:44:49

## Sources

``` r
raw_bush.df %>%
  dplyr::group_by(media_name) %>%
  dplyr::summarize(n = n(),
                   `%` = round(n() / nrow(raw_bush.df) * 100,2)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(media_name = gsub("\\|", "~", media_name)) %>%
kable(format = 'markdown')
```

| media\_name                                                                                                                            |    n |    % |
| :------------------------------------------------------------------------------------------------------------------------------------- | ---: | ---: |
| News.com.au                                                                                                                            | 1418 | 2.43 |
| ABC New South Wales                                                                                                                    | 1244 | 2.13 |
| ABC South Australia                                                                                                                    | 1226 | 2.10 |
| ABC Queensland                                                                                                                         | 1210 | 2.07 |
| ABC Victoria                                                                                                                           | 1209 | 2.07 |
| ABC Western Australia                                                                                                                  | 1182 | 2.02 |
| The Age                                                                                                                                | 1095 | 1.87 |
| The Sydney Morning Herald                                                                                                              | 1091 | 1.87 |
| geelongadvertiser.com.au                                                                                                               | 1039 | 1.78 |
| Australian Broadcast Company (ABC)                                                                                                     | 1008 | 1.72 |
| townsvillebulletin.com.au                                                                                                              | 1008 | 1.72 |
| brisbanetimes.com.au                                                                                                                   |  938 | 1.60 |
| cairnspost.com.au                                                                                                                      |  900 | 1.54 |
| couriermail.com.au                                                                                                                     |  836 | 1.43 |
| North West News \~ Leader Newspapers North West Melbourne \~ Local Community News VIC \~ Moreland Leader \~ Hume Leader \~ Sunbury Le  |  834 | 1.43 |
| Brisbane News \~ Brisbane News \~ The Courier Mail                                                                                     |  829 | 1.42 |
| Leader Newspapers Inner East Melbourne \~ Local Community News VIC \~ Progress Leader \~ Stonnington Leader \~ Herald Sun              |  827 | 1.41 |
| Brisbane Southeast News \~ City South News \~ Southern Star \~ South East Advertiser \~ Wynnum Herald \~ East Brisbane Newspapers \~ T |  826 | 1.41 |
| caboolturenews.com.au                                                                                                                  |  825 | 1.41 |
| watoday.com.au                                                                                                                         |  818 | 1.40 |
| Brisbane North News \~ City North News \~ Northside Chronicle \~ Northwest Chronicle \~ North Brisbane Newspapers \~ The Courier Mail  |  810 | 1.39 |
| Brisbane Southwest News \~ Westside News \~ South West News \~ Springfield News \~ South Brisbane Newspapers \~ The Courier Mail       |  807 | 1.38 |
| South East & Peninsula News \~ Leader Newspapers South East Melbourne \~ Local Community News VIC \~ Moorabbin Leader \~ Dandenong L   |  805 | 1.38 |
| heraldsun.com.au                                                                                                                       |  804 | 1.38 |
| Leader \~ Leader Newspapers Melbourne \~ Melbourne Local News and Community News VIC \~ Herald Sun                                     |  802 | 1.37 |
| Moreton News \~ Redcliffe & Bayside Herald \~ Pine Rivers Press \~ Caboolture Herald \~ Northern Times \~ North Lakes Times \~ The Cou |  800 | 1.37 |
| Inner South News \~ Leader Newspapers Inner South Melbourne \~ Local Community News VIC \~ Bayside Leader \~ Frankston Leader \~ Morn  |  799 | 1.37 |
| The Weekly Times                                                                                                                       |  793 | 1.36 |
| Albert and Logan News                                                                                                                  |  788 | 1.35 |
| ABC Rural                                                                                                                              |  787 | 1.35 |
| East News \~ Leader Newspapers East Melbourne \~ Local Community News VIC \~ Manningham Leader \~ Whitehorse Leader \~ Waverley Leade  |  782 | 1.34 |
| North News \~ Leader Newspapers North Melbourne \~ Local Community News VIC \~ Preston Leader \~ Northcote Leader \~ Whittlesea Leade  |  765 | 1.31 |
| Lilydale and Yarra Valley Leader                                                                                                       |  763 | 1.31 |
| Radio Australia - Australia (Tok Pisin)                                                                                                |  761 | 1.30 |
| goldcoastbulletin.com.au                                                                                                               |  695 | 1.19 |
| <https://thewest.com.au/news/kimberley?r=1>                                                                                            |  573 | 0.98 |
| <https://thewest.com.au/news/mid-west?r=1>                                                                                             |  559 | 0.96 |
| Albany Advertiser                                                                                                                      |  558 | 0.95 |
| <https://thewest.com.au/news/south-west?r=1>                                                                                           |  555 | 0.95 |
| West Australian                                                                                                                        |  555 | 0.95 |
| Countryman                                                                                                                             |  548 | 0.94 |
| riverineherald.com.au                                                                                                                  |  535 | 0.92 |
| Daily Telegraph Australia                                                                                                              |  528 | 0.90 |
| <https://thewest.com.au/news/goldfields?r=1>                                                                                           |  520 | 0.89 |
| Liverpool Leader \~ News Local Newspaper \~ Daily Telegraph \~ Liverpool Leader \~ Daily Telegraph                                     |  503 | 0.86 |
| seymourtelegraph.com.au                                                                                                                |  500 | 0.86 |
| Macarthur Chronicle                                                                                                                    |  497 | 0.85 |
| North Shore Times \~ News Local Newspaper Daily Telegraph \~ News Local Newspapers North Shore Sydney \~ Local Community News NSW \~   |  495 | 0.85 |
| Central Coast Express Advocate Gosford and Wyong \~ News Local Newspapers \~ Daily Telegraph \~ News Local Newspapers Central Coast    |  489 | 0.84 |
| perthnow.com.au                                                                                                                        |  489 | 0.84 |
| Hills Shire Times \~ News Local Newspaper \~ Daily Telegraph \~ Hills Shire Times \~ Daily Telegraph                                   |  472 | 0.81 |
| Inner West Courier                                                                                                                     |  472 | 0.81 |
| Parramatta Advertiser                                                                                                                  |  466 | 0.80 |
| Sydney News                                                                                                                            |  460 | 0.79 |
| Manly Daily                                                                                                                            |  444 | 0.76 |
| The Guardian AU                                                                                                                        |  441 | 0.75 |
| Penrith Press \~ News Local Newspaper \~ Daily Telegraph \~ Penrith Press \~ Daily Telegraph                                           |  436 | 0.75 |
| batemansbaypost.com.au                                                                                                                 |  355 | 0.61 |
| begadistrictnews.com.au                                                                                                                |  343 | 0.59 |
| The Australian                                                                                                                         |  315 | 0.54 |
| illawarramercury.com.au                                                                                                                |  304 | 0.52 |
| The New Daily                                                                                                                          |  301 | 0.51 |
| southcoastregister.com.au                                                                                                              |  289 | 0.49 |
| southernhighlandnews.com.au                                                                                                            |  263 | 0.45 |
| Milton Ulladulla Times                                                                                                                 |  258 | 0.44 |
| Portside Messenger                                                                                                                     |  250 | 0.43 |
| 9 News                                                                                                                                 |  248 | 0.42 |
| City                                                                                                                                   |  247 | 0.42 |
| adelaidenow.com.au                                                                                                                     |  244 | 0.42 |
| Australian News.Net: Daily Australian, World & Business News                                                                           |  208 | 0.36 |
| bordermail.com.au                                                                                                                      |  198 | 0.34 |
| sunshinecoastdaily.com.au                                                                                                              |  184 | 0.31 |
| Merimbula News Weekly                                                                                                                  |  173 | 0.30 |
| Brisbane News \~ Continual Updates \~ Brisbane News.Net                                                                                |  169 | 0.29 |
| dailyadvertiser.com.au                                                                                                                 |  168 | 0.29 |
| Manning River Times                                                                                                                    |  167 | 0.29 |
| edenmagnet.com.au                                                                                                                      |  166 | 0.28 |
| Lithgow Mercury                                                                                                                        |  163 | 0.28 |
| portnews.com.au                                                                                                                        |  161 | 0.28 |
| Fox Sports Australia                                                                                                                   |  160 | 0.27 |
| Daily Mail Australia                                                                                                                   |  156 | 0.27 |
| dailyexaminer.com.au                                                                                                                   |  155 | 0.27 |
| westernadvocate.com.au                                                                                                                 |  154 | 0.26 |
| northerndailyleader.com.au                                                                                                             |  149 | 0.25 |
| Narooma News                                                                                                                           |  147 | 0.25 |
| Australian Herald: Breaking News from Australia Online                                                                                 |  146 | 0.25 |
| goulburnpost.com.au                                                                                                                    |  145 | 0.25 |
| Islander - Australia                                                                                                                   |  141 | 0.24 |
| theherald.com.au                                                                                                                       |  139 | 0.24 |
| macleayargus.com.au                                                                                                                    |  138 | 0.24 |
| dailyliberal.com.au                                                                                                                    |  136 | 0.23 |
| theland.com.au                                                                                                                         |  131 | 0.22 |
| Great Lakes Advocate                                                                                                                   |  128 | 0.22 |
| northernstar.com.au                                                                                                                    |  127 | 0.22 |
| Bombala Times                                                                                                                          |  126 | 0.22 |
| thechronicle.com.au                                                                                                                    |  124 | 0.21 |
| Central Western Daily                                                                                                                  |  118 | 0.20 |
| thecourier.com.au                                                                                                                      |  116 | 0.20 |
| Guardian News - Australia                                                                                                              |  114 | 0.20 |
| armidaleexpress.com.au                                                                                                                 |  109 | 0.19 |
| Mudgee Guardian                                                                                                                        |  108 | 0.18 |
| Sporting News                                                                                                                          |  106 | 0.18 |
| mailtimes.com.au                                                                                                                       |  105 | 0.18 |
| Braidwood Times                                                                                                                        |  100 | 0.17 |
| Huffington Post AU                                                                                                                     |   98 | 0.17 |
| Penrith City Gazette                                                                                                                   |   93 | 0.16 |
| Stock Journal                                                                                                                          |   93 | 0.16 |
| Ararat Advertiser                                                                                                                      |   89 | 0.15 |
| bendigoadvertiser.com.au                                                                                                               |   89 | 0.15 |
| Wauchope Gazette                                                                                                                       |   85 | 0.15 |
| Wingham Chronicle                                                                                                                      |   85 | 0.15 |
| Beaudesert Times                                                                                                                       |   82 | 0.14 |
| standard.net.au                                                                                                                        |   82 | 0.14 |
| stawelltimes.com.au                                                                                                                    |   80 | 0.14 |
| Oberon Review                                                                                                                          |   79 | 0.14 |
| stockandland.com.au                                                                                                                    |   79 | 0.14 |
| Blue Mountains Gazette                                                                                                                 |   78 | 0.13 |
| Camden Haven Courier                                                                                                                   |   76 | 0.13 |
| gleninnesexaminer.com.au                                                                                                               |   76 | 0.13 |
| noosanews.com.au                                                                                                                       |   76 | 0.13 |
| bellingencourier.com.au                                                                                                                |   75 | 0.13 |
| Independent Australia                                                                                                                  |   75 | 0.13 |
| theleader.com.au                                                                                                                       |   75 | 0.13 |
| gympietimes.com.au                                                                                                                     |   72 | 0.12 |
| coffscoastadvocate.com.au                                                                                                              |   71 | 0.12 |
| warwickdailynews.com.au                                                                                                                |   71 | 0.12 |
| Blayney Chronicle                                                                                                                      |   70 | 0.12 |
| Gloucester Advocate                                                                                                                    |   69 | 0.12 |
| news-mail.com.au                                                                                                                       |   69 | 0.12 |
| portlincolntimes.com.au                                                                                                                |   69 | 0.12 |
| macarthuradvertiser.com.au                                                                                                             |   68 | 0.12 |
| camdenadvertiser.com.au                                                                                                                |   67 | 0.11 |
| victorharbortimes.com.au                                                                                                               |   67 | 0.11 |
| Wollondilly Advertiser                                                                                                                 |   66 | 0.11 |
| dailymercury.com.au                                                                                                                    |   65 | 0.11 |
| Jimboomba Times                                                                                                                        |   65 | 0.11 |
| Narromine News                                                                                                                         |   64 | 0.11 |
| byronnews.com.au                                                                                                                       |   62 | 0.11 |
| echonews.com.au                                                                                                                        |   62 | 0.11 |
| Sports News Australia                                                                                                                  |   62 | 0.11 |
| Cowra Guardian                                                                                                                         |   61 | 0.10 |
| Northern Argus                                                                                                                         |   60 | 0.10 |
| Parkes Champion Post                                                                                                                   |   60 | 0.10 |
| qt.com.au                                                                                                                              |   60 | 0.10 |
| redlandcitybulletin.com.au                                                                                                             |   60 | 0.10 |
| Nyngan Observer                                                                                                                        |   59 | 0.10 |
| wellingtontimes.com.au                                                                                                                 |   59 | 0.10 |
| blacktownsun.com.au                                                                                                                    |   58 | 0.10 |
| guyraargus.com.au                                                                                                                      |   58 | 0.10 |
| Young Witness                                                                                                                          |   58 | 0.10 |
| Cootamundra Herald                                                                                                                     |   56 | 0.10 |
| echo.net.au                                                                                                                            |   56 | 0.10 |
| barossaherald.com.au                                                                                                                   |   55 | 0.09 |
| Boorowa News                                                                                                                           |   55 | 0.09 |
| whyallanewsonline.com.au                                                                                                               |   55 | 0.09 |
| portpirierecorder.com.au                                                                                                               |   54 | 0.09 |
| Walcha News                                                                                                                            |   54 | 0.09 |
| Berwick Gazette                                                                                                                        |   53 | 0.09 |
| Flinders News                                                                                                                          |   53 | 0.09 |
| Forbes Advocate                                                                                                                        |   53 | 0.09 |
| Pakenham Gazette                                                                                                                       |   53 | 0.09 |
| northqueenslandregister.com.au                                                                                                         |   52 | 0.09 |
| eyretribune.com.au                                                                                                                     |   51 | 0.09 |
| Hills News                                                                                                                             |   51 | 0.09 |
| Tenterfield Star                                                                                                                       |   51 | 0.09 |
| westcoastsentinel.com.au                                                                                                               |   51 | 0.09 |
| coastalleader.com.au                                                                                                                   |   50 | 0.09 |
| St. Marys Mt. Druitt Star                                                                                                              |   50 | 0.09 |
| Tweed Daily News                                                                                                                       |   50 | 0.09 |
| farmweekly.com.au                                                                                                                      |   49 | 0.08 |
| Naracoorte Herald                                                                                                                      |   49 | 0.08 |
| transcontinental.com.au                                                                                                                |   49 | 0.08 |
| ballinaadvocate.com.au                                                                                                                 |   48 | 0.08 |
| Hawkesbury Gazette                                                                                                                     |   48 | 0.08 |
| The Australian Independent Media Network (AIMN)                                                                                        |   48 | 0.08 |
| Advertiser - Australia                                                                                                                 |   47 | 0.08 |
| Canowindra News                                                                                                                        |   47 | 0.08 |
| Farm Online                                                                                                                            |   47 | 0.08 |
| Stanthorpe Border Post                                                                                                                 |   47 | 0.08 |
| hardenexpress.com.au                                                                                                                   |   46 | 0.08 |
| Grenfell Record                                                                                                                        |   45 | 0.08 |
| maitlandmercury.com.au                                                                                                                 |   45 | 0.08 |
| AAP                                                                                                                                    |   43 | 0.07 |
| Area News                                                                                                                              |   43 | 0.07 |
| murrayvalleystandard.com.au                                                                                                            |   43 | 0.07 |
| gladstoneobserver.com.au                                                                                                               |   42 | 0.07 |
| frasercoastchronicle.com.au                                                                                                            |   41 | 0.07 |
| Inverell Times                                                                                                                         |   41 | 0.07 |
| Crookwell Gazette                                                                                                                      |   40 | 0.07 |
| Border Chronicle                                                                                                                       |   39 | 0.07 |
| Crickey                                                                                                                                |   39 | 0.07 |
| Hunter Valley News                                                                                                                     |   39 | 0.07 |
| Lakes Mail                                                                                                                             |   38 | 0.07 |
| The Roar                                                                                                                               |   38 | 0.07 |
| Fairfield City Champion                                                                                                                |   37 | 0.06 |
| Ipswich Advertiser                                                                                                                     |   37 | 0.06 |
| bunburymail.com.au                                                                                                                     |   36 | 0.06 |
| The Australian Financial Review (AFR)                                                                                                  |   36 | 0.06 |
| Green Left Weekly                                                                                                                      |   34 | 0.06 |
| singletonargus.com.au                                                                                                                  |   34 | 0.06 |
| northweststar.com.au                                                                                                                   |   33 | 0.06 |
| Country News                                                                                                                           |   32 | 0.05 |
| esperanceexpress.com.au                                                                                                                |   32 | 0.05 |
| irrigator.com.au                                                                                                                       |   30 | 0.05 |
| mandurahmail.com.au                                                                                                                    |   30 | 0.05 |
| parramattasun.com.au                                                                                                                   |   30 | 0.05 |
| Northam News, sport and weather \~ The Avon Valley Advocate                                                                            |   29 | 0.05 |
| Muswellbrook Chronicle                                                                                                                 |   28 | 0.05 |
| yasstribune.com.au                                                                                                                     |   28 | 0.05 |
| kiamaindependent.com.au                                                                                                                |   26 | 0.04 |
| whitsundaytimes.com.au                                                                                                                 |   26 | 0.04 |
| liverpoolchampion.com.au                                                                                                               |   25 | 0.04 |
| Mountain Views Mail                                                                                                                    |   25 | 0.04 |
| Moree Champion                                                                                                                         |   24 | 0.04 |
| Scone Advocate                                                                                                                         |   24 | 0.04 |
| New Matilda                                                                                                                            |   23 | 0.04 |
| Busselton Dunsborough Mail                                                                                                             |   22 | 0.04 |
| Catholic Outlook                                                                                                                       |   22 | 0.04 |
| colliemail.com.au                                                                                                                      |   22 | 0.04 |
| Miner                                                                                                                                  |   22 | 0.04 |
| Ranges Trader Mail                                                                                                                     |   22 | 0.04 |
| Augusta Margaret River Mail                                                                                                            |   20 | 0.03 |
| Wanneroo Times                                                                                                                         |   20 | 0.03 |
| Eastern Riverina Chronicle                                                                                                             |   18 | 0.03 |
| Ferntree Gully Belgrave Mail                                                                                                           |   18 | 0.03 |
| latrobevalleyexpress.com.au                                                                                                            |   18 | 0.03 |
| wangarattachronicle.com.au                                                                                                             |   18 | 0.03 |
| Mount Evelyn Mail                                                                                                                      |   17 | 0.03 |
| Rural                                                                                                                                  |   17 | 0.03 |
| cqnews.com.au                                                                                                                          |   16 | 0.03 |
| Business News Australia                                                                                                                |   15 | 0.03 |
| hepburnadvocate.com.au                                                                                                                 |   15 | 0.03 |
| Star - Australia                                                                                                                       |   15 | 0.03 |
| Business Insider Australia                                                                                                             |   14 | 0.02 |
| District Bulletin                                                                                                                      |   14 | 0.02 |
| Riverina Leader                                                                                                                        |   14 | 0.02 |
| catholicleader.com.au                                                                                                                  |   13 | 0.02 |
| Goondiwindi Argus                                                                                                                      |   13 | 0.02 |
| starobserver.com.au                                                                                                                    |   13 | 0.02 |
| The Shovel                                                                                                                             |   13 | 0.02 |
| Alpine Observer                                                                                                                        |   12 | 0.02 |
| Blackwater Herald                                                                                                                      |   12 | 0.02 |
| Stirling Times                                                                                                                         |   12 | 0.02 |
| The Monthly                                                                                                                            |   12 | 0.02 |
| Colac Herald                                                                                                                           |   11 | 0.02 |
| donnybrookmail.com.au                                                                                                                  |   10 | 0.02 |
| Myall Cdoast News                                                                                                                      |   10 | 0.02 |
| Spec.com.au \~ News Online from Hamilton, Portland and South-West Victoria \~ Australia - News headlines from Hamilton, Portland a     |   10 | 0.02 |
| Barrier Daily Truth                                                                                                                    |    9 | 0.02 |
| Mandurah Coastal Times                                                                                                                 |    9 | 0.02 |
| radioaustralianews-km                                                                                                                  |    9 | 0.02 |
| Adviser                                                                                                                                |    8 | 0.01 |
| Geelong Indy                                                                                                                           |    8 | 0.01 |
| jewishnews.net.au                                                                                                                      |    8 | 0.01 |
| North                                                                                                                                  |      |      |
| Central Review                                                                                                                         |    8 | 0.01 |
| Radio Australia                                                                                                                        |    8 | 0.01 |
| Surf Coast Times                                                                                                                       |    8 | 0.01 |
| Campus Review                                                                                                                          |    7 | 0.01 |
| Fremantle Gazette                                                                                                                      |    7 | 0.01 |
| Southern Free Times                                                                                                                    |    7 | 0.01 |
| westernweekender.com.au                                                                                                                |    7 | 0.01 |
| businessnews.com.au                                                                                                                    |    6 | 0.01 |
| Leader - Australia                                                                                                                     |    5 | 0.01 |
| Maryborough District                                                                                                                   |      |      |
| Advertiser                                                                                                                             |    5 | 0.01 |
| Melbourne Observer                                                                                                                     |    5 | 0.01 |
| Moorabool News                                                                                                                         |    5 | 0.01 |
| Ocean Grove Voice                                                                                                                      |    5 | 0.01 |
| The Chaser                                                                                                                             |    5 | 0.01 |
| Berwick Beaconsfield News                                                                                                              |    4 | 0.01 |
| Cranbourne News                                                                                                                        |    4 | 0.01 |
| Melville Times                                                                                                                         |    4 | 0.01 |
| Orange City Life                                                                                                                       |    4 | 0.01 |
| Red Flag                                                                                                                               |    4 | 0.01 |
| The Conversation Australia                                                                                                             |    4 | 0.01 |
| The Saturday Paper                                                                                                                     |    4 | 0.01 |
| theguardian.com.au                                                                                                                     |    4 | 0.01 |
| Business Chief Australia                                                                                                               |    3 | 0.01 |
| Great Southern Star                                                                                                                    |    3 | 0.01 |
| Moyne Gazette                                                                                                                          |    3 | 0.01 |
| Northern Valleys News                                                                                                                  |    3 | 0.01 |
| Peninsula News - 27 Nov 2017                                                                                                           |    3 | 0.01 |
| South Gippsland Sentinel Times                                                                                                         |    3 | 0.01 |
| Warrandyte Diary                                                                                                                       |    3 | 0.01 |
| Bunyip                                                                                                                                 |    2 | 0.00 |
| Business Review Australia                                                                                                              |    2 | 0.00 |
| Camperdown Chronicle                                                                                                                   |    2 | 0.00 |
| Courier - Australia - South Australia                                                                                                  |    2 | 0.00 |
| Euroa Gazette                                                                                                                          |    2 | 0.00 |
| Gannawarra Times                                                                                                                       |    2 | 0.00 |
| Great Southern Weekender \~ Newspaper and Advertising                                                                                  |    2 | 0.00 |
| Hinterland Times                                                                                                                       |    2 | 0.00 |
| Toodyay Herald                                                                                                                         |    2 | 0.00 |
| Warragul and Drouin Gazette                                                                                                            |    2 | 0.00 |
| ABC Far North Queensland                                                                                                               |    1 | 0.00 |
| ABC Mid North Coast NSW                                                                                                                |    1 | 0.00 |
| ABC Sydney                                                                                                                             |    1 | 0.00 |
| Along the Grapevine                                                                                                                    |    1 | 0.00 |
| Australia Business News                                                                                                                |    1 | 0.00 |
| Cobar Weekly                                                                                                                           |    1 | 0.00 |
| Condobolin Argus                                                                                                                       |    1 | 0.00 |
| Coober Pedy Regional Times                                                                                                             |    1 | 0.00 |
| Mansfield Courier                                                                                                                      |    1 | 0.00 |
| Pakenham Officer News                                                                                                                  |    1 | 0.00 |
| Plains Producer                                                                                                                        |    1 | 0.00 |
| Tropic Now                                                                                                                             |    1 | 0.00 |
| Warrego Watchman                                                                                                                       |    1 | 0.00 |
| Westender - Australia                                                                                                                  |    1 | 0.00 |

``` r
bush_count.df <-
  raw_bush.df %>%
  dplyr::group_by(day = as.Date(posix)) %>%
  dplyr::count()
peak_day <- 
  bush_count.df$day[which.max(bush_count.df$n)]
```

  - Peak day: 2020-01-04
  - Lower boundary: 2019-11-05
  - Upper boundary: 2020-03-04

<!-- end list -->

``` r
bush_count.df %>%
ggplot(aes(x=day,y=n)) +
  geom_line() +
  geom_vline(xintercept = c(peak_day-30, peak_day+30))
```

![Frequency distribution and 60-day window around
peak](README_files/figure-gfm/unnamed-chunk-5-1.png)

# Coronavirus

``` r
raw_corona.df <- 
  read.csv("coronavirus-all-story-urls-20200504062305.csv",
           stringsAsFactors = F)
raw_corona.df$posix <- 
  as.POSIXct(raw_corona.df$publish_date, tz = "CET")
```

  - Records: 226517
  - Search term: `coronavirus`
  - Media Cloud collections:
      - `Australia - National`
        <https://sources.mediacloud.org/#/collections/34412282>
      - `Australia - State & Local`
        <https://sources.mediacloud.org/#/collections/38378024>
  - From: 2019-12-13
  - To: 2020-05-01

## Sources

``` r
raw_corona.df %>%
  dplyr::group_by(media_name) %>%
  dplyr::summarize(n = n(),
                   `%` = round(n() / nrow(raw_corona.df) * 100,2)) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::mutate(media_name = gsub("\\|", "~", media_name)) %>%
kable(format = 'markdown')
```

| media\_name                                                                                                                            |    n |    % |
| :------------------------------------------------------------------------------------------------------------------------------------- | ---: | ---: |
| Daily Mail Australia                                                                                                                   | 7927 | 3.50 |
| News.com.au                                                                                                                            | 6359 | 2.81 |
| The Sydney Morning Herald                                                                                                              | 5586 | 2.47 |
| geelongadvertiser.com.au                                                                                                               | 4558 | 2.01 |
| The Age                                                                                                                                | 4204 | 1.86 |
| townsvillebulletin.com.au                                                                                                              | 4195 | 1.85 |
| ABC New South Wales                                                                                                                    | 4038 | 1.78 |
| ABC Western Australia                                                                                                                  | 4000 | 1.77 |
| ABC Queensland                                                                                                                         | 3971 | 1.75 |
| ABC South Australia                                                                                                                    | 3970 | 1.75 |
| ABC Victoria                                                                                                                           | 3951 | 1.74 |
| Australian Broadcast Company (ABC)                                                                                                     | 3865 | 1.71 |
| The Guardian AU                                                                                                                        | 3715 | 1.64 |
| cairnspost.com.au                                                                                                                      | 3657 | 1.61 |
| heraldsun.com.au                                                                                                                       | 3621 | 1.60 |
| goldcoastbulletin.com.au                                                                                                               | 3497 | 1.54 |
| Brisbane North News \~ City North News \~ Northside Chronicle \~ Northwest Chronicle \~ North Brisbane Newspapers \~ The Courier Mail  | 3481 | 1.54 |
| Leader \~ Leader Newspapers Melbourne \~ Melbourne Local News and Community News VIC \~ Herald Sun                                     | 3382 | 1.49 |
| Brisbane Southwest News \~ Westside News \~ South West News \~ Springfield News \~ South Brisbane Newspapers \~ The Courier Mail       | 3346 | 1.48 |
| Albert and Logan News                                                                                                                  | 3324 | 1.47 |
| North West News \~ Leader Newspapers North West Melbourne \~ Local Community News VIC \~ Moreland Leader \~ Hume Leader \~ Sunbury Le  | 3307 | 1.46 |
| South East & Peninsula News \~ Leader Newspapers South East Melbourne \~ Local Community News VIC \~ Moorabbin Leader \~ Dandenong L   | 3243 | 1.43 |
| Inner South News \~ Leader Newspapers Inner South Melbourne \~ Local Community News VIC \~ Bayside Leader \~ Frankston Leader \~ Morn  | 3237 | 1.43 |
| East News \~ Leader Newspapers East Melbourne \~ Local Community News VIC \~ Manningham Leader \~ Whitehorse Leader \~ Waverley Leade  | 3234 | 1.43 |
| Brisbane News \~ Brisbane News \~ The Courier Mail                                                                                     | 3224 | 1.42 |
| North News \~ Leader Newspapers North Melbourne \~ Local Community News VIC \~ Preston Leader \~ Northcote Leader \~ Whittlesea Leade  | 3213 | 1.42 |
| Leader Newspapers Inner East Melbourne \~ Local Community News VIC \~ Progress Leader \~ Stonnington Leader \~ Herald Sun              | 3201 | 1.41 |
| Brisbane Southeast News \~ City South News \~ Southern Star \~ South East Advertiser \~ Wynnum Herald \~ East Brisbane Newspapers \~ T | 3166 | 1.40 |
| couriermail.com.au                                                                                                                     | 3165 | 1.40 |
| Lilydale and Yarra Valley Leader                                                                                                       | 3159 | 1.39 |
| Moreton News \~ Redcliffe & Bayside Herald \~ Pine Rivers Press \~ Caboolture Herald \~ Northern Times \~ North Lakes Times \~ The Cou | 3150 | 1.39 |
| caboolturenews.com.au                                                                                                                  | 3124 | 1.38 |
| The Weekly Times                                                                                                                       | 2968 | 1.31 |
| brisbanetimes.com.au                                                                                                                   | 2913 | 1.29 |
| ABC Rural                                                                                                                              | 2864 | 1.26 |
| riverineherald.com.au                                                                                                                  | 2852 | 1.26 |
| Radio Australia - Australia (Tok Pisin)                                                                                                | 2826 | 1.25 |
| seymourtelegraph.com.au                                                                                                                | 2696 | 1.19 |
| Daily Telegraph Australia                                                                                                              | 2304 | 1.02 |
| AAP                                                                                                                                    | 2302 | 1.02 |
| Business Insider Australia                                                                                                             | 2178 | 0.96 |
| Liverpool Leader \~ News Local Newspaper \~ Daily Telegraph \~ Liverpool Leader \~ Daily Telegraph                                     | 2079 | 0.92 |
| Manly Daily                                                                                                                            | 2077 | 0.92 |
| North Shore Times \~ News Local Newspaper Daily Telegraph \~ News Local Newspapers North Shore Sydney \~ Local Community News NSW \~   | 2064 | 0.91 |
| Parramatta Advertiser                                                                                                                  | 2039 | 0.90 |
| Macarthur Chronicle                                                                                                                    | 2030 | 0.90 |
| <https://thewest.com.au/news/mid-west?r=1>                                                                                             | 2015 | 0.89 |
| Central Coast Express Advocate Gosford and Wyong \~ News Local Newspapers \~ Daily Telegraph \~ News Local Newspapers Central Coast    | 1982 | 0.87 |
| Hills Shire Times \~ News Local Newspaper \~ Daily Telegraph \~ Hills Shire Times \~ Daily Telegraph                                   | 1964 | 0.87 |
| Albany Advertiser                                                                                                                      | 1954 | 0.86 |
| <https://thewest.com.au/news/kimberley?r=1>                                                                                            | 1946 | 0.86 |
| Countryman                                                                                                                             | 1935 | 0.85 |
| Inner West Courier                                                                                                                     | 1935 | 0.85 |
| <https://thewest.com.au/news/south-west?r=1>                                                                                           | 1930 | 0.85 |
| West Australian                                                                                                                        | 1924 | 0.85 |
| <https://thewest.com.au/news/goldfields?r=1>                                                                                           | 1921 | 0.85 |
| Penrith Press \~ News Local Newspaper \~ Daily Telegraph \~ Penrith Press \~ Daily Telegraph                                           | 1901 | 0.84 |
| perthnow.com.au                                                                                                                        | 1756 | 0.78 |
| 9 News                                                                                                                                 | 1735 | 0.77 |
| Fox Sports Australia                                                                                                                   | 1522 | 0.67 |
| The New Daily                                                                                                                          | 1412 | 0.62 |
| watoday.com.au                                                                                                                         | 1366 | 0.60 |
| The Australian                                                                                                                         | 1265 | 0.56 |
| Sydney News                                                                                                                            | 1140 | 0.50 |
| Australian News.Net: Daily Australian, World & Business News                                                                           |  953 | 0.42 |
| Sporting News                                                                                                                          |  793 | 0.35 |
| Brisbane News \~ Continual Updates \~ Brisbane News.Net                                                                                |  772 | 0.34 |
| Sports News Australia                                                                                                                  |  757 | 0.33 |
| Portside Messenger                                                                                                                     |  689 | 0.30 |
| adelaidenow.com.au                                                                                                                     |  685 | 0.30 |
| theherald.com.au                                                                                                                       |  631 | 0.28 |
| thecourier.com.au                                                                                                                      |  610 | 0.27 |
| sunshinecoastdaily.com.au                                                                                                              |  601 | 0.27 |
| City                                                                                                                                   |  598 | 0.26 |
| Australian Herald: Breaking News from Australia Online                                                                                 |  593 | 0.26 |
| dailymercury.com.au                                                                                                                    |  581 | 0.26 |
| illawarramercury.com.au                                                                                                                |  552 | 0.24 |
| Huffington Post AU                                                                                                                     |  517 | 0.23 |
| bordermail.com.au                                                                                                                      |  469 | 0.21 |
| Central Western Daily                                                                                                                  |  422 | 0.19 |
| bendigoadvertiser.com.au                                                                                                               |  416 | 0.18 |
| thechronicle.com.au                                                                                                                    |  400 | 0.18 |
| dailyadvertiser.com.au                                                                                                                 |  397 | 0.18 |
| standard.net.au                                                                                                                        |  380 | 0.17 |
| dailyliberal.com.au                                                                                                                    |  365 | 0.16 |
| mailtimes.com.au                                                                                                                       |  350 | 0.15 |
| northerndailyleader.com.au                                                                                                             |  343 | 0.15 |
| whitsundaytimes.com.au                                                                                                                 |  340 | 0.15 |
| frasercoastchronicle.com.au                                                                                                            |  336 | 0.15 |
| The Australian Financial Review (AFR)                                                                                                  |  336 | 0.15 |
| qt.com.au                                                                                                                              |  335 | 0.15 |
| gympietimes.com.au                                                                                                                     |  331 | 0.15 |
| jewishnews.net.au                                                                                                                      |  323 | 0.14 |
| westernadvocate.com.au                                                                                                                 |  320 | 0.14 |
| warwickdailynews.com.au                                                                                                                |  319 | 0.14 |
| news-mail.com.au                                                                                                                       |  315 | 0.14 |
| stawelltimes.com.au                                                                                                                    |  304 | 0.13 |
| Ararat Advertiser                                                                                                                      |  297 | 0.13 |
| noosanews.com.au                                                                                                                       |  282 | 0.12 |
| theleader.com.au                                                                                                                       |  267 | 0.12 |
| cqnews.com.au                                                                                                                          |  253 | 0.11 |
| northernstar.com.au                                                                                                                    |  252 | 0.11 |
| northweststar.com.au                                                                                                                   |  250 | 0.11 |
| theland.com.au                                                                                                                         |  245 | 0.11 |
| southcoastregister.com.au                                                                                                              |  239 | 0.11 |
| Blackwater Herald                                                                                                                      |  237 | 0.10 |
| dailyexaminer.com.au                                                                                                                   |  232 | 0.10 |
| Ipswich Advertiser                                                                                                                     |  217 | 0.10 |
| Manning River Times                                                                                                                    |  216 | 0.10 |
| Tweed Daily News                                                                                                                       |  214 | 0.09 |
| Mudgee Guardian                                                                                                                        |  212 | 0.09 |
| stockandland.com.au                                                                                                                    |  203 | 0.09 |
| The Roar                                                                                                                               |  202 | 0.09 |
| Great Lakes Advocate                                                                                                                   |  196 | 0.09 |
| maitlandmercury.com.au                                                                                                                 |  194 | 0.09 |
| Stanthorpe Border Post                                                                                                                 |  194 | 0.09 |
| northqueenslandregister.com.au                                                                                                         |  192 | 0.08 |
| begadistrictnews.com.au                                                                                                                |  191 | 0.08 |
| portnews.com.au                                                                                                                        |  191 | 0.08 |
| victorharbortimes.com.au                                                                                                               |  191 | 0.08 |
| Stock Journal                                                                                                                          |  190 | 0.08 |
| southernhighlandnews.com.au                                                                                                            |  189 | 0.08 |
| byronnews.com.au                                                                                                                       |  188 | 0.08 |
| mandurahmail.com.au                                                                                                                    |  187 | 0.08 |
| goulburnpost.com.au                                                                                                                    |  186 | 0.08 |
| Parkes Champion Post                                                                                                                   |  185 | 0.08 |
| batemansbaypost.com.au                                                                                                                 |  183 | 0.08 |
| ballinaadvocate.com.au                                                                                                                 |  182 | 0.08 |
| redlandcitybulletin.com.au                                                                                                             |  180 | 0.08 |
| Milton Ulladulla Times                                                                                                                 |  178 | 0.08 |
| Lithgow Mercury                                                                                                                        |  177 | 0.08 |
| portpirierecorder.com.au                                                                                                               |  176 | 0.08 |
| Cowra Guardian                                                                                                                         |  175 | 0.08 |
| Islander - Australia                                                                                                                   |  172 | 0.08 |
| Narromine News                                                                                                                         |  172 | 0.08 |
| wellingtontimes.com.au                                                                                                                 |  171 | 0.08 |
| armidaleexpress.com.au                                                                                                                 |  170 | 0.08 |
| Farm Online                                                                                                                            |  170 | 0.08 |
| whyallanewsonline.com.au                                                                                                               |  170 | 0.08 |
| Forbes Advocate                                                                                                                        |  169 | 0.07 |
| eyretribune.com.au                                                                                                                     |  168 | 0.07 |
| Young Witness                                                                                                                          |  164 | 0.07 |
| Border Chronicle                                                                                                                       |  163 | 0.07 |
| coastalleader.com.au                                                                                                                   |  163 | 0.07 |
| Nyngan Observer                                                                                                                        |  163 | 0.07 |
| portlincolntimes.com.au                                                                                                                |  163 | 0.07 |
| westcoastsentinel.com.au                                                                                                               |  161 | 0.07 |
| barossaherald.com.au                                                                                                                   |  160 | 0.07 |
| Naracoorte Herald                                                                                                                      |  160 | 0.07 |
| transcontinental.com.au                                                                                                                |  158 | 0.07 |
| Flinders News                                                                                                                          |  157 | 0.07 |
| Northern Argus                                                                                                                         |  157 | 0.07 |
| echonews.com.au                                                                                                                        |  155 | 0.07 |
| macleayargus.com.au                                                                                                                    |  155 | 0.07 |
| murrayvalleystandard.com.au                                                                                                            |  153 | 0.07 |
| Area News                                                                                                                              |  152 | 0.07 |
| Blayney Chronicle                                                                                                                      |  152 | 0.07 |
| Canowindra News                                                                                                                        |  150 | 0.07 |
| camdenadvertiser.com.au                                                                                                                |  149 | 0.07 |
| The Conversation Australia                                                                                                             |  149 | 0.07 |
| Blue Mountains Gazette                                                                                                                 |  147 | 0.06 |
| Grenfell Record                                                                                                                        |  145 | 0.06 |
| Cootamundra Herald                                                                                                                     |  144 | 0.06 |
| Oberon Review                                                                                                                          |  144 | 0.06 |
| Fairfield City Champion                                                                                                                |  142 | 0.06 |
| Jimboomba Times                                                                                                                        |  142 | 0.06 |
| Penrith City Gazette                                                                                                                   |  140 | 0.06 |
| Inverell Times                                                                                                                         |  138 | 0.06 |
| farmweekly.com.au                                                                                                                      |  137 | 0.06 |
| liverpoolchampion.com.au                                                                                                               |  137 | 0.06 |
| macarthuradvertiser.com.au                                                                                                             |  137 | 0.06 |
| sunraysiadaily.com.au                                                                                                                  |  137 | 0.06 |
| irrigator.com.au                                                                                                                       |  136 | 0.06 |
| hardenexpress.com.au                                                                                                                   |  135 | 0.06 |
| coffscoastadvocate.com.au                                                                                                              |  134 | 0.06 |
| Crickey                                                                                                                                |  134 | 0.06 |
| parramattasun.com.au                                                                                                                   |  132 | 0.06 |
| Wollondilly Advertiser                                                                                                                 |  132 | 0.06 |
| Moree Champion                                                                                                                         |  131 | 0.06 |
| Guardian News - Australia                                                                                                              |  130 | 0.06 |
| Boorowa News                                                                                                                           |  129 | 0.06 |
| Beaudesert Times                                                                                                                       |  127 | 0.06 |
| gladstoneobserver.com.au                                                                                                               |  121 | 0.05 |
| Muswellbrook Chronicle                                                                                                                 |  121 | 0.05 |
| Camden Haven Courier                                                                                                                   |  116 | 0.05 |
| Advertiser - Australia                                                                                                                 |  113 | 0.05 |
| Lakes Mail                                                                                                                             |  113 | 0.05 |
| Northam News, sport and weather \~ The Avon Valley Advocate                                                                            |  113 | 0.05 |
| singletonargus.com.au                                                                                                                  |  112 | 0.05 |
| Busselton Dunsborough Mail                                                                                                             |  111 | 0.05 |
| Hunter Valley News                                                                                                                     |  111 | 0.05 |
| Wauchope Gazette                                                                                                                       |  111 | 0.05 |
| bellingencourier.com.au                                                                                                                |  110 | 0.05 |
| Augusta Margaret River Mail                                                                                                            |  108 | 0.05 |
| kiamaindependent.com.au                                                                                                                |  108 | 0.05 |
| Merimbula News Weekly                                                                                                                  |  107 | 0.05 |
| edenmagnet.com.au                                                                                                                      |  106 | 0.05 |
| Gloucester Advocate                                                                                                                    |  106 | 0.05 |
| gleninnesexaminer.com.au                                                                                                               |  105 | 0.05 |
| Bombala Times                                                                                                                          |  103 | 0.05 |
| Narooma News                                                                                                                           |  103 | 0.05 |
| Catholic Outlook                                                                                                                       |  102 | 0.05 |
| Walcha News                                                                                                                            |  102 | 0.05 |
| Wingham Chronicle                                                                                                                      |  102 | 0.05 |
| esperanceexpress.com.au                                                                                                                |  101 | 0.04 |
| Star - Australia                                                                                                                       |  101 | 0.04 |
| Rural                                                                                                                                  |  100 | 0.04 |
| Crookwell Gazette                                                                                                                      |   99 | 0.04 |
| Eastern Riverina Chronicle                                                                                                             |   98 | 0.04 |
| Tenterfield Star                                                                                                                       |   96 | 0.04 |
| Goondiwindi Argus                                                                                                                      |   95 | 0.04 |
| Braidwood Times                                                                                                                        |   94 | 0.04 |
| Scone Advocate                                                                                                                         |   94 | 0.04 |
| bunburymail.com.au                                                                                                                     |   93 | 0.04 |
| Hills News                                                                                                                             |   93 | 0.04 |
| yasstribune.com.au                                                                                                                     |   92 | 0.04 |
| guyraargus.com.au                                                                                                                      |   91 | 0.04 |
| Hawkesbury Gazette                                                                                                                     |   90 | 0.04 |
| Business News Australia                                                                                                                |   88 | 0.04 |
| colliemail.com.au                                                                                                                      |   88 | 0.04 |
| Radio Australia                                                                                                                        |   88 | 0.04 |
| radioaustralianews-km                                                                                                                  |   88 | 0.04 |
| Berwick Gazette                                                                                                                        |   85 | 0.04 |
| Pakenham Gazette                                                                                                                       |   84 | 0.04 |
| Independent Australia                                                                                                                  |   83 | 0.04 |
| The Australian Independent Media Network (AIMN)                                                                                        |   82 | 0.04 |
| businessnews.com.au                                                                                                                    |   81 | 0.04 |
| St. Marys Mt. Druitt Star                                                                                                              |   81 | 0.04 |
| westernweekender.com.au                                                                                                                |   81 | 0.04 |
| donnybrookmail.com.au                                                                                                                  |   78 | 0.03 |
| blacktownsun.com.au                                                                                                                    |   77 | 0.03 |
| International Business Times AU                                                                                                        |   77 | 0.03 |
| hepburnadvocate.com.au                                                                                                                 |   74 | 0.03 |
| Geelong Indy                                                                                                                           |   73 | 0.03 |
| Green Left Weekly                                                                                                                      |   71 | 0.03 |
| Riverina Leader                                                                                                                        |   69 | 0.03 |
| Country News                                                                                                                           |   68 | 0.03 |
| echo.net.au                                                                                                                            |   65 | 0.03 |
| Maryborough District                                                                                                                   |      |      |
| Advertiser                                                                                                                             |   58 | 0.03 |
| Berwick Beaconsfield News                                                                                                              |   54 | 0.02 |
| catholicleader.com.au                                                                                                                  |   54 | 0.02 |
| The Chaser                                                                                                                             |   53 | 0.02 |
| Surf Coast Times                                                                                                                       |   48 | 0.02 |
| Spec.com.au \~ News Online from Hamilton, Portland and South-West Victoria \~ Australia - News headlines from Hamilton, Portland a     |   40 | 0.02 |
| latrobevalleyexpress.com.au                                                                                                            |   39 | 0.02 |
| starobserver.com.au                                                                                                                    |   38 | 0.02 |
| The Monthly                                                                                                                            |   38 | 0.02 |
| Red Flag                                                                                                                               |   35 | 0.02 |
| Colac Herald                                                                                                                           |   34 | 0.02 |
| The Shovel                                                                                                                             |   34 | 0.02 |
| Sky News Australia                                                                                                                     |   33 | 0.01 |
| Campus Review                                                                                                                          |   32 | 0.01 |
| South Gippsland Sentinel Times                                                                                                         |   29 | 0.01 |
| wangarattachronicle.com.au                                                                                                             |   27 | 0.01 |
| theguardian.com.au                                                                                                                     |   26 | 0.01 |
| North                                                                                                                                  |      |      |
| Central Review                                                                                                                         |   23 | 0.01 |
| Ferntree Gully Belgrave Mail                                                                                                           |   22 | 0.01 |
| 3aw.com.au                                                                                                                             |   21 | 0.01 |
| Business Chief Australia                                                                                                               |   21 | 0.01 |
| Mandurah Coastal Times                                                                                                                 |   21 | 0.01 |
| Orange City Life                                                                                                                       |   21 | 0.01 |
| Cranbourne News                                                                                                                        |   20 | 0.01 |
| Mount Evelyn Mail                                                                                                                      |   20 | 0.01 |
| Ranges Trader Mail                                                                                                                     |   20 | 0.01 |
| 2gb.com                                                                                                                                |   19 | 0.01 |
| Tropic Now                                                                                                                             |   19 | 0.01 |
| Myall Cdoast News                                                                                                                      |   18 | 0.01 |
| Southern Free Times                                                                                                                    |   18 | 0.01 |
| Alpine Observer                                                                                                                        |   17 | 0.01 |
| Business Review Australia                                                                                                              |   17 | 0.01 |
| Wanneroo Times                                                                                                                         |   17 | 0.01 |
| alankabout                                                                                                                             |   15 | 0.01 |
| Gannawarra Times                                                                                                                       |   15 | 0.01 |
| Westender - Australia                                                                                                                  |   15 | 0.01 |
| Melville Times                                                                                                                         |   14 | 0.01 |
| Mountain Views Mail                                                                                                                    |   14 | 0.01 |
| District Bulletin                                                                                                                      |   12 | 0.01 |
| Fremantle Gazette                                                                                                                      |   12 | 0.01 |
| Stirling Times                                                                                                                         |   12 | 0.01 |
| Warragul and Drouin Gazette                                                                                                            |   12 | 0.01 |
| Bendigo Weekly                                                                                                                         |   11 | 0.00 |
| Miner                                                                                                                                  |   10 | 0.00 |
| New Matilda                                                                                                                            |   10 | 0.00 |
| Adviser                                                                                                                                |    9 | 0.00 |
| Barrier Daily Truth                                                                                                                    |    9 | 0.00 |
| The Saturday Paper                                                                                                                     |    9 | 0.00 |
| Tribune International (Australia)                                                                                                      |    9 | 0.00 |
| Plains Producer                                                                                                                        |    8 | 0.00 |
| Courier - Australia - South Australia                                                                                                  |    6 | 0.00 |
| Leader - Australia                                                                                                                     |    6 | 0.00 |
| Bunyip                                                                                                                                 |    5 | 0.00 |
| Euroa Gazette                                                                                                                          |    5 | 0.00 |
| Moorabool News                                                                                                                         |    5 | 0.00 |
| Northern Valleys News                                                                                                                  |    5 | 0.00 |
| Ocean Grove Voice                                                                                                                      |    5 | 0.00 |
| Warrego Watchman                                                                                                                       |    4 | 0.00 |
| Australia Business News                                                                                                                |    3 | 0.00 |
| Camperdown Chronicle                                                                                                                   |    3 | 0.00 |
| Condobolin Argus                                                                                                                       |    3 | 0.00 |
| Great Southern Weekender \~ Newspaper and Advertising                                                                                  |    3 | 0.00 |
| Mansfield Courier                                                                                                                      |    3 | 0.00 |
| Melbourne Observer                                                                                                                     |    3 | 0.00 |
| themorningbulletin.com.au                                                                                                              |    3 | 0.00 |
| Cobden Timboon Coast Times                                                                                                             |    2 | 0.00 |
| Great Southern Star                                                                                                                    |    2 | 0.00 |
| Moyne Gazette                                                                                                                          |    2 | 0.00 |
| Pakenham Officer News                                                                                                                  |    2 | 0.00 |
| 4bc.com.au                                                                                                                             |    1 | 0.00 |
| ABC Broken Hill                                                                                                                        |    1 | 0.00 |
| ABC Capricornia                                                                                                                        |    1 | 0.00 |
| ABC Central Victoria                                                                                                                   |    1 | 0.00 |
| ABC Central West NSW                                                                                                                   |    1 | 0.00 |
| ABC Coffs Coast                                                                                                                        |    1 | 0.00 |
| ABC Esperance                                                                                                                          |    1 | 0.00 |
| ABC Far North Queensland                                                                                                               |    1 | 0.00 |
| ABC Gippsland                                                                                                                          |    1 | 0.00 |
| ABC Gold & Tweed Coasts - Australian Broadcasting Corporation                                                                          |    1 | 0.00 |
| ABC Goldfields Esperance WA                                                                                                            |    1 | 0.00 |
| ABC Goulburn Murray                                                                                                                    |    1 | 0.00 |
| ABC Great Southern WA                                                                                                                  |    1 | 0.00 |
| ABC Illawarra                                                                                                                          |    1 | 0.00 |
| ABC Mid North Coast NSW                                                                                                                |    1 | 0.00 |
| ABC Mid West and Wheatbelt WA                                                                                                          |    1 | 0.00 |
| ABC Mildura Swan Hill                                                                                                                  |    1 | 0.00 |
| ABC New England North West                                                                                                             |    1 | 0.00 |
| ABC NewsRadio - Breaking Australian News stories                                                                                       |    1 | 0.00 |
| ABC North and West SA                                                                                                                  |    1 | 0.00 |
| ABC North Coast NSW                                                                                                                    |    1 | 0.00 |
| ABC North Queensland                                                                                                                   |    1 | 0.00 |
| ABC North West Queensland                                                                                                              |    1 | 0.00 |
| ABC North West WA                                                                                                                      |    1 | 0.00 |
| ABC Riverina                                                                                                                           |    1 | 0.00 |
| ABC Riverland SA                                                                                                                       |    1 | 0.00 |
| ABC South East NSW                                                                                                                     |    1 | 0.00 |
| ABC South East SA                                                                                                                      |    1 | 0.00 |
| ABC South West WA                                                                                                                      |    1 | 0.00 |
| ABC Southern Queensland                                                                                                                |    1 | 0.00 |
| ABC Tropical North Queensland                                                                                                          |    1 | 0.00 |
| ABC Upper Hunter                                                                                                                       |    1 | 0.00 |
| ABC West Coast SA - Australian Broadcasting Corporation                                                                                |    1 | 0.00 |
| ABC Western Plains                                                                                                                     |    1 | 0.00 |
| ABC Western Queensland                                                                                                                 |    1 | 0.00 |
| ABC Western Victoria                                                                                                                   |    1 | 0.00 |
| ABC Wide Bay Queensland                                                                                                                |    1 | 0.00 |
| Cobar Weekly                                                                                                                           |    1 | 0.00 |
| Coober Pedy Regional Times                                                                                                             |    1 | 0.00 |
| Gorizont                                                                                                                               |    1 | 0.00 |
| Hinterland Times                                                                                                                       |    1 | 0.00 |
| Newsweekly                                                                                                                             |    1 | 0.00 |
| Ovens and Murray Advertiser                                                                                                            |    1 | 0.00 |
| TenPlay                                                                                                                                |    1 | 0.00 |
| Toodyay Herald                                                                                                                         |    1 | 0.00 |
| Warrandyte Diary                                                                                                                       |    1 | 0.00 |

``` r
corona_count.df <-
  raw_corona.df %>%
  dplyr::group_by(day = as.Date(posix)) %>%
  dplyr::count()
peak_day <- 
  corona_count.df$day[which.max(corona_count.df$n)]
```

  - Peak day: 2020-03-23
  - Lower boundary: 2020-01-23
  - Upper boundary: 2020-05-22

<!-- end list -->

``` r
corona_count.df %>%
ggplot(aes(x=day,y=n)) +
  geom_line() +
  geom_vline(xintercept = c(peak_day-30, peak_day+30))
```

![Frequency distribution and 60-day window around
peak](README_files/figure-gfm/unnamed-chunk-9-1.png)
