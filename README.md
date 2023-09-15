# Safer Route

To begin routing check out the live app [here](http://lemberg.geog.uni-heidelberg.de:3838/)!


_A Collaborative Mapping Web-App_

Welcome to the [Safer Route repository](https://github.com/GIScience/Safer-Route)! Safer Route is a collaborative mapping web application designed to enhance safety awareness for pedestrians by providing insights into the perceived safety of urban roads, paths, and routes. This README will guide you through the project, its goals, features, and how to contribute.

| Fast | Safe | updated Safe |
|---|---|---|
|![Mannheim example for a fastest route](https://github.com/GIScience/Safer-Route/blob/main/aux/faster_route.png)|![Mannheim example for a safer route](https://github.com/GIScience/Safer-Route/blob/main/aux/safer_route.png)|![Mannheim example for an updated safer route](https://github.com/GIScience/Safer-Route/blob/main/aux/updated_safer_route.png)|

## Introduction

Safer Route aims to empower users with the ability to make informed decisions about their routes in urban areas by leveraging data from various sources to assess the safety of different road segments and paths. The web-app combines both environmental data such as lighting levels from street lamps as well as user feedback on route safety. This collaborative approach ensures that the safety information provided reflects real user ratings and is up-to-date.


## Features

### 1. Interactive Mapping

The core feature of Safer Route is its interactive map interface. Users can generate routes based on different parameters for quickest routes, safest routes, or the brightest routes as well as explore urban areas, view different road segments, paths, and ways, and assess their safety levels based on color-coded indicators. 

### 2. Data Aggregation and Transformation

Safer Route aggregates data from various sources, including public street light databases and user-generated ratings. This data is then transformed into road segment level data to allow for routing based on individual road segment characteristics such as user generated safety scores and brightness. 

### 3. User Ratings and Reviews

Users can contribute to the safety perception assessment by rating and reviewing routes. This collaborative aspect allows individuals to share their experiences and insights, contributing to the overall safety evaluation. The goal here is that over time the wisdom of the masses will prevail and will generate an accurate reflection of nighttime road safety across cities.

### 4. Route Recommendations

Based on user preferences and safety concerns, Safer Route can suggest alternative routes that prioritize shortest distance, route safety ratings, or route brightness. This helps users make well-informed decisions when planning their journeys.

### 5. Route Scoring

#### 5.1 User Safety Score

The safety score for each road segment is calculated by taking the average of all user safety ratings for that segment. This score is updated every time a user rates their route. For instance, if a user travels from Point A to Point B via Road Segments X, Y, and Z, and gives the route a score of 8, the ratings for Road Segments X, Y, and Z will be updated with a new average safety scores based on all previous ratings as well as this new rating. The goal of the safety score is to reflect a collaborative consensus of the safety and comfort of road segments at night. 

#### 5.2 Brightness Score

The brightness score is based on the density of street lamps per road segment as compared to other segments of its road type. I.e. Street A is a secondary road so we calculate its brightness score as a z-score based on the mean and standard deviation of all secondary road segments in the city then transform the z-score to a percentile rank which is diplayed as a score between 1-10. For example, a road segment with a brightness z-score of 3 will have a percentile rank of 99.87 which will be displayed as a brightness score of 9.9. Whereas a road segment with a z-score of 0 will have a percentile rank of 50 and a displayed brightness score of 5, as a z-score of 0 means the route sits right at the mean brightness of road segments of its respective type. This allows us to describe the brightness of a road segment relatively to other road segments of its type in each respective city. 

## How to set it up yourself

tbd.

## License

tbd 
