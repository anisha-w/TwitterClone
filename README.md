# TwitterClone
Distributed Twitter-like engine that simulates various functionalities such as registering accounts, sharing tweets, and querying tweets based on hashtags, mentions and user profiles. 

## Project Overview

The goal of this project is to create a Twitter-like engine along with a web interface to allow users to subscribe, tweet and search. Along with this a client simulator is developed in order to mimic the actions of thousands of user to measure performance of the distributed engine. 
  
### Project Functionalities 

* Register account
* Tweet
* Subscribe to user
* Update feed with new tweets by subscribed users dynamically
* Serach for tweets based on criterias :
  - Hashtags
  - Mentions
  - Users

### Implementation

Implemented in Erlang and using Cowboy Server. 
