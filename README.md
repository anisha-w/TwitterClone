
#### Course : COP5615 - Distributed Operating Systems Principles

# TwitterClone 
Distributed Twitter-like engine that simulates various functionalities such as registering accounts, sharing tweets, and querying tweets based on hashtags, mentions and user profiles. 

## Problem Statement

The goal of this project is to create a Twitter-like engine along with a web interface to allow users to subscribe, tweet and search. Along with this a client simulator is developed in order to mimic the actions of thousands of user to measure performance of the distributed engine. The project is divided into 2 milestones. 
    
Part 1: 
* Focuses on developing the backend of the twitter engine. 
* Simulator engine is created that internally invokes the actions of a normal user like registering itself, subscribing to other users, sending tweet posts, and retweeting other users’ tweets. 

Part 2 :
* Integrate WebSockets to provide a web interface
 

#### Project Features
*  Register account
*  Send tweet. Tweets can have hashtags (e.g. #topic) and mentions (@bestuser)
*  Subscribe to user's tweets
*  Re-tweets 
*  Display the new tweets by subscribed users live (without querying).  
*  Allow querying tweets subscribed to, tweets with specific hashtags, tweets in which the user is mentioned

## System
Language : Erlang   

## Installation and Execution

### Installation
* Erlang : (on mac) 
 ```sh
 brew install erlang
 ```

* Project code : Download files 

### Execution 

#### Start Erlang Shell (erl)
```sh
erl
```
#### Compile Files
Copy below commands into the erl shell. Expected output {Link}
```sh
c(client).
c(simulator).
c(twitterserver).
```
#### Start server  
```erl
PID = twitterserver:start_server().
```
#### Simulate different actions 
Refer below examples to simulate different actions and test the backend of the system
* Creation of N user profiles  :
```erl
simulator:simulateUsers(PID , 5). % Creating 5 user profiles
```
* User1 subscribing to content of user2 
```erl
simulator:subscribe(PID,"user1","user2").
```
* User2 tweeting
```erl
simulator:tweet(PID, "user2", "Hello #topic @user3").
```
* User1 retweeting
```erl
simulator:tweet(PID, "user1", "Hello #topic @user3").
```
* Searching tweets by Hashtag/ Mention / Keyword
simulator:search(ServerPID,Query,User,QueryKeyword)
```erl
% User4 Searching by Hashtag
simulator:search(PID, "hashtag", "user4", "#topic").
```
Note : PID is a variable that stores the process id of the twitterserver (referred as the server). During initalization, the function start_server() returns the process id. Refer command {Link}

## Implementation
TwitterClone is a system designed to mimic the functionality of Twitter. It follows the Actor model paradigm, where each user profile is treated as an independent actor referred to as the client. The system includes a main engine acting as a server that keeps track of user profile names and their associated process IDs. To create a realistic simulation of the Twitter engine, a simulator engine is implemented. The simulator replicates the actions performed on a regular Twitter platform and invokes the backend functions of the client actors to simulate different actions. This simulator is useful for measuring performance metrics and scaling out the Twitter engine by generating a large number of users in bulk.

##### What is the actor model?
The actor model is a programming paradigm that provides a conceptual framework for building concurrent and distributed systems. In the actor model, an actor represents a fundamental unit of computation. Each actor encapsulates its own state, behavior, and communication mechanism. Actors can asynchronously send messages to other actors, receive messages, and modify their internal state based on incoming messages. Actors cannot access each others data or state, they can only share information through messages. 

##### Responsibilites of each Actor/process
Server Engine
1. Keep track of registered accounts
2. Keeps track of tweets    
    a. map for key:hashtag ; value:list of tweets with that hashtag and the tweeter’s usernames    
    b. map for key:mention ; value:list of tweets with that mention and the tweeter’s usernames     
3. Responds to client requests for search queries, allowing users to search for specific hashtags, mentions, or keywords within subscribed tweets.

Client
1. Tweet
2. Retweet
3. Request for search by hashtag, mention, or search for a keyword only in subscribed tweets
4. Display Feed

Simulator
1. Create N users
2. Simulate following functions      
    a. Subscribe     
    b. Tweet     
    c. Retweet      
    d. Search       

#### System Design Diagram 
<p align="center">
    <img width="548" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/3e241b5c-2d65-4eaf-a890-b6fca7a01d90">
</p>

####  Flow Diagrams 

<p align="center">
<img width="534" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/67da6e23-84aa-4370-840c-54e3a261f05a">
<img width="580" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/25428ec4-67f5-4a33-931a-0212e54934c7">
<img width="637" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/67773214-0bd6-4f76-915e-48bdfa6da022">
<img width="610" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/c8919e0b-4bf9-40a5-9219-0e3b180b39d6">
<img width="552" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/d63eab2f-6c0c-450b-9dda-56a0c1d4117d">
</p>

## Output
Below are sample outputs when executed through the simulator. 

<img width="450" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/0c78102a-2667-4a97-a687-31c1bc197d52">
