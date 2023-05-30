
#### 
#### Course : COP5615 - Distributed Operating Systems Principles

# TwitterClone 
Distributed Twitter-like engine that simulates various functionalities such as registering accounts, sharing tweets, and querying tweets based on hashtags, mentions and user profiles

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
*	 Display the new tweets by subscribed users live (without querying).  
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

#### System Design Diagram 
<p align="center">
<img width="507" alt="system_design" src="https://github.com/anisha-w/TwitterClone/assets/36306448/333e3f73-9ea1-466e-85ad-7a39a34e52c1">
</p>

####  Flow Diagrams 

<p align="center">
<img width="533" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/00a35444-2c64-4880-b31a-2ad5988d90a7">
<img width="580" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/9f3fe648-c25d-423e-95af-e7def78fc2b0">
<img width="625" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/ef7a33c2-dadb-49af-b614-5add2bc6963b">
<img width="633" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/ab55fd12-3958-4f8f-93bc-029bfedd85f0">
<img width="559" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/2547b3f3-f6f6-47ee-9925-ef1c9f13afb2">
</p>

## Output
Below are sample outputs when executed through the simulator. 

<img width="474" alt="image" src="https://github.com/anisha-w/TwitterClone/assets/36306448/2f099287-5b97-499e-8420-3897735012b2">
