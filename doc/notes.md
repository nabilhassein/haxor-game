# the HAXOR game
Nabil Hassein  
8 August 2013  

# the XOR game
- n players where n is a positive integer
- players should conceptually move simultaneously
- each player gets a bit of input; all n bits are XORed
- a player gets a point for correctly predicting the outcome of the XOR
- I chose it because it is minimal -- each player gets one bit of input

# previously
- I wrote a simple Sinatra server, my first Ruby program
- But the standard HTTP request-response model works poorly for real-time games
- I failed to meet my own specification! Players didn't move simultaneously

# websockets
- full-duplex communication over a single TCP connection
- an entirely different protocol from HTTP, even though it uses port 80
- a more natural model for a real-time game than the client-server model

# implementation
- HAskell XOR game -- HAXOR game
- I wanted chat in my game anyway, so I just modified the library's example (a chat server)
- IRC style: commands start with a '/', everything else is broadcasted
- server accepts incoming requests and communicates with clients
- a different thread loops forever, sleeping for twenty seconds and then updating clients' scores
- "the world's finest imperative programming language" -- stateful program was no problem

# demo

# TODOs
1. a browser client (with buttons!) was kind of the entire point...but what is the front end I don't even
2. deployment
3. any changes that make the game more fun!

Come and commit!

# thanks for listening
github.com/nabilhassein/haxor-game

email:   nabil.hassein@gmail.com  
twitter: @NabilHassein  
