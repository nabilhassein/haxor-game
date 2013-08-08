# the HAXOR game
Nabil Hassein  
8 August 2013  

# the XOR game
- n players where n is a positive integer
- players should conceptually move simultaneously
- each player gets a bit of input; all n bits are XORed
- a player gets a point for correctly predicting the outcome of the XOR

# previously
- I wrote a dead simple Sinatra server, my first Ruby program
- But the standard HTTP request-response model works poorly for real-time games
- I failed to meet my own specification! Players didn't move simultaneously

# the HAXOR game
- the HAskell XOR game!

# websockets
- full-duplex communication over a single TCP connection; better for real-time
- an entirely different protocol from HTTP, even though it uses port 80

# implementation
- once again, I wrote it in Haskell because HASKELL
- I wanted chat in my game anyway, so I just modified the library's example (a chat server)
- IRC style: commands start with a '/', everything else is broadcasted
- server accepts incoming requests and communicates with clients
- a different thread sleeps for twenty seconds and then handles updating the score
- stateful program full of mutable variables (MVars)
- "the world's finest imperative programming language"

# demo

# TODOs
1. a browser client was kind of the entire point...but what is the front end I don't even
2. deployment
3. optional persistent identity for matchmaking, records
4. change the mechanics to make the game more fun!

Come and commit!

# thanks for listening
github.com/nabilhassein/haxor-game

email:   nabil.hassein@gmail.com  
twitter: @NabilHassein  
