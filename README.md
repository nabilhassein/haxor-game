# THE XOR GAME

This program implements a game for n players where each player makes 2 choices:
which bit, zero or one, to XOR with every other player's choice of bit;
and which bit, zero or one, the player bets will be the outcome of this XOR.
The player can toggle each of these bits with a button.

The XOR takes place at a specified interval; a timer is visible to players.
A player gains a point if their bet matches the XOR at the critical moment.
Points accumulate without limit or reward. There is no notion of persistence;
no password is necessary to play, and there is no way to recover a past record.

I chose to implement this game because it is in some sense "minimal":
each player has exactly one bit of influence on the outcome. On the other hand,
in some sense every player holds the outcome of the game in her hands!

# THIS PROGRAM

Previously, I tried to implement this game using a simple HTTP server
but I quickly realized that the request-response model is insufficient
to model a game which takes place in real time. So I used WebSockets.

Now I have successfully deployed a minimal version of it, I intend to add
more game mechanics, as well as improving the design.
Ideas here are very welcome.

Improvements to security or deployment strategy are also highly appreciated.

# TODOS
1. Ensure security.
2. Add to design: short explanation of rules; timer; anything else.
3. Improve deployment.
4. Analyze performance.
5. Add new game mechanics.
