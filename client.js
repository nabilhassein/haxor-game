var scoreboard;
var play;
var bet;
var result;

function onMessage(message) {
    var event = JSON.parse(message.data);
    console.log(event);
    if(event.type === 'warning') {
        $('#warning').html('');
        $('#warning').append(event.warning);
    }
    else if(event.type === 'initialize') {
        play       = event.play;
        bet        = event.bet;
        scoreboard = event.scoreboard;
        result     = event.result;
        $('#join-section').hide();
        $('#chat-section').show();
        $('#result-section').show();
        $('#play-button').show();
        $('#bet-button').show();
        $('#scoreboard').show();
        refreshScoreboard();
        refreshResult();
        refreshButtons();
    }
    else if(event.type === 'update') {
        // not `if(event.bet)` because bet might be 0, which is falsy
        if(event.hasOwnProperty('bet')){
            bet = event.bet;
        }
        if(event.hasOwnProperty('play')){
            play = event.play;
        }
        refreshButtons();
    }
    else if(event.type === 'scoreboard') {
        scoreboard = event.scoreboard;
        result     = event.result;
        refreshScoreboard();
        refreshResult();
    }
    else if(event.type === 'joined') {
        var name = event.name;
        $('#messages').append(name + ' joined the game.');
        $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    }
    else if(event.type === 'left') {
        var name = event.name;
        $('#messages').append(name + ' left the game.');
        $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    }
    else if(event.type === 'chat') {
        var p = $(document.createElement('p')).text(event.message);
        $('#messages').append(p);
        $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    }
}

function refreshScoreboard() {
    $('#scoreboard').html('');
    for(var i in scoreboard) {
        $('#scoreboard').append(
            $(document.createElement('li')).text(i + ': ' + scoreboard[i])
            // scoreboard is an array of objects; keys are names, vals are ints
        );
    }
}

function refreshResult() {
    $('#result').html('');
    $('#result').append(result);
}

function refreshButtons() {
    $('#play-button').html('');
    $('#play-button').append(play);
    $('#bet-button').html('');
    $('#bet-button').append(bet);    
}

function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':8000' + path;
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

$(document).ready(function () {
    var ws;
    $('#join-form').submit(function () {
        $('#warnings').html('');
        var user = $('#user').val();
        ws = createWebSocket('/');
        ws.onmessage = onMessage;
        ws.onopen = function() {
            ws.send(user);
            $('#join-section').append('Connecting...');
        };
    });
    $('#message-form').submit(function () {
        var text = $('#text').val();
        ws.send(text);
        $('#text').val('');
    });
    $('#play-button').click(function () {
        ws.send(JSON.stringify(
            {play : play === 1 ? 0 : 1}
        ));
    });
    $('#bet-button').click(function () {
        ws.send(JSON.stringify(
            {bet : bet === 1 ? 0 : 1}
        ));
    });
    return false;
});

