var scoreboard;
var play;
var bet;
var result;

function onMessage(message) {
    var event = JSON.parse(message.data);
    if(event.type === 'warning') {
        $('#warning').html('');
        $('#warning').append(event.data.warning);
    }
    else if(event.type === 'initialize') {
        play       = event.data.play;
        bet        = event.data.bet;
        scoreboard = event.data.scoreboard;
        $('#join-section').hide();
        $('#chat-section').show();
        $('#result-section').show();
        $('#play-button').show();
        $('#bet-button').show();
        $('#scoreboard').show();
        refreshScoreboard();
        refreshButtons();
    }
    else if(event.type === 'update') {
    //not `if(event.data.bet|play)` because bet|play might be 0, which is falsy
        if(event.data.hasOwnProperty('bet')){
            bet = event.data.bet;
        }
        if(event.data.hasOwnProperty('play')){
            play = event.data.play;
        }
        refreshButtons();
    }
    else if(event.type === 'scoreboard') {
        scoreboard = event.data.scoreboard;
        result     = event.data.result;
        refreshScoreboard();
        refreshResult();
    }
    else if(event.type === 'joined') {
        var name = event.data.name;
        $('#messages').append(name + ' joined the game.\n');
        $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    }
    else if(event.type === 'left') {
        var name = event.data.name;
        $('#messages').append(name + ' left the game.\n');
        $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    }
    else if(event.type === 'chat') {
        var p = $(document.createElement('p')).text(event.data.message);
        $('#messages').append(p);
        $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    }
}

function refreshScoreboard() {
    $('#scoreboard').html('');
    $(scoreboard).each(showScore);
}

var showScore = function(i, elem){
    $('#scoreboard').append(
        $(document.createElement('li')).text(elem.name + ': ' + elem.score)
    );
};

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
    var port = 8080;
    var uri = 'ws://' + host + ':' + port + path;
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

