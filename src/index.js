'use strict';

require ('./index.html');

var Elm = require ('./Pomodoro.elm');
var mountNode = document.getElementById ('main');

var app = Elm.Pomodoro.embed (mountNode);

app.ports.ring.subscribe(function()
{
    document.getElementById('theaudio').play();            
});
