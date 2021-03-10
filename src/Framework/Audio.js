"use strict";

exports.loadAudio = function (path) {
  return function () {
    var a = new Audio(path);
    return a;
  };
};

exports.playAudio = function (delay) {
  return function (audio) {
    return function () {
      setTimeout(
        function() { audio.cloneNode().play() }
        , delay
      )
    };
  };
};
