"use strict";

exports.screenX = function (e) {
  return e.screenX;
};

exports.screenY = function (e) {
  return e.screenY;
};

exports.pointerId = function(e) {
  return e.pointerId;
};

exports.clientX = function (e) {
  return e.clientX;
};

exports.clientY = function (e) {
  return e.clientY;
};

exports.offsetX = function (e) {
  return e.pageX - e.target.offsetLeft;
};

exports.offsetY = function (e) {
  return e.pageY - e.target.offsetTop;
};

exports.pageX = function (e) {
  return e.pageX;
};

exports.pageY = function (e) {
  return e.pageY;
};

exports.ctrlKey = function (e) {
  return e.ctrlKey;
};

exports.shiftKey = function (e) {
  return e.shiftKey;
};

exports.altKey = function (e) {
  return e.altKey;
};

exports.metaKey = function (e) {
  return e.metaKey;
};

exports.button = function (e) {
  return e.button;
};

exports._relatedTarget = function (e) {
  return e.relatedTarget;
};

exports.buttons = function (e) {
  return e.buttons;
};

exports.getModifierState = function (s) {
  return function (e) {
    return function () {
      return e.getModifierState(s);
    };
  };
};

