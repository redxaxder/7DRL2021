"use strict";

exports.setTextBaselineHanging = function (ctx) {
  return function () {
    ctx.textBaseline = "top";
  }
};

