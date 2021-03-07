var Main = require('../../output/Main');

function main () {
  Main.main();
}

document.addEventListener("keydown", function(e) {
  // suppress page scrolling from keyboard input
  if([32, 37, 38, 39, 40].indexOf(e.keyCode) > -1) {
    e.preventDefault();
  }
}, false);

main();
