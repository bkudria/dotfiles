// Unmap proxy stuff
unmap("cp");
unmap(";cp");
unmap(";ap");

map("<Backspace>", "S");
map(";u", ";U");
map("h", "E");
map("l", "R");

map("\\ttn", "k", /./, "Scroll up [TTN]");
map("\\tts", "j", /./, "Scroll down [TTS]");
map("\\tte", "x", /./, "Close Tab [TTE]");
mapkey("\\ttw", "Scroll page down", () => Normal.scroll("fullPageDown"));

// debugger;
removeSticky = () => {
  let position;
  for (const element of document.querySelectorAll("body *")) {
    position = getComputedStyle(element).position;
    if (position === "fixed" || position === "sticky") {
      element.parentNode.removeChild(element);
    }
  }
};

mapkey("\\]", "Remove Sticky", removeSticky);
