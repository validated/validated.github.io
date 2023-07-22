(function () {
  function init() {
    if (typeof ClipboardJS === "undefined") {
      setTimeout(init, 500);
    } else {
      new ClipboardJS(".clipboardjs");
    }
  }
  init();
})();
