(function() {
  "use strict";

  // ELM
  Elm.Main.init({ node: document.querySelector("main") });

  customElements.define(
    "rendered-html",
    class RenderedHtml extends HTMLElement {
      constructor() {
        super();
        this._content = "";
      }

      set content(value) {
        if (this._content === value) return;
        this._content = value;
        this.innerHTML = value;
      }

      get content() {
        return this._content;
      }
    }
  );

  // Service worker
  if ("serviceWorker" in navigator) {
    navigator.serviceWorker.register("./service-worker.js").then(function() {
      console.log("Service Worker Registered");
    });
  }
})();
