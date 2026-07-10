// Keep the clicked language tab in place when switching languages.
//
// Chapters use many `.panel-tabset group="language"` tabsets. Clicking one
// language tab makes Quarto re-sync every tabset on the page (see
// site_libs/quarto-html/tabsets/tabsets.js). Tabsets above the clicked one can
// change height (R vs. Python content differ in length), which shifts the page
// and makes the tab the user just clicked jump out from under the cursor.
//
// This listener runs in the capture phase (before Quarto's own click handler),
// records where the clicked tab sits in the viewport, and — after Quarto has
// toggled the tabs and the browser has reflowed — scrolls by the delta so the
// clicked tab stays visually anchored.
document.addEventListener(
  "click",
  (event) => {
    const tab = event.target.closest("a[id^='tabset-']");
    if (!tab) return;

    const beforeTop = tab.getBoundingClientRect().top;

    // Quarto's sync runs synchronously in the bubble phase after this handler,
    // so by the next animation frame the layout reflects the new tab heights.
    requestAnimationFrame(() => {
      const afterTop = tab.getBoundingClientRect().top;
      const delta = afterTop - beforeTop;
      if (delta) {
        // "instant" overrides the site's global smooth-scroll so the
        // correction is imperceptible rather than an animated jump.
        window.scrollBy({ top: delta, behavior: "instant" });
      }
    });
  },
  true // capture: run before Quarto's tab-sync handler
);
