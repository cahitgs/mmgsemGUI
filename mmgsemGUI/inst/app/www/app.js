// Keep the page position sensible when moving between steps.
document.addEventListener("DOMContentLoaded", function () {
  $(document).on("shiny:inputchanged", function (event) {
    if (event.name === "step") {
      var main = document.querySelector(".main");
      if (main) main.scrollTo({ top: 0 });
    }
  });
});
