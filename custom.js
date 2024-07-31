document.addEventListener("DOMContentLoaded", function() {
  var header = document.querySelector(".book-header.fixed");
  if (header) {
    header.innerHTML = `
      <a class="btn pull-left js-toolbar-action" aria-label="Toggle Sidebar" title="Toggle Sidebar" href="#">
        <i class="fa fa-align-justify"></i>
      </a>
      <a class="btn pull-left js-toolbar-action" aria-label="Search" title="Search" href="#">
        <i class="fa fa-search"></i>
      </a>
    `;
  }
});