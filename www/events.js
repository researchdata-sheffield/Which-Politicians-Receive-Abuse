$(document).on('shiny:connected', function(event) {
  // once connected click button to load data
  $(document).ready(function() {
    $("#applyFilter").click();
    console.log("Shiny connected");
  });

});