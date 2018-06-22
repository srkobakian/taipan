Shiny.addCustomMessageHandler('get_dim', function(id) {
  var img = document.getElementById("taipan_current_img");
  if(img !== null){
    Shiny.setInputValue("taipan_img_dim", [img.naturalWidth, img.naturalHeight]);
  }
});
