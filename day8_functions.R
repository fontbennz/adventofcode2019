

layer_values = function(layer, digits_in_layer, img_code){
  layer_digits = img_code[(layer*digits_in_layer-digits_in_layer+1) : (digits_in_layer*layer)]
  return(layer_digits)
}

layer_row_values = function(values, px_tall, px_wide){
  df = seq(1:px_tall) %>% 
    enframe(name = NULL) %>% 
    rename(layer_row = value) %>% 
    mutate(values = map(layer_row, layer_values, px_wide, values))
  return(df)
}