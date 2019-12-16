


intcode_computer_relative = function(input_val, input_df){
  keep_on = TRUE
  pos = 1
  loop = 1
  inputval_counter = 1
  relative_pos = 1
  output = c()
  
  while(keep_on == TRUE){
  # while(loop <  18){
    x = input_df[pos] %>% 
      as.character() %>% 
      stringr::str_split("") %>% 
      unlist() %>% 
      enframe(name = NULL)
    
    # print(x)
    
    num_digits = x %>% 
      nrow()
    
    if(num_digits == 1) {
      mode_a = 0
      mode_b = 0
      mode_c = 0
      op_code = x
    } else {
      D_row = x %>% 
        nrow() - 1
      
      C_row = x %>% 
        nrow() - 2
      
      B_row = x %>% 
        nrow() - 3
      
      val_de = x %>% 
        slice(D_row:num_digits)
      op_code = paste0(val_de$value[1], val_de$value[2]) %>% 
        as.numeric()
      
      mode_c = x %>% 
        slice(C_row)
      
      mode_b = x %>% 
        slice(B_row)
      
      mode_a = 0
      
      if(num_digits == 4){
        mode_a = 0
      } 
      
      if(num_digits == 3){
        mode_a = 0
        mode_b = 0
      }
      
      if(num_digits == 2){
        mode_a = 0
        mode_b = 0
        mode_c = 0
      }
      
      if(num_digits == 5){
        A_row = x %>% 
          nrow() - 4
        mode_a = x %>% 
          slice(A_row)
      }
    }
    
    
    # day2notation_day5notation  
    a_c = input_df[pos + 1] # value of the first parameter
    b_b = input_df[pos + 2] # value of the second parameter
    z_a = input_df[pos + 3] # value of the third parameter
    
    if(mode_c == 0){ # mode 0 position
      aa = input_df[a_c + 1]
    } else if(mode_c == 1) { # mode 1 immediate
      aa = a_c
    } else { # mode_c = 2 # mode 2 relative position
      aa = input_df[relative_pos + a_c + 1]
    }
    
    if(mode_b == 0){ # mode 0 position
      bb = input_df[b_b + 1]  
    } else if (mode_b == 1) { # mode 1 immediate
      bb = b_b
    } else {# mode 2 relative position
      bb = input_df[relative_pos + b_b + 1]
    }
    
    # if(mode_a == 0){ # mode 0 position
    #   cc = input_df[z_a + 1]
    # } else if (mode_a == 1) { # mode 1 immediate
    cc = z_a
    # } else {# mode 2 relative position
    #   cc = input_df[relative_pos + z_a + 1]
    # }
    
    # if(mode_a == 2){
    #   cc = input_df[relative_pos + z_a]
    # } else {
    #   cc = z_a
    # }
    
    

    print(paste("op code", op_code))
    print(paste("pos", pos))
    print(paste("relative pos", relative_pos))
    print(paste("mode_c", mode_c))
    print(paste("mode_b", mode_b))
    print(paste("mode_a", mode_a))
    
    
    if(op_code == 99){
      keep_on = FALSE
      output = output
    } else if(op_code == 1){ # add the two parameters
      c = aa + bb
      input_df[cc+1] = c # then store the answer at the third parameter based on parameter mode
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if(op_code == 2){ # multipy the two parameters
      c = aa * bb
      input_df[cc+1] = c
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if(op_code == 3){ # takes a single integer as input and saves it to the position of it's only parameter
      input_df[aa] = input_val[inputval_counter]
      inputval_counter = inputval_counter + 1
      pos = pos + 2
      x = input_df[pos]
      if(x == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if(op_code == 4){ # outputs the value of it's only parameter
      output = c(output, aa)
      # print(output)
      keep_on = TRUE
      # keep_on = FALSE
      pos = pos + 2
      x = input_df[pos]
      if(x == 99){
        keep_on = FALSE
        output = c(output, 99)
        print("end")
      }
    } else if(op_code == 5){ # use paramter a_c and b_b # jump instruction pointer if true
      if(aa != 0){
        # if(mode_b == 2){
        #   pos = bb
        # } else {
        #   pos = bb + 1  
        # }
        pos = bb + 1
      } else {
        pos = pos + 3
      }
    } else if (op_code == 6){ # jump instruction pointer if false
      if(aa == 0){
        # if(mode_b == 2){
        #   pos = bb
        # } else {
        #   pos = bb + 1  
        # }
        pos = bb + 1
      } else {
        pos = pos + 3
      }
    } else if (op_code == 7){ # If first parameter is less than second paramter, store 1 in third parameter,otherwise store 0
      if(aa < bb){
        input_df[cc+1] = 1
      } else {
        input_df[cc+1] = 0
      }
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if (op_code == 8){# If first parameter is equal to second paramter, store 1 in third parameter,otherwise store 0
      if(aa == bb){
        input_df[cc+1] = 1
      } else {
        input_df[cc+1] = 0
      }
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if (op_code == 9){# adjust the realative base by the value of it's only parameter
      # if(mode_c == 0){ # 0 position mode
      #   relative_pos = relative_pos + input_df[a_c + 1]
      # } else if(mode_c == 1) { # 1 immediate mode
      #   relative_pos = relative_pos + a_c
      # } else { # mode_c = 2 # relative position mode
      #   #relative_pos = input_df[relative_pos + a_c]
      #   relative_pos = relative_pos + input_df[relative_pos + a_c]
      # }
      
      # if(mode_c == 0){ # mode 0 position
      #   aa = input_df[a_c + 1]
      # } else if(mode_c == 1) { # mode 1 immediate
      #   aa = a_c
      # } else { # mode_c = 2 # mode 2 relative position
      #   aa = input_df[relative_pos + a_c + 1]
      # }
      
      
      relative_pos = relative_pos + aa
      pos = pos + 2
      x = input_df[pos]
      if(x == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else {
      output = "error"
      keep_on = FALSE
    }
    
    if(op_code == 4){
      print(paste("output = ", output))  
    } else {
      print("not an output step")
    }
    
    loop = loop + 1

  }
  
  return(output)
  
}
