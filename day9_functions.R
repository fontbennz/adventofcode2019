


intcode_computer_relative = function(input_val, input_df){
  keep_on = TRUE
  instr_pointer = 1
  loop = 1
  inputval_counter = 1
  relative_pos = 1
  output = c()
  
  while(keep_on == TRUE){
    op_code_instr = input_df[instr_pointer] %>% 
      as.character() %>% 
      stringr::str_split("") %>% 
      unlist() %>% 
      enframe(name = NULL)
    
    num_digits = op_code_instr %>% 
      nrow()
    
    par1_mode_row = op_code_instr %>% 
      nrow() - 2
    
    par2_mode_row = op_code_instr %>% 
      nrow() - 3
    
    par3_mode_row = op_code_instr %>% 
      nrow() - 4
    
    if(num_digits != 1){
      op_code = op_code_instr %>% 
        slice((num_digits-1):num_digits)
      op_code = paste0(op_code$value[1], op_code$value[2]) %>% 
        as.numeric()  
    } else {
      op_code = op_code_instr %>% 
        slice(num_digits)
    }
    
    
    mode_par1 = op_code_instr %>% 
      slice(par1_mode_row)
    
    mode_par2 = op_code_instr %>% 
      slice(par2_mode_row)
    
    mode_par3 = op_code_instr %>% 
      slice(par3_mode_row)
    
    if(num_digits == 4){
      mode_par3 = 0
    } else if (num_digits == 3){
      mode_par2 = 0
      mode_par3 = 0
    } else if (num_digits == 2){
      mode_par1 = 0
      mode_par2 = 0
      mode_par3 = 0
    } else if(num_digits == 1){
      mode_par1 = 0
      mode_par2 = 0
      mode_par3 = 0
    }

    par1_pos = instr_pointer + 1
    par2_pos = instr_pointer + 2
    par3_pos = instr_pointer + 3
    par1_value = input_df[par1_pos] # value of the first parameter
    par2_value = input_df[par2_pos] # value of the second parameter
    par3_value = input_df[par3_pos] # value of the third parameter
    
    if(mode_par1 == 0){ # mode 0 =  position
      par1_pos_val = input_df[par1_value + 1]
    } else if(mode_par1 == 1) { # mode 1 =  immediate
      par1_pos_val = par1_value
    } else { # mode_c = 2 # mode 2 = relative position
      par1_pos_val = input_df[relative_pos + par1_value]
    }
    
    if(mode_par2 == 0){ # mode 0 position
      par2_pos_val = input_df[par2_value + 1]  
    } else if (mode_par2 == 1) { # mode 1 immediate
      par2_pos_val = par2_value
    } else {# mode 2 relative position
      par2_pos_val = input_df[relative_pos + par2_value]
    }
    
    if(mode_par3 == 0){ # mode 0 position
      par3_pos_val = input_df[par3_value + 1]  
    } else if (mode_par3 == 1) { # mode 1 immediate
      par3_pos_val = par3_value
    } else {# mode 2 relative position
      par3_pos_val = input_df[relative_pos + par3_value]
    }

    print(paste("op code", op_code))
    print(paste("instr_pointer", instr_pointer))
    print(paste("relative pos", relative_pos))
    print(paste("mode_par1", mode_par1))
    print(paste("mode_par2", mode_par2))
    print(paste("mode_par3", mode_par3))
    
    
    if(op_code == 99){
      keep_on = FALSE
      output = output
    } else if(op_code == 1){ # add the two parameters
      op_result = par1_pos_val + par2_pos_val
      input_df[par3_pos_val+1] = op_result # then store the answer at the third parameter based on parameter mode
      instr_pointer = instr_pointer + 4
      op_code_instr = input_df[instr_pointer]
      if(op_code_instr == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if(op_code == 2){ # multipy the two parameters
      op_result = par1_pos_val * par2_pos_val
      input_df[par3_pos_val+1] = op_result
      instr_pointer = instr_pointer + 4
      op_code_instr = input_df[instr_pointer]
      if(op_code_instr == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if(op_code == 3){ # takes a single integer as input and saves it to the position of it's only parameter
      input_df[par1_pos_val] = input_val[inputval_counter]
      inputval_counter = inputval_counter + 1
      instr_pointer = instr_pointer + 2
      op_code_instr = input_df[instr_pointer]
      if(op_code_instr == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if(op_code == 4){ # outputs the value of it's only parameter
      output = c(output, par1_pos_val)
      # output = c(output, input_df[par1_value + 1])
      # print(output)
      keep_on = TRUE
      # keep_on = FALSE
      instr_pointer = instr_pointer + 2
      op_code_instr = input_df[instr_pointer]
      if(op_code_instr == 99){
        keep_on = FALSE
        output = c(output, 99)
        print("end")
      }
    } else if(op_code == 5){ # jump instruction pointer if true
      # if(par1_pos_val != 0){
      if(par1_value != 0){
        instr_pointer = par1_pos_val + 1
      } else {
        instr_pointer = instr_pointer + 3
      }
    } else if (op_code == 6){ # jump instruction pointer if false
      # if(par1_pos_val == 0){
      if(par1_value == 0){
        instr_pointer = par2_pos_val + 1
      } else {
        instr_pointer = instr_pointer + 3
      }
    } else if (op_code == 7){ # If first parameter is less than second paramter, store 1 in third parameter,otherwise store 0
      # if(par1_pos_val < par2_pos_val){
      if(par1_value < par2_value){
        input_df[par3_pos_val+1] = 1
      } else {
        input_df[par3_pos_val+1] = 0
      }
      instr_pointer = instr_pointer + 4
      op_code_instr = input_df[instr_pointer]
      if(op_code_instr == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if (op_code == 8){# If first parameter is equal to second paramter, store 1 in third parameter,otherwise store 0
      # if(par1_pos_val == par2_pos_val){
      if(par1_value == par2_value){
        input_df[par3_pos_val+1] = 1
      } else {
        input_df[par3_pos_val+1] = 0
      }
      instr_pointer = instr_pointer + 4
      op_code_instr = input_df[instr_pointer]
      if(op_code_instr == 99){
        keep_on = FALSE
        output = c(output, 99)
      }
    } else if (op_code == 9){# adjust the relative base by the value of it's only parameter
      relative_pos = relative_pos + par1_pos_val
      instr_pointer = instr_pointer + 2
      op_code_instr = input_df[instr_pointer]
      if(op_code_instr == 99){
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





