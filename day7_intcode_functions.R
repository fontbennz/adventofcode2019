# intcode functions for day 7

final_ampE_output = function(A, B, C, D, E, input_df){

  ampA_first_pass = TRUE
  ampB_first_pass = TRUE
  ampC_first_pass = TRUE
  ampD_first_pass = TRUE
  ampE_first_pass = TRUE
  keep_going = TRUE
  ampA_instruction_pointer_pos = 1
  ampB_instruction_pointer_pos = 1
  ampC_instruction_pointer_pos = 1
  ampD_instruction_pointer_pos = 1
  ampE_instruction_pointer_pos = 1
  ampA_controller_software = input_df
  ampB_controller_software = input_df
  ampC_controller_software = input_df
  ampD_controller_software = input_df
  ampE_controller_software = input_df
  
  while(keep_going == TRUE){
    
    # Apmlifier A
    if(ampA_first_pass){
      ampA_input = 0
      ampA_first_pass = FALSE
    } else {
      A = ampE_output
      ampA_input = NA
    }
    ampA_full_output = amplifier_fxn(A, ampA_input, pos = ampA_instruction_pointer_pos, ampA_controller_software)
    ampA_output = ampA_full_output[1] %>% unlist()
    if(ampA_output == 99){
      keep_going = FALSE
    } else {
      ampA_controller_software = ampA_full_output[2] %>% unlist()
      ampA_instruction_pointer_pos = ampA_full_output[3] %>% unlist()
      # Amplifier B
      if(ampB_first_pass){
        ampB_input = ampA_output
        ampB_first_pass = FALSE
      } else {
        B = ampA_output
        ampB_input = NA
      }
      ampB_full_output = amplifier_fxn(B, ampB_input, pos = ampB_instruction_pointer_pos, ampB_controller_software)
      ampB_output = ampB_full_output[1] %>% unlist()
      if(ampB_output == 99){
        keep_going = FALSE
      } else {
        ampB_controller_software = ampB_full_output[2] %>% unlist()
        ampB_instruction_pointer_pos = ampB_full_output[3] %>% unlist()
        # Amplifier C
        if(ampC_first_pass){
          ampC_input = ampB_output
          ampC_first_pass = FALSE
        } else {
          C = ampB_output
          ampC_input = NA
        }
        ampC_full_output = amplifier_fxn(C, ampC_input, pos = ampC_instruction_pointer_pos, ampC_controller_software)
        ampC_output = ampC_full_output[1] %>% unlist()
        if(ampC_output == 99){
          keep_going = FALSE
        } else {
          ampC_controller_software = ampC_full_output[2] %>% unlist()
          ampC_instruction_pointer_pos = ampC_full_output[3] %>% unlist()
          # Amplifier D
          if(ampD_first_pass){
            ampD_input = ampC_output
            ampD_first_pass = FALSE
          } else {
            D = ampC_output
            ampD_input = NA
          }
          ampD_full_output = amplifier_fxn(D, ampD_input, pos = ampD_instruction_pointer_pos, ampD_controller_software)
          ampD_output = ampD_full_output[1] %>% unlist()
          if(ampD_output == 99){
            keep_going = FALSE
          } else {
            ampD_controller_software = ampD_full_output[2] %>% unlist()
            ampD_instruction_pointer_pos = ampD_full_output[3] %>% unlist()
            # Amplifier E
            if(ampE_first_pass){
              ampE_input = ampD_output
              ampE_first_pass = FALSE
            } else {
              E = ampD_output
              ampE_input = NA
            }
            ampE_full_output = amplifier_fxn(E, ampE_input, pos = ampE_instruction_pointer_pos, ampE_controller_software)
            ampE_output = ampE_full_output[1] %>% unlist()
            if(ampE_output == 99){
              keep_going = FALSE
            } else {
              ampE_controller_software = ampE_full_output[2] %>% unlist()
              ampE_instruction_pointer_pos = ampE_full_output[3] %>% unlist()
              ampE_output_prev = ampE_output
            }
          }
        }
        
      }
      
    }
    
  }
  
  return(ampE_output_prev)
  
}


amplifier_fxn = function(input1, input2, pos = 1, input_df){
  inputs = c(input1, input2)
  amp_out = intcode_computer(input_val = inputs, input_df = input_df, pos)
  return(amp_out)
}



intcode_computer = function(input_val, input_df, pos = 1){
  keep_on = TRUE
  loop = 1
  inputval_counter = 1
  
  while(keep_on == TRUE){
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
    }
    
    
    # day2notation_day5notation  
    a_c = input_df[pos + 1]
    b_b = input_df[pos + 2]
    z_a = input_df[pos + 3]
    
    if(mode_c == 0){
      aa = input_df[a_c + 1]
    } else {
      aa = a_c
    }
    
    if(mode_b == 0){
      bb = input_df[b_b + 1]  
    } else {
      bb = b_b
    }
    
    
    cc = z_a
    
    
    
    if(op_code == 99){
      keep_on = FALSE
      output = 99
    } else if(op_code == 1){
      c = aa + bb
      input_df[z_a+1] = c
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        output = 99
        keep_on = FALSE
      }
    } else if(op_code == 2){
      c = aa * bb
      input_df[z_a+1] = c
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        output = 99
        keep_on = FALSE
      }
    } else if(op_code == 3){
      input_df[a_c + 1] = input_val[inputval_counter]
      inputval_counter = inputval_counter + 1
      pos = pos + 2
      x = input_df[pos]
      if(x == 99){
        output = 99
        keep_on = FALSE
      }
    } else if(op_code == 4){
      output = input_df[a_c + 1]
      # print(output)
      pos = pos + 2
      x = input_df[pos]
      keep_on = FALSE
      if(x == 99){
        output = input_df[a_c + 1]
        keep_on = FALSE
      }
    } else if(op_code == 5){ # use paramter a_c and b_b
      if(aa != 0){
        pos = bb + 1
      } else {
        pos = pos + 3
      }
    } else if (op_code == 6){
      if(aa == 0){
        pos = bb + 1
      } else {
        pos = pos + 3
      }
    } else if (op_code == 7){
      if(aa < bb){
        input_df[cc+1] = 1
      } else {
        input_df[cc+1] = 0
      }
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        output = 99
        keep_on = FALSE
      }
    } else if (op_code == 8){
      if(aa == bb){
        input_df[cc+1] = 1
      } else {
        input_df[cc+1] = 0
      }
      pos = pos + 4
      x = input_df[pos]
      if(x == 99){
        output = 99
        keep_on = FALSE
      }
    } else {
      output = "error"
      keep_on = FALSE
    }
    
    loop = loop + 1
    
  }
  
  outputlist = list(output, input_df, pos)
  
  return(outputlist)
  
}
