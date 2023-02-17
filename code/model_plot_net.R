# 'deepviz' plot for keras model
# plots deepviz network, given a keras model
# model a keras model (e.g., object of class keras.engine.sequential.Sequential)

model_plot_net <- 
     function(model){
          require(keras)
          
          # no. of nodes in shape of first layer
          inputnodes <- get_input_shape_at(model,1)[[2]]
          
          # output nodes
          outputnodes_list <- 
               lapply(model$layers, FUN = function(x) {
               get_output_shape_at(x,1)[[2]]
          })
          outputnodes <- unlist(outputnodes_list)
          
          # plot
          plot_deepviz(c(inputnodes, outputnodes))
          
          return(NULL)
          
     }