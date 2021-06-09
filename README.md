# FP-Project1

This project is made by Group 12 in the course 02257 Applied Functional Programming at DTU.

### Group 12 consist of:
* Daniel Larsen, s151641
* Emil Toftegaard Gæde, s164414
* Niklas Broch Thielemann, s145032
* Troels Lund, s161791

### Abstract

The topic of this project is drawing aesthetically pleasing trees given a labelled tree with nodes and their positioning. To draw any tree, there was constructed a program to translate a tree to PostScript. To test if a tree satisfied the aesthetic rules, four different properties were tested using property-based testing. Timing experiments for drawing the trees were performed as well, and finally an extension for handling long labels were made as well.

### Project structure

* PostScriptGenerator
   - Contains all code to produce the Postscript tree drawings.
   
* Runner 
    - This have been used to run performance tests and PDF creation. 

* Test  
    - Contains all property tests. 
    
* TreeManager
    - Contains all functions to manipulate trees.