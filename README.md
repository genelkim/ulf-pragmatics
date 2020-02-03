# ulf-pragmatics
Code for generating pragmatic inferences from ULFs

This directory contains the source code for the ULF Inference Project.

## Directory/File Organization
  `load.lisp` - Initialization script for running the inference code.
 
  `inference-core.lisp` - Core inference functionality.
  
  `inference.lisp` - Top-level inference functionality.
  
  `*-inferences.lisp` - Code for inferences of each phenomenon.
  
  `ttt-*.lisp` - Any TTT related code.
  
  `util-*.lisp` - Utility functions that may be useful in mulitple files.
  
  `test/` - All test functions are contained in here, with more subdirectories for conceptually grouped sets of tests.
  
  `dynamic-polarity/` - Code for getting dynamic polarity marking of ULF formulas.

  `run-tests.lisp` - Loads the package and runs all the tests.

## Testing

Testing is done with lisp-unit, a simple unit testing framework. Please see the
[documentation on using
lisp-unit](https://github.com/OdonataResearchLLC/lisp-unit/wiki). The interface
for defining tests is pretty simple.  You can take a look at
  test/pilot/len-pilot-tests.lisp for examples of it, though I used a couple of
  macros to reduce redundancy, so it might be hard to understand.
