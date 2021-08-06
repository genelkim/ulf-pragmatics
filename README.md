# ulf-pragmatics
Code for generating pragmatic inferences from ULFs

This is a pared down portion of the code (only including portions that are relevant for downstream projects) from the paper [Generating Discourse Inferences from Unscoped Episodic Logical Formulas](https://aclanthology.org/W19-3306/).

## Installation

1. Install SBCL
3. Install [Quicklisp](https://www.quicklisp.org/beta/) by following the link and copying the example installation session.
4. Download the latest [asdf.lisp](https://common-lisp.net/project/asdf/#downloads) file and include it in your lisp start-up script (e.g. `.sbclrc`). I recommend also overwriting `quicklisp/asdf.lisp` to eliminate the possibility of accidentally loading the out-of-date copy of `asdf.lisp` that comes with Quicklisp be default.
5. Install [Ultralisp](https://ultralisp.org/) by running `(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)` from the SBCL REPL. This must be done after installing quicklisp.
6. Load the project via quicklisp `(ql:quickload :ulf-pragmatics)`.

This project is only tested on SBCL and all of its dependencies are available through quicklisp and ultralisp.

### Dynamic Polarity

Implicative inference functions rely on polarity annotations that are obtained via Stanford CoreNLP which uses Java 8. To use these functions, install Java 8 and download the relevant jars. The jars will automatically get downloaded the first time they is needed, but this can take a while. In order to avoid this hold up in the middle of your program, the jars can be downloaded ahead of time with the following commands (from the project's root directory).
```
cd dynamic-polarity
./get-jars.sh
```


## Testing

Testing is done with lisp-unit, a simple unit testing framework. You can run all of the tests by simply loading the `run-tests.lisp` file (from the project's root directory).
```
[from the lisp REPL]
* (load "run-tests")
```
or
```
[from the command line]
$ sbcl --load run-tests.lisp
```

For adding or modifying tests, please see the
[documentation on using
lisp-unit](https://github.com/OdonataResearchLLC/lisp-unit/wiki). The interface
for defining tests is pretty simple.  You can take a look at
  `test/pilot/len-pilot-tests.lisp` for examples of it, though I used a couple of
  macros to reduce redundancy, so it might be hard to understand.

## Directory/File Organization
  `load.lisp` - Initialization script for running the inference code (I recommend using quicklisp instead).
 
  `inference-core.lisp` - Core inference functionality.
  
  `inference.lisp` - Top-level inference functionality.
  
  `*-inferences.lisp` - Code for inferences of each phenomenon.
  
  `ttt-*.lisp` - Any TTT related code.
  
  `util-*.lisp` - Utility functions that may be useful in mulitple files.
  
  `test/` - All test functions are contained in here, with more subdirectories for conceptually grouped sets of tests.
  
  `dynamic-polarity/` - Code for getting dynamic polarity marking of ULF formulas.

  `run-tests.lisp` - Loads the package and runs all the tests.

