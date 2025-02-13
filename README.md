## cl-autocorrect

Suggests corrections to your misspelled functions. Can be loaded as "autocorrect".
Should work (somewhat) in SBCL and CCL (and can be extended to other implementations by modifying a single line).

### Usage

I haven't figured out how to override the Sly `*debugger-hook*` yet, but try for yourself whether the following works:
```lisp
(setf *debugger-hook* autocorrect:autocorrecting-debugger)
```
Afterwards, when you execute a misspelled function you should see an autocorrect suggestion like this:
```lisp
(caf '(1 2))
=>
The function COMMON-LISP-USER::CAF is undefined.
   [Condition of type UNDEFINED-FUNCTION]

Restarts:
 0: [AUTOCORRECT] Replace the function with CAR.
 1: [CONTINUE] Retry calling CAF.
 2: [USE-VALUE] Call specified function.
 3: [RETURN-VALUE] Return specified values.
 4: [RETURN-NOTHING] Return zero values.
 ...
 ```
Pressing 0 here will replace `caf` with `car`, returning 1 as the result.

Locally overriding `*debugger-hook*` works in Sly, but is not really practical:
```lisp
(let ((*debugger-hook* #'autocorrect:autocorrecting-debugger))
  (caf '(1 2)))
```

### Configuration

See the `*alphabet*` and `*correction-mode*` variables.

### Improvements/TODOs

- Figure out how to install the hook globally in Sly and SLIME
- Maybe provide multiple autocorrect suggestions?
- Speed up autocorrect function
- Improve suggestion quality by assigning probabilities to suggestions: Currently all one-edit fixes take priority over two-edit fixes (and the first one that happens to work gets suggested), but this can be further improved by scanning the codebase/REPL history to get some function usage stats, or by hardcoding in some probabilities extracted from a lisp code dataset somewhere.
- Suggestions can be weighted using keyboard layout info to prioritize letters that are close together.
